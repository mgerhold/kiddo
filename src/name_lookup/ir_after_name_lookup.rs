use std::collections::HashMap;
use std::path::Path;

use bumpalo::Bump;
use hashbrown::hash_map::DefaultHashBuilder;

use crate::constants::BackseatSize;
use crate::import_resolution::representations::{NonTypeDefinition, TypeDefinition};
use crate::parser::ir_parsed;
pub(crate) use crate::parser::ir_parsed::Mutability;
pub(crate) use crate::parser::ir_parsed::NonTypeIdentifier;
pub(crate) use crate::parser::ir_parsed::TypeIdentifier;
use crate::parser::ir_parsed::{Block, Expression};

#[derive(Debug, Clone, Copy)]
pub(crate) enum Definition<'a> {
    Function(PartiallyResolvedFunctionDefinition<'a>),
    GlobalVariable(GlobalVariableDefinition<'a>),
    Struct(StructDefinition<'a>),
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct PartiallyResolvedFunctionParameter<'a> {
    pub(crate) name: &'a NonTypeIdentifier<'a>,
    pub(crate) type_: &'a PartiallyResolvedDataType<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct PartiallyResolvedFunctionDefinition<'a> {
    pub(crate) name: NonTypeIdentifier<'a>,
    pub(crate) parameters: &'a [&'a PartiallyResolvedFunctionParameter<'a>],
    pub(crate) return_type: Option<&'a PartiallyResolvedDataType<'a>>,
    pub(crate) body: Block<'a>,
    pub(crate) is_exported: bool,
}

pub(crate) type PartiallyResolvedOverloadSet<'a> =
    &'a [&'a PartiallyResolvedFunctionDefinition<'a>];

#[derive(Debug, Clone)]
pub(crate) struct CompletelyResolvedFunctionParameter<'a> {
    pub(crate) name: &'a NonTypeIdentifier<'a>,
    pub(crate) type_: &'a CompletelyResolvedDataType<'a>,
}

#[derive(Debug, Clone)]
pub(crate) struct CompletelyResolvedFunctionDefinition<'a> {
    pub(crate) name: NonTypeIdentifier<'a>,
    pub(crate) parameters: &'a [&'a CompletelyResolvedFunctionParameter<'a>],
    pub(crate) return_type: Option<&'a CompletelyResolvedDataType<'a>>,
    pub(crate) body: Block<'a>,
    pub(crate) is_exported: bool,
}

type CompletelyResolvedOverloadSet<'a> = &'a [&'a CompletelyResolvedFunctionDefinition<'a>];

#[derive(Debug, Clone, Copy)]
pub(crate) struct LocalVariableDefinition<'a> {
    pub(crate) mutability: Mutability,
    pub(crate) name: NonTypeIdentifier<'a>,
    pub(crate) type_: Option<PartiallyResolvedDataType<'a>>,
    pub(crate) initial_value: Expression<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct GlobalVariableDefinition<'a> {
    pub(crate) mutability: Mutability,
    pub(crate) name: NonTypeIdentifier<'a>,
    pub(crate) type_: Option<&'a PartiallyResolvedDataType<'a>>,
    pub(crate) initial_value: Expression<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct StructDefinition<'a> {
    pub(crate) name: TypeIdentifier<'a>,
    pub(crate) members: &'a [StructMember<'a>],
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct StructMember<'a> {
    pub(crate) name: NonTypeIdentifier<'a>,
    pub(crate) type_: &'a PartiallyResolvedDataType<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ResolvedValue<'a> {
    FunctionParameter(&'a PartiallyResolvedFunctionParameter<'a>),
    LocalVariable(&'a LocalVariableDefinition<'a>),
    FunctionOverloadSet(&'a [PartiallyResolvedFunctionDefinition<'a>]),
    GlobalVariable(&'a GlobalVariableDefinition<'a>),
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum PartiallyResolvedDataType<'a> {
    Named {
        definition: PartiallyResolvedTypeName<'a>,
    },
    Pointer {
        mutability: Mutability,
        pointee_type: &'a PartiallyResolvedDataType<'a>,
    },
    Array {
        contained_type: &'a PartiallyResolvedDataType<'a>,
        size: BackseatSize,
    },
    FunctionPointer {
        parameter_types: &'a [&'a PartiallyResolvedDataType<'a>],
        return_type: &'a PartiallyResolvedDataType<'a>,
    },
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum PartiallyResolvedTypeName<'a> {
    Struct(&'a ir_parsed::StructDefinition<'a>),
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ResolvedNonTypeName<'a> {
    FunctionParameter(&'a PartiallyResolvedFunctionParameter<'a>),
    LocalVariable(&'a LocalVariableDefinition<'a>),
    GlobalVariable(&'a GlobalVariableDefinition<'a>),
    FunctionDefinition(&'a PartiallyResolvedFunctionDefinition<'a>),
}

#[derive(Debug, Clone)]
pub(crate) struct PartiallyResolvedStructMember<'a> {
    pub(crate) name: NonTypeIdentifier<'a>,
    pub(crate) type_: &'a PartiallyResolvedDataType<'a>,
}

#[derive(Debug, Clone)]
pub(crate) struct PartiallyResolvedStructDefinition<'a> {
    pub(crate) is_exported: bool,
    pub(crate) name: TypeIdentifier<'a>,
    pub(crate) members: &'a [PartiallyResolvedStructMember<'a>],
}

#[derive(Debug, Clone)]
pub(crate) enum PartiallyResolvedTypeDefinition<'a> {
    Struct(&'a PartiallyResolvedStructDefinition<'a>),
}

#[derive(Debug, Clone)]
pub(crate) struct PartiallyResolvedGlobalVariableDefinition<'a> {
    pub(crate) is_exported: bool,
    pub(crate) mutability: Mutability,
    pub(crate) name: NonTypeIdentifier<'a>,
    pub(crate) type_: &'a PartiallyResolvedDataType<'a>,
    pub(crate) initial_value: Expression<'a>,
}

#[derive(Debug, Clone)]
pub(crate) enum PartiallyResolvedNonTypeDefinition<'a> {
    GlobalVariable(&'a PartiallyResolvedGlobalVariableDefinition<'a>),
    Function(PartiallyResolvedOverloadSet<'a>),
}

#[derive(Debug, Clone)]
pub(crate) struct CompletelyResolvedGlobalVariableDefinition<'a> {
    pub(crate) is_exported: bool,
    pub(crate) mutability: Mutability,
    pub(crate) name: NonTypeIdentifier<'a>,
    pub(crate) type_: &'a CompletelyResolvedDataType<'a>,
    pub(crate) initial_value: Expression<'a>,
}

#[derive(Debug, Clone)]
pub(crate) enum CompletelyResolvedNonTypeDefinition<'a> {
    GlobalVariable(&'a CompletelyResolvedGlobalVariableDefinition<'a>),
    Function(CompletelyResolvedOverloadSet<'a>),
}

impl CompletelyResolvedNonTypeDefinition<'_> {
    pub(crate) fn to_string(&self, type_table: &[&CompletelyResolvedTypeDefinition]) -> String {
        match self {
            CompletelyResolvedNonTypeDefinition::GlobalVariable(definition) => {
                format!(
                    "{}let {} {}: {} = {};",
                    if definition.is_exported {
                        "export "
                    } else {
                        ""
                    },
                    definition.mutability,
                    definition.name.0.lexeme(),
                    definition.type_.to_string(type_table),
                    definition.initial_value,
                )
            }
            CompletelyResolvedNonTypeDefinition::Function(definition) => format!(
                "[{}]",
                definition
                    .iter()
                    .map(|overload| {
                        format!(
                            "function {}({}){}",
                            overload.name.0.lexeme(),
                            overload
                                .parameters
                                .iter()
                                .map(|parameter| format!(
                                    "{}: {}",
                                    parameter.name.0.lexeme(),
                                    parameter.type_.to_string(type_table)
                                ))
                                .intersperse(", ".to_string())
                                .collect::<String>(),
                            match overload.return_type {
                                None => "".to_string(),
                                Some(type_) => format!(" ~> {}", type_.to_string(type_table)),
                            }
                        )
                    })
                    .intersperse(", ".to_string())
                    .collect::<String>(),
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct PartiallyResolvedModule<'a> {
    pub(crate) canonical_path: &'a Path,
    pub(crate) local_type_names: hashbrown::HashMap<
        &'a str,
        &'a PartiallyResolvedTypeDefinition<'a>,
        DefaultHashBuilder,
        &'a Bump,
    >,
    pub(crate) local_non_type_names: hashbrown::HashMap<
        &'a str,
        &'a PartiallyResolvedNonTypeDefinition<'a>,
        DefaultHashBuilder,
        &'a Bump,
    >,
    pub(crate) imported_type_names:
        hashbrown::HashMap<&'a str, &'a TypeDefinition<'a>, DefaultHashBuilder, &'a Bump>,
    pub(crate) imported_non_type_names:
        hashbrown::HashMap<&'a str, &'a NonTypeDefinition<'a>, DefaultHashBuilder, &'a Bump>,
    pub(crate) definitions: &'a [&'a ir_parsed::Definition<'a>],
}

#[derive(Debug, Clone)]
pub(crate) struct CompletelyResolvedNamedDataType<'a> {
    pub(crate) global_type_table_index: usize,
    pub(crate) name: TypeIdentifier<'a>,
}

impl<'a> CompletelyResolvedNamedDataType<'a> {
    pub(crate) fn type_definition(
        &self,
        data_types: &[&'a CompletelyResolvedTypeDefinition<'a>],
    ) -> &'a CompletelyResolvedTypeDefinition<'a> {
        data_types[self.global_type_table_index]
    }
}

#[derive(Debug, Clone)]
pub(crate) enum CompletelyResolvedDataType<'a> {
    Named(CompletelyResolvedNamedDataType<'a>),
    Pointer {
        mutability: Mutability,
        pointee_type: &'a CompletelyResolvedDataType<'a>,
    },
    Array {
        contained_type: &'a CompletelyResolvedDataType<'a>,
        size: BackseatSize,
    },
    FunctionPointer {
        parameter_types: &'a [&'a CompletelyResolvedDataType<'a>],
        return_type: &'a CompletelyResolvedDataType<'a>,
    },
}

impl CompletelyResolvedDataType<'_> {
    pub(crate) fn to_string<'a>(
        &self,
        global_type_table: &[&'a CompletelyResolvedTypeDefinition<'a>],
    ) -> String {
        match self {
            CompletelyResolvedDataType::Named(definition) => {
                match definition.type_definition(global_type_table) {
                    CompletelyResolvedTypeDefinition::Struct(definition) => {
                        definition.name.0.lexeme().to_string()
                    }
                }
            }
            CompletelyResolvedDataType::Pointer {
                mutability,
                pointee_type,
            } => {
                format!(
                    "->{} {}",
                    mutability,
                    pointee_type.to_string(global_type_table)
                )
            }
            CompletelyResolvedDataType::Array {
                contained_type,
                size,
            } => {
                format!(
                    "[{}; {}]",
                    contained_type.to_string(global_type_table),
                    size
                )
            }
            CompletelyResolvedDataType::FunctionPointer {
                parameter_types,
                return_type,
            } => {
                let joined: String = parameter_types
                    .iter()
                    .map(|type_| type_.to_string(global_type_table))
                    .intersperse(", ".to_string())
                    .collect();
                format!(
                    "Function({}) ~> {}",
                    joined,
                    return_type.to_string(global_type_table)
                )
            }
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct CompletelyResolvedStructMember<'a> {
    pub(crate) name: NonTypeIdentifier<'a>,
    pub(crate) type_: &'a CompletelyResolvedDataType<'a>,
}

#[derive(Debug, Clone)]
pub(crate) struct CompletelyResolvedStructDefinition<'a> {
    pub(crate) is_exported: bool,
    pub(crate) name: TypeIdentifier<'a>,
    pub(crate) members: &'a [CompletelyResolvedStructMember<'a>],
}

#[derive(Debug, Clone)]
pub(crate) enum AlmostCompletelyResolvedTypeDefinition<'a> {
    Struct(Option<&'a CompletelyResolvedStructDefinition<'a>>),
}

#[derive(Debug, Clone)]
pub(crate) enum CompletelyResolvedTypeDefinition<'a> {
    Struct(&'a CompletelyResolvedStructDefinition<'a>),
}

#[derive(Debug, Clone)]
pub(crate) struct Scope<'a> {
    pub(crate) type_definitions: HashMap<&'a str, &'a CompletelyResolvedTypeDefinition<'a>>,
    pub(crate) non_type_definitions: HashMap<&'a str, &'a CompletelyResolvedNonTypeDefinition<'a>>,
}

#[derive(Debug, Clone)]
pub(crate) struct ModuleForNameResolution<'a> {
    pub(crate) canonical_path: &'a Path,
    pub(crate) definitions: &'a [&'a ir_parsed::Definition<'a>],
    // todo: remove? (I think this is not needed)
    pub(crate) global_scope: &'a Scope<'a>,
    pub(crate) non_type_definitions: &'a [&'a CompletelyResolvedNonTypeDefinition<'a>],
}

#[derive(Debug, Clone)]
pub(crate) struct ProgramWithResolvedTypes<'a> {
    pub(crate) type_table: &'a [&'a CompletelyResolvedTypeDefinition<'a>],
    pub(crate) non_type_table: &'a [&'a CompletelyResolvedNonTypeDefinition<'a>],
    pub(crate) modules: Vec<ModuleForNameResolution<'a>>,
}

#[derive(Debug, Clone)]
pub(crate) struct Program<'a> {
    pub(crate) data_type: &'a [CompletelyResolvedTypeDefinition<'a>],
}
