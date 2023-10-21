use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::path::Path;

use bumpalo::Bump;
use hashbrown::hash_map::DefaultHashBuilder;
use itertools::Itertools;

use crate::constants::BackseatSize;
use crate::import_resolution::representations::{NonTypeDefinition, TypeDefinition};
use crate::name_lookup::errors::{CouldNotResolveName, NameLookupError};
use crate::parser::ir_parsed;
pub(crate) use crate::parser::ir_parsed::Mutability;
pub(crate) use crate::parser::ir_parsed::NonTypeIdentifier;
pub(crate) use crate::parser::ir_parsed::TypeIdentifier;
use crate::parser::ir_parsed::{Block, DataType, Expression, QualifiedNonTypeName};

#[derive(Debug, Clone, Copy)]
pub(crate) enum Definition<'a> {
    Function(PartiallyResolvedFunctionDefinition<'a>),
    GlobalVariable(GlobalVariableDefinition<'a>),
    Struct(StructDefinition<'a>),
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct PartiallyResolvedFunctionParameter<'a> {
    pub(crate) mutability: Mutability,
    pub(crate) name: &'a NonTypeIdentifier<'a>,
    pub(crate) type_: &'a PartiallyResolvedDataType<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct PartiallyResolvedFunctionDefinition<'a> {
    pub(crate) name: &'a NonTypeIdentifier<'a>,
    pub(crate) parameters: &'a [&'a PartiallyResolvedFunctionParameter<'a>],
    pub(crate) return_type: Option<&'a PartiallyResolvedDataType<'a>>,
    pub(crate) body: Block<'a>,
    pub(crate) is_exported: bool,
}

pub(crate) type PartiallyResolvedOverloadSet<'a> =
    &'a [&'a PartiallyResolvedFunctionDefinition<'a>];

#[derive(Debug, Clone)]
pub(crate) struct CompletelyResolvedFunctionParameter<'a> {
    pub(crate) mutability: Mutability,
    pub(crate) name: &'a NonTypeIdentifier<'a>,
    pub(crate) type_: &'a CompletelyResolvedDataType<'a>,
}

#[derive(Debug, Clone)]
pub(crate) struct CompletelyResolvedFunctionDefinition<'a> {
    pub(crate) name: &'a NonTypeIdentifier<'a>,
    pub(crate) parameters: &'a [&'a CompletelyResolvedFunctionParameter<'a>],
    pub(crate) return_type: Option<&'a CompletelyResolvedDataType<'a>>,
    pub(crate) body: Block<'a>,
    pub(crate) is_exported: bool,
}

pub(crate) type CompletelyResolvedOverloadSet<'a> =
    &'a [&'a CompletelyResolvedFunctionDefinition<'a>];

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
    pub(crate) name: &'a NonTypeIdentifier<'a>,
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
    pub(crate) name: &'a NonTypeIdentifier<'a>,
    pub(crate) type_: &'a CompletelyResolvedDataType<'a>,
    pub(crate) initial_value: Expression<'a>,
}

#[derive(Debug, Clone)]
pub(crate) struct FunctionParameter<'a> {
    pub(crate) mutability: Mutability,
    pub(crate) name: &'a NonTypeIdentifier<'a>,
    pub(crate) type_: &'a CompletelyResolvedDataType<'a>,
}

#[derive(Debug, Clone)]
pub(crate) struct LocalVariable<'a> {
    pub(crate) mutability: Mutability,
    pub(crate) name: &'a NonTypeIdentifier<'a>,
    pub(crate) type_: Option<&'a LookedUpDataType<'a>>,
    // pub(crate) initial_value: Expression<'a>, // todo!
}

#[derive(Debug, Clone)]
pub(crate) enum CompletelyResolvedNonTypeDefinition<'a> {
    GlobalVariable(&'a CompletelyResolvedGlobalVariableDefinition<'a>),
    Function(CompletelyResolvedOverloadSet<'a>),
    FunctionParameter(&'a FunctionParameter<'a>),
    LocalVariable(&'a LocalVariable<'a>),
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
                                .join(", "),
                            match overload.return_type {
                                None => "".to_string(),
                                Some(type_) => format!(" ~> {}", type_.to_string(type_table)),
                            }
                        )
                    })
                    .join(", "),
            ),
            CompletelyResolvedNonTypeDefinition::FunctionParameter(parameter) => {
                format!(
                    "{} {}: {}",
                    parameter.mutability,
                    parameter.name.0.lexeme(),
                    parameter.type_.to_string(type_table)
                )
            }
            CompletelyResolvedNonTypeDefinition::LocalVariable(variable) => {
                format!(
                    "let {} {}{} = {};",
                    variable.mutability,
                    variable.name.0.lexeme(),
                    match variable.type_ {
                        Some(type_) => format!(": {type_}"),
                        None => "".to_string(),
                    },
                    "expression not yet known", // todo!
                )
            }
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
                    .join(", ");
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

impl Display for CompletelyResolvedTypeDefinition<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CompletelyResolvedTypeDefinition::Struct(definition) => {
                write!(
                    f,
                    "{}{}",
                    if definition.is_exported {
                        "export "
                    } else {
                        ""
                    },
                    definition.name.0.lexeme()
                )
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub(crate) struct Scope<'a> {
    pub(crate) type_definitions: HashMap<&'a str, &'a CompletelyResolvedTypeDefinition<'a>>,
    pub(crate) non_type_definitions: HashMap<&'a str, &'a CompletelyResolvedNonTypeDefinition<'a>>,
}

impl<'a> Scope<'a> {
    pub(crate) fn from_non_type_definitions(
        non_type_definitions: HashMap<&'a str, &'a CompletelyResolvedNonTypeDefinition<'a>>,
    ) -> Self {
        Self {
            type_definitions: Default::default(),
            non_type_definitions,
        }
    }

    pub(crate) fn get_type_definition(
        &self,
        name: &str,
    ) -> Option<&'a CompletelyResolvedTypeDefinition<'a>> {
        self.type_definitions.get(name).copied()
    }

    pub(crate) fn get_non_type_definition(
        &self,
        name: &str,
    ) -> Option<&'a CompletelyResolvedNonTypeDefinition<'a>> {
        self.non_type_definitions.get(name).copied()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ScopeStack<'a> {
    global_type_table: &'a [&'a CompletelyResolvedTypeDefinition<'a>],
    scopes: Vec<Scope<'a>>,
    bump_allocator: &'a Bump,
}

impl<'a> ScopeStack<'a> {
    pub(crate) fn new(
        global_type_table: &'a [&'a CompletelyResolvedTypeDefinition<'a>],
        global_scope: Scope<'a>,
        bump_allocator: &'a Bump,
    ) -> Self {
        Self {
            global_type_table,
            scopes: vec![global_scope],
            bump_allocator,
        }
    }

    pub(crate) fn push(&mut self, scope: Scope<'a>) {
        self.scopes.push(scope);
    }

    pub(crate) fn pop(&mut self) -> Scope<'a> {
        self.scopes
            .pop()
            .expect("this should never be called on the last scope in the stack")
    }

    pub(crate) fn len(&self) -> usize {
        self.scopes.len()
    }

    pub(crate) fn truncate(&mut self, number_of_scopes: usize) {
        self.scopes.truncate(number_of_scopes);
    }

    pub(crate) fn lookup_non_type(
        &self,
        qualified_name: &'a QualifiedNonTypeName<'a>,
    ) -> Result<&'a CompletelyResolvedNonTypeDefinition<'a>, NameLookupError<'a>> {
        let name = qualified_name.canonical_name();
        for scope in self.scopes.iter().rev() {
            if let Some(definition) = scope.non_type_definitions.get(name.as_str()) {
                return Ok(definition);
            }
        }
        Err(NameLookupError::CouldNotResolveName(
            CouldNotResolveName::new(qualified_name.tokens()),
        ))
    }

    pub(crate) fn lookup_type(
        &self,
        data_type: &'a DataType<'a>,
    ) -> Result<&'a LookedUpDataType<'a>, NameLookupError<'a>> {
        match data_type {
            DataType::Named {
                name: qualified_type_name,
            } => {
                let name = qualified_type_name.canonical_name();
                for scope in self.scopes.iter().rev() {
                    if let Some(definition) = scope.type_definitions.get(name.as_str()) {
                        return Ok(self
                            .bump_allocator
                            .alloc(LookedUpDataType::Named(definition)));
                    }
                }
                return Err(NameLookupError::CouldNotResolveName(
                    CouldNotResolveName::new(qualified_type_name.tokens()),
                ));
            }
            DataType::Pointer {
                mutability,
                pointee_type,
                ..
            } => self
                .lookup_type(pointee_type)
                .map(|looked_up_pointee_type| {
                    &*self.bump_allocator.alloc(LookedUpDataType::Pointer {
                        mutability: *mutability,
                        pointee_type: looked_up_pointee_type,
                    })
                })
                .map_err(
                    |NameLookupError::CouldNotResolveName(could_not_resolve_name)| {
                        could_not_resolve_name
                            .with_added_surrounding_tokens(data_type.tokens())
                            .into()
                    },
                ),
            DataType::Array { .. } => {
                todo!()
            }
            DataType::FunctionPointer { .. } => {
                todo!()
            }
        }
    }
}

impl Display for ScopeStack<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        const EMPTY: &str = "";
        const DASH: &str = "-";
        for (depth, scope) in self.scopes.iter().enumerate() {
            let indentation = depth * 4;
            if depth > 0 {
                write!(f, "{EMPTY:0$}>---", (depth - 1) * 4)?;
            }
            if scope.non_type_definitions.is_empty() && scope.type_definitions.is_empty() {
                writeln!(f, "| (empty scope)")?;
                continue;
            }

            writeln!(
                f,
                "| {}",
                scope
                    .type_definitions
                    .keys()
                    .chain(scope.non_type_definitions.keys())
                    .next()
                    .unwrap()
            )?;

            for name in scope
                .type_definitions
                .keys()
                .chain(scope.non_type_definitions.keys())
                .skip(1)
            {
                writeln!(f, "{EMPTY:indentation$}| {name}")?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ModuleForNameResolution<'a> {
    pub(crate) canonical_path: &'a Path,
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

#[derive(Debug, Clone)]
pub(crate) enum LookedUpDataType<'a> {
    Named(&'a CompletelyResolvedTypeDefinition<'a>),
    Pointer {
        mutability: Mutability,
        pointee_type: &'a LookedUpDataType<'a>,
    },
    Array {
        contained_type: &'a LookedUpDataType<'a>,
        size: BackseatSize,
    },
    FunctionPointer {
        parameter_types: &'a [&'a LookedUpDataType<'a>],
        return_type: &'a LookedUpDataType<'a>,
    },
}

impl Display for LookedUpDataType<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LookedUpDataType::Named(definition) => definition.fmt(f),
            LookedUpDataType::Pointer {
                mutability,
                pointee_type,
            } => {
                write!(f, "-> {} {}", mutability, pointee_type)
            }
            LookedUpDataType::Array {
                contained_type,
                size,
            } => {
                write!(f, "[{}; {}]", contained_type, size)
            }
            LookedUpDataType::FunctionPointer {
                parameter_types,
                return_type,
            } => {
                write!(
                    f,
                    "Function({}) ~> {}",
                    parameter_types.iter().map(ToString::to_string).join(", "),
                    return_type
                )
            }
        }
    }
}
