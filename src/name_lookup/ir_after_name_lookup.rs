use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use bumpalo::Bump;
use itertools::Itertools;

use crate::constants::BackseatSize;
use crate::mutual_type_resolution::ir_for_mutual_type_resolution::{
    CompletelyResolvedNonTypeDefinition, CompletelyResolvedTypeDefinition,
};
use crate::name_lookup::errors::{CouldNotResolveName, NameLookupError};
pub(crate) use crate::parser::ir_parsed::Mutability;
pub(crate) use crate::parser::ir_parsed::NonTypeIdentifier;
pub(crate) use crate::parser::ir_parsed::TypeIdentifier;
use crate::parser::ir_parsed::{DataType, QualifiedNonTypeName};

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

    pub(crate) fn peek(&self) -> &Scope<'a> {
        self.scopes
            .last()
            .expect("the scope stack must not be empty")
    }

    pub(crate) fn peek_mut(&mut self) -> &mut Scope<'a> {
        self.scopes
            .last_mut()
            .expect("the scope stack must not be empty")
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
            DataType::Array {
                contained_type,
                size,
                ..
            } => {
                let looked_up_type = self.lookup_type(contained_type)?;
                Ok(self.bump_allocator.alloc(LookedUpDataType::Array {
                    contained_type: looked_up_type,
                    size: *size,
                }))
            }
            DataType::FunctionPointer {
                parameter_list,
                return_type,
                ..
            } => {
                let looked_up_parameter_types = parameter_list
                    .iter()
                    .map(|parameter| self.lookup_type(parameter.data_type))
                    .collect::<Result<Vec<_>, _>>()?;
                let looked_up_parameter_types = &*self
                    .bump_allocator
                    .alloc_slice_clone(&looked_up_parameter_types);
                let looked_up_return_type = self.lookup_type(return_type)?;

                Ok(self
                    .bump_allocator
                    .alloc(LookedUpDataType::FunctionPointer {
                        parameter_types: looked_up_parameter_types,
                        return_type: looked_up_return_type,
                    }))
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
