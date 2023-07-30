use std::collections::HashMap;

use bumpalo::Bump;

use crate::import_resolution::representations::ModulesWithConnectedImports;
use crate::name_lookup::errors::NameLookupError;
use crate::name_lookup::ir_after_name_lookup as target_ir;
use crate::parser::ir_parsed as source_ir;
use crate::token::TokenType;

pub(crate) mod errors;
pub(crate) mod ir_after_name_lookup;

#[derive(Debug, Clone)]
pub(crate) struct ScopeStack<'a> {
    scopes: Vec<Scope<'a>>,
}

#[derive(Debug, Clone)]
pub(crate) struct Scope<'a> {
    types: HashMap<String, &'a target_ir::ResolvedDataType<'a>>,
    values: HashMap<String, &'a target_ir::ResolvedValue<'a>>,
}

impl<'a> ScopeStack<'a> {
    fn type_lookup(&self, name: &str) -> Option<&'a target_ir::ResolvedDataType<'a>> {
        for scope in &self.scopes {
            if let Some(resolved_type) = scope.types.get(name) {
                return Some(resolved_type);
            }
        }
        None
    }

    fn value_lookup(&self, name: &str) -> Option<target_ir::ResolvedValue<'a>> {
        for scope in &self.scopes {
            if let Some(resolved_value) = scope.values.get(name) {
                return Some(**resolved_value);
            }
        }
        None
    }
}

pub(crate) fn resolve_data_type<'a>(
    data_type: source_ir::DataType<'a>,
    scope_stack: &ScopeStack<'a>,
    bump_allocator: &'a Bump,
) -> Result<&'a target_ir::ResolvedDataType<'a>, NameLookupError<'a>> {
    match data_type {
        source_ir::DataType::Named { name } => scope_stack
            .type_lookup(&name.tokens().to_string())
            .ok_or_else(|| NameLookupError::CouldNotResolveName(name.tokens())),
        source_ir::DataType::Pointer {
            mutability,
            pointee_type,
            ..
        } => resolve_data_type(*pointee_type, scope_stack, bump_allocator).map(
            |resolved_pointee_type| {
                &*bump_allocator.alloc(target_ir::ResolvedDataType::Pointer {
                    mutability,
                    pointee_type: resolved_pointee_type,
                })
            },
        ),
        source_ir::DataType::Array {
            contained_type,
            size,
            ..
        } => resolve_data_type(*contained_type, scope_stack, bump_allocator).map(
            |resolved_contained_type| {
                &*bump_allocator.alloc(target_ir::ResolvedDataType::Array {
                    contained_type: resolved_contained_type,
                    size,
                })
            },
        ),
        source_ir::DataType::FunctionPointer {
            parameter_list: parameter_types,
            return_type,
            ..
        } => {
            let resolved_parameter_types: Result<Vec<_>, _> = parameter_types
                .iter()
                .map(|parameter| {
                    resolve_data_type(parameter.data_type, scope_stack, bump_allocator)
                })
                .collect();
            let resolved_parameter_types = resolved_parameter_types?;
            let resolved_return_type =
                resolve_data_type(*return_type, scope_stack, bump_allocator)?;
            Ok(
                &*bump_allocator.alloc(target_ir::ResolvedDataType::FunctionPointer {
                    parameter_types: bump_allocator.alloc_slice_copy(&resolved_parameter_types),
                    return_type: resolved_return_type,
                }),
            )
        }
    }
}

pub(crate) fn perform_name_lookup<'a>(
    modules: ModulesWithConnectedImports<'a>,
    bump_allocator: &'a Bump,
) -> Result<target_ir::Program<'a>, NameLookupError<'a>> {
    for module in modules {
        // --------------------------------------------------------------------
        // gather all data types that are available in the current module
        // --------------------------------------------------------------------

        let mut types: HashMap<String, &target_ir::ResolvedDataType> = HashMap::new();

        // imports that do not just import only one symbol, but the whole module
        for import in module.resolved_imports {
            let mut qualified_name_prefix = match import.import {
                source_ir::Import::Import { what } => what.to_string(),
                source_ir::Import::ImportAs { as_, .. } => as_.as_string(),
                source_ir::Import::FromImport { .. } | source_ir::Import::FromImportAs { .. } => {
                    continue
                }
            };
            qualified_name_prefix.push_str("::");

            for definition in import.from_module.exported_definitions {
                let mut qualified_name = qualified_name_prefix.clone();
                qualified_name.push_str(&definition.identifier().as_string());
                let insertion_result = match definition {
                    source_ir::Definition::Struct(struct_definition) => types
                        .insert(
                            qualified_name,
                            &*bump_allocator.alloc(target_ir::ResolvedDataType::Named {
                                name: target_ir::ResolvedTypeName::Struct(*struct_definition),
                            }),
                        )
                        .is_none(),
                    source_ir::Definition::Function(_)
                    | source_ir::Definition::GlobalVariable(_) => continue,
                };
                assert!(insertion_result);
            }
        }

        // imports that import a single symbol (already connected)
        for import in module.connected_imports {
            let imported_name_token = match import.import {
                source_ir::Import::FromImport { symbol, .. } => symbol.token(),
                source_ir::Import::FromImportAs { as_, .. } => as_.token(),
                _ => unreachable!(),
            };

            // ignore values for now, only consider types
            if imported_name_token.type_ == TokenType::LowercaseIdentifier {
                continue;
            }
            assert_eq!(imported_name_token.type_, TokenType::UppercaseIdentifier);

            if let source_ir::Definition::Struct(struct_definition) = import.definition {
                let resolved_data_type = bump_allocator.alloc(target_ir::ResolvedDataType::Named {
                    name: target_ir::ResolvedTypeName::Struct(struct_definition),
                });
                let insertion_result =
                    types.insert(imported_name_token.lexeme().to_string(), resolved_data_type);
                assert!(insertion_result.is_none());
            } else {
                unreachable!();
            };
        }

        // add all type definitions local to the current module
        for definition in module.module.definitions {
            match definition {
                source_ir::Definition::Struct(struct_definition) => {
                    let data_type = bump_allocator.alloc(target_ir::ResolvedDataType::Named {
                        name: target_ir::ResolvedTypeName::Struct(*struct_definition),
                    });
                    let insertion_result =
                        types.insert(struct_definition.name.0.lexeme().to_string(), data_type);
                    assert!(insertion_result.is_none());
                }
                source_ir::Definition::Function(_) | source_ir::Definition::GlobalVariable(_) => {
                    continue
                }
            }
        }

        // --------------------------------------------------------------------
        // perform name-lookup for the data types contained in structs of the local module
        // --------------------------------------------------------------------

        let scopes = ScopeStack {
            scopes: vec![Scope {
                types,
                values: HashMap::new(),
            }],
        };

        let mut resolved_struct_definitions = Vec::new();

        for struct_definition in
            module
                .module
                .definitions
                .iter()
                .filter_map(|definition| match definition {
                    source_ir::Definition::Struct(struct_definition) => Some(struct_definition),
                    source_ir::Definition::Function(_)
                    | source_ir::Definition::GlobalVariable(_) => None,
                })
        {
            let mut resolved_members = Vec::new();
            for member in struct_definition.members {
                resolved_members.push(target_ir::StructMember {
                    name: member.name,
                    type_: resolve_data_type(member.type_, &scopes, bump_allocator)?,
                });
            }

            resolved_struct_definitions.push(target_ir::StructDefinition {
                name: struct_definition.name,
                members: bump_allocator.alloc_slice_copy(&resolved_members),
            });
        }
    }

    Ok(target_ir::Program { definitions: &[] })
}
