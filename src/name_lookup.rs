use std::collections::HashMap;
use std::path::Path;

use bumpalo::Bump;

use crate::import_resolution::representations::{
    ModuleWithCategorizedNames, NonTypeDefinition, TypeDefinitionKind,
};
use crate::name_lookup::errors::{CouldNotResolveName, NameLookupError};
use crate::name_lookup::ir_after_name_lookup as target_ir;
use crate::parser::ir_parsed as source_ir;

pub(crate) mod errors;
pub(crate) mod ir_after_name_lookup;

#[derive(Debug, Clone)]
pub(crate) struct ScopeStack<'a> {
    scopes: Vec<Scope<'a>>,
}

#[derive(Debug, Clone)]
pub(crate) struct Scope<'a> {
    types: HashMap<String, &'a target_ir::PartiallyResolvedDataType<'a>>,
    values: HashMap<String, &'a target_ir::ResolvedValue<'a>>,
}

impl<'a> ScopeStack<'a> {
    fn type_lookup(&self, name: &str) -> Option<&'a target_ir::PartiallyResolvedDataType<'a>> {
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

pub(crate) fn partially_resolve_data_type<'a>(
    data_type: &'a source_ir::DataType<'a>,
    scope_stack: &ScopeStack<'a>,
    bump_allocator: &'a Bump,
) -> Result<&'a target_ir::PartiallyResolvedDataType<'a>, NameLookupError<'a>> {
    match data_type {
        source_ir::DataType::Named { name } => scope_stack
            .type_lookup(&name.tokens().to_string())
            .ok_or_else(|| CouldNotResolveName::new(name.tokens()).into()),
        source_ir::DataType::Pointer {
            mutability,
            pointee_type,
            ..
        } => partially_resolve_data_type(pointee_type, scope_stack, bump_allocator)
            .map(|resolved_pointee_type| {
                &*bump_allocator.alloc(target_ir::PartiallyResolvedDataType::Pointer {
                    mutability: *mutability,
                    pointee_type: resolved_pointee_type,
                })
            })
            .map_err(
                |NameLookupError::CouldNotResolveName(could_not_resolve_name)| {
                    could_not_resolve_name
                        .with_added_surrounding_tokens(data_type.tokens())
                        .into()
                },
            ),
        source_ir::DataType::Array {
            contained_type,
            size,
            ..
        } => partially_resolve_data_type(contained_type, scope_stack, bump_allocator)
            .map(|resolved_contained_data_type| {
                &*bump_allocator.alloc(target_ir::PartiallyResolvedDataType::Array {
                    contained_type: resolved_contained_data_type,
                    size: *size,
                })
            })
            .map_err(
                |NameLookupError::CouldNotResolveName(could_not_resolve_name)| {
                    could_not_resolve_name
                        .with_added_surrounding_tokens(data_type.tokens())
                        .into()
                },
            ),
        source_ir::DataType::FunctionPointer {
            parameter_list,
            return_type,
            ..
        } => {
            let mut resolved_parameter_types = Vec::new();
            for parameter in *parameter_list {
                match partially_resolve_data_type(&parameter.data_type, scope_stack, bump_allocator)
                {
                    Ok(resolved_parameter_type) => {
                        resolved_parameter_types.push(resolved_parameter_type)
                    }
                    Err(NameLookupError::CouldNotResolveName(could_not_resolve_name)) => {
                        return Err(could_not_resolve_name
                            .with_added_surrounding_tokens(data_type.tokens())
                            .into())
                    }
                }
            }

            match partially_resolve_data_type(return_type, scope_stack, bump_allocator) {
                Ok(resolved_return_type) => Ok(&*bump_allocator.alloc(
                    target_ir::PartiallyResolvedDataType::FunctionPointer {
                        parameter_types: bump_allocator.alloc_slice_copy(&resolved_parameter_types),
                        return_type: resolved_return_type,
                    },
                )),
                Err(NameLookupError::CouldNotResolveName(could_not_resolve_name)) => {
                    Err(could_not_resolve_name
                        .with_added_surrounding_tokens(data_type.tokens())
                        .into())
                }
            }
        }
    }
}

pub(crate) fn partially_resolve_module<'a>(
    module: &'a ModuleWithCategorizedNames<'a>,
    bump_allocator: &'a Bump,
) -> Result<target_ir::PartiallyResolvedModule<'a>, NameLookupError<'a>> {
    let types: HashMap<_, _> = module
        .type_names
        .iter()
        .map(|(name, type_)| {
            (
                name.to_string(),
                &*bump_allocator.alloc(match type_.definition {
                    TypeDefinitionKind::Struct(struct_definition) => {
                        target_ir::PartiallyResolvedDataType::Named {
                            definition: target_ir::PartiallyResolvedTypeName::Struct(
                                struct_definition,
                            ),
                        }
                    }
                }),
            )
        })
        .collect();

    let mut scope_stack = ScopeStack {
        scopes: vec![Scope {
            types,
            values: Default::default(),
        }],
    };

    let mut values = HashMap::new();
    for (name, non_type) in &module.non_type_names {
        let resolved_value = &*bump_allocator.alloc(match non_type {
            NonTypeDefinition::GlobalVariable { definition, .. } => {
                target_ir::ResolvedValue::GlobalVariable(&*bump_allocator.alloc(
                    ir_after_name_lookup::GlobalVariableDefinition {
                        mutability: definition.mutability,
                        name: definition.name,
                        type_: match definition.type_ {
                            None => None,
                            Some(data_type) => Some(partially_resolve_data_type(
                                data_type,
                                &scope_stack,
                                bump_allocator,
                            )?),
                        },
                        initial_value: definition.initial_value,
                    },
                ))
            }
            NonTypeDefinition::Function(overload_set) => {
                let mut overloads = Vec::new();
                for overload in *overload_set {
                    let mut parameters = Vec::new();
                    for parameter in overload.definition.parameters {
                        let type_ = partially_resolve_data_type(
                            &parameter.type_,
                            &scope_stack,
                            bump_allocator,
                        )?;
                        let resolved_parameter = target_ir::FunctionParameter {
                            name: &parameter.name,
                            type_,
                        };
                        parameters.push(resolved_parameter);
                    }
                    let parameters = &*bump_allocator.alloc_slice_clone(&parameters);

                    let return_type = match overload.definition.return_type {
                        Some(type_) => Some(partially_resolve_data_type(
                            type_,
                            &scope_stack,
                            bump_allocator,
                        )?),
                        None => None,
                    };

                    overloads.push(target_ir::FunctionDefinition {
                        name: overload.definition.name,
                        parameters,
                        return_type,
                        body: overload.definition.body,
                    })
                }
                let overloads = &*bump_allocator.alloc_slice_clone(&overloads);
                target_ir::ResolvedValue::FunctionOverloadSet(overloads)
            }
        });
        values.insert(name.to_string(), resolved_value);
    }

    scope_stack
        .scopes
        .first_mut()
        .expect("we inserted the global scope")
        .values = values;

    let local_type_definitions = module
        .type_names
        .iter()
        .filter(|(_, type_definition)| type_definition.origin.is_none());
    let mut partially_resolved_local_types = hashbrown::HashMap::new_in(bump_allocator);
    for (name, type_definition) in local_type_definitions {
        match *type_definition.definition {
            TypeDefinitionKind::Struct(struct_definition) => {
                let mut partially_resolved_members = Vec::new();
                for member in struct_definition.members {
                    let partially_resolved_member_type =
                        partially_resolve_data_type(&member.type_, &scope_stack, bump_allocator)?;
                    partially_resolved_members.push(target_ir::PartiallyResolvedStructMember {
                        name: member.name,
                        type_: partially_resolved_member_type,
                    });
                }
                let partially_resolved_members =
                    &*bump_allocator.alloc_slice_clone(&partially_resolved_members);
                let partially_resolved_struct_definition =
                    &*bump_allocator.alloc(target_ir::PartiallyResolvedStructDefinition {
                        is_exported: struct_definition.is_exported,
                        name: struct_definition.name,
                        members: partially_resolved_members,
                    });
                let partially_resolved_type_definition =
                    &*bump_allocator.alloc(target_ir::PartiallyResolvedTypeDefinition::Struct(
                        partially_resolved_struct_definition,
                    ));
                partially_resolved_local_types.insert(*name, partially_resolved_type_definition);
            }
        }
    }

    let mut imported_type_definitions = hashbrown::HashMap::new_in(bump_allocator);
    module
        .type_names
        .iter()
        .filter(|(_, type_definition)| type_definition.origin.is_some())
        .map(|(name, type_definition)| (*name, *type_definition))
        .collect_into(&mut imported_type_definitions);

    Ok(target_ir::PartiallyResolvedModule {
        canonical_path: module.canonical_path,
        local_type_names: partially_resolved_local_types,
        imported_type_names: imported_type_definitions,
        non_type_names: &module.non_type_names,
    })
}

pub(crate) fn completely_resolve_data_type<'a: 'b, 'b>(
    partially_resolved_type: &'a target_ir::PartiallyResolvedDataType<'a>,
    type_mapping: &HashMap<(&Path, &str), usize>,
    bump_allocator: &'a Bump,
) -> &'a target_ir::CompletelyResolvedDataType<'a> {
    println!(
        "trying to resolve data type '{:?}'",
        partially_resolved_type
    );
    match partially_resolved_type {
        target_ir::PartiallyResolvedDataType::Named { definition } => match definition {
            target_ir::PartiallyResolvedTypeName::Struct(definition) => {
                let unique_type_name = (
                    definition.name.0.source_location.filename(),
                    definition.name.0.lexeme(),
                );
                let global_type_table_index = *type_mapping
                    .get(&unique_type_name)
                    .expect("was inserted into map before");
                &*bump_allocator.alloc(target_ir::CompletelyResolvedDataType::Named(
                    target_ir::CompletelyResolvedNamedDataType {
                        global_type_table_index,
                        name: definition.name,
                    },
                ))
            }
        },
        target_ir::PartiallyResolvedDataType::Pointer {
            mutability,
            pointee_type,
        } => &*bump_allocator.alloc(target_ir::CompletelyResolvedDataType::Pointer {
            mutability: *mutability,
            pointee_type: completely_resolve_data_type(pointee_type, type_mapping, bump_allocator),
        }),
        target_ir::PartiallyResolvedDataType::Array {
            contained_type,
            size,
        } => &*bump_allocator.alloc(target_ir::CompletelyResolvedDataType::Array {
            contained_type: completely_resolve_data_type(
                contained_type,
                type_mapping,
                bump_allocator,
            ),
            size: *size,
        }),
        target_ir::PartiallyResolvedDataType::FunctionPointer {
            parameter_types,
            return_type,
        } => {
            let mut resolved_parameter_types = Vec::new();
            for type_ in *parameter_types {
                resolved_parameter_types.push(completely_resolve_data_type(
                    type_,
                    type_mapping,
                    bump_allocator,
                ));
            }
            let parameter_types = &*bump_allocator.alloc_slice_copy(&resolved_parameter_types);
            &*bump_allocator.alloc(target_ir::CompletelyResolvedDataType::FunctionPointer {
                parameter_types,
                return_type: completely_resolve_data_type(
                    return_type,
                    type_mapping,
                    bump_allocator,
                ),
            })
        }
    }
}

pub(crate) fn completely_resolve_modules<'a>(
    partially_resolved_modules: &'a [target_ir::PartiallyResolvedModule<'a>],
    bump_allocator: &'a Bump,
) {
    let type_table = freeze_type_table(
        generate_intermediate_type_table(partially_resolved_modules, bump_allocator),
        bump_allocator,
    );

    println!("{} type definitions", type_table.len());
    for type_ in type_table {
        match type_ {
            target_ir::CompletelyResolvedTypeDefinition::Struct(definition) => {
                println!(
                    "{} ({} members)",
                    definition.name.0.lexeme(),
                    definition.members.len()
                );
                for member in definition.members {
                    println!(
                        "\t{}: {}",
                        member.name.0.lexeme(),
                        member.type_.to_string(type_table),
                    );
                }
            }
        }
    }
}

fn freeze_type_table<'a>(
    intermediate_global_type_table: &'a [&'a target_ir::AlmostCompletelyResolvedTypeDefinition],
    bump_allocator: &'a Bump,
) -> &'a [&'a target_ir::CompletelyResolvedTypeDefinition<'a>] {
    bump_allocator.alloc_slice_clone(
        &intermediate_global_type_table
            .iter()
            .map(|type_| match type_ {
                target_ir::AlmostCompletelyResolvedTypeDefinition::Struct(struct_definition) => {
                    &*bump_allocator.alloc(target_ir::CompletelyResolvedTypeDefinition::Struct(
                        struct_definition.expect("must have been set before"),
                    ))
                }
            })
            .collect::<Vec<_>>(),
    )
}

fn generate_intermediate_type_table<'a>(
    partially_resolved_modules: &'a [target_ir::PartiallyResolvedModule<'a>],
    bump_allocator: &'a Bump,
) -> &'a [&'a target_ir::AlmostCompletelyResolvedTypeDefinition<'a>] {
    let mut intermediate_global_type_table = Vec::new();
    let mut type_mapping = HashMap::new();
    for module in partially_resolved_modules {
        for (name, type_definition) in &module.local_type_names {
            match type_definition {
                target_ir::PartiallyResolvedTypeDefinition::Struct(_) => {
                    intermediate_global_type_table.push(bump_allocator.alloc(
                        target_ir::AlmostCompletelyResolvedTypeDefinition::Struct(None),
                    ));
                    let unique_type_name = (module.canonical_path, *name);
                    type_mapping.insert(unique_type_name, intermediate_global_type_table.len() - 1);
                }
            }
        }
    }

    for module in partially_resolved_modules {
        for (name, type_definition) in &module.local_type_names {
            match type_definition {
                target_ir::PartiallyResolvedTypeDefinition::Struct(definition) => {
                    let members = &*bump_allocator.alloc_slice_clone(
                        &definition
                            .members
                            .iter()
                            .map(|member| target_ir::CompletelyResolvedStructMember {
                                name: member.name,
                                type_: completely_resolve_data_type(
                                    member.type_,
                                    &type_mapping,
                                    bump_allocator,
                                ),
                            })
                            .collect::<Vec<_>>(),
                    );

                    let resolved =
                        &*bump_allocator.alloc(target_ir::CompletelyResolvedStructDefinition {
                            is_exported: definition.is_exported,
                            name: definition.name,
                            members,
                        });
                    let unique_type_name = (module.canonical_path, *name);
                    let index = *type_mapping
                        .get(&unique_type_name)
                        .expect("has been inserted before");

                    match &mut intermediate_global_type_table[index] {
                        target_ir::AlmostCompletelyResolvedTypeDefinition::Struct(definition) => {
                            if definition.is_none() {
                                *definition = Some(resolved);
                            }
                        }
                    }
                }
            }
        }
    }

    bump_allocator.alloc_slice_clone(
        &intermediate_global_type_table
            .into_iter()
            .map(|definition| &*definition)
            .collect::<Vec<_>>(),
    )
}
