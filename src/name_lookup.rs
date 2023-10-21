use std::collections::HashMap;
use std::path::Path;

use bumpalo::Bump;

use crate::import_resolution::representations::{
    ModuleWithCategorizedNames, NonTypeDefinition, TypeDefinitionKind,
};
use crate::name_lookup::errors::{CouldNotResolveName, NameLookupError};
use crate::name_lookup::ir_after_name_lookup as target_ir;
use crate::name_lookup::ir_after_name_lookup::Scope;
use crate::name_lookup::target_ir::{
    CompletelyResolvedFunctionDefinition, CompletelyResolvedFunctionParameter,
    CompletelyResolvedGlobalVariableDefinition, CompletelyResolvedNonTypeDefinition,
    CompletelyResolvedOverloadSet, CompletelyResolvedTypeDefinition, FunctionParameter,
    LocalVariable, ModuleForNameResolution, PartiallyResolvedFunctionDefinition,
    PartiallyResolvedFunctionParameter, PartiallyResolvedGlobalVariableDefinition,
    PartiallyResolvedNonTypeDefinition, ProgramWithResolvedTypes, ScopeStack,
};
use crate::parser::ir_parsed as source_ir;
use crate::parser::ir_parsed::{Expression, Statement};

pub(crate) mod errors;
pub(crate) mod ir_after_name_lookup;

#[derive(Debug, Clone)]
pub(crate) struct TypeDictionary<'a>(HashMap<String, &'a target_ir::PartiallyResolvedDataType<'a>>);

impl<'a> TypeDictionary<'a> {
    fn type_lookup(&self, name: &str) -> Option<&'a target_ir::PartiallyResolvedDataType<'a>> {
        self.0.get(name).copied()
    }
}

pub(crate) fn partially_resolve_data_type<'a>(
    data_type: &'a source_ir::DataType<'a>,
    type_dictionary: &TypeDictionary<'a>,
    bump_allocator: &'a Bump,
) -> Result<&'a target_ir::PartiallyResolvedDataType<'a>, NameLookupError<'a>> {
    match data_type {
        source_ir::DataType::Named { name } => type_dictionary
            .type_lookup(&name.tokens().to_string())
            .ok_or_else(|| CouldNotResolveName::new(name.tokens()).into()),
        source_ir::DataType::Pointer {
            mutability,
            pointee_type,
            ..
        } => partially_resolve_data_type(pointee_type, type_dictionary, bump_allocator)
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
        } => partially_resolve_data_type(contained_type, type_dictionary, bump_allocator)
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
                match partially_resolve_data_type(
                    &parameter.data_type,
                    type_dictionary,
                    bump_allocator,
                ) {
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

            match partially_resolve_data_type(return_type, type_dictionary, bump_allocator) {
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

pub(crate) fn partially_resolve_type_definitions<'a>(
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

    let type_dictionary = TypeDictionary(types);

    let mut local_non_type_names = hashbrown::HashMap::new_in(bump_allocator);
    let global_variable_definitions =
        partially_resolve_non_type_name_data_types(&type_dictionary, module, bump_allocator)?;
    for (name, definition) in global_variable_definitions {
        local_non_type_names.insert(name, definition);
    }

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
                    let partially_resolved_member_type = partially_resolve_data_type(
                        &member.type_,
                        &type_dictionary,
                        bump_allocator,
                    )?;
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

    let mut imported_non_type_definitions = hashbrown::HashMap::new_in(bump_allocator);
    module
        .non_type_names
        .iter()
        .filter_map(|(name, definition)| match definition {
            NonTypeDefinition::GlobalVariable(global_variable) => {
                if global_variable.origin.is_some() {
                    Some((*name, *definition))
                } else {
                    None
                }
            }
            NonTypeDefinition::Function(overload_set) => {
                let overload_set = &*bump_allocator.alloc_slice_clone(
                    &overload_set
                        .iter()
                        .filter(|overload| overload.origin.is_some())
                        .copied()
                        .collect::<Vec<_>>(),
                );
                match overload_set.is_empty() {
                    true => None,
                    false => Some((
                        *name,
                        &*bump_allocator.alloc(NonTypeDefinition::Function(overload_set)),
                    )),
                }
            }
        })
        .collect_into(&mut imported_non_type_definitions);

    Ok(target_ir::PartiallyResolvedModule {
        canonical_path: module.canonical_path,
        local_type_names: partially_resolved_local_types,
        local_non_type_names,
        imported_type_names: imported_type_definitions,
        imported_non_type_names: imported_non_type_definitions,
        definitions: module.definitions,
    })
}

pub(crate) fn completely_resolve_data_type<'a: 'b, 'b>(
    partially_resolved_type: &'a target_ir::PartiallyResolvedDataType<'a>,
    type_mapping: &HashMap<(&Path, &str), usize>,
    bump_allocator: &'a Bump,
) -> &'a target_ir::CompletelyResolvedDataType<'a> {
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

pub(crate) fn completely_resolve_type_definitions<'a>(
    partially_resolved_modules: &'a [target_ir::PartiallyResolvedModule<'a>],
    bump_allocator: &'a Bump,
) -> &'a ProgramWithResolvedTypes<'a> {
    let (type_mapping, intermediate_type_table) =
        generate_intermediate_type_table(partially_resolved_modules, bump_allocator);
    let type_table = freeze_type_table(intermediate_type_table, bump_allocator);

    let mut resolved_non_type_definitions_by_module = HashMap::new();

    let mut non_type_mapping = HashMap::new();
    let mut non_type_table = Vec::new();
    for module in partially_resolved_modules {
        let mut completely_resolved_non_type_definitions = Vec::new();
        for (name, definition) in &module.local_non_type_names {
            match definition {
                PartiallyResolvedNonTypeDefinition::GlobalVariable(definition) => {
                    let resolved_type = completely_resolve_data_type(
                        definition.type_,
                        &type_mapping,
                        bump_allocator,
                    );
                    let definition = &*bump_allocator.alloc(
                        CompletelyResolvedNonTypeDefinition::GlobalVariable(
                            &*bump_allocator.alloc(CompletelyResolvedGlobalVariableDefinition {
                                is_exported: definition.is_exported,
                                mutability: definition.mutability,
                                name: definition.name,
                                type_: resolved_type,
                                initial_value: definition.initial_value,
                            }),
                        ),
                    );
                    completely_resolved_non_type_definitions.push(definition);
                    non_type_table.push(definition);
                    non_type_mapping
                        .insert((module.canonical_path, *name), non_type_table.len() - 1);
                }
                PartiallyResolvedNonTypeDefinition::Function(overload_set) => {
                    let resolved_overload_set = &*bump_allocator.alloc_slice_fill_iter(
                        overload_set.iter().map(|overload| {
                            let resolved_parameters = &*bump_allocator.alloc_slice_fill_iter(
                                overload.parameters.iter().map(|parameter| {
                                    let resolved_type = completely_resolve_data_type(
                                        parameter.type_,
                                        &type_mapping,
                                        bump_allocator,
                                    );
                                    &*bump_allocator.alloc(CompletelyResolvedFunctionParameter {
                                        mutability: parameter.mutability,
                                        name: parameter.name,
                                        type_: resolved_type,
                                    })
                                }),
                            );
                            let resolved_return_type = overload.return_type.map(|type_| {
                                completely_resolve_data_type(type_, &type_mapping, bump_allocator)
                            });
                            &*bump_allocator.alloc(CompletelyResolvedFunctionDefinition {
                                name: overload.name,
                                parameters: resolved_parameters,
                                return_type: resolved_return_type,
                                body: overload.body,
                                is_exported: overload.is_exported,
                            })
                        }),
                    );
                    let definition = &*bump_allocator.alloc(
                        CompletelyResolvedNonTypeDefinition::Function(resolved_overload_set),
                    );
                    completely_resolved_non_type_definitions.push(definition);
                    non_type_table.push(definition);
                    non_type_mapping
                        .insert((module.canonical_path, *name), non_type_table.len() - 1);
                }
            }
        }
        let completely_resolved_non_type_definitions =
            &*bump_allocator.alloc_slice_clone(&completely_resolved_non_type_definitions);
        resolved_non_type_definitions_by_module.insert(
            module.canonical_path,
            completely_resolved_non_type_definitions,
        );
    }

    let mut global_scopes = hashbrown::HashMap::new_in(bump_allocator);

    for module in partially_resolved_modules {
        let mut type_definitions = HashMap::new();
        for (name, type_definition) in &module.local_type_names {
            match type_definition {
                target_ir::PartiallyResolvedTypeDefinition::Struct(struct_) => {
                    type_definitions.insert(
                        *name,
                        type_table[*type_mapping
                            .get(&(
                                struct_.name.0.source_location.filename(),
                                struct_.name.0.lexeme(),
                            ))
                            .expect("was inserted before")],
                    );
                }
            }
        }

        for (name, type_definition) in &module.imported_type_names {
            assert!(type_definition.origin.is_some());
            match type_definition.definition {
                TypeDefinitionKind::Struct(definition) => {
                    type_definitions.insert(
                        *name,
                        type_table[*type_mapping
                            .get(&(
                                definition.name.0.source_location.filename(),
                                definition.name.0.lexeme(),
                            ))
                            .expect("was inserted before")],
                    );
                }
            }
        }

        let mut overload_sets: HashMap<_, Vec<_>> = HashMap::new();
        let mut non_type_definitions = HashMap::new();
        for (name, non_type_definition) in &module.local_non_type_names {
            match non_type_definition {
                PartiallyResolvedNonTypeDefinition::GlobalVariable(definition) => {
                    let key = (
                        definition.name.0.source_location.filename(),
                        definition.name.0.lexeme(),
                    );
                    let definition = non_type_table[*non_type_mapping
                        .get(&key)
                        .expect("has been inserted before")];
                    non_type_definitions.insert(*name, definition);
                }
                PartiallyResolvedNonTypeDefinition::Function(overload_set) => {
                    assert!(!overload_set.is_empty());
                    let key = (
                        overload_set
                            .first()
                            .unwrap()
                            .name
                            .0
                            .source_location
                            .filename(),
                        overload_set.first().unwrap().name.0.lexeme(),
                    );
                    let resolved_overload_set = non_type_table[non_type_mapping[&key]];
                    let CompletelyResolvedNonTypeDefinition::Function(resolved_overload_set) =
                        resolved_overload_set
                    else {
                        unreachable!();
                    };
                    let entry = overload_sets.entry(*name).or_default();
                    for overload in *resolved_overload_set {
                        entry.push(*overload);
                    }
                }
            }
        }

        for (name, non_type_definition) in &module.imported_non_type_names {
            match non_type_definition {
                NonTypeDefinition::GlobalVariable(definition) => {
                    assert!(definition.origin.is_some());
                    let key = (
                        definition.name.0.source_location.filename(),
                        definition.name.0.lexeme(),
                    );
                    let definition = non_type_table[*non_type_mapping
                        .get(&key)
                        .expect("has been inserted before")];
                    non_type_definitions.insert(*name, definition);
                }
                NonTypeDefinition::Function(overload_set) => {
                    assert!(!overload_set.is_empty());
                    assert!(overload_set
                        .iter()
                        .all(|overload| overload.origin.is_some()));
                    let entry = overload_sets.entry(name).or_default();
                    let mut used_keys = Vec::new();
                    for overload in *overload_set {
                        let key = (
                            overload.definition.name.0.source_location.filename(),
                            overload.definition.name.0.lexeme(),
                        );
                        if used_keys.iter().any(|k| *k == key) {
                            continue;
                        }
                        used_keys.push(key);
                        let resolved_overload_set = non_type_table
                            [*non_type_mapping.get(&key).expect("was inserted before")];
                        let CompletelyResolvedNonTypeDefinition::Function(resolved_overload_set) =
                            resolved_overload_set
                        else {
                            unreachable!();
                        };
                        for resolved_overload in resolved_overload_set
                            .iter()
                            .filter(|overload| overload.is_exported)
                        {
                            entry.push(resolved_overload);
                        }
                    }
                }
            }
        }

        for (name, overload_set) in overload_sets {
            non_type_definitions.insert(
                name,
                &*bump_allocator.alloc(CompletelyResolvedNonTypeDefinition::Function(
                    &*bump_allocator.alloc_slice_clone(&overload_set),
                )),
            );
        }

        global_scopes.insert(
            module.canonical_path,
            &*bump_allocator.alloc(Scope {
                type_definitions,
                non_type_definitions,
            }),
        );
    }

    let non_type_table = &*bump_allocator.alloc_slice_clone(&non_type_table);

    let modules = partially_resolved_modules
        .iter()
        .map(|module| ModuleForNameResolution {
            canonical_path: module.canonical_path,
            global_scope: global_scopes
                .get(module.canonical_path)
                .expect("each module has its global scope resolved"),
            non_type_definitions: resolved_non_type_definitions_by_module
                .get(module.canonical_path)
                .expect("modules without such definitions have an empty vector"),
        })
        .collect();

    &*bump_allocator.alloc(ProgramWithResolvedTypes {
        type_table,
        non_type_table,
        modules,
    })
}

pub(crate) fn partially_resolve_non_type_name_data_types<'a>(
    type_dictionary: &TypeDictionary<'a>,
    module: &'a ModuleWithCategorizedNames<'a>,
    bump_allocator: &'a Bump,
) -> Result<HashMap<&'a str, &'a PartiallyResolvedNonTypeDefinition<'a>>, NameLookupError<'a>> {
    let mut partially_resolved_definitions = HashMap::new();

    for (name, definition) in &module.non_type_names {
        match definition {
            NonTypeDefinition::GlobalVariable(definition) => {
                if definition.origin.is_some() {
                    continue;
                }
                let partially_resolved_type = partially_resolve_data_type(
                    &definition.type_,
                    type_dictionary,
                    bump_allocator,
                )?;
                partially_resolved_definitions.insert(
                    *name,
                    &*bump_allocator.alloc(PartiallyResolvedNonTypeDefinition::GlobalVariable(
                        &*bump_allocator.alloc(PartiallyResolvedGlobalVariableDefinition {
                            is_exported: definition.is_exported,
                            mutability: definition.mutability,
                            name: definition.name,
                            type_: partially_resolved_type,
                            initial_value: definition.initial_value,
                        }),
                    )),
                );
            }
            NonTypeDefinition::Function(overload_set) => {
                let mut partially_resolved_overload_set = Vec::new();
                for overload in *overload_set {
                    if overload.origin.is_some() {
                        continue;
                    }
                    let mut partially_resolved_parameters = Vec::new();
                    for parameter in overload.definition.parameters {
                        let partially_resolved_type = partially_resolve_data_type(
                            &parameter.type_,
                            type_dictionary,
                            bump_allocator,
                        )?;
                        partially_resolved_parameters.push(&*bump_allocator.alloc(
                            PartiallyResolvedFunctionParameter {
                                mutability: parameter.mutability,
                                name: parameter.name,
                                type_: partially_resolved_type,
                            },
                        ));
                    }

                    let partially_resolved_return_type = overload
                        .definition
                        .return_type
                        .map(|type_| {
                            partially_resolve_data_type(type_, type_dictionary, bump_allocator)
                        })
                        .transpose()?;

                    partially_resolved_overload_set.push(&*bump_allocator.alloc(
                        PartiallyResolvedFunctionDefinition {
                            name: overload.definition.name,
                            parameters:
                                &*bump_allocator.alloc_slice_clone(&partially_resolved_parameters),
                            return_type: partially_resolved_return_type,
                            body: overload.definition.body,
                            is_exported: overload.definition.is_exported,
                        },
                    ))
                }
                if partially_resolved_overload_set.is_empty() {
                    continue;
                }
                partially_resolved_definitions.insert(
                    *name,
                    &*bump_allocator.alloc(PartiallyResolvedNonTypeDefinition::Function(
                        &*bump_allocator.alloc_slice_clone(&partially_resolved_overload_set),
                    )),
                );
            }
        }
    }

    Ok(partially_resolved_definitions)
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
) -> (
    HashMap<(&'a Path, &'a str), usize>,
    &'a [&'a target_ir::AlmostCompletelyResolvedTypeDefinition<'a>],
) {
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

    (
        type_mapping,
        bump_allocator.alloc_slice_clone(
            &intermediate_global_type_table
                .into_iter()
                .map(|definition| &*definition)
                .collect::<Vec<_>>(),
        ),
    )
}

fn perform_name_lookup_for_expression<'a>(
    expression: &'a Expression<'a>,
    scope_stack: &mut ScopeStack<'a>,
    bump_allocator: &'a Bump,
) -> Result<(), NameLookupError<'a>> {
    match expression {
        Expression::Literal(_) => {
            // nothing do do since a literal cannot contain a name reference
        }
        Expression::BinaryOperator { lhs, operator, rhs } => {
            perform_name_lookup_for_expression(lhs, scope_stack, bump_allocator)?;
            perform_name_lookup_for_expression(rhs, scope_stack, bump_allocator)?;
        }
        Expression::Block(block) => {
            let number_of_scopes = scope_stack.len();
            scope_stack.push(Scope::default());
            for statement in block.statements {
                perform_name_lookup_for_statement(statement, scope_stack, bump_allocator)?;
            }
            scope_stack.truncate(number_of_scopes);
        }
        Expression::Name(qualified_name) => {
            scope_stack.lookup_non_type(qualified_name)?;
        }
    }
    Ok(())
}

fn perform_name_lookup_for_global_variable_definition<'a>(
    definition: &'a CompletelyResolvedGlobalVariableDefinition<'a>,
    scope_stack: &mut ScopeStack<'a>,
    bump_allocator: &'a Bump,
) -> Result<(), NameLookupError<'a>> {
    perform_name_lookup_for_expression(&definition.initial_value, scope_stack, bump_allocator)
}

fn perform_name_lookup_for_statement<'a>(
    statement: &'a Statement<'a>,
    scope_stack: &mut ScopeStack<'a>,
    bump_allocator: &'a Bump,
) -> Result<(), NameLookupError<'a>> {
    match statement {
        Statement::ExpressionStatement(expression) => {
            perform_name_lookup_for_expression(expression, scope_stack, bump_allocator)?;
        }
        Statement::Yield(_) => {}
        Statement::Return(_) => {}
        Statement::VariableDefinition(definition) => {
            perform_name_lookup_for_expression(
                &definition.initial_value,
                scope_stack,
                bump_allocator,
            )?;

            scope_stack.push(Scope::from_non_type_definitions(HashMap::from([(
                definition.name.0.lexeme(),
                &*bump_allocator.alloc(CompletelyResolvedNonTypeDefinition::LocalVariable(
                    &*bump_allocator.alloc(LocalVariable {
                        mutability: definition.mutability,
                        name: definition.name,
                        type_: definition
                            .type_
                            .map(|type_| scope_stack.lookup_type(&type_))
                            .transpose()?,
                    }),
                )),
            )])));
            println!("{scope_stack}");
        }
    }
    Ok(())
}

fn perform_name_lookup_for_overload<'a>(
    overload: &'a CompletelyResolvedFunctionDefinition<'a>,
    scope_stack: &mut ScopeStack<'a>,
    bump_allocator: &'a Bump,
) -> Result<(), NameLookupError<'a>> {
    println!("name lookup for function '{}'", overload.name.0.lexeme());
    let number_of_scopes = scope_stack.len();
    let function_scope = Scope {
        type_definitions: Default::default(),
        non_type_definitions: overload
            .parameters
            .iter()
            .map(|parameter| {
                (
                    parameter.name.0.lexeme(),
                    &*bump_allocator.alloc(CompletelyResolvedNonTypeDefinition::FunctionParameter(
                        &*bump_allocator.alloc(FunctionParameter {
                            mutability: parameter.mutability,
                            name: parameter.name,
                            type_: parameter.type_,
                        }),
                    )),
                )
            })
            .collect(),
    };
    scope_stack.push(function_scope);
    println!("{scope_stack}");
    for statement in overload.body.statements {
        perform_name_lookup_for_statement(statement, scope_stack, bump_allocator)?;
    }

    assert!(scope_stack.len() >= number_of_scopes);
    scope_stack.truncate(number_of_scopes);

    Ok(())
}

fn perform_name_lookup_for_overload_set<'a>(
    overload_set: CompletelyResolvedOverloadSet<'a>,
    scope_stack: &mut ScopeStack<'a>,
    bump_allocator: &'a Bump,
) -> Result<(), NameLookupError<'a>> {
    for overload in overload_set {
        perform_name_lookup_for_overload(overload, scope_stack, bump_allocator)?;
    }
    Ok(())
}

fn perform_name_lookup_for_module<'a>(
    global_type_table: &'a [&'a CompletelyResolvedTypeDefinition<'a>],
    module: &'a ModuleForNameResolution<'a>,
    bump_allocator: &'a Bump,
) -> Result<(), NameLookupError<'a>> {
    let mut scope_stack = ScopeStack::new(
        global_type_table,
        module.global_scope.clone(),
        bump_allocator,
    );
    for definition in module.non_type_definitions {
        match definition {
            CompletelyResolvedNonTypeDefinition::GlobalVariable(definition) => {
                perform_name_lookup_for_global_variable_definition(
                    definition,
                    &mut scope_stack,
                    bump_allocator,
                )?;
            }
            CompletelyResolvedNonTypeDefinition::Function(overload_set) => {
                perform_name_lookup_for_overload_set(
                    overload_set,
                    &mut scope_stack,
                    bump_allocator,
                )?;
            }
            CompletelyResolvedNonTypeDefinition::FunctionParameter(_) => {
                unreachable!("the global scope of a module must never contain a function parameter")
            }
            CompletelyResolvedNonTypeDefinition::LocalVariable(_) => {
                unreachable!("the global scope of a module must never contain a local variable")
            }
        }
    }
    Ok(())
}

pub(crate) fn perform_name_lookup<'a>(
    program: &'a ProgramWithResolvedTypes<'a>,
    bump_allocator: &'a Bump,
) -> Result<(), NameLookupError<'a>> {
    for module in &program.modules {
        perform_name_lookup_for_module(program.type_table, module, bump_allocator)?;
    }
    Ok(())
}
