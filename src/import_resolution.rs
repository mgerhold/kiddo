use std::collections::{HashMap, HashSet, VecDeque};
use std::path::{Path, PathBuf};

use bumpalo::Bump;

pub use representations::ModuleWithImports;

use crate::constants::SOURCE_FILE_EXTENSION;
use crate::import_resolution::errors::{ImportError, NameError};
use crate::import_resolution::representations::{
    ConnectedImport, ConnectedModule, ConnectedModules, ModuleImports, ModuleWithCategorizedNames,
    NonTypeDefinition, Overload, ResolvedDefinition, ResolvedModule, TypeDefinition,
    TypeDefinitionKind,
};
use crate::lexer::LexerError;
use crate::parser::errors::ErrorReport;
use crate::parser::ir_parsed::{
    Definition, Import, Module, QualifiedName, QualifiedNonTypeName, QualifiedTypeName,
};
use crate::parser::parse_module;
use crate::token::TokenType;
use crate::utils::AllocPath;

pub(crate) mod errors;
pub(crate) mod representations;

fn find_path(relative_path: &Path, possible_roots: &[&Path]) -> Option<PathBuf> {
    possible_roots
        .iter()
        .map(|root| root.join(relative_path))
        .find(|path| path.exists())
}

pub(crate) fn find_imports<'a>(
    module_directory: &'a Path,
    import_directories: &[&Path],
    module: &Module<'a>,
    bump_allocator: &'a Bump,
) -> Result<ModuleImports<'a>, ImportError<'a>> {
    let directories_for_absolute_imports = import_directories;
    let directories_for_relative_imports = &[module_directory][..];

    let path_from_name = |name| {
        let mut path = PathBuf::from(name);
        path.set_extension(SOURCE_FILE_EXTENSION);
        path
    };
    let mut imports = Vec::new();
    for import in module.imports.iter() {
        let (what, possible_root_directories) = match import {
            Import::Import {
                what:
                    what @ QualifiedName::QualifiedNonTypeName(QualifiedNonTypeName::Absolute { .. }),
                ..
            }
            | Import::Import {
                what: what @ QualifiedName::QualifiedTypeName(QualifiedTypeName::Absolute { .. }),
                ..
            }
            | Import::ImportAs {
                what:
                    what @ QualifiedName::QualifiedNonTypeName(QualifiedNonTypeName::Absolute { .. }),
                ..
            }
            | Import::ImportAs {
                what: what @ QualifiedName::QualifiedTypeName(QualifiedTypeName::Absolute { .. }),
                ..
            }
            | Import::FromImport {
                where_:
                    what @ QualifiedName::QualifiedNonTypeName(QualifiedNonTypeName::Absolute { .. }),
                ..
            }
            | Import::FromImport {
                where_: what @ QualifiedName::QualifiedTypeName(QualifiedTypeName::Absolute { .. }),
                ..
            }
            | Import::FromImportAs {
                where_:
                    what @ QualifiedName::QualifiedNonTypeName(QualifiedNonTypeName::Absolute { .. }),
                ..
            }
            | Import::FromImportAs {
                where_: what @ QualifiedName::QualifiedTypeName(QualifiedTypeName::Absolute { .. }),
                ..
            } => (what, directories_for_absolute_imports),
            Import::Import {
                what:
                    what_or_where @ QualifiedName::QualifiedNonTypeName(QualifiedNonTypeName::Relative {
                        ..
                    }),
                ..
            }
            | Import::Import {
                what:
                    what_or_where @ QualifiedName::QualifiedTypeName(QualifiedTypeName::Relative {
                        ..
                    }),
                ..
            }
            | Import::ImportAs {
                what:
                    what_or_where @ QualifiedName::QualifiedNonTypeName(QualifiedNonTypeName::Relative {
                        ..
                    }),
                ..
            }
            | Import::ImportAs {
                what:
                    what_or_where @ QualifiedName::QualifiedTypeName(QualifiedTypeName::Relative {
                        ..
                    }),
                ..
            }
            | Import::FromImport {
                where_:
                    what_or_where @ QualifiedName::QualifiedNonTypeName(QualifiedNonTypeName::Relative {
                        ..
                    }),
                ..
            }
            | Import::FromImport {
                where_:
                    what_or_where @ QualifiedName::QualifiedTypeName(QualifiedTypeName::Relative {
                        ..
                    }),
                ..
            }
            | Import::FromImportAs {
                where_:
                    what_or_where @ QualifiedName::QualifiedNonTypeName(QualifiedNonTypeName::Relative {
                        ..
                    }),
                ..
            }
            | Import::FromImportAs {
                where_:
                    what_or_where @ QualifiedName::QualifiedTypeName(QualifiedTypeName::Relative {
                        ..
                    }),
                ..
            } => (what_or_where, directories_for_relative_imports),
        };
        let path_to_search = path_from_name(what);
        let path = bump_allocator.alloc_path(
            find_path(&path_to_search, possible_root_directories).ok_or_else(|| {
                ImportError::ModuleNotFound {
                    import_path: *what,
                    path_to_search: path_to_search.clone(),
                }
            })?,
        );
        imports.push((import, path));
    }
    let imports = bump_allocator.alloc_slice_copy(&imports);
    Ok(imports)
}

type ImportResult<'a> =
    Result<HashMap<&'a Path, (&'a Module<'a>, ModuleImports<'a>)>, Box<dyn ErrorReport + 'a>>;

fn find_all_imports<'a>(
    main_module: ModuleWithImports<'a>,
    import_directories: &[&Path],
    bump_allocator: &'a Bump,
) -> ImportResult<'a> {
    let mut processed_files = HashSet::new();
    processed_files.insert(main_module.canonical_path.to_owned());

    let mut files_to_process: VecDeque<_> = main_module.imports.iter().collect();

    let mut all_modules: HashMap<&'a Path, (&'a Module<'a>, ModuleImports<'a>)> = HashMap::new();
    all_modules.insert(
        main_module.canonical_path,
        (
            bump_allocator.alloc(main_module.module),
            main_module.imports,
        ),
    );

    while !files_to_process.is_empty() {
        let (_, next_filename) = files_to_process.pop_back().expect("queue is not empty");
        if processed_files.contains(*next_filename) {
            continue;
        }

        let canonical_filename = bump_allocator.alloc_path(next_filename);

        let source = bump_allocator.alloc_str(
            &std::fs::read_to_string(canonical_filename).map_err(LexerError::FailedToReadFile)?,
        );

        let module = parse_module(canonical_filename, source, bump_allocator)?;

        let module_directory = canonical_filename
            .parent()
            .expect("variable contains complete filename and thus has a parent");
        let imports = find_imports(
            module_directory,
            import_directories,
            &module,
            bump_allocator,
        )?;

        processed_files.insert(canonical_filename.to_owned());

        files_to_process.extend(imports.iter());

        debug_assert!(!all_modules.contains_key(canonical_filename));
        all_modules.insert(canonical_filename, (bump_allocator.alloc(module), imports));
    }

    Ok(all_modules)
}

fn check_capitalization_of_renamed_imports<'a>(module: &'a Module) -> Result<(), ImportError<'a>> {
    for import in module.imports {
        let (expected_token_type, actual_token, hint_location) = match import {
            Import::Import { .. } | Import::FromImport { .. } => {
                continue;
            }
            Import::ImportAs { what, as_, .. } => (
                TokenType::LowercaseIdentifier,
                as_.token(),
                what.source_location(),
            ),
            Import::FromImportAs { symbol, as_, .. } => (
                symbol.token().type_,
                as_.token(),
                symbol.token().source_location,
            ),
        };

        let actual_token_type = actual_token.type_;
        if expected_token_type != actual_token_type {
            return Err(ImportError::ImportedAsForbiddenName {
                as_: actual_token,
                hint_location,
            });
        }
    }
    Ok(())
}

pub(crate) fn connect_modules<'a>(
    main_module: ModuleWithImports<'a>,
    import_directories: &[&Path],
    bump_allocator: &'a Bump,
) -> Result<ConnectedModules<'a>, Box<dyn ErrorReport + 'a>> {
    let all_modules = find_all_imports(main_module, import_directories, bump_allocator)?;

    for (module, _) in all_modules.values() {
        check_capitalization_of_renamed_imports(module)?;
    }

    let mut connected_modules = Vec::new();
    for (path, (module, imports)) in &all_modules {
        let connected_imports: Vec<_> = imports
            .iter()
            .map(|(import, path)| ConnectedImport {
                import,
                target_module: all_modules.get(*path).expect("all imports were resolved").0,
                target_module_path: path,
            })
            .collect();
        let connected_imports = bump_allocator.alloc_slice_clone(&connected_imports);
        connected_modules.push(ConnectedModule {
            canonical_path: path,
            imports: connected_imports,
            definitions: module.definitions,
        });
    }
    let connected_modules = bump_allocator.alloc_slice_clone(&connected_modules);
    Ok(connected_modules)
}

pub(crate) fn resolve_imports<'a>(
    modules: ConnectedModules<'a>,
    bump_allocator: &'a Bump,
) -> Result<&'a [ResolvedModule<'a>], Box<dyn ErrorReport + 'a>> {
    let mut resolved_modules = Vec::with_capacity(modules.len());
    for module in modules {
        let mut definitions = Vec::new();
        for connected_import in module.imports {
            match connected_import.import {
                Import::Import { what, .. } => {
                    for definition in connected_import.target_module.exported_definitions() {
                        let name = bump_allocator.alloc_str(&format!(
                            "{}::{}",
                            what.tokens(),
                            definition.identifier().as_string()
                        ));
                        definitions.push(ResolvedDefinition {
                            name,
                            definition,
                            origin: Some(connected_import.import),
                        })
                    }
                }
                Import::ImportAs { as_, .. } => {
                    for definition in connected_import.target_module.exported_definitions() {
                        let name = bump_allocator.alloc_str(&format!(
                            "{}::{}",
                            as_.as_string(),
                            definition.identifier().as_string()
                        ));
                        definitions.push(ResolvedDefinition {
                            name,
                            definition,
                            origin: Some(connected_import.import),
                        })
                    }
                }
                Import::FromImport { where_, symbol, .. } => {
                    let mut found = false;
                    for definition in connected_import.target_module.exported_definitions() {
                        if symbol.token().lexeme() == definition.identifier().token().lexeme() {
                            found = true;
                            definitions.push(ResolvedDefinition {
                                name: symbol.token().lexeme(),
                                definition,
                                origin: Some(connected_import.import),
                            })
                        }
                    }
                    if !found {
                        return Err(Box::new(ImportError::SymbolNotFound {
                            imported_module_path: connected_import.target_module_path,
                            import_path: *where_,
                            symbol_token: *symbol,
                            non_exported_definition: connected_import
                                .target_module
                                .private_definitions()
                                .cloned()
                                .find(|definition| {
                                    definition.identifier().token().lexeme()
                                        == symbol.token().lexeme()
                                }),
                        }));
                    }
                }
                Import::FromImportAs {
                    where_,
                    symbol,
                    as_,
                    ..
                } => {
                    let mut found = false;
                    for definition in connected_import.target_module.exported_definitions() {
                        if symbol.token().lexeme() == definition.identifier().token().lexeme() {
                            found = true;
                            definitions.push(ResolvedDefinition {
                                name: as_.token().lexeme(),
                                definition,
                                origin: Some(connected_import.import),
                            })
                        }
                    }
                    if !found {
                        return Err(Box::new(ImportError::SymbolNotFound {
                            imported_module_path: connected_import.target_module_path,
                            import_path: *where_,
                            symbol_token: *symbol,
                            non_exported_definition: connected_import
                                .target_module
                                .private_definitions()
                                .cloned()
                                .find(|definition| {
                                    definition.identifier().token().lexeme()
                                        == symbol.token().lexeme()
                                }),
                        }));
                    }
                }
            }
        }

        for definition in module.definitions {
            definitions.push(ResolvedDefinition {
                name: definition.identifier().token().lexeme(),
                definition,
                origin: None,
            });
        }

        let resolved_definitions = bump_allocator.alloc_slice_clone(&definitions);
        resolved_modules.push(ResolvedModule {
            canonical_path: module.canonical_path,
            definitions: resolved_definitions,
        });
    }
    let resolved_modules = bump_allocator.alloc_slice_clone(&resolved_modules);
    Ok(resolved_modules)
}

pub(crate) fn categorize_names<'a>(
    module: &'a ResolvedModule<'a>,
    bump_allocator: &'a Bump,
) -> Result<&'a ModuleWithCategorizedNames<'a>, Box<dyn ErrorReport + 'a>> {
    let mut type_names: hashbrown::HashMap<&'a str, &'a TypeDefinition<'a>, _, _> =
        hashbrown::HashMap::new_in(bump_allocator);
    let mut non_type_names: hashbrown::HashMap<&'a str, &'a NonTypeDefinition<'a>, _, _> =
        hashbrown::HashMap::new_in(bump_allocator);

    let mut overload_sets: HashMap<_, Vec<_>> = HashMap::new();

    for definition in module.definitions {
        match definition.definition {
            Definition::Struct(struct_) => {
                let type_definition = &*bump_allocator.alloc(TypeDefinition {
                    origin: definition.origin,
                    definition: bump_allocator.alloc(TypeDefinitionKind::Struct(struct_)),
                });
                if let Some(previous_definition) =
                    type_names.insert(definition.name, type_definition)
                {
                    return Err(Box::new(NameError::DuplicateTypeName {
                        name: definition.name,
                        previous_type_definition: previous_definition,
                        current_type_definition: type_definition,
                    }));
                }
            }
            Definition::Function(function) => {
                overload_sets
                    .entry(definition.name)
                    .or_default()
                    .push(&*bump_allocator.alloc(Overload {
                        origin: definition.origin,
                        definition: function,
                    }));
            }
            Definition::GlobalVariable(global_variable) => {
                let global_variable_definition =
                    &*bump_allocator.alloc(NonTypeDefinition::GlobalVariable {
                        origin: definition.origin,
                        definition: global_variable,
                    });
                if let Some(previous_definition) =
                    non_type_names.insert(definition.name, global_variable_definition)
                {
                    return Err(Box::new(NameError::DuplicateNonTypeName {
                        name: definition.name,
                        previous_definition,
                        current_definition: global_variable_definition,
                    }));
                }
            }
        }
    }

    for (name, overloads) in overload_sets {
        non_type_names.insert(
            name,
            bump_allocator.alloc(NonTypeDefinition::Function(
                &*bump_allocator.alloc_slice_clone(&overloads),
            )),
        );
    }

    Ok(bump_allocator.alloc(ModuleWithCategorizedNames {
        canonical_path: module.canonical_path,
        type_names,
        non_type_names,
    }))
}
