use std::collections::{HashSet, VecDeque};
use std::ops::Deref;
use std::path::{Path, PathBuf};

use bumpalo::Bump;

pub use representations::ModuleWithImports;

use crate::constants::SOURCE_FILE_EXTENSION;
use crate::import_resolution::errors::{DuplicateIdentifiersError, ImportError};
use crate::import_resolution::representations::{
    ConnectedImport, ModuleImports, ModuleWithConnectedImports, ModuleWithImportsAndExports,
    ModuleWithResolvedImportsAndExports, ModulesWithConnectedImports, ModulesWithImports,
    ModulesWithImportsAndExports, ModulesWithResolvedImportsAndExports, ResolvedImport,
};
use crate::lexer::LexerError;
use crate::parser::errors::ErrorReport;
use crate::parser::ir_parsed::{
    Import, Module, QualifiedName, QualifiedNonTypeName, QualifiedTypeName,
};
use crate::parser::parse_module;
use crate::utils::AllocPath;

pub(crate) mod errors;
mod representations;

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
            }
            | Import::Import {
                what: what @ QualifiedName::QualifiedTypeName(QualifiedTypeName::Absolute { .. }),
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
            }
            | Import::Import {
                what:
                    what_or_where @ QualifiedName::QualifiedTypeName(QualifiedTypeName::Relative {
                        ..
                    }),
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
        imports.push((*import, path));
    }
    let imports = bump_allocator.alloc_slice_copy(&imports);
    Ok(imports)
}

trait CheckAgainstDuplicateIdentifierDefinitions<'a> {
    fn check_against_duplicate_identifier_definitions(
        self,
    ) -> Result<ModulesWithImports<'a>, DuplicateIdentifiersError<'a>>;
}

impl<'a> CheckAgainstDuplicateIdentifierDefinitions<'a> for ModulesWithImports<'a> {
    fn check_against_duplicate_identifier_definitions(
        self,
    ) -> Result<ModulesWithImports<'a>, DuplicateIdentifiersError<'a>> {
        for module_with_imports in self {
            for (i, current_definition) in module_with_imports.module.definitions.iter().enumerate()
            {
                for definition in &module_with_imports.module.definitions[..i] {
                    if current_definition.identifier().token().lexeme()
                        == definition.identifier().token().lexeme()
                    {
                        return Err(DuplicateIdentifiersError {
                            definition: *current_definition,
                            previous_definition: *definition,
                        });
                    }
                }
            }
        }
        Ok(self)
    }
}

fn find_all_imports<'a>(
    main_module: ModuleWithImports<'a>,
    import_directories: &[&Path],
    bump_allocator: &'a Bump,
) -> Result<ModulesWithImports<'a>, Box<dyn ErrorReport + 'a>> {
    println!("main module is {}", main_module.canonical_path.display());

    let mut processed_files = HashSet::new();
    processed_files.insert(main_module.canonical_path.deref().to_owned());

    let mut files_to_process: VecDeque<_> =
        main_module.imports.iter().map(|(_, path)| *path).collect();

    let mut all_modules = vec![main_module];

    while !files_to_process.is_empty() {
        let next_filename = files_to_process.pop_back().expect("queue is not empty");
        if processed_files.contains(next_filename) {
            continue;
        }

        println!("processing next module: {}...", next_filename.display());

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

        for (_, path) in imports {
            println!("\timport is: {}", path.display());
        }

        processed_files.insert(canonical_filename.deref().to_owned());

        files_to_process.extend(imports.iter().map(|(_, path)| *path));

        all_modules.push(ModuleWithImports {
            canonical_path: canonical_filename,
            module,
            imports,
        });
    }

    let all_modules = bump_allocator.alloc_slice_copy(&all_modules);

    Ok(all_modules)
}

trait ResolveAllImports<'a> {
    fn resolve_all_imports(
        self,
        bump_allocator: &'a Bump,
    ) -> ModulesWithResolvedImportsAndExports<'a>;
}

impl<'a> ResolveAllImports<'a> for ModulesWithImportsAndExports<'a> {
    fn resolve_all_imports(
        self,
        bump_allocator: &'a Bump,
    ) -> ModulesWithResolvedImportsAndExports<'a> {
        let modules_with_resolved_imports: Vec<_> = self
            .iter()
            .map(|module| resolve_imports(*module, self, bump_allocator))
            .collect();
        bump_allocator.alloc_slice_copy(&modules_with_resolved_imports)
    }
}

fn resolve_imports<'a>(
    module_with_imports_and_exports: ModuleWithImportsAndExports<'a>,
    all_modules: ModulesWithImportsAndExports<'a>,
    bump_allocator: &'a Bump,
) -> ModuleWithResolvedImportsAndExports<'a> {
    let resolved_imports: Vec<_> = module_with_imports_and_exports
        .imports
        .iter()
        .map(|(import, path)| ResolvedImport {
            import: *import,
            from_module: module_by_canonical_path(path, all_modules)
                .expect("path has been found before"),
        })
        .collect();
    let resolved_imports = bump_allocator.alloc_slice_copy(&resolved_imports);
    ModuleWithResolvedImportsAndExports {
        canonical_path: module_with_imports_and_exports.canonical_path,
        module: module_with_imports_and_exports.module,
        imports: resolved_imports,
        exported_definitions: module_with_imports_and_exports.exported_definitions,
    }
}

trait GatherAllExports<'a> {
    fn gather_all_exports(self, bump_allocator: &'a Bump) -> ModulesWithImportsAndExports;
}

impl<'a> GatherAllExports<'a> for ModulesWithImports<'a> {
    fn gather_all_exports(self, bump_allocator: &'a Bump) -> ModulesWithImportsAndExports {
        let result: Vec<_> = self
            .iter()
            .map(|module| gather_exports(*module, bump_allocator))
            .collect();
        bump_allocator.alloc_slice_copy(&result)
    }
}

fn gather_exports<'a>(
    module_with_imports: ModuleWithImports<'a>,
    bump_allocator: &'a Bump,
) -> ModuleWithImportsAndExports<'a> {
    let exports: Vec<_> = module_with_imports
        .module
        .definitions
        .iter()
        .filter(|definition| definition.is_exported())
        .copied()
        .collect();
    ModuleWithImportsAndExports {
        canonical_path: module_with_imports.canonical_path,
        module: module_with_imports.module,
        imports: module_with_imports.imports,
        exported_definitions: bump_allocator.alloc_slice_copy(&exports),
    }
}

trait CheckImports<'a> {
    fn check_imports(
        self,
        bump_allocator: &'a Bump,
    ) -> Result<ModulesWithConnectedImports<'a>, ImportError<'a>>;
}

impl<'a> CheckImports<'a> for ModulesWithResolvedImportsAndExports<'a> {
    fn check_imports(
        self,
        bump_allocator: &'a Bump,
    ) -> Result<ModulesWithConnectedImports<'a>, ImportError<'a>> {
        let modules_with_connected_imports: Result<Vec<_>, _> = self
            .iter()
            .map(|module| check_imports_for_module(*module, bump_allocator))
            .collect();
        Ok(bump_allocator.alloc_slice_copy(&(modules_with_connected_imports?)))
    }
}

fn check_imports_for_module<'a>(
    module_with_resolved_imports_and_exports: ModuleWithResolvedImportsAndExports<'a>,
    bump_allocator: &'a Bump,
) -> Result<ModuleWithConnectedImports<'a>, ImportError<'a>> {
    let mut connected_imports = Vec::new();
    for resolved_import in module_with_resolved_imports_and_exports.imports {
        let symbol_to_import = resolved_import.import.imported_symbol();
        if symbol_to_import.is_none() {
            continue;
        }
        let symbol_to_import = symbol_to_import.unwrap();
        let name_to_import = symbol_to_import.token().lexeme();
        let source_module = resolved_import.from_module;
        let definition = source_module
            .exported_definitions
            .iter()
            .cloned()
            .find(|definition| definition.identifier().token().lexeme() == name_to_import);

        match definition {
            Some(definition) => connected_imports.push(ConnectedImport {
                import: resolved_import.import,
                definition,
            }),
            None => {
                let non_exported_definition =
                    source_module
                        .module
                        .definitions
                        .iter()
                        .find_map(|definition| {
                            (definition.identifier().token().lexeme() == name_to_import)
                                .then_some(*definition)
                        });
                return Err(ImportError::SymbolNotFound {
                    imported_module: resolved_import.from_module,
                    symbol_token: symbol_to_import,
                    import_path: resolved_import.import.what_or_where(),
                    non_exported_definition,
                });
            }
        }
    }

    for connected_import in connected_imports.iter().copied() {
        let Some(imported_as) = connected_import.import.as_what() else {
            continue;
        };

        if let Some(definition) = module_with_resolved_imports_and_exports
            .module
            .definitions
            .iter()
            .copied()
            .find(|definition| {
                definition.identifier().token().lexeme() == imported_as.token().lexeme()
            })
        {
            return Err(ImportError::ImportedClashWithLocalDefinition {
                import: connected_import,
                local_definition_with_same_identifier: definition,
            });
        }
    }

    for (i, resolved_import) in module_with_resolved_imports_and_exports
        .imports
        .iter()
        .enumerate()
    {
        match resolved_import.import.imported_namespace() {
            Some(qualified_name_or_identifier) => {
                let duplicate_import = module_with_resolved_imports_and_exports.imports[..i]
                    .iter()
                    .filter_map(|resolved_import| {
                        resolved_import
                            .import
                            .imported_namespace()
                            .map(|namespace| (*resolved_import, namespace))
                    })
                    .find(|(_, imported_namespace)| {
                        *imported_namespace == qualified_name_or_identifier
                    });
                if let Some((duplicate_import, _)) = duplicate_import {
                    return Err(ImportError::DuplicateImport {
                        import: *resolved_import,
                        previous_import: duplicate_import,
                    });
                }
            }
            None => {
                continue;
            }
        }
    }

    Ok(ModuleWithConnectedImports {
        canonical_path: module_with_resolved_imports_and_exports.canonical_path,
        module: module_with_resolved_imports_and_exports.module,
        imports: bump_allocator.alloc_slice_copy(&connected_imports),
    })
}

fn module_by_canonical_path<'a>(
    canonical_path: &'a Path,
    all_modules: ModulesWithImportsAndExports<'a>,
) -> Option<ModuleWithImportsAndExports<'a>> {
    all_modules
        .iter()
        .find_map(|module| (module.canonical_path == canonical_path).then_some(*module))
}

pub(crate) fn perform_import_resolution<'a>(
    main_module: ModuleWithImports<'a>,
    import_directories: &[&Path],
    bump_allocator: &'a Bump,
) -> Result<ModulesWithConnectedImports<'a>, Box<dyn ErrorReport + 'a>> {
    Ok(
        find_all_imports(main_module, import_directories, bump_allocator)?
            .check_against_duplicate_identifier_definitions()?
            .gather_all_exports(bump_allocator)
            .resolve_all_imports(bump_allocator)
            .check_imports(bump_allocator)?,
    )
}
