use std::collections::{HashSet, VecDeque};
use std::ops::Deref;
use std::path::{Path, PathBuf};

use bumpalo::Bump;

use crate::constants::SOURCE_FILE_EXTENSION;
use crate::lexer::LexerError;
use crate::parser::errors::{print_error, print_note, ErrorReport};
use crate::parser::ir_parsed::{Definition, Identifier, Import, Module, QualifiedName};
use crate::parser::parse_module;
use crate::utils::AllocPath;

#[derive(Debug)]
pub enum ImportError<'a> {
    ModuleNotFound {
        import_path: QualifiedName<'a>,
        path_to_search: PathBuf,
    },
    SymbolNotFound {
        imported_module: ModuleWithImportsAndExports<'a>,
        import_path: QualifiedName<'a>,
        symbol_token: Identifier<'a>,
        non_exported_definition: Option<Definition<'a>>,
    },
}

impl ErrorReport for ImportError<'_> {
    fn print_report(&self) {
        match self {
            ImportError::ModuleNotFound {
                import_path,
                path_to_search,
            } => {
                print_error(
                    &import_path
                        .tokens()
                        .last()
                        .expect("there should be at least one token")
                        .source_location,
                    format!(
                        "import error: '{}' not found in import paths",
                        path_to_search.display()
                    ),
                    "unable to resolve this import",
                );
            }
            ImportError::SymbolNotFound {
                imported_module,
                import_path,
                symbol_token,
                non_exported_definition,
            } => {
                print_error(
                    &symbol_token.token.source_location,
                    format!(
                        "import error: module '{}' (in '{}') does not export symbol '{}'",
                        import_path.as_string(),
                        imported_module.canonical_path.display(),
                        symbol_token.token.lexeme()
                    ),
                    "symbol not found",
                );
                if let Some(non_exported_definition) = non_exported_definition {
                    print_note(
                        &non_exported_definition.identifier().token.source_location,
                        "there is a definition with the requested name that has not been exported",
                        "did you forget to export this definition?",
                    );
                }
            }
        }
    }
}

fn find_path(relative_path: &Path, possible_roots: &[&Path]) -> Option<PathBuf> {
    possible_roots
        .iter()
        .map(|root| root.join(relative_path))
        .find(|path| path.exists())
}

type ModuleImports<'a> = &'a [(Import<'a>, &'a Path)];

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
                what: what @ QualifiedName::Absolute { .. },
            }
            | Import::ImportAs {
                what: what @ QualifiedName::Absolute { .. },
                ..
            }
            | Import::FromImport {
                where_: what @ QualifiedName::Absolute { .. },
                ..
            }
            | Import::FromImportAs {
                where_: what @ QualifiedName::Absolute { .. },
                ..
            } => (what, directories_for_absolute_imports),
            Import::Import {
                what: what_or_where @ QualifiedName::Relative { .. },
            }
            | Import::ImportAs {
                what: what_or_where @ QualifiedName::Relative { .. },
                ..
            }
            | Import::FromImport {
                where_: what_or_where @ QualifiedName::Relative { .. },
                ..
            }
            | Import::FromImportAs {
                where_: what_or_where @ QualifiedName::Relative { .. },
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

#[derive(Debug, Clone, Copy)]
pub(crate) struct ModuleWithImports<'a> {
    pub(crate) canonical_path: &'a Path,
    pub(crate) module: Module<'a>,
    pub(crate) imports: ModuleImports<'a>,
}

#[derive(Debug, Clone, Copy)]
pub struct ModuleWithImportsAndExports<'a> {
    pub(crate) canonical_path: &'a Path,
    pub(crate) module: Module<'a>,
    pub(crate) imports: ModuleImports<'a>,
    pub(crate) exported_definitions: &'a [Definition<'a>],
}

pub(crate) fn find_all_imports<'a>(
    main_module: ModuleWithImports<'a>,
    import_directories: &[&Path],
    bump_allocator: &'a Bump,
) -> Result<&'a [ModuleWithImports<'a>], Box<dyn ErrorReport + 'a>> {
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

#[derive(Debug, Clone, Copy)]
pub(crate) struct ResolvedImport<'a> {
    pub(crate) import: Import<'a>,
    pub(crate) from_module: ModuleWithImportsAndExports<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct ModuleWithResolvedImportsAndExports<'a> {
    pub(crate) canonical_path: &'a Path,
    pub(crate) module: Module<'a>,
    pub(crate) imports: &'a [ResolvedImport<'a>],
    pub(crate) exported_definitions: &'a [Definition<'a>],
}

pub(crate) fn resolve_all_imports<'a>(
    all_modules: &'a [ModuleWithImportsAndExports<'a>],
    bump_allocator: &'a Bump,
) -> &'a [ModuleWithResolvedImportsAndExports<'a>] {
    let modules_with_resolved_imports: Vec<_> = all_modules
        .iter()
        .map(|module| resolve_imports(*module, all_modules, bump_allocator))
        .collect();
    bump_allocator.alloc_slice_copy(&modules_with_resolved_imports)
}

fn resolve_imports<'a>(
    module_with_imports_and_exports: ModuleWithImportsAndExports<'a>,
    all_modules: &'a [ModuleWithImportsAndExports<'a>],
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

pub(crate) fn gather_all_exports<'a>(
    modules_with_imports: &'a [ModuleWithImports<'a>],
    bump_allocator: &'a Bump,
) -> &'a [ModuleWithImportsAndExports<'a>] {
    let result: Vec<_> = modules_with_imports
        .iter()
        .map(|module| gather_exports(*module, bump_allocator))
        .collect();
    bump_allocator.alloc_slice_copy(&result)
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

pub(crate) fn check_imports<'a>(
    modules_with_resolved_imports_and_exports: &'a [ModuleWithResolvedImportsAndExports],
) -> Result<(), ImportError<'a>> {
    for module in modules_with_resolved_imports_and_exports {
        check_imports_for_module(*module)?;
    }
    Ok(())
}

fn check_imports_for_module(
    module: ModuleWithResolvedImportsAndExports,
) -> Result<(), ImportError> {
    for resolved_import in module.imports {
        let symbol_to_import = resolved_import.import.imported_symbol();
        if symbol_to_import.is_none() {
            continue;
        }
        let symbol_to_import = symbol_to_import.unwrap();
        let name_to_import = symbol_to_import.token.lexeme();
        let source_module = resolved_import.from_module;
        if !source_module
            .exported_definitions
            .iter()
            .any(|definition| definition.identifier().token.lexeme() == name_to_import)
        {
            let non_exported_definition =
                source_module
                    .module
                    .definitions
                    .iter()
                    .find_map(|definition| {
                        (definition.identifier().token.lexeme() == name_to_import)
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
    Ok(())
}

fn module_by_canonical_path<'a>(
    canonical_path: &'a Path,
    all_modules: &'a [ModuleWithImportsAndExports],
) -> Option<ModuleWithImportsAndExports<'a>> {
    all_modules
        .iter()
        .find_map(|module| (module.canonical_path == canonical_path).then_some(*module))
}
