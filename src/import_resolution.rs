use std::collections::{HashSet, VecDeque};
use std::ops::Deref;
use std::path::{Path, PathBuf};

use bumpalo::Bump;
use thiserror::Error;

use crate::constants::SOURCE_FILE_EXTENSION;
use crate::lexer::LexerError;
use crate::parser::errors::ErrorReport;
use crate::parser::ir_parsed::{Import, Module, QualifiedName};
use crate::parser::parse_module;
use crate::utils::AllocPath;

#[derive(Debug, Error)]
pub enum ImportError {
    #[error("unable to find module with filename '{path_to_search:1?}'")]
    ModuleNotFound { path_to_search: PathBuf },
}

impl ErrorReport for ImportError {
    fn print_report(&self) {
        todo!()
    }
}

fn find_path(relative_path: &Path, possible_roots: &[&Path]) -> Option<PathBuf> {
    possible_roots
        .iter()
        .map(|root| root.join(relative_path))
        .find(|path| path.exists())
}

type ModuleImports<'a> = Vec<(Import<'a>, PathBuf)>;

pub(crate) fn resolve_imports<'a>(
    module_directory: &'a Path,
    import_directories: &[&Path],
    module: &Module<'a>,
) -> Result<ModuleImports<'a>, ImportError> {
    let directories_for_absolute_imports = import_directories;
    let directories_for_relative_imports = &[module_directory][..];

    let path_from_name = |name| {
        let mut path = PathBuf::from(name);
        path.set_extension(SOURCE_FILE_EXTENSION);
        path
    };
    let mut imports = ModuleImports::new();
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
        let path = find_path(&path_to_search, possible_root_directories).ok_or_else(|| {
            ImportError::ModuleNotFound {
                path_to_search: path_to_search.clone(),
            }
        })?;
        imports.push((*import, path));
    }
    Ok(imports)
}

#[derive(Debug, Clone)]
pub(crate) struct ModuleWithImports<'a> {
    pub(crate) canonical_path: &'a Path,
    pub(crate) module: Module<'a>,
    pub(crate) imports: ModuleImports<'a>,
}

pub(crate) fn resolve_all_imports<'a>(
    main_module: ModuleWithImports<'a>,
    import_directories: &[&Path],
    bump_allocator: &'a Bump,
) -> Result<Vec<ModuleWithImports<'a>>, Box<dyn ErrorReport + 'a>> {
    println!("main module is {}", main_module.canonical_path.display());

    let mut processed_files = HashSet::new();
    processed_files.insert(main_module.canonical_path.deref().to_owned());

    let mut files_to_process: VecDeque<_> = main_module
        .imports
        .iter()
        .map(|(_, path)| path.clone())
        .collect();

    let mut all_modules = vec![main_module];

    while !files_to_process.is_empty() {
        let next_filename = files_to_process.pop_back().expect("queue is not empty");
        if processed_files.contains(&next_filename) {
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
        let imports = resolve_imports(module_directory, import_directories, &module)?;

        for (_, path) in &imports {
            println!("\timport is: {}", path.display());
        }

        processed_files.insert(canonical_filename.deref().to_owned());

        files_to_process.extend(imports.iter().map(|(_, path)| path.clone()));

        all_modules.push(ModuleWithImports {
            canonical_path: canonical_filename,
            module,
            imports,
        });
    }

    Ok(all_modules)
}
