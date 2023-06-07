use crate::parser::errors::ParserError;
use crate::parser::ir_parsed::{Import, Module, QualifiedName};
use crate::parser::parse;
use crate::token::Token;
use lexer::tokenize;
use std::collections::{HashSet, VecDeque};
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use thiserror::Error;

mod lexer;
mod parser;
mod token;

const SOURCE_FILE_EXTENSION: &str = "ceat";

#[derive(Debug, Error)]
enum ImportError {
    #[error("unable to find module with filename '{path_to_search:1?}'")]
    ModuleNotFound { path_to_search: PathBuf },
}

fn find_path(
    relative_path: &std::path::Path,
    possible_roots: &[&std::path::Path],
) -> Option<PathBuf> {
    possible_roots
        .iter()
        .map(|root| root.join(relative_path))
        .find(|path| path.exists())
}

type ModuleImports = Vec<(Import, PathBuf)>;

fn resolve_imports(
    module_directory: &std::path::Path,
    import_directories: &[&std::path::Path],
    module: &Module,
) -> Result<ModuleImports, ImportError> {
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
                what: what @ QualifiedName::Relative { .. },
            }
            | Import::ImportAs {
                what: what @ QualifiedName::Relative { .. },
                ..
            }
            | Import::FromImport {
                where_: what @ QualifiedName::Relative { .. },
                ..
            }
            | Import::FromImportAs {
                where_: what @ QualifiedName::Relative { .. },
                ..
            } => (what, directories_for_relative_imports),
        };
        let path_to_search = path_from_name(what);
        let path = find_path(&path_to_search, possible_root_directories).ok_or_else(|| {
            ImportError::ModuleNotFound {
                path_to_search: path_to_search.clone(),
            }
        })?;
        imports.push((import.clone(), path));
    }
    Ok(imports)
}

#[derive(Debug, Clone)]
struct ModuleWithImports {
    canonical_path: Rc<Path>,
    module: Module,
    imports: ModuleImports,
}

fn resolve_all_imports(
    main_module: ModuleWithImports,
    import_directories: &[&std::path::Path],
) -> Result<Vec<ModuleWithImports>, ImportError> {
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

        let canonical_filename: Rc<Path> = next_filename.into();

        let source: Rc<str> = std::fs::read_to_string(&*canonical_filename)
            .unwrap()
            .into();

        let module = parse_module(Rc::clone(&canonical_filename), Rc::clone(&source)).unwrap(); // todo: handle parser error

        let module_directory = canonical_filename.parent().expect("unable to get parent");
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

fn parse_module<'a>(filename: Rc<Path>, source: Rc<str>) -> Result<Module, ParserError> {
    let tokens: Rc<[Token]> = tokenize(filename, source).unwrap().into(); // todo: handle lexer error
    parse(tokens)
}

fn main() {
    let filename = std::path::PathBuf::from("test.ceat");
    let canonical_path: Rc<Path> = filename
        .canonicalize()
        .expect("unable to canonicalize path")
        .into();
    let module_directory = canonical_path
        .parent()
        .expect("unable to get parent directory")
        .to_owned();
    let import_directories = vec![module_directory.deref()]; // this is a vector since this will be filled dynamically later
    let source: Rc<str> = std::fs::read_to_string(&filename).unwrap().into();
    let module = parse_module(Rc::clone(&canonical_path), source);
    match module {
        Ok(module) => {
            let imports = resolve_imports(&module_directory, &import_directories, &module).unwrap();
            let main_module = ModuleWithImports {
                canonical_path,
                module,
                imports,
            };
            let all_modules = resolve_all_imports(main_module, &import_directories).unwrap();
        }
        Err(error) => eprintln!("{error:#?}"),
    }
}
