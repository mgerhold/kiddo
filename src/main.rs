use std::ops::Deref;
use std::path::Path;
use std::rc::Rc;

use crate::import_resolution::{resolve_all_imports, resolve_imports, ModuleWithImports};
use crate::parser::parse_module;

mod import_resolution;
mod lexer;
mod parser;
mod token;

fn main() {
    let main_module_filename = std::path::PathBuf::from("test.ceat");
    let main_module_canonical_path: Rc<Path> = main_module_filename
        .canonicalize()
        .expect("unable to canonicalize path")
        .into();

    let main_module_directory = main_module_canonical_path
        .parent()
        .expect("unable to get parent directory")
        .to_owned();

    let import_directories = vec![main_module_directory.deref()]; // this is a vector since this will be filled dynamically later
    let main_module_source: Rc<str> = std::fs::read_to_string(&main_module_filename)
        .unwrap()
        .into();

    let main_module = parse_module(Rc::clone(&main_module_canonical_path), main_module_source);
    match main_module {
        Ok(main_module) => {
            let main_module_imports =
                resolve_imports(&main_module_directory, &import_directories, &main_module).unwrap();
            let main_module = ModuleWithImports {
                canonical_path: main_module_canonical_path,
                module: main_module,
                imports: main_module_imports,
            };
            let all_modules = resolve_all_imports(main_module, &import_directories).unwrap();
            dbg!(all_modules);
        }
        Err(error) => eprintln!("{error:#?}"),
    }
}
