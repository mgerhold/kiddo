use crate::import_resolution::{resolve_all_imports, resolve_imports, ModuleWithImports};
use crate::parser::parse_module;
use std::ops::Deref;
use std::path::Path;
use std::rc::Rc;

mod import_resolution;
mod lexer;
mod parser;
mod token;

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
