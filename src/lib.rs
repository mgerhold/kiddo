#![allow(dead_code)]
#![feature(os_str_bytes)]

use std::ops::Deref;

use bumpalo::Bump;

use crate::import_resolution::{resolve_all_imports, resolve_imports, ModuleWithImports};
use crate::parser::errors::ErrorReport;
use crate::parser::parse_module;
use crate::utils::AllocPath;

mod import_resolution;
mod lexer;
mod parser;
mod token;
mod utils;

pub fn main<'a>(bump_allocator: &'a Bump) -> Result<(), Box<dyn ErrorReport + 'a>> {
    let main_module_filename = bump_allocator.alloc_path(std::path::PathBuf::from("test.ceat"));
    let main_module_canonical_path = bump_allocator.alloc_path(
        main_module_filename
            .canonicalize()
            .expect("unable to canonicalize path"),
    );

    let main_module_directory = bump_allocator.alloc_path(
        main_module_canonical_path
            .parent()
            .expect("unable to get parent directory"),
    );

    let import_directories = vec![main_module_directory.deref()]; // this is a vector since this will be filled dynamically later
    let main_module_source =
        bump_allocator.alloc_str(&std::fs::read_to_string(main_module_filename).unwrap());

    let main_module = parse_module(
        main_module_canonical_path,
        main_module_source,
        bump_allocator,
    )?;

    let main_module_imports =
        resolve_imports(main_module_directory, &import_directories, &main_module)?;
    let main_module = ModuleWithImports {
        canonical_path: main_module_canonical_path,
        module: main_module,
        imports: main_module_imports,
    };
    let all_modules =
        resolve_all_imports(main_module, &import_directories, bump_allocator).unwrap();
    dbg!(all_modules);

    Ok(())
}