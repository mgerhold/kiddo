#![allow(dead_code)]
#![feature(os_str_bytes)]

use bumpalo::Bump;

pub use crate::command_line_arguments::CommandLineArguments;
use crate::helpers::{gather_import_directories, get_canonical_path_to_main_module};
use crate::import_resolution::{find_imports, perform_import_resolution, ModuleWithImports};
use crate::parser::errors::ErrorReport;
use crate::parser::parse_module;
use crate::utils::AllocPath;

mod command_line_arguments;
mod constants;
mod helpers;
mod import_resolution;
mod lexer;
mod parser;
mod token;
mod utils;

#[cfg(test)]
mod test;

pub fn main<'a>(
    bump_allocator: &'a Bump,
    command_line_args: CommandLineArguments,
) -> Result<(), Box<dyn ErrorReport + 'a>> {
    let main_module_canonical_path =
        get_canonical_path_to_main_module(&command_line_args, bump_allocator)?;
    let main_module_directory = bump_allocator.alloc_path(
        main_module_canonical_path
            .parent()
            .expect("the main module is a file and therefore must reside in a directory"),
    );

    let import_directories =
        gather_import_directories(main_module_directory, &command_line_args, bump_allocator)?;

    let main_module_source =
        bump_allocator.alloc_str(&std::fs::read_to_string(main_module_canonical_path).unwrap());

    let main_module = parse_module(
        main_module_canonical_path,
        main_module_source,
        bump_allocator,
    )?;

    // get imports of main module
    let main_module_imports = find_imports(
        main_module_directory,
        import_directories,
        &main_module,
        bump_allocator,
    )?;
    let main_module = ModuleWithImports {
        canonical_path: main_module_canonical_path,
        module: main_module,
        imports: main_module_imports,
    };

    let all_modules = perform_import_resolution(main_module, import_directories, bump_allocator)?;

    if let Some(ast_output_path) = command_line_args.ast_output_path {
        let ast = format!("{:#?}", &all_modules);
        std::fs::write(ast_output_path, ast).unwrap();
    }

    // dbg!(all_modules);

    Ok(())
}
