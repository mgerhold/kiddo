#![allow(dead_code)]
#![allow(clippy::result_large_err)]
#![feature(ptr_from_ref)]
#![feature(slice_from_ptr_range)]
#![feature(entry_insert)]
#![feature(iter_collect_into)]
#![feature(iter_intersperse)]

extern crate core;

use bumpalo::Bump;

pub use crate::command_line_arguments::CommandLineArguments;
use crate::helpers::{gather_import_directories, get_canonical_path_to_main_module};
use crate::import_resolution::representations::NonTypeDefinition;
use crate::import_resolution::{
    categorize_names, connect_modules, find_imports, resolve_imports, ModuleWithImports,
};
use crate::name_lookup::{completely_resolve_modules, partially_resolve_module};
use crate::parser::errors::ErrorReport;
use crate::parser::parse_module;
use crate::utils::AllocPath;

mod command_line_arguments;
mod constants;
mod helpers;
mod import_resolution;
mod lexer;
mod name_lookup;
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

    let all_modules = connect_modules(main_module, import_directories, bump_allocator)?;
    let all_modules = resolve_imports(all_modules, bump_allocator)?;

    if let Some(ast_output_path) = command_line_args.ast_output_path {
        let ast = format!("{:#?}", &all_modules);
        std::fs::write(ast_output_path, ast).unwrap();
    }

    let all_modules: Result<Vec<_>, _> = all_modules
        .iter()
        .map(|module| categorize_names(module, bump_allocator))
        .collect();

    let all_modules = all_modules?;

    for module in &all_modules {
        println!("{}", module.canonical_path.display());
        println!("type names:");
        for (name, _) in &module.type_names {
            println!("\t{name}");
        }
        println!("non-type names:");
        for (name, definition) in &module.non_type_names {
            match definition {
                NonTypeDefinition::GlobalVariable { .. } => println!("\t{name}"),
                NonTypeDefinition::Function(overload_set) => {
                    println!("\t{name} ({} overload(s))", overload_set.len())
                }
            }
        }
    }

    let all_modules = all_modules
        .iter()
        .map(|module| partially_resolve_module(module, bump_allocator))
        .collect::<Result<Vec<_>, _>>()?;

    let all_modules = &*bump_allocator.alloc_slice_clone(&all_modules);

    let all_modules = completely_resolve_modules(all_modules, bump_allocator);

    // let after_lookup = perform_name_lookup(all_modules, bump_allocator)?;

    // dbg!(all_modules);

    Ok(())
}
