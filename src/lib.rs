#![allow(dead_code)]
#![feature(os_str_bytes)]

use std::path::PathBuf;

use bumpalo::Bump;
use clap::Parser;

use crate::import_resolution::errors::ImportError;
use crate::import_resolution::{find_imports, perform_input_resolution, ModuleWithImports};
use crate::parser::errors::ErrorReport;
use crate::parser::parse_module;
use crate::utils::AllocPath;

mod constants;
mod import_resolution;
mod lexer;
mod parser;
mod token;
mod utils;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct CommandLineArguments {
    #[arg(short, long)]
    source_file: PathBuf,

    #[clap(short, long, num_args = 0..)]
    import_paths: Vec<PathBuf>,
}

pub fn main<'a>(bump_allocator: &'a Bump) -> Result<(), Box<dyn ErrorReport + 'a>> {
    let command_line_arguments = CommandLineArguments::parse();

    let main_module_filename = bump_allocator.alloc_path(command_line_arguments.source_file);
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

    let import_directories: Vec<_> = std::iter::once(main_module_directory)
        .chain(
            command_line_arguments
                .import_paths
                .iter()
                .map(|path| bump_allocator.alloc_path(path)),
        )
        .collect();
    let import_directories = bump_allocator.alloc_slice_copy(&import_directories);

    for path in import_directories.iter() {
        if !path.exists() || !path.is_dir() {
            return Err(Box::new(ImportError::ImportPathNotFound {
                import_path: path,
            }));
        }
    }

    let main_module_source =
        bump_allocator.alloc_str(&std::fs::read_to_string(main_module_filename).unwrap());

    let main_module = parse_module(
        main_module_canonical_path,
        main_module_source,
        bump_allocator,
    )?;

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

    let all_modules = perform_input_resolution(main_module, import_directories, bump_allocator)?;

    dbg!(all_modules);

    Ok(())
}
