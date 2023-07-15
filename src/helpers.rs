use std::path::Path;

use bumpalo::Bump;

use crate::import_resolution::errors::ImportError;
use crate::parser::errors::ErrorReport;
use crate::utils::AllocPath;
use crate::CommandLineArguments;

pub(crate) fn get_canonical_path_to_main_module<'a>(
    command_line_arguments: &CommandLineArguments,
    bump_allocator: &'a Bump,
) -> Result<&'a Path, Box<dyn ErrorReport + 'a>> {
    // get canonical path to main module
    let main_module_filename = bump_allocator.alloc_path(&command_line_arguments.source_file);
    if !main_module_filename.exists() || !main_module_filename.is_file() {
        return Err(Box::new(ImportError::FileNotFound {
            path: main_module_filename,
        }));
    }
    Ok(
        bump_allocator.alloc_path(main_module_filename.canonicalize().map_err(|_| {
            ImportError::UnableToCanonicalize {
                path: main_module_filename,
            }
        })?),
    )
}

pub(crate) fn gather_import_directories<'a>(
    main_module_directory: &'a Path,
    command_line_arguments: &CommandLineArguments,
    bump_allocator: &'a Bump,
) -> Result<&'a [&'a Path], Box<dyn ErrorReport + 'a>> {
    // gather import directories
    let import_directories: Vec<_> = std::iter::once(main_module_directory)
        .chain(
            command_line_arguments
                .import_paths
                .iter()
                .map(|path| bump_allocator.alloc_path(path)),
        )
        .collect();
    let import_directories = bump_allocator.alloc_slice_copy(&import_directories);

    // ensure all import directories exist
    for path in import_directories.iter() {
        if !path.exists() || !path.is_dir() {
            return Err(Box::new(ImportError::ImportPathNotFound {
                import_path: path,
            }));
        }
    }

    Ok(import_directories)
}
