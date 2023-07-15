use std::path::{Path, PathBuf};

use crate::import_resolution::{ConnectedImport, ModuleWithImportsAndExports, ResolvedImport};
use crate::parser::errors::{print_error, print_note, ErrorReport};
use crate::parser::ir_parsed::{Definition, Identifier, QualifiedName};

#[derive(Debug)]
pub struct DuplicateIdentifiersError<'a> {
    pub(crate) definition: Definition<'a>,
    pub(crate) previous_definition: Definition<'a>,
}

impl ErrorReport for DuplicateIdentifiersError<'_> {
    fn print_report(&self) {
        print_error(
            &self.definition.identifier().token.source_location,
            format!(
                "cannot re-define symbol '{}'",
                self.definition.identifier().token.lexeme()
            ),
            "re-defined here",
        );
        print_note(
            &self.previous_definition.identifier().token.source_location,
            "previous definition occurred here",
            "already defined here",
        );
    }
}

#[derive(Debug)]
pub enum ImportError<'a> {
    ImportPathNotFound {
        import_path: &'a Path,
    },
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
    ImportedClashWithLocalDefinition {
        import: ConnectedImport<'a>,
        local_definition_with_same_identifier: Definition<'a>,
    },
    DuplicateImport {
        import: ResolvedImport<'a>,
        previous_import: ResolvedImport<'a>,
    },
    UnableToCanonicalize {
        path: &'a Path,
    },
    FileNotFound {
        path: &'a Path,
    },
}

impl ErrorReport for ImportError<'_> {
    fn print_report(&self) {
        match self {
            ImportError::ImportPathNotFound { import_path } => {
                eprintln!(
                    "specified import path '{}' could not be found or is not a directory",
                    import_path.display()
                );
            }
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
                    format!("'{}' not found in import paths", path_to_search.display()),
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
                        "module '{}' (in '{}') does not export symbol '{}'",
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
            ImportError::ImportedClashWithLocalDefinition {
                import,
                local_definition_with_same_identifier,
            } => {
                print_error(
                    &import.import.as_what().expect("this error can only occur when importing as a name").token.source_location,
                    format!("imported definition '{}' clashes with module-local definition with the same name", import.import.as_what().unwrap().token.lexeme()),
                    "symbol imported here",
                );
                print_note(
                    &local_definition_with_same_identifier
                        .identifier()
                        .token
                        .source_location,
                    "module-local definition with the same name prevents import",
                    "symbol defined here",
                );
            }
            ImportError::DuplicateImport {
                import,
                previous_import,
            } => {
                print_error(
                    &import.import.imported_namespace().expect("this error can only occur when importing a whole namespace").source_location(),
                    format!("imported module '{}' clashes with previously imported module with the same name", import.import.imported_namespace().unwrap().as_string()),
                    "module imported here",
                );
                print_note(
                    &previous_import
                        .import
                        .imported_namespace()
                        .unwrap()
                        .source_location(),
                    "previously imported module prevents import",
                    "module imported here",
                );
            }
            ImportError::UnableToCanonicalize { path } => {
                eprintln!("error: unable to canonicalize path '{}'", path.display());
            }
            ImportError::FileNotFound { path } => {
                eprintln!("error: file not found: '{}'", path.display());
            }
        }
    }
}
