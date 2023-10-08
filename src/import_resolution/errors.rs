use std::path::{Path, PathBuf};

use crate::import_resolution::representations::{
    NonTypeDefinition, TypeDefinition, TypeDefinitionKind,
};
use crate::parser::errors::{print_error, print_note, ErrorReport};
use crate::parser::ir_parsed::{Definition, Identifier, Import, QualifiedName};
use crate::token::{SourceLocation, Token, TokenType};

#[derive(Debug)]
pub struct DuplicateIdentifiersError<'a> {
    pub(crate) definition: Definition<'a>,
    pub(crate) previous_definition: Definition<'a>,
}

impl ErrorReport for DuplicateIdentifiersError<'_> {
    fn print_report(&self, output_filename: Option<&Path>) {
        print_error(
            &self.definition.identifier().token().source_location,
            format!(
                "cannot re-define symbol '{}'",
                self.definition.identifier().token().lexeme()
            ),
            "re-defined here",
            output_filename,
        );
        print_note(
            &self
                .previous_definition
                .identifier()
                .token()
                .source_location,
            "previous definition occurred here",
            "already defined here",
            output_filename,
        );
    }
}

#[derive(Debug)]
pub enum NameError<'a> {
    DuplicateTypeName {
        name: &'a str,
        previous_type_definition: &'a TypeDefinition<'a>,
        current_type_definition: &'a TypeDefinition<'a>,
    },
    DuplicateNonTypeName {
        name: &'a str,
        previous_definition: &'a NonTypeDefinition<'a>,
        current_definition: &'a NonTypeDefinition<'a>,
    },
}

enum PrintStyle {
    Error,
    Note,
}

impl NameError<'_> {
    fn print_duplicate_type_name(
        type_definition: &TypeDefinition,
        message: &str,
        output_filename: Option<&Path>,
        print_style: PrintStyle,
    ) {
        let print_function = match print_style {
            PrintStyle::Error => print_error,
            PrintStyle::Note => print_note,
        };

        if let Some(import) = type_definition.origin {
            print_function(
                &import.source_location(),
                message,
                "type definition imported here",
                output_filename,
            );
        } else {
            let source_location = match type_definition.definition {
                TypeDefinitionKind::Struct(struct_definition) => {
                    struct_definition.name.0.source_location
                }
            };
            print_function(
                &source_location,
                message,
                "type defined here",
                output_filename,
            );
        };
    }
}

impl ErrorReport for NameError<'_> {
    fn print_report(&self, output_filename: Option<&Path>) {
        match self {
            NameError::DuplicateTypeName {
                name,
                previous_type_definition: previous_definition,
                current_type_definition: current_definition,
            } => {
                Self::print_duplicate_type_name(
                    current_definition,
                    &format!("duplicate type '{name}' found"),
                    output_filename,
                    PrintStyle::Error,
                );

                Self::print_duplicate_type_name(
                    previous_definition,
                    &format!("type '{name}' already imported here"),
                    output_filename,
                    PrintStyle::Note,
                );
            }
            NameError::DuplicateNonTypeName {
                name,
                previous_definition,
                current_definition,
            } => {
                if let Some(import) = current_definition.get_first_origin() {
                    print_error(
                        &import.source_location(),
                        format!("redefinition of symbol '{name}'"),
                        "symbol is imported here",
                        output_filename,
                    );
                } else {
                    match current_definition {
                        NonTypeDefinition::GlobalVariable(definition) => {
                            print_error(
                                &definition.name.0.source_location,
                                format!("redefinition of symbol '{name}'"),
                                "global variable is defined here",
                                output_filename,
                            );
                        }
                        NonTypeDefinition::Function(overload_set) => {
                            assert!(!overload_set.is_empty());
                            print_error(
                                &overload_set
                                    .first()
                                    .unwrap()
                                    .definition
                                    .name
                                    .0
                                    .source_location,
                                format!("redefinition of symbol '{name}'"),
                                "function is defined here",
                                output_filename,
                            );
                        }
                    }
                }

                if let Some(import) = previous_definition.get_first_origin() {
                    print_note(
                        &import.source_location(),
                        format!("symbol '{name}' was already defined"),
                        "symbol is imported here",
                        output_filename,
                    );
                } else {
                    match previous_definition {
                        NonTypeDefinition::GlobalVariable(definition) => {
                            print_note(
                                &definition.name.0.source_location,
                                format!("symbol '{name}' was already defined"),
                                "previous definition is here",
                                output_filename,
                            );
                        }
                        NonTypeDefinition::Function(overload_set) => {
                            assert!(!overload_set.is_empty());
                            print_error(
                                &overload_set
                                    .first()
                                    .unwrap()
                                    .definition
                                    .name
                                    .0
                                    .source_location,
                                format!("symbol '{name}' was already defined"),
                                "previous definition is here",
                                output_filename,
                            );
                        }
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
#[allow(clippy::large_enum_variant)] // ¯\_(ツ)_/¯
pub enum ImportError<'a> {
    ImportPathNotFound {
        import_path: &'a Path,
    },
    ModuleNotFound {
        import_path: QualifiedName<'a>,
        path_to_search: PathBuf,
    },
    SymbolNotFound {
        imported_module_path: &'a Path,
        import_path: QualifiedName<'a>,
        symbol_token: Identifier<'a>,
        non_exported_definition: Option<Definition<'a>>,
    },
    ImportedClashWithLocalDefinition {
        import: Import<'a>,
        local_definition_with_same_identifier: Definition<'a>,
    },
    UnableToCanonicalize {
        path: &'a Path,
    },
    FileNotFound {
        path: &'a Path,
    },
    DoublyImportedSymbol {
        import: Import<'a>,
        previous_import: Import<'a>,
    },
    ImportedAsForbiddenName {
        as_: &'a Token<'a>,
        hint_location: SourceLocation<'a>,
    },
}

impl ErrorReport for ImportError<'_> {
    fn print_report(&self, output_filename: Option<&Path>) {
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
                    output_filename,
                );
            }
            ImportError::SymbolNotFound {
                imported_module_path,
                import_path,
                symbol_token,
                non_exported_definition,
            } => {
                print_error(
                    &symbol_token.token().source_location,
                    format!(
                        "module '{}' (in '{}') does not export symbol '{}'",
                        import_path,
                        imported_module_path.display(),
                        symbol_token.token().lexeme()
                    ),
                    "symbol not found",
                    output_filename,
                );
                if let Some(non_exported_definition) = non_exported_definition {
                    print_note(
                        &non_exported_definition.identifier().token().source_location,
                        "there is a definition with the requested name that has not been exported",
                        "did you forget to export this definition?",
                        output_filename,
                    );
                }
            }
            ImportError::ImportedClashWithLocalDefinition {
                import,
                local_definition_with_same_identifier,
            } => {
                print_error(
                    &import.as_what().expect("this error can only occur when importing as a name").token().source_location,
                    format!("imported definition or module '{}' clashes with module-local definition with the same name", import.as_what().unwrap().token().lexeme()),
                    "symbol imported here",
                    output_filename,
                );
                print_note(
                    &local_definition_with_same_identifier
                        .identifier()
                        .token()
                        .source_location,
                    "module-local definition with the same name prevents import",
                    "symbol defined here",
                    output_filename,
                );
            }
            /*ImportError::DuplicateImport {
                import,
                previous_import,
            } => {
                print_error(
                    &import.import.imported_namespace().expect("this error can only occur when importing a whole namespace").source_location(),
                    format!("imported module '{}' clashes with previously imported module with the same name", import.import.imported_namespace().unwrap().as_string()),
                    "module imported here",
                    output_filename,
                );
                print_note(
                    &previous_import
                        .import
                        .imported_namespace()
                        .unwrap()
                        .source_location(),
                    "previously imported module prevents import",
                    "module imported here",
                    output_filename,
                );
            }*/
            ImportError::UnableToCanonicalize { path } => {
                eprintln!("error: unable to canonicalize path '{}'", path.display());
            }
            ImportError::FileNotFound { path } => {
                eprintln!("error: file not found: '{}'", path.display());
            }
            ImportError::DoublyImportedSymbol {
                import,
                previous_import,
            } => {
                print_error(
                    &import.as_what().expect("this error can only occur when importing a symbol").token().source_location,
                    format!("imported symbol '{}' clashes with previously imported symbol with the same name", import.as_what().unwrap().token().lexeme()),
                    "symbol imported here",
                    output_filename,
                );
                print_note(
                    &previous_import.as_what().unwrap().token().source_location,
                    "previously imported symbol prevents import",
                    "symbol imported here",
                    output_filename,
                );
            }
            ImportError::ImportedAsForbiddenName { as_, hint_location } => match as_.type_ {
                TokenType::LowercaseIdentifier => {
                    print_error(
                        &as_.source_location,
                        format!("cannot import type as '{}'", as_.lexeme()),
                        "type names must start with an uppercase character",
                        output_filename,
                    );
                    print_note(
                        hint_location,
                        "error occurred while trying to import this type",
                        "this is a type name",
                        output_filename,
                    );
                }
                TokenType::UppercaseIdentifier => {
                    print_error(
                        &as_.source_location,
                        format!("cannot import non-type as '{}'", as_.lexeme()),
                        "non-type names must start with a lowercase character",
                        output_filename,
                    );
                    print_note(
                        hint_location,
                        "error occurred while trying to import this non-type symbol",
                        "this is a non-type",
                        output_filename,
                    );
                }
                _ => unreachable!(),
            },
        }
    }
}
