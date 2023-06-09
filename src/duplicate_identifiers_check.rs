use crate::import_resolution::ModuleWithImports;
use crate::parser::errors::{print_error, print_note, ErrorReport};
use crate::parser::ir_parsed::Definition;

#[derive(Debug)]
pub struct DuplicateIdentifiersError<'a> {
    definition: Definition<'a>,
    previous_definition: Definition<'a>,
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

pub(crate) fn check_against_duplicate_identifier_definitions<'a>(
    modules_with_imports: &'a [ModuleWithImports<'a>],
) -> Result<(), DuplicateIdentifiersError<'a>> {
    for module_with_imports in modules_with_imports {
        for (i, current_definition) in module_with_imports.module.definitions.iter().enumerate() {
            for definition in &module_with_imports.module.definitions[..i] {
                if current_definition.identifier().token.lexeme()
                    == definition.identifier().token.lexeme()
                {
                    return Err(DuplicateIdentifiersError {
                        definition: *current_definition,
                        previous_definition: *definition,
                    });
                }
            }
        }
    }
    Ok(())
}
