use std::path::Path;

use crate::parser::errors::ErrorReport;

#[derive(Debug, Clone, Copy)]
pub(crate) enum NameLookupError<'a> {
    SomeErrorToRemoveLater(&'a str),
}

impl ErrorReport for NameLookupError<'_> {
    fn print_report(&self, output_filename: Option<&Path>) {
        todo!()
    }
}
