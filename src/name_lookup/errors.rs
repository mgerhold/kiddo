use std::path::Path;

use crate::parser::errors::{print_error, ErrorReport};
use crate::parser::ir_parsed::TokenSlice;

#[derive(Debug, Clone, Copy)]
pub(crate) enum NameLookupError<'a> {
    CouldNotResolveName(TokenSlice<'a>),
}

impl ErrorReport for NameLookupError<'_> {
    fn print_report(&self, output_filename: Option<&Path>) {
        match self {
            NameLookupError::CouldNotResolveName(token_slice) => {
                print_error(
                    &token_slice.source_location(),
                    "undefined reference",
                    "this name could not be resolved",
                    output_filename,
                );
            }
        }
    }
}
