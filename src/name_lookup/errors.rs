use std::path::Path;

use crate::parser::errors::{print_error, print_note, ErrorReport};
use crate::parser::ir_parsed::TokenSlice;

#[derive(Debug, Clone)]
pub(crate) struct CouldNotResolveName<'a> {
    pub(crate) innermost_token_slice: TokenSlice<'a>,
    pub(crate) outermost_token_slice: Option<TokenSlice<'a>>,
}

impl<'a> CouldNotResolveName<'a> {
    pub(crate) fn new(tokens: TokenSlice<'a>) -> Self {
        Self {
            innermost_token_slice: tokens,
            outermost_token_slice: None,
        }
    }

    pub(crate) fn with_added_surrounding_tokens(self, tokens: TokenSlice<'a>) -> Self {
        Self {
            innermost_token_slice: self.innermost_token_slice,
            outermost_token_slice: Some(tokens),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum NameLookupError<'a> {
    CouldNotResolveName(CouldNotResolveName<'a>),
}

impl ErrorReport for NameLookupError<'_> {
    fn print_report(&self, output_filename: Option<&Path>) {
        match self {
            NameLookupError::CouldNotResolveName(CouldNotResolveName {
                innermost_token_slice,
                outermost_token_slice,
            }) => {
                print_error(
                    &innermost_token_slice.source_location(),
                    "undefined reference",
                    "this name could not be resolved",
                    output_filename,
                );
                if let Some(outermost_token_slice) = outermost_token_slice {
                    print_note(
                        &outermost_token_slice.source_location(),
                        "unresolved name is part of a surrounding type",
                        "error occurred while trying to resolve this type",
                        output_filename,
                    );
                }
            }
        }
    }
}

impl<'a> From<CouldNotResolveName<'a>> for NameLookupError<'a> {
    fn from(value: CouldNotResolveName<'a>) -> Self {
        NameLookupError::CouldNotResolveName(value)
    }
}
