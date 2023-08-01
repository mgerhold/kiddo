use std::ffi::OsStr;
use std::path::Path;

use bumpalo::Bump;

use crate::parser::errors::ParserError;
use crate::token::Token;

pub(crate) trait AllocPath {
    fn alloc_path(&self, path: impl AsRef<Path>) -> &Path;
}

impl AllocPath for Bump {
    fn alloc_path(&self, path: impl AsRef<Path>) -> &Path {
        Path::new(unsafe {
            OsStr::from_os_str_bytes_unchecked(
                self.alloc_slice_copy(path.as_ref().as_os_str().as_os_str_bytes()),
            )
        })
    }
}

pub(crate) fn parse_unsigned_int<'a, T: num_traits::Unsigned>(
    token: &'a Token<'a>,
) -> Result<T, ParserError<'a>> {
    let lexeme = token.lexeme();
    if let Some(lexeme) = lexeme.strip_prefix("0x") {
        T::from_str_radix(lexeme, 16).map_err(|_| ParserError::IntegerLiteralOutOfBounds { token })
    } else if let Some(lexeme) = lexeme.strip_prefix("0o") {
        T::from_str_radix(lexeme, 8).map_err(|_| ParserError::IntegerLiteralOutOfBounds { token })
    } else if let Some(lexeme) = lexeme.strip_prefix("0b") {
        T::from_str_radix(lexeme, 2).map_err(|_| ParserError::IntegerLiteralOutOfBounds { token })
    } else {
        T::from_str_radix(lexeme, 10).map_err(|_| ParserError::IntegerLiteralOutOfBounds { token })
    }
}
