use crate::token::TokenType;
use thiserror::Error;

#[derive(Error, Debug)]
pub(crate) enum ParserError {
    #[error("token type mismatch (expected '{expected:?}', actual '{actual:?}'")]
    TokenTypeMismatch {
        expected: Vec<TokenType>,
        actual: Option<TokenType>,
    },
    #[error("unexpected end of input (expected '{expected:?}')")]
    UnexpectedEndOfInput { expected: &'static [TokenType] },
}
