use std::error::Error;

use ariadne::{Label, Report, ReportKind, Source};
use thiserror::Error;

use crate::token::{SourceLocation, TokenType};

pub(crate) fn print_error(location: &SourceLocation, message: String, label_message: String) {
    let filename = location.filename();
    let filename = filename.to_string_lossy();
    let filename = filename.strip_prefix("\\\\?\\").unwrap();
    Report::build(ReportKind::Error, filename, location.char_offset())
        .with_message(message)
        .with_label(Label::new((filename, location.char_span())).with_message(label_message))
        .finish()
        .print((filename, Source::from(location.source())))
        .unwrap();
}

pub trait ErrorReport: Error {
    fn print_report(&self);
}

impl<E> From<E> for Box<dyn ErrorReport>
where
    E: ErrorReport + 'static,
{
    fn from(value: E) -> Self {
        Box::new(value)
    }
}

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("token type mismatch (expected '{expected:?}', actual '{actual:?}'")]
    TokenTypeMismatch {
        expected: Vec<TokenType>,
        actual: Option<TokenType>,
    },
    #[error("unexpected end of input (expected '{expected:?}')")]
    UnexpectedEndOfInput { expected: &'static [TokenType] },
}

impl ErrorReport for ParserError {
    fn print_report(&self) {
        todo!()
    }
}
