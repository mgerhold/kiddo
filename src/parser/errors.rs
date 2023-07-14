use std::fmt::Debug;

use ariadne::{Color, Label, Report, ReportKind, Source};

use crate::token::{SourceLocation, Token, TokenType};

pub(crate) fn print_error<S1: Into<String>, S2: Into<String>>(
    location: &SourceLocation,
    message: S1,
    label_message: S2,
) {
    print_report(ReportKind::Error, location, message, label_message);
}

pub(crate) fn print_note<S1: Into<String>, S2: Into<String>>(
    location: &SourceLocation,
    message: S1,
    label_message: S2,
) {
    print_report(
        ReportKind::Custom("Note", Color::Blue),
        location,
        message,
        label_message,
    );
}

fn print_report<S1: Into<String>, S2: Into<String>>(
    report_kind: ReportKind,
    location: &SourceLocation,
    message: S1,
    label_message: S2,
) {
    let filename = location.filename();
    let filename = filename.to_string_lossy();
    let filename = filename.strip_prefix("\\\\?\\").unwrap();
    Report::build(report_kind, filename, location.char_offset())
        .with_message(message.into())
        .with_label(Label::new((filename, location.char_span())).with_message(label_message.into()))
        .finish()
        .print((filename, Source::from(location.source())))
        .unwrap();
}

pub trait ErrorReport: Debug {
    fn print_report(&self);
}

impl<'a, E> From<E> for Box<dyn ErrorReport + 'a>
where
    E: ErrorReport + 'a,
{
    fn from(value: E) -> Self {
        Box::new(value)
    }
}

#[derive(Debug)]
pub enum ParserError<'a> {
    TokenTypeMismatch {
        expected: &'a [TokenType],
        actual: Token<'a>,
    },
    UnexpectedEndOfInput {
        expected: &'static [TokenType],
        end_of_input_token: Option<Token<'a>>,
    },
    IntegerLiteralOutOfBounds {
        token: Token<'a>,
    },
}

impl ErrorReport for ParserError<'_> {
    fn print_report(&self) {
        match self {
            ParserError::TokenTypeMismatch { expected, actual } => {
                let str_expected: Vec<String> = expected
                    .iter()
                    .map(|token| format!("'{token:?}'"))
                    .collect();

                let joined = str_expected.join(" or ");
                print_error(
                    &actual.source_location,
                    format!(
                        "unexpected token type '{:?}' (expected {joined})",
                        actual.type_
                    ),
                    "unexpected token encountered here",
                );
            }
            ParserError::UnexpectedEndOfInput {
                expected,
                end_of_input_token,
            } => {
                let str_expected: Vec<String> = expected
                    .iter()
                    .map(|token| format!("'{:?}'", token))
                    .collect();

                let joined = str_expected.join(" or ");
                match end_of_input_token {
                    Some(token) => {
                        print_error(
                            &token.source_location,
                            format!("unexpected end of input (expected {joined})"),
                            "unexpected token encountered here",
                        );
                    }
                    None => {
                        eprintln!("unexpected end of input (expected {joined})")
                    }
                }
            }
            ParserError::IntegerLiteralOutOfBounds { token } => print_error(
                &token.source_location,
                format!("integer literal '{}' out of bounds", token.lexeme()),
                "this integer literal is out of bounds",
            ),
        }
    }
}
