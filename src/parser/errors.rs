use std::fmt::Debug;
use std::path::Path;

use ariadne::{Color, Label, Report, ReportKind, Source};

use crate::token::{SourceLocation, Token, TokenType};

pub(crate) fn print_error<S1: Into<String>, S2: Into<String>>(
    location: &SourceLocation,
    message: S1,
    label_message: S2,
    output_filename: Option<&Path>,
) {
    print_report(
        ReportKind::Error,
        location,
        message,
        label_message,
        output_filename,
    );
}

pub(crate) fn print_note<S1: Into<String>, S2: Into<String>>(
    location: &SourceLocation,
    message: S1,
    label_message: S2,
    output_filename: Option<&Path>,
) {
    print_report(
        ReportKind::Custom("Note", Color::Blue),
        location,
        message,
        label_message,
        output_filename,
    );
}

fn print_report<S1: Into<String>, S2: Into<String>>(
    report_kind: ReportKind,
    location: &SourceLocation,
    message: S1,
    label_message: S2,
    output_filename: Option<&Path>,
) {
    let filename = location.filename();
    let filename = filename.to_string_lossy();
    let filename = if let Some(stripped) = filename.strip_prefix("\\\\?\\") {
        stripped
    } else {
        &filename
    };

    let report = Report::build(report_kind, filename, location.char_offset())
        .with_message(message.into())
        .with_label(Label::new((filename, location.char_span())).with_message(label_message.into()))
        .finish();

    if let Some(output_filename) = output_filename {
        std::fs::write(output_filename, format!("{:#?}", report)).unwrap();
    }

    report
        .eprint((filename, Source::from(location.source())))
        .unwrap();
}

pub trait ErrorReport: Debug {
    fn print_report(&self, output_filename: Option<&Path>);
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
    fn print_report(&self, output_filename: Option<&Path>) {
        match self {
            ParserError::TokenTypeMismatch { expected, actual } => match (expected, actual.type_) {
                ([TokenType::UppercaseIdentifier], TokenType::LowercaseIdentifier) => {
                    print_error(
                        &actual.source_location,
                        "type identifier expected, got non-type identifier instead",
                        "this identifier must start with an uppercase character",
                        output_filename,
                    );
                }
                ([TokenType::LowercaseIdentifier], TokenType::UppercaseIdentifier) => {
                    print_error(
                        &actual.source_location,
                        "non-type identifier expected, got type-identifier instead",
                        "this identifier must start with a lowercase character",
                        output_filename,
                    );
                }
                ([TokenType::EndOfInput], _) => {
                    print_error(
                        &actual.source_location,
                        format!("unexpected token type '{:?}'", actual.type_),
                        "unexpected token encountered here",
                        output_filename,
                    );
                }
                _ => {
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
                        output_filename,
                    );
                }
            },
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
                            output_filename,
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
                output_filename,
            ),
        }
    }
}
