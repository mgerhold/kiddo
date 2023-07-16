use std::path::Path;
use std::{error::Error, fmt};

use unicode_xid::UnicodeXID;

use crate::parser::errors::{print_error, ErrorReport};
use crate::token::{SourceLocation, Token, TokenType};

#[derive(Debug)]
pub enum LexerError<'a> {
    InvalidInput(SourceLocation<'a>, char),
    UnterminatedMultilineComment(SourceLocation<'a>),
    ExpectedBinaryDigit(SourceLocation<'a>),
    ExpectedOctalDigit(SourceLocation<'a>),
    ExpectedDecimalDigit(SourceLocation<'a>),
    ExpectedChar(SourceLocation<'a>),
    InvalidEscapeSequence(SourceLocation<'a>, char),
    UnexpectedCharacter {
        source_location: SourceLocation<'a>,
        expected: char,
        actual: char,
    },
    FailedToReadFile(std::io::Error),
}

impl Error for LexerError<'_> {}

impl fmt::Display for LexerError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl ErrorReport for LexerError<'_> {
    fn print_report(&self, output_filename: Option<&Path>) {
        match self {
            LexerError::InvalidInput(location, char) => {
                print_error(
                    location,
                    format!("invalid input '{char}'"),
                    "invalid character",
                    output_filename,
                );
            }
            LexerError::UnterminatedMultilineComment(location) => {
                print_error(
                    location,
                    "unterminated multiline comment",
                    "multiline comment starts here",
                    output_filename,
                );
            }
            LexerError::ExpectedBinaryDigit(location) => print_error(
                location,
                format!("'{}' is not a valid binary digit", location.lexeme()),
                "error occurred here",
                output_filename,
            ),
            LexerError::ExpectedOctalDigit(location) => print_error(
                location,
                format!("'{}' is not a valid octal digit", location.lexeme()),
                "error occurred here",
                output_filename,
            ),
            LexerError::ExpectedDecimalDigit(location) => print_error(
                location,
                format!("'{}' is not a valid decimal digit", location.lexeme()),
                "error occurred here",
                output_filename,
            ),
            LexerError::ExpectedChar(location) => print_error(
                location,
                format!("'{}' is not a valid char", location.lexeme()),
                "error occurred here",
                output_filename,
            ),
            LexerError::InvalidEscapeSequence(location, char) => print_error(
                location,
                format!("'\\{}' is not a valid escape sequence", char),
                "error occurred here",
                output_filename,
            ),
            LexerError::UnexpectedCharacter {
                source_location,
                expected,
                actual,
            } => print_error(
                source_location,
                format!("expected '{expected}', got '{actual}' instead"),
                "unexpected character encountered here",
                output_filename,
            ),
            LexerError::FailedToReadFile(error) => eprintln!("failed to read file: {:?}", error),
        }
    }
}

struct LexerState<'a> {
    filename: &'a Path,
    source: &'a str,
    offset: usize,
}

impl<'a> LexerState<'a> {
    fn new(filename: &'a Path, source: &'a str) -> Result<Self, LexerError<'a>> {
        Ok(Self {
            filename,
            source,
            offset: 0,
        })
    }

    fn is_end_of_input(&self) -> bool {
        self.offset >= self.source.len()
    }

    fn current(&self) -> char {
        if self.is_end_of_input() {
            ' '
        } else {
            self.source[self.offset..].chars().next().unwrap() // unwrap since the original string is valid utf-8
        }
    }

    fn peek(&self) -> char {
        let current = self.current();
        let current_size = current.len_utf8();
        if self.offset + current_size >= self.source.len() {
            ' '
        } else {
            self.source[self.offset + current_size..]
                .chars()
                .next()
                .unwrap() // unwrap since the original string is valid utf-8
        }
    }

    fn advance(&mut self) -> char {
        let result = self.current();
        let current_size = result.len_utf8();
        self.offset += current_size;
        result
    }

    fn lexeme(&self, start_offset: usize, num_bytes: usize) -> &'a str {
        &self.source[start_offset..][..num_bytes]
    }

    fn source_location_at(&self, byte_offset: usize, num_bytes: usize) -> SourceLocation<'a> {
        SourceLocation::new(self.filename, self.source, byte_offset, num_bytes)
    }

    fn current_source_location(&self, num_bytes: usize) -> SourceLocation<'a> {
        SourceLocation::new(self.filename, self.source, self.offset, num_bytes)
    }
}

pub(crate) fn tokenize<'a>(
    filename: &'a Path,
    source: &'a str,
) -> Result<Vec<Token<'a>>, LexerError<'a>> {
    let mut state = LexerState::new(filename, source)?;
    let mut tokens = Vec::new();
    while !state.is_end_of_input() {
        let single_char_token = match state.current() {
            '{' => Some(TokenType::LeftCurlyBracket),
            '}' => Some(TokenType::RightCurlyBracket),
            '(' => Some(TokenType::LeftParenthesis),
            ')' => Some(TokenType::RightParenthesis),
            '[' => Some(TokenType::LeftSquareBracket),
            ']' => Some(TokenType::RightSquareBracket),
            '+' => Some(TokenType::Plus),
            '*' => Some(TokenType::Asterisk),
            '=' => Some(TokenType::Equals),
            '@' => Some(TokenType::At),
            ';' => Some(TokenType::Semicolon),
            ',' => Some(TokenType::Comma),
            '.' => Some(TokenType::Dot),
            _ => None,
        };

        if let Some(type_) = single_char_token {
            tokens.push(Token {
                source_location: state.current_source_location(1),
                type_,
            });
            state.advance();
            continue;
        }

        if state.current() == '-' {
            if state.peek() == '>' {
                tokens.push(Token {
                    source_location: state.current_source_location(2),
                    type_: TokenType::Arrow,
                });
                state.advance();
            } else {
                tokens.push(Token {
                    source_location: state.current_source_location(1),
                    type_: TokenType::Minus,
                });
            }
            state.advance();
            continue;
        }

        if state.current() == '!' {
            if state.peek() == '=' {
                tokens.push(Token {
                    source_location: state.current_source_location(2),
                    type_: TokenType::ExclamationMarkEquals,
                });
                state.advance();
            } else {
                tokens.push(Token {
                    source_location: state.current_source_location(1),
                    type_: TokenType::ExclamationMark,
                });
            }
            state.advance();
            continue;
        }

        if state.current() == '>' {
            if state.peek() == '=' {
                tokens.push(Token {
                    source_location: state.current_source_location(2),
                    type_: TokenType::GreaterThanEquals,
                });
                state.advance();
            } else {
                tokens.push(Token {
                    source_location: state.current_source_location(1),
                    type_: TokenType::GreaterThan,
                })
            }
            state.advance();
            continue;
        }

        if state.current() == '<' {
            if state.peek() == '=' {
                tokens.push(Token {
                    source_location: state.current_source_location(2),
                    type_: TokenType::LessThanEquals,
                });
                state.advance();
            } else {
                tokens.push(Token {
                    source_location: state.current_source_location(1),
                    type_: TokenType::LessThan,
                })
            }
            state.advance();
            continue;
        }

        if state.current() == '~' && state.peek() == '>' {
            tokens.push(Token {
                source_location: state.current_source_location(2),
                type_: TokenType::TildeArrow,
            });
            state.advance();
            state.advance();
            continue;
        }

        if state.current() == ':' {
            if state.peek() == ':' {
                tokens.push(Token {
                    source_location: state.current_source_location(2),
                    type_: TokenType::ColonColon,
                });
                state.advance();
            } else {
                tokens.push(Token {
                    source_location: state.current_source_location(1),
                    type_: TokenType::Colon,
                })
            }
            state.advance();
            continue;
        }

        let is_binary_number = state.current() == '0' && state.peek() == 'b';
        let is_octal_number = state.current() == '0' && state.peek() == 'o';
        let is_hex_number = state.current() == '0' && state.peek() == 'x';

        if is_hex_number || state.current().is_ascii_digit() {
            let is_decimal_number = !is_hex_number && !is_octal_number && !is_binary_number;

            let is_binary_digit = |c: char| c == '0' || c == '1';
            let is_octal_digit = |c: char| ('0'..='7').contains(&c);
            let is_hex_digit = |c: char| {
                c.is_ascii_digit() || ('A'..='F').contains(&c) || ('a'..='f').contains(&c)
            };

            let start_offset = state.offset;
            if is_binary_number {
                state.advance(); // consume '0'
                state.advance(); // consume 'b'
                if !is_binary_digit(state.current()) {
                    return Err(LexerError::ExpectedBinaryDigit(
                        state.current_source_location(state.current().len_utf8()),
                    ));
                }
            } else if is_octal_number {
                state.advance(); // consume '0'
                state.advance(); // consume 'o'
                if !is_octal_digit(state.current()) {
                    return Err(LexerError::ExpectedOctalDigit(
                        state.current_source_location(state.current().len_utf8()),
                    ));
                }
            } else if is_hex_number {
                state.advance(); // consume '0'
                state.advance(); // consume 'x'
                if !is_hex_digit(state.current()) {
                    return Err(LexerError::ExpectedDecimalDigit(
                        state.current_source_location(state.current().len_utf8()),
                    ));
                }
            } else {
                assert!(is_decimal_number);
                state.advance(); // consume first digit since it has already been checked
            }

            while (is_decimal_number && state.current().is_ascii_digit())
                || (is_binary_number && is_binary_digit(state.current()))
                || (is_octal_number && is_octal_digit(state.current()))
                || (is_hex_number && is_hex_digit(state.current()))
            {
                state.advance();
            }

            if is_binary_number
                && (is_octal_digit(state.current())
                    || is_hex_digit(state.current())
                    || state.current().is_ascii_digit())
            {
                return Err(LexerError::ExpectedBinaryDigit(
                    state.current_source_location(state.current().len_utf8()),
                ));
            }

            if is_octal_number
                && (is_hex_digit(state.current()) || state.current().is_ascii_digit())
            {
                return Err(LexerError::ExpectedOctalDigit(
                    state.current_source_location(state.current().len_utf8()),
                ));
            }

            if is_decimal_number && is_hex_digit(state.current()) {
                return Err(LexerError::ExpectedDecimalDigit(
                    state.current_source_location(state.current().len_utf8()),
                ));
            }

            let end_offset = state.offset;
            let num_bytes = end_offset - start_offset;
            tokens.push(Token {
                source_location: state.source_location_at(start_offset, num_bytes),
                type_: TokenType::Integer,
            });
            continue;
        }

        if state.current().is_xid_start() {
            let start_offset = state.offset;
            state.advance();
            while state.current().is_xid_continue() {
                state.advance();
            }
            let end_offset = state.offset;
            let num_bytes = end_offset - start_offset;

            let lexeme = state.lexeme(start_offset, num_bytes);

            let type_ = match lexeme {
                "import" => TokenType::Import,
                "from" => TokenType::From,
                "as" => TokenType::As,
                "struct" => TokenType::Struct,
                "function" => TokenType::Function,
                "Function" => TokenType::CapitalizedFunction,
                "if" => TokenType::If,
                "else" => TokenType::Else,
                "loop" => TokenType::Loop,
                "while" => TokenType::While,
                "break" => TokenType::Break,
                "continue" => TokenType::Continue,
                "let" => TokenType::Let,
                "mutable" => TokenType::Mutable,
                "const" => TokenType::Const,
                "and" => TokenType::And,
                "not" => TokenType::Not,
                "or" => TokenType::Or,
                "return" => TokenType::Return,
                "true" => TokenType::True,
                "false" => TokenType::False,
                "nothing" => TokenType::Nothing,
                "yield" => TokenType::Yield,
                "export" => TokenType::Export,
                _ if lexeme.chars().next().unwrap().is_uppercase() => {
                    TokenType::UppercaseIdentifier
                }
                _ => TokenType::LowercaseIdentifier,
            };

            tokens.push(Token {
                source_location: state.source_location_at(start_offset, num_bytes),
                type_,
            });
            continue;
        }

        if state.current() == '\'' {
            let is_valid_escape_character = |c: char| ['t', 'n', 'r', 'v', '\\', '\''].contains(&c);
            let start_offset = state.offset;
            state.advance(); // consume opening '
            if state.current() == '\'' {
                return Err(LexerError::ExpectedChar(state.current_source_location(1)));
            }
            let is_escape_sequence = state.current() == '\\';
            if is_escape_sequence {
                if !is_valid_escape_character(state.peek()) {
                    return Err(LexerError::InvalidEscapeSequence(
                        state.source_location_at(state.offset + 1, 1 + state.peek().len_utf8()),
                        state.peek(),
                    ));
                }
                state.advance(); // consume '\'
            }
            state.advance(); // consume escape sequence character or actual character
            if state.current() != '\'' {
                return Err(LexerError::UnexpectedCharacter {
                    source_location: state.current_source_location(state.peek().len_utf8()),
                    expected: '\'',
                    actual: state.current(),
                });
            }
            state.advance(); // consume closing '
            let end_offset = state.offset;
            let num_bytes = end_offset - start_offset;
            tokens.push(Token {
                source_location: state.source_location_at(start_offset, num_bytes),
                type_: TokenType::Char,
            });
            continue;
        }

        if state.current() == '/' {
            if state.peek() == '/' {
                // single line comment
                while !state.is_end_of_input() && state.current() != '\n' {
                    state.advance();
                }
                if state.current() == '\n' {
                    state.advance();
                }
            } else if state.peek() == '*' {
                // multi line comment
                let mut depth: usize = 1;
                let source_location = state.current_source_location(2);
                state.advance();
                state.advance();
                while !state.is_end_of_input() {
                    if state.current() == '*' && state.peek() == '/' {
                        state.advance();
                        state.advance();
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    } else if state.current() == '/' && state.peek() == '*' {
                        state.advance();
                        state.advance();
                        depth += 1;
                    } else {
                        state.advance();
                    }
                }
                if depth != 0 {
                    return Err(LexerError::UnterminatedMultilineComment(source_location));
                }
            } else {
                tokens.push(Token {
                    source_location: state.current_source_location(1),
                    type_: TokenType::Slash,
                });
                state.advance();
            }
            continue;
        }

        if state.current().is_whitespace() {
            state.advance();
            continue;
        }

        return Err(LexerError::InvalidInput(
            state.current_source_location(state.current().len_utf8()),
            state.current(),
        ));
    }

    tokens.push(Token {
        source_location: state.current_source_location(0),
        type_: TokenType::EndOfInput,
    });
    Ok(tokens)
}
