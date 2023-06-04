use std::{error::Error, fmt};
use token::{SourceLocation, Token, TokenType};
use unicode_width::UnicodeWidthChar;
use unicode_xid::UnicodeXID;

mod token;

#[derive(Debug)]
enum LexerError<'filename> {
    InvalidInput(char),
    UnterminatedMultilineComment(SourceLocation<'filename>),
}

impl Error for LexerError<'_> {}

impl fmt::Display for LexerError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

struct LexerState<'filename, 'source> {
    filename: &'filename std::path::Path,
    source: &'source str,
    offset: usize,
    line: usize,
    column: usize,
}

impl<'filename, 'source> LexerState<'filename, 'source> {
    fn new(
        filename: &'filename std::path::Path,
        source: &'source str,
    ) -> Result<Self, LexerError<'filename>> {
        let mut result = Self {
            filename,
            source,
            offset: 0,
            line: 1,
            column: 1,
        };
        Ok(result)
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
        self.column += result.width().unwrap_or(0);
        if result == '\n' {
            self.column = 1;
            self.line += 1;
        }
        result
    }

    fn lexeme(&self, start_offset: usize, num_bytes: usize) -> &'source str {
        &self.source[start_offset..][..num_bytes]
    }

    fn source_location(&self, num_chars: usize) -> SourceLocation<'filename> {
        SourceLocation {
            filename: self.filename,
            line: self.line,
            column: self.column,
            num_chars,
        }
    }
}

fn tokenize<'filename, 'source>(
    filename: &'filename std::path::Path,
    source: &'source str,
) -> Result<Vec<Token<'source, 'filename>>, LexerError<'filename>> {
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
            _ => None,
        };

        if let Some(type_) = single_char_token {
            tokens.push(Token {
                lexeme: state.lexeme(state.offset, 1),
                source_location: state.source_location(1),
                type_,
            });
            state.advance();
            continue;
        }

        if state.current().is_xid_start() {
            let mut source_location = state.source_location(1);
            let start_offset = state.offset;
            let mut num_chars = state.current().width().unwrap(); // unwrap is safe since width() doesn't return None for xid_start chars
            state.advance();
            while state.current().is_xid_continue() {
                num_chars += state.current().width().unwrap(); // unwrap is safe since width() doesn't return None for xid_continue chars
                state.advance();
            }
            let end_offset = state.offset;
            let num_bytes = end_offset - start_offset;
            source_location.num_chars = num_chars;
            tokens.push(Token {
                lexeme: state.lexeme(start_offset, num_bytes),
                source_location,
                type_: TokenType::Identifier,
            });
            continue;
        }

        // comments
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
                let source_location = state.source_location(2);
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
            }
            continue;
        }

        if state.current().is_whitespace() {
            state.advance();
            continue;
        }

        return Err(LexerError::InvalidInput(state.current()));
    }

    tokens.push(Token {
        lexeme: "",
        source_location: state.source_location(0),
        type_: TokenType::EndOfInput,
    });
    Ok(tokens)
}

fn main() -> Result<(), Box<dyn Error>> {
    let filename = std::path::Path::new("test.ceat");
    let source = std::fs::read_to_string(filename)?;
    let tokens = tokenize(filename, &source)?;
    for token in tokens {
        println!("{token}");
    }
    Ok(())
}
