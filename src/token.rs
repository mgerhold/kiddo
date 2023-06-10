use std::fmt::Display;
use std::ops::Range;
use std::path::Path;
use std::rc::Rc;

use unicode_width::UnicodeWidthStr;

struct LineColumnContents<'a> {
    line: usize,
    column: usize,
    contents: &'a str,
}

#[derive(Debug, Clone)]
pub struct SourceLocation {
    filename: Rc<Path>,
    source: Rc<str>,
    byte_offset: usize,
    num_bytes: usize,
}

impl SourceLocation {
    pub(crate) fn new(
        filename: Rc<Path>,
        source: Rc<str>,
        byte_offset: usize,
        num_bytes: usize,
    ) -> Self {
        let _ = &source[byte_offset..][..num_bytes]; // only used as a check
        Self {
            filename,
            source,
            byte_offset,
            num_bytes,
        }
    }

    pub(crate) fn line(&self) -> usize {
        self.line_column_contents().line
    }

    pub(crate) fn column(&self) -> usize {
        self.line_column_contents().column
    }

    pub(crate) fn width(&self) -> usize {
        self.lexeme().width()
    }

    pub(crate) fn num_chars(&self) -> usize {
        self.lexeme().chars().count()
    }

    pub(crate) fn line_contents(&self) -> &str {
        self.line_column_contents().contents
    }

    pub(crate) fn lexeme(&self) -> &str {
        &self.source[self.byte_offset..][..self.num_bytes]
    }

    pub(crate) fn filename(&self) -> Rc<Path> {
        Rc::clone(&self.filename)
    }

    pub(crate) fn source(&self) -> Rc<str> {
        Rc::clone(&self.source)
    }

    pub(crate) fn byte_offset(&self) -> usize {
        self.byte_offset
    }

    pub(crate) fn num_bytes(&self) -> usize {
        self.num_bytes
    }

    pub(crate) fn char_offset(&self) -> usize {
        self.source[..self.byte_offset].chars().count()
    }

    pub(crate) fn char_span(&self) -> Range<usize> {
        let start = self.char_offset();
        start..start + self.num_chars()
    }

    fn line_column_contents(&self) -> LineColumnContents {
        let mut total_size = 0;
        for (i, contents) in self.source.lines().enumerate() {
            let start_offset = total_size;
            let end_offset = total_size + contents.len();

            if (start_offset..end_offset).contains(&self.byte_offset) {
                let bytes_until_column = self.byte_offset - start_offset;
                let until_column = &contents[..bytes_until_column];
                let num_chars = until_column.width();
                return LineColumnContents {
                    line: i + 1,
                    column: num_chars + 1,
                    contents,
                };
            }
            total_size += contents.len();
        }
        unreachable!("line with offset not found");
    }
}

impl Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.filename.display(),
            self.line(),
            self.column()
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    LeftCurlyBracket,
    RightCurlyBracket,
    LeftParenthesis,
    RightParenthesis,
    LeftSquareBracket,
    RightSquareBracket,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Equals,
    GreaterThan,
    LessThan,
    GreaterThanEquals,
    LessThanEquals,
    TildeGreaterThan,
    MinusGreaterThan,
    At,
    ExclamationMark,
    Colon,
    ColonColon,
    Semicolon,
    Comma,
    Dot,

    Import,
    From,
    As,
    Struct,
    Function,
    If,
    Else,
    Loop,
    While,
    Break,
    Continue,
    Let,
    Mutable,
    Const,
    And,
    Not,
    Or,
    Return,
    True,
    False,
    Nothing,

    Identifier,

    Integer,
    Char,

    EndOfInput,
}

#[derive(Debug, Clone)]
pub(crate) struct Token {
    pub source_location: SourceLocation,
    pub type_: TokenType,
}

impl Token {
    pub(crate) fn lexeme(&self) -> &str {
        self.source_location.lexeme()
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "'{}' ({:?}, {}:{}:{}, width {})",
            self.lexeme(),
            self.type_,
            self.source_location.filename.display(),
            self.source_location.line(),
            self.source_location.column(),
            self.source_location.width()
        )
    }
}
