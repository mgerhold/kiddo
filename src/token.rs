use std::fmt::{Debug, Display, Formatter};
use std::ops::Range;
use std::path::Path;

use unicode_width::UnicodeWidthStr;

struct LineColumnContents<'a> {
    line: usize,
    column: usize,
    contents: &'a str,
}

#[derive(Clone, Copy)]
pub struct SourceLocation<'a> {
    filename: &'a Path,
    source: &'a str,
    byte_offset: usize,
    num_bytes: usize,
}

impl Debug for SourceLocation<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "\"{}\" ({}:{}:{}, byte_offset={})",
            self.lexeme(),
            self.filename.display(),
            self.line(),
            self.column(),
            self.byte_offset,
        )
    }
}

impl<'a> SourceLocation<'a> {
    pub(crate) fn new(
        filename: &'a Path,
        source: &'a str,
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

    pub(crate) fn line_contents(&self) -> &'a str {
        self.line_column_contents().contents
    }

    pub(crate) fn lexeme(&self) -> &'a str {
        &self.source[self.byte_offset..][..self.num_bytes]
    }

    pub(crate) fn filename(&self) -> &'a Path {
        self.filename
    }

    pub(crate) fn source(&self) -> &'a str {
        self.source
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

    fn line_column_contents(&self) -> LineColumnContents<'a> {
        let mut total_size = 0;
        for (i, contents) in self.source.lines().enumerate() {
            let start_offset = total_size;
            let end_offset = total_size + contents.len() + 1; // + 1 for the newline

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
            total_size += contents.len() + 1; // + 1 for the newline
        }
        unreachable!("line with offset not found");
    }
}

impl Display for SourceLocation<'_> {
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
    TildeArrow,
    Arrow,
    At,
    ExclamationMark,
    ExclamationMarkEquals,
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
    CapitalizedFunction,
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
    Yield,
    Export,

    Identifier,

    Integer,
    Char,

    EndOfInput,
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    pub source_location: SourceLocation<'a>,
    pub type_: TokenType,
}

impl<'a> Token<'a> {
    pub(crate) fn lexeme(&self) -> &'a str {
        self.source_location.lexeme()
    }
}

impl Display for Token<'_> {
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
