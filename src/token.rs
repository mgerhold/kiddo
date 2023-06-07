use std::fmt::Display;
use std::path::Path;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub(crate) struct SourceLocation {
    pub filename: Rc<Path>,
    pub source: Rc<str>,
    pub line: usize,
    pub column: usize,
    pub num_chars: usize,
    pub byte_offset: usize,
    pub num_bytes: usize,
}

impl SourceLocation {
    fn lexeme(&self) -> &str {
        &self.source[self.byte_offset..][..self.num_bytes]
    }
}

impl Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.filename.display(),
            self.line,
            self.column
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum TokenType {
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
            "'{}' ({:?}, {}:{}:{}, {} chars)",
            self.lexeme(),
            self.type_,
            self.source_location.filename.display(),
            self.source_location.line,
            self.source_location.column,
            self.source_location.num_chars
        )
    }
}
