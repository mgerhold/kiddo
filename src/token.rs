use std::fmt::Display;

#[derive(Debug)]
pub(crate) struct SourceLocation<'filename> {
    pub filename: &'filename std::path::Path,
    pub line: usize,
    pub column: usize,
    pub num_chars: usize,
}

impl Display for SourceLocation<'_> {
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

#[derive(Debug)]
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

    EndOfInput,
}

#[derive(Debug)]
pub(crate) struct Token<'source, 'filename> {
    pub lexeme: &'source str,
    pub source_location: SourceLocation<'filename>,
    pub type_: TokenType,
}

impl Display for Token<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "'{}' ({:?}, {}:{}:{}, {} chars)",
            self.lexeme,
            self.type_,
            self.source_location.filename.display(),
            self.source_location.line,
            self.source_location.column,
            self.source_location.num_chars
        )
    }
}
