use std::path::PathBuf;

use crate::constants::BackseatSize;
use crate::parser::ends_with_identifier;
use crate::parser::errors::ParserError;
use crate::token::{Token, TokenType};

#[derive(Debug, Clone)]
pub(crate) struct Module<'a> {
    pub(crate) imports: Vec<Import<'a>>,
    pub(crate) definitions: Vec<Definition<'a>>,
}

#[derive(Debug, Clone, Copy)]
#[allow(clippy::enum_variant_names)]
pub(crate) enum Import<'a> {
    Import {
        what: QualifiedName<'a>,
    },
    ImportAs {
        what: QualifiedName<'a>,
        as_: Identifier<'a>,
    },
    FromImport {
        where_: QualifiedName<'a>,
        symbol: Identifier<'a>,
    },
    FromImportAs {
        where_: QualifiedName<'a>,
        symbol: Identifier<'a>,
        as_: Identifier<'a>,
    },
}

#[derive(Debug, Clone)]
pub(crate) enum Definition<'a> {
    Struct(StructDefinition<'a>),
}

#[derive(Debug, Clone)]
pub(crate) struct StructDefinition<'a> {
    pub(crate) name: Identifier<'a>,
    pub(crate) members: Vec<StructMember<'a>>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Mutability {
    Constant,
    Mutable,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum DataType<'a> {
    Named {
        name: QualifiedName<'a>,
    },
    Pointer {
        mutability: Mutability,
        pointee_type: &'a DataType<'a>,
    },
    Array {
        contained_type: &'a DataType<'a>,
        size: BackseatSize,
    },
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct StructMember<'a> {
    pub(crate) name: Identifier<'a>,
    pub(crate) type_: DataType<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum QualifiedName<'a> {
    Absolute { tokens: &'a [Token<'a>] },
    Relative { tokens: &'a [Token<'a>] },
}

impl From<&QualifiedName<'_>> for PathBuf {
    fn from(name: &QualifiedName) -> Self {
        let tokens = match name {
            QualifiedName::Absolute { tokens } => {
                assert_eq!(tokens[0].type_, TokenType::ColonColon);
                &tokens[1..]
            }
            QualifiedName::Relative { tokens } => tokens,
        };
        assert!(!tokens.is_empty());
        let path: PathBuf = tokens
            .iter()
            .step_by(2)
            .map(|token| PathBuf::from(token.lexeme()))
            .collect();
        path
    }
}

impl<'a> QualifiedName<'a> {
    fn are_repeated(repeated: &[TokenType], in_: &[Token]) -> Result<(), ParserError<'a>> {
        assert!(!repeated.is_empty());

        for (token, &expected) in in_.iter().zip(repeated.iter().cycle()) {
            if token.type_ != expected {
                return Err(ParserError::TokenTypeMismatch {
                    expected: vec![expected],
                    actual: Some(token.type_),
                });
            }
        }

        Ok(())
    }
}

impl<'a> TryFrom<&'a [Token<'a>]> for QualifiedName<'a> {
    type Error = ParserError<'a>;

    fn try_from(tokens: &'a [Token<'a>]) -> Result<Self, Self::Error> {
        match tokens {
            [Token {
                type_: TokenType::ColonColon,
                ..
            }] => Err(ParserError::TokenTypeMismatch {
                expected: vec![TokenType::Identifier],
                actual: None,
            }),
            [Token {
                type_: TokenType::ColonColon,
                ..
            }, rest @ ..] => {
                assert!(!rest.is_empty());
                ends_with_identifier(tokens)?;
                Self::are_repeated(&[TokenType::Identifier, TokenType::ColonColon], rest)?;
                Ok(QualifiedName::Absolute { tokens })
            }
            [Token {
                type_: TokenType::Identifier,
                ..
            }, rest @ ..] => {
                ends_with_identifier(tokens)?;
                Self::are_repeated(&[TokenType::ColonColon, TokenType::Identifier], rest)?;
                Ok(QualifiedName::Relative { tokens })
            }
            [first, ..] => Err(ParserError::TokenTypeMismatch {
                expected: vec![TokenType::Identifier, TokenType::ColonColon],
                actual: Some(first.type_),
            }),
            [] => Err(ParserError::TokenTypeMismatch {
                expected: vec![TokenType::Identifier, TokenType::ColonColon],
                actual: None,
            }),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Identifier<'a> {
    pub(crate) token: Token<'a>,
}
