use crate::parser::ends_with_identifier;
use crate::parser::errors::ParserError;
use crate::token::{Token, TokenType};
use std::path::PathBuf;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub(crate) struct Module {
    pub(crate) imports: Vec<Import>,
    pub(crate) definitions: Vec<Definition>,
}

#[derive(Debug, Clone)]
#[allow(clippy::enum_variant_names)]
pub(crate) enum Import {
    Import {
        what: QualifiedName,
    },
    ImportAs {
        what: QualifiedName,
        as_: Identifier,
    },
    FromImport {
        where_: QualifiedName,
        symbol: Identifier,
    },
    FromImportAs {
        where_: QualifiedName,
        symbol: Identifier,
        as_: Identifier,
    },
}

#[derive(Debug, Clone)]
pub(crate) enum Definition {
    Struct(StructDefinition),
}

#[derive(Debug, Clone)]
pub(crate) struct StructDefinition {
    pub(crate) name: Identifier,
    pub(crate) members: Vec<StructMember>,
}

#[derive(Debug, Clone)]
pub(crate) struct StructMember {
    name: Identifier,
    type_: Identifier,
}

impl TryFrom<&[Token]> for StructMember {
    type Error = ParserError;

    fn try_from(tokens: &[Token]) -> Result<Self, Self::Error> {
        match tokens {
            [name @ Token { .. }, Token {
                type_: TokenType::Colon,
                ..
            }, type_ @ Token { .. }, ..] => Ok(Self {
                name: Identifier::try_from(name.clone())?,
                type_: Identifier::try_from(type_.clone())?,
            }),
            [Token { .. }, separator @ Token { .. }, Token { .. }, ..] => {
                Err(ParserError::TokenTypeMismatch {
                    expected: vec![TokenType::Colon],
                    actual: Some(separator.type_),
                })
            }
            [name @ Token { .. }] => {
                Identifier::try_from(name.clone())?;
                Err(ParserError::TokenTypeMismatch {
                    expected: vec![TokenType::Colon],
                    actual: None,
                })
            }
            [name @ Token { .. }, Token {
                type_: TokenType::Colon,
                ..
            }] => {
                Identifier::try_from(name.clone())?;
                Err(ParserError::TokenTypeMismatch {
                    expected: vec![TokenType::Identifier],
                    actual: None,
                })
            }
            [name @ Token { .. }, separator @ Token { .. }] => {
                Identifier::try_from(name.clone())?;
                Err(ParserError::TokenTypeMismatch {
                    expected: vec![TokenType::Colon],
                    actual: Some(separator.type_),
                })
            }
            [] => Err(ParserError::TokenTypeMismatch {
                expected: vec![TokenType::Identifier],
                actual: None,
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum QualifiedName {
    Absolute { tokens: Rc<[Token]> },
    Relative { tokens: Rc<[Token]> },
}

impl From<&QualifiedName> for PathBuf {
    fn from(name: &QualifiedName) -> Self {
        let tokens = match name {
            QualifiedName::Absolute { tokens } => {
                assert_eq!(tokens[0].type_, TokenType::ColonColon);
                let tokens: Rc<[Token]> = (&tokens[1..]).into();
                tokens
            }
            QualifiedName::Relative { tokens } => tokens.clone(),
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

impl QualifiedName {
    fn are_repeated(repeated: &[TokenType], in_: &[Token]) -> Result<(), ParserError> {
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

impl TryFrom<&[Token]> for QualifiedName {
    type Error = ParserError;

    fn try_from(tokens: &[Token]) -> Result<Self, Self::Error> {
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
                Ok(QualifiedName::Absolute {
                    tokens: Rc::from(tokens),
                })
            }
            [Token {
                type_: TokenType::Identifier,
                ..
            }, rest @ ..] => {
                ends_with_identifier(tokens)?;
                Self::are_repeated(&[TokenType::ColonColon, TokenType::Identifier], rest)?;
                Ok(QualifiedName::Relative {
                    tokens: Rc::from(tokens),
                })
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

#[derive(Debug, Clone)]
pub(crate) struct Identifier {
    token: Token,
}

impl TryFrom<Token> for Identifier {
    type Error = ParserError;

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        if token.type_ == TokenType::Identifier {
            Ok(Self { token })
        } else {
            Err(ParserError::TokenTypeMismatch {
                expected: vec![TokenType::Identifier],
                actual: Some(token.type_),
            })
        }
    }
}
