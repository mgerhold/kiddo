use crate::parser::ends_with_identifier;
use crate::parser::errors::ParserError;
use crate::token::{Token, TokenType};

#[derive(Debug)]
pub(crate) struct Module<'a> {
    pub(crate) imports: Vec<Import<'a>>,
    pub(crate) definitions: Vec<Definition<'a>>,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub(crate) enum Definition<'a> {
    Struct(StructDefinition<'a>),
}

#[derive(Debug)]
pub(crate) struct StructDefinition<'a> {
    pub(crate) name: Identifier<'a>,
    pub(crate) members: Vec<StructMember<'a>>,
}

#[derive(Debug)]
pub(crate) struct StructMember<'a> {
    name: Identifier<'a>,
    type_: Identifier<'a>,
}

impl<'a> TryFrom<&'a [Token<'a>]> for StructMember<'a> {
    type Error = ParserError;

    fn try_from(tokens: &'a [Token<'a>]) -> Result<Self, Self::Error> {
        match tokens {
            [name @ Token { .. }, Token {
                type_: TokenType::Colon,
                ..
            }, type_ @ Token { .. }, ..] => Ok(Self {
                name: Identifier::try_from(*name)?,
                type_: Identifier::try_from(*type_)?,
            }),
            [Token { .. }, separator @ Token { .. }, Token { .. }, ..] => {
                Err(ParserError::TokenTypeMismatch {
                    expected: vec![TokenType::Colon],
                    actual: Some(separator.type_),
                })
            }
            [name @ Token { .. }] => {
                Identifier::try_from(*name)?;
                Err(ParserError::TokenTypeMismatch {
                    expected: vec![TokenType::Colon],
                    actual: None,
                })
            }
            [name @ Token { .. }, Token {
                type_: TokenType::Colon,
                ..
            }] => {
                Identifier::try_from(*name)?;
                Err(ParserError::TokenTypeMismatch {
                    expected: vec![TokenType::Identifier],
                    actual: None,
                })
            }
            [name @ Token { .. }, separator @ Token { .. }] => {
                Identifier::try_from(*name)?;
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

#[derive(Debug)]
pub(crate) enum QualifiedName<'a> {
    Absolute { tokens: &'a [Token<'a>] },
    Relative { tokens: &'a [Token<'a>] },
}

impl QualifiedName<'_> {
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

impl<'a> TryFrom<&'a [Token<'a>]> for QualifiedName<'a> {
    type Error = ParserError;

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

#[derive(Debug)]
pub(crate) struct Identifier<'a> {
    token: Token<'a>,
}

impl<'a> TryFrom<Token<'a>> for Identifier<'a> {
    type Error = ParserError;

    fn try_from(token: Token<'a>) -> Result<Self, Self::Error> {
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
