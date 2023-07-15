use std::path::PathBuf;

use crate::constants::BackseatSize;
use crate::token::{SourceLocation, Token, TokenType};

#[derive(Debug, Clone, Copy)]
pub(crate) struct Module<'a> {
    pub(crate) imports: &'a [Import<'a>],
    pub(crate) definitions: &'a [Definition<'a>],
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum QualifiedNameOrIdentifier<'a> {
    QualifiedName(QualifiedName<'a>),
    Identifier(Identifier<'a>),
}

impl PartialEq for QualifiedNameOrIdentifier<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                QualifiedNameOrIdentifier::QualifiedName(lhs),
                QualifiedNameOrIdentifier::QualifiedName(rhs),
            ) => lhs == rhs,
            (
                QualifiedNameOrIdentifier::QualifiedName(lhs),
                QualifiedNameOrIdentifier::Identifier(rhs),
            ) => lhs == rhs,
            (
                QualifiedNameOrIdentifier::Identifier(lhs),
                QualifiedNameOrIdentifier::QualifiedName(rhs),
            ) => lhs == rhs,
            (
                QualifiedNameOrIdentifier::Identifier(lhs),
                QualifiedNameOrIdentifier::Identifier(rhs),
            ) => lhs == rhs,
        }
    }
}

impl<'a> QualifiedNameOrIdentifier<'a> {
    pub(crate) fn as_string(&self) -> String {
        match self {
            QualifiedNameOrIdentifier::QualifiedName(qualified_name) => qualified_name.as_string(),
            QualifiedNameOrIdentifier::Identifier(identifier) => identifier.as_string(),
        }
    }

    pub(crate) fn source_location(&self) -> SourceLocation<'a> {
        match self {
            QualifiedNameOrIdentifier::QualifiedName(qualified_name) => {
                qualified_name.source_location()
            }
            QualifiedNameOrIdentifier::Identifier(identifier) => identifier.token.source_location,
        }
    }
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

impl<'a> Import<'a> {
    pub(crate) fn imported_symbol(self) -> Option<Identifier<'a>> {
        match self {
            Import::Import { .. } => None,
            Import::ImportAs { .. } => None,
            Import::FromImport { symbol, .. } => Some(symbol),
            Import::FromImportAs { symbol, .. } => Some(symbol),
        }
    }

    pub(crate) fn what_or_where(self) -> QualifiedName<'a> {
        match self {
            Import::Import { what, .. } => what,
            Import::ImportAs { what, .. } => what,
            Import::FromImport { where_, .. } => where_,
            Import::FromImportAs { where_, .. } => where_,
        }
    }

    pub(crate) fn as_what(self) -> Option<Identifier<'a>> {
        match self {
            Import::Import { .. } => None,
            Import::ImportAs { as_, .. } => Some(as_),
            Import::FromImport { symbol, .. } => Some(symbol),
            Import::FromImportAs { as_, .. } => Some(as_),
        }
    }

    pub(crate) fn imported_namespace(self) -> Option<QualifiedNameOrIdentifier<'a>> {
        match self {
            Import::Import { what } => Some(QualifiedNameOrIdentifier::QualifiedName(what)),
            Import::ImportAs { as_, .. } => Some(QualifiedNameOrIdentifier::Identifier(as_)),
            Import::FromImport { .. } | Import::FromImportAs { .. } => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Definition<'a> {
    Struct(StructDefinition<'a>),
    Function(FunctionDefinition<'a>),
}

impl Definition<'_> {
    pub(crate) fn is_exported(&self) -> bool {
        match self {
            Definition::Struct(StructDefinition { is_exported, .. }) => *is_exported,
            Definition::Function(FunctionDefinition { is_exported, .. }) => *is_exported,
        }
    }

    pub(crate) fn identifier(&self) -> Identifier {
        match self {
            Definition::Struct(StructDefinition { name, .. }) => *name,
            Definition::Function(FunctionDefinition { name, .. }) => *name,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StructDefinition<'a> {
    pub(crate) is_exported: bool,
    pub(crate) name: Identifier<'a>,
    pub(crate) members: &'a [StructMember<'a>],
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionDefinition<'a> {
    pub(crate) is_exported: bool,
    pub(crate) name: Identifier<'a>,
    pub(crate) parameters: &'a [FunctionParameter<'a>],
    pub(crate) return_type: Option<DataType<'a>>,
    pub(crate) body: Block<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct FunctionParameter<'a> {
    pub(crate) name: Identifier<'a>,
    pub(crate) type_: DataType<'a>,
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
    FunctionPointer {
        parameter_types: &'a [DataType<'a>],
        return_type: &'a DataType<'a>,
    },
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct StructMember<'a> {
    pub(crate) name: Identifier<'a>,
    pub(crate) type_: DataType<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Block<'a> {
    pub(crate) statements: &'a [Statement<'a>],
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Expression<'a> {
    IntegerLiteral(Token<'a>),
    BinaryOperator {
        lhs: &'a Expression<'a>,
        operator: Token<'a>,
        rhs: &'a Expression<'a>,
    },
    Block(Block<'a>),
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Statement<'a> {
    #[allow(clippy::enum_variant_names)]
    ExpressionStatement(Expression<'a>),
    Yield(Expression<'a>),
    Return(Option<Expression<'a>>),
}

#[derive(Debug, Clone, Copy)]
pub enum QualifiedName<'a> {
    Absolute { tokens: &'a [Token<'a>] },
    Relative { tokens: &'a [Token<'a>] },
}

impl PartialEq for QualifiedName<'_> {
    fn eq(&self, other: &Self) -> bool {
        let our_tokens = self.tokens();
        let other_tokens = other.tokens();
        our_tokens.len() == other_tokens.len()
            && our_tokens
                .iter()
                .zip(other_tokens)
                .all(|(our_token, other_token)| our_token.lexeme() == other_token.lexeme())
    }
}

impl PartialEq<Identifier<'_>> for QualifiedName<'_> {
    fn eq(&self, other: &Identifier<'_>) -> bool {
        match self.tokens() {
            [Token {
                source_location,
                type_: TokenType::Identifier,
            }] => source_location.lexeme() == other.token.lexeme(),
            _ => false,
        }
    }
}

impl PartialEq<QualifiedName<'_>> for Identifier<'_> {
    fn eq(&self, other: &QualifiedName<'_>) -> bool {
        other == self
    }
}

impl Eq for QualifiedName<'_> {}

impl<'a> QualifiedName<'a> {
    pub(crate) fn tokens(&self) -> &'a [Token<'a>] {
        match self {
            QualifiedName::Absolute { tokens } => tokens,
            QualifiedName::Relative { tokens } => tokens,
        }
    }

    pub(crate) fn as_string(&self) -> String {
        self.tokens()
            .iter()
            .map(|token| String::from(token.lexeme()))
            .collect()
    }

    pub(crate) fn source_location(&self) -> SourceLocation<'a> {
        let tokens = self.tokens();
        let first_token = tokens
            .first()
            .expect("qualified names must consist of at least one token");
        let last_token = tokens
            .last()
            .expect("qualified names must consist of at least one token");
        let start_offset = first_token.source_location.byte_offset();
        let end_offset =
            last_token.source_location.byte_offset() + last_token.source_location.num_bytes();
        let num_bytes = end_offset - start_offset;
        SourceLocation::new(
            first_token.source_location.filename(),
            first_token.source_location.source(),
            start_offset,
            num_bytes,
        )
    }
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

#[derive(Debug, Clone, Copy)]
pub struct Identifier<'a> {
    pub(crate) token: Token<'a>,
}

impl PartialEq for Identifier<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.token.lexeme() == other.token.lexeme()
    }
}

impl Eq for Identifier<'_> {}

impl Identifier<'_> {
    pub(crate) fn as_string(&self) -> String {
        self.token.lexeme().to_string()
    }
}
