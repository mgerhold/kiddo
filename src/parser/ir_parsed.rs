use std::path::PathBuf;

use crate::constants::BackseatSize;
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
    Function(FunctionDefinition<'a>),
}

#[derive(Debug, Clone)]
pub(crate) struct StructDefinition<'a> {
    pub(crate) name: Identifier<'a>,
    pub(crate) members: Vec<StructMember<'a>>,
}

#[derive(Debug, Clone)]
pub(crate) struct FunctionDefinition<'a> {
    pub(crate) name: Identifier<'a>,
    pub(crate) parameters: Vec<FunctionParameter<'a>>,
    pub(crate) return_type: Option<DataType<'a>>,
}

#[derive(Debug, Clone)]
pub(crate) struct FunctionParameter<'a> {
    pub(crate) name: Identifier<'a>,
    pub(crate) type_: DataType<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Mutability {
    Constant,
    Mutable,
}

#[derive(Debug, Clone)]
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
        parameter_types: Vec<DataType<'a>>,
        return_type: &'a DataType<'a>,
    },
}

#[derive(Debug, Clone)]
pub(crate) struct StructMember<'a> {
    pub(crate) name: Identifier<'a>,
    pub(crate) type_: DataType<'a>,
}

#[derive(Debug, Clone, Copy)]
pub enum QualifiedName<'a> {
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

#[derive(Debug, Clone, Copy)]
pub(crate) struct Identifier<'a> {
    pub(crate) token: Token<'a>,
}
