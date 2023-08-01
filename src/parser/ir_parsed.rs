use std::fmt::{Display, Formatter};
use std::ops::Deref;
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
            QualifiedNameOrIdentifier::QualifiedName(qualified_name) => qualified_name.to_string(),
            QualifiedNameOrIdentifier::Identifier(identifier) => identifier.as_string(),
        }
    }

    pub(crate) fn source_location(&self) -> SourceLocation<'a> {
        match self {
            QualifiedNameOrIdentifier::QualifiedName(qualified_name) => {
                qualified_name.source_location()
            }
            QualifiedNameOrIdentifier::Identifier(identifier) => identifier.token().source_location,
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[allow(clippy::enum_variant_names)]
pub enum Import<'a> {
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
    GlobalVariable(GlobalVariableDefinition<'a>),
}

impl Definition<'_> {
    pub(crate) fn is_exported(&self) -> bool {
        match self {
            Definition::Struct(StructDefinition { is_exported, .. }) => *is_exported,
            Definition::Function(FunctionDefinition { is_exported, .. }) => *is_exported,
            Definition::GlobalVariable(GlobalVariableDefinition { is_exported, .. }) => {
                *is_exported
            }
        }
    }

    pub(crate) fn identifier(&self) -> Identifier {
        match self {
            Definition::Struct(StructDefinition { name, .. }) => Identifier::TypeIdentifier(*name),
            Definition::Function(FunctionDefinition { name, .. }) => {
                Identifier::NonTypeIdentifier(*name)
            }
            Definition::GlobalVariable(GlobalVariableDefinition { name, .. }) => {
                Identifier::NonTypeIdentifier(*name)
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StructDefinition<'a> {
    pub(crate) is_exported: bool,
    pub(crate) name: TypeIdentifier<'a>,
    pub(crate) members: &'a [StructMember<'a>],
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionDefinition<'a> {
    pub(crate) is_exported: bool,
    pub(crate) name: NonTypeIdentifier<'a>,
    pub(crate) parameters: &'a [FunctionParameter<'a>],
    pub(crate) return_type: Option<DataType<'a>>,
    pub(crate) body: Block<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct FunctionParameter<'a> {
    pub(crate) name: NonTypeIdentifier<'a>,
    pub(crate) type_: DataType<'a>,
}

#[derive(Debug, Clone, Copy)]
pub struct GlobalVariableDefinition<'a> {
    pub(crate) is_exported: bool,
    pub(crate) mutability: Mutability,
    pub(crate) name: NonTypeIdentifier<'a>,
    pub(crate) type_: Option<DataType<'a>>,
    pub(crate) initial_value: Expression<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct LocalVariableDefinition<'a> {
    pub(crate) mutability: Mutability,
    pub(crate) name: NonTypeIdentifier<'a>,
    pub(crate) type_: Option<DataType<'a>>,
    pub(crate) initial_value: Expression<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Mutability {
    Constant,
    Mutable,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct TypeListElement<'a> {
    pub(crate) data_type: DataType<'a>,
    pub(crate) comma_token: Option<&'a Token<'a>>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum DataType<'a> {
    Named {
        name: QualifiedTypeName<'a>,
    },
    Pointer {
        arrow_token: &'a Token<'a>,
        mutability: Mutability,
        pointee_type: &'a DataType<'a>,
    },
    Array {
        left_square_bracket_token: &'a Token<'a>,
        contained_type: &'a DataType<'a>,
        size: BackseatSize,
        right_square_bracket_token: &'a Token<'a>,
    },
    FunctionPointer {
        function_keyword_token: &'a Token<'a>,
        parameter_list: &'a [TypeListElement<'a>],
        return_type: &'a DataType<'a>,
    },
}

impl<'a> DataType<'a> {
    pub(crate) fn tokens(&self) -> TokenSlice<'a> {
        match self {
            DataType::Named { name } => name.tokens(),
            DataType::Pointer {
                arrow_token,
                pointee_type,
                ..
            } => {
                let first_token_address = std::ptr::from_ref(*arrow_token);
                let pointee_tokens = pointee_type.tokens();
                let last_token_address = std::ptr::from_ref(pointee_tokens.last().unwrap());
                let token_slice = unsafe {
                    std::slice::from_ptr_range(first_token_address..last_token_address.offset(1))
                };
                TokenSlice(token_slice)
            }
            DataType::Array {
                left_square_bracket_token,
                right_square_bracket_token,
                ..
            } => {
                let first_token_address = std::ptr::from_ref(*left_square_bracket_token);
                let last_token_address = std::ptr::from_ref(*right_square_bracket_token);
                let token_slice = unsafe {
                    std::slice::from_ptr_range(first_token_address..last_token_address.offset(1))
                };
                TokenSlice(token_slice)
            }
            DataType::FunctionPointer {
                function_keyword_token,
                return_type,
                ..
            } => {
                let first_token_address = std::ptr::from_ref(*function_keyword_token);
                let return_type_tokens = return_type.tokens();
                let last_token_address =
                    std::ptr::from_ref(return_type_tokens.iter().last().unwrap());
                let token_slice = unsafe {
                    std::slice::from_ptr_range(first_token_address..last_token_address.offset(1))
                };
                TokenSlice(token_slice)
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct StructMember<'a> {
    pub(crate) name: NonTypeIdentifier<'a>,
    pub(crate) type_: DataType<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Block<'a> {
    pub(crate) statements: &'a [Statement<'a>],
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Literal<'a> {
    Integer(&'a Token<'a>),
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Expression<'a> {
    Literal(Literal<'a>),
    BinaryOperator {
        lhs: &'a Expression<'a>,
        operator: &'a Token<'a>,
        rhs: &'a Expression<'a>,
    },
    Block(Block<'a>),
    Name(QualifiedNonTypeName<'a>),
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Statement<'a> {
    #[allow(clippy::enum_variant_names)]
    ExpressionStatement(Expression<'a>),
    Yield(Expression<'a>),
    Return(Option<Expression<'a>>),
    VariableDefinition(LocalVariableDefinition<'a>),
}

#[derive(Debug, Clone, Copy)]
pub enum QualifiedTypeName<'a> {
    Absolute { tokens: TokenSlice<'a> },
    Relative { tokens: TokenSlice<'a> },
}

impl<'a> QualifiedTypeName<'a> {
    pub(crate) fn tokens(self) -> TokenSlice<'a> {
        match self {
            QualifiedTypeName::Absolute { tokens } => tokens,
            QualifiedTypeName::Relative { tokens } => tokens,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TokenSlice<'a>(&'a [Token<'a>]);

impl<'a> TokenSlice<'a> {
    pub(crate) fn source_location(&self) -> SourceLocation<'a> {
        let tokens = self.0;
        let first_token = tokens
            .first()
            .expect("token slice must consist of at least one token");
        let last_token = tokens
            .last()
            .expect("token slice must consist of at least one token");
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

impl<'a> From<&'a [Token<'a>]> for TokenSlice<'a> {
    fn from(value: &'a [Token<'a>]) -> Self {
        Self(value)
    }
}

impl<'a> Deref for TokenSlice<'a> {
    type Target = &'a [Token<'a>];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> Display for TokenSlice<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for token in self.0 {
            write!(f, "{}", token.lexeme())?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum QualifiedNonTypeName<'a> {
    Absolute { tokens: TokenSlice<'a> },
    Relative { tokens: TokenSlice<'a> },
}

impl<'a> QualifiedNonTypeName<'a> {
    fn tokens(self) -> TokenSlice<'a> {
        match self {
            QualifiedNonTypeName::Absolute { tokens } => tokens,
            QualifiedNonTypeName::Relative { tokens } => tokens,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum QualifiedName<'a> {
    QualifiedTypeName(QualifiedTypeName<'a>),
    QualifiedNonTypeName(QualifiedNonTypeName<'a>),
}

impl PartialEq for QualifiedName<'_> {
    fn eq(&self, other: &Self) -> bool {
        let our_tokens = self.tokens();
        let other_tokens = other.tokens();
        our_tokens.len() == other_tokens.len()
            && our_tokens
                .iter()
                .zip(*other_tokens)
                .all(|(our_token, other_token)| our_token.lexeme() == other_token.lexeme())
    }
}

impl PartialEq<Identifier<'_>> for QualifiedName<'_> {
    fn eq(&self, other: &Identifier<'_>) -> bool {
        match &*self.tokens() {
            [Token {
                source_location,
                type_: TokenType::LowercaseIdentifier,
            }] => source_location.lexeme() == other.token().lexeme(),
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
    fn is_absolute(&self) -> bool {
        match self {
            QualifiedName::QualifiedTypeName(QualifiedTypeName::Absolute { .. })
            | QualifiedName::QualifiedNonTypeName(QualifiedNonTypeName::Absolute { .. }) => true,
            QualifiedName::QualifiedTypeName(QualifiedTypeName::Relative { .. })
            | QualifiedName::QualifiedNonTypeName(QualifiedNonTypeName::Relative { .. }) => false,
        }
    }

    pub(crate) fn tokens(&self) -> TokenSlice<'a> {
        match self {
            QualifiedName::QualifiedTypeName(qualified_type_name) => qualified_type_name.tokens(),
            QualifiedName::QualifiedNonTypeName(qualified_non_type_name) => {
                qualified_non_type_name.tokens()
            }
        }
    }

    pub(crate) fn source_location(&self) -> SourceLocation<'a> {
        self.tokens().source_location()
    }
}

impl Display for QualifiedName<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.tokens())
    }
}

impl From<&QualifiedName<'_>> for PathBuf {
    fn from(name: &QualifiedName) -> Self {
        let tokens = if name.is_absolute() {
            &name.tokens().0[1..]
        } else {
            name.tokens().0
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
pub struct TypeIdentifier<'a>(pub &'a Token<'a>);

#[derive(Debug, Clone, Copy)]
pub struct NonTypeIdentifier<'a>(pub &'a Token<'a>);

#[derive(Debug, Clone, Copy)]
pub enum Identifier<'a> {
    TypeIdentifier(TypeIdentifier<'a>),
    NonTypeIdentifier(NonTypeIdentifier<'a>),
}

impl<'a> Identifier<'a> {
    pub(crate) fn token(self) -> &'a Token<'a> {
        match self {
            Identifier::TypeIdentifier(type_identifier) => type_identifier.0,
            Identifier::NonTypeIdentifier(non_type_identifier) => non_type_identifier.0,
        }
    }

    pub(crate) fn as_string(&self) -> String {
        self.token().lexeme().to_string()
    }

    pub(crate) fn as_qualified_name(&self) -> QualifiedName<'a> {
        match self {
            Identifier::TypeIdentifier(_) => {
                QualifiedName::QualifiedTypeName(QualifiedTypeName::Relative {
                    tokens: TokenSlice(std::slice::from_ref(self.token())),
                })
            }
            Identifier::NonTypeIdentifier(_) => {
                QualifiedName::QualifiedNonTypeName(QualifiedNonTypeName::Relative {
                    tokens: TokenSlice(std::slice::from_ref(self.token())),
                })
            }
        }
    }
}

impl PartialEq for Identifier<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.token().lexeme() == other.token().lexeme()
    }
}

impl Eq for Identifier<'_> {}
