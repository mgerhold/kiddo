use crate::constants::BackseatSize;
pub(crate) use crate::parser::ir_parsed::Mutability;
pub(crate) use crate::parser::ir_parsed::NonTypeIdentifier;
pub(crate) use crate::parser::ir_parsed::TypeIdentifier;
pub(crate) use crate::token::Token;

#[derive(Debug, Clone, Copy)]
pub(crate) struct Program<'a> {
    pub(crate) definitions: &'a [Definition<'a>],
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Definition<'a> {
    Function(FunctionDefinition<'a>),
    GlobalVariable(GlobalVariableDefinition<'a>),
    Struct(StructDefinition<'a>),
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct FunctionDefinition<'a> {
    pub(crate) name: NonTypeIdentifier<'a>,
    pub(crate) parameters: &'a [FunctionParameter<'a>],
    pub(crate) return_type: ResolvedDataType<'a>,
    pub(crate) body: Block<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct FunctionParameter<'a> {
    pub(crate) name: NonTypeIdentifier<'a>,
    pub(crate) type_: ResolvedDataType<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Block<'a> {
    pub(crate) statements: &'a [Statement<'a>],
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
pub(crate) struct LocalVariableDefinition<'a> {
    pub(crate) mutability: Mutability,
    pub(crate) name: NonTypeIdentifier<'a>,
    pub(crate) type_: Option<ResolvedDataType<'a>>,
    pub(crate) initial_value: Expression<'a>,
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
    Name(ResolvedNonTypeName<'a>),
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct GlobalVariableDefinition<'a> {
    pub(crate) mutability: Mutability,
    pub(crate) name: NonTypeIdentifier<'a>,
    pub(crate) type_: Option<ResolvedDataType<'a>>,
    pub(crate) initial_value: Expression<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct StructDefinition<'a> {
    pub(crate) name: TypeIdentifier<'a>,
    pub(crate) members: &'a [StructMember<'a>],
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct StructMember<'a> {
    pub(crate) name: NonTypeIdentifier<'a>,
    pub(crate) type_: ResolvedDataType<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ResolvedDataType<'a> {
    Named {
        name: ResolvedTypeName<'a>,
    },
    Pointer {
        mutability: Mutability,
        pointee_type: &'a ResolvedDataType<'a>,
    },
    Array {
        contained_type: &'a ResolvedDataType<'a>,
        size: BackseatSize,
    },
    FunctionPointer {
        parameter_types: &'a [ResolvedDataType<'a>],
        return_type: &'a ResolvedDataType<'a>,
    },
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ResolvedTypeName<'a> {
    Struct(StructDefinition<'a>),
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ResolvedNonTypeName<'a> {
    FunctionParameter(&'a FunctionParameter<'a>),
    LocalVariable(&'a LocalVariableDefinition<'a>),
    GlobalVariable(&'a GlobalVariableDefinition<'a>),
    FunctionDefinition(&'a FunctionDefinition<'a>),
}
