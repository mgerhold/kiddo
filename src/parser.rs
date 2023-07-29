use std::path::Path;

use bumpalo::Bump;

use crate::lexer::tokenize;
use crate::parser::errors::{ErrorReport, ParserError};
use crate::parser::ir_parsed::{
    Block, DataType, Definition, Expression, FunctionDefinition, FunctionParameter,
    GlobalVariableDefinition, Identifier, Import, LocalVariableDefinition, Module, Mutability,
    NonTypeIdentifier, QualifiedName, QualifiedNonTypeName, QualifiedTypeName, Statement,
    StructDefinition, StructMember, TypeIdentifier,
};
use crate::token::{Token, TokenType};
use crate::utils::parse_unsigned_int;

pub(crate) mod errors;
pub(crate) mod ir_parsed;

struct ParserState<'a> {
    tokens: &'a [Token<'a>],
    current_index: usize,
    bump_allocator: &'a Bump,
}

impl<'a> ParserState<'a> {
    fn new(tokens: &'a [Token<'a>], bump_allocator: &'a Bump) -> Self {
        Self {
            tokens,
            current_index: 0,
            bump_allocator,
        }
    }

    fn current(&self) -> Token<'a> {
        match self.tokens.get(self.current_index) {
            Some(token) => token.clone(),
            None => self.tokens.last().expect("token list is not empty").clone(),
        }
    }

    fn peek(&self) -> Option<Token<'a>> {
        self.tokens.get(self.current_index + 1).cloned()
    }

    fn expect(&mut self, type_: TokenType) -> Result<Token<'a>, ParserError<'a>> {
        let current_token = self.current();
        self.consume(type_)
            .ok_or_else(move || ParserError::TokenTypeMismatch {
                expected: self.bump_allocator.alloc([type_]),
                actual: current_token,
            })
    }

    fn expect_one_of(
        &mut self,
        expected: &'static [TokenType],
    ) -> Result<Token<'a>, ParserError<'a>> {
        let current_token = self.current();
        if expected.iter().any(|type_| current_token.type_ == *type_) {
            self.advance(1);
            Ok(current_token)
        } else {
            Err(ParserError::TokenTypeMismatch {
                expected,
                actual: current_token,
            })
        }
    }

    fn consume(&mut self, type_: TokenType) -> Option<Token<'a>> {
        let result = self.current();
        if result.type_ == type_ {
            self.advance(1);
            Some(result)
        } else {
            None
        }
    }

    fn consume_one_of(&mut self, types: &[TokenType]) -> Option<Token<'a>> {
        types.iter().filter_map(|type_| self.consume(*type_)).next()
    }

    fn advance(&mut self, amount: usize) {
        self.current_index += amount
    }

    fn module(&mut self) -> Result<Module<'a>, ParserError<'a>> {
        /*
        Module:
            (imports=Imports)
            (definitions=Definitions)
         */
        let imports = self.imports()?; // maybe empty
        let definitions = self.definitions()?; // maybe empty

        match self.current() {
            Token { type_, .. } if type_ != TokenType::EndOfInput => {
                // leftover tokens
                Err(ParserError::TokenTypeMismatch {
                    expected: &[TokenType::EndOfInput],
                    actual: self.current(),
                })
            }
            _ => {
                let imports = self.bump_allocator.alloc_slice_copy(&imports);
                let definitions = self.bump_allocator.alloc_slice_copy(&definitions);
                Ok(Module {
                    imports,
                    definitions,
                })
            }
        }
    }

    fn imports(&mut self) -> Result<Vec<Import<'a>>, ParserError<'a>> {
        /*
        Imports:
            (imports+=Import)*
         */
        let mut imports = Vec::new();
        while let Token {
            type_: TokenType::Import | TokenType::From,
            ..
        } = self.current()
        {
            imports.push(self.import()?);
        }
        Ok(imports)
    }

    fn import(&mut self) -> Result<Import<'a>, ParserError<'a>> {
        /*
        Import:
            'import' what=QualifiedName ('as' as=Identifier)? ';'
            | 'from' where=QualifiedName 'import' symbol=Identifier ('as' as=Identifier)? ';'
         */
        assert!(matches!(
            self.current(),
            Token {
                type_: TokenType::Import | TokenType::From,
                ..
            }
        ));
        let token = self.current();
        self.advance(1); // consume 'import' or 'from'

        let import = match token.type_ {
            TokenType::Import => {
                let what = self.qualified_name()?;
                match self.consume(TokenType::As) {
                    Some(_) => Import::ImportAs {
                        what,
                        as_: self.identifier()?,
                    },
                    None => Import::Import { what },
                }
            }
            TokenType::From => {
                let where_ = self.qualified_name()?;
                self.expect(TokenType::Import)?;
                let symbol = self.identifier()?;
                match self.consume(TokenType::As) {
                    Some(_) => Import::FromImportAs {
                        where_,
                        symbol,
                        as_: self.identifier()?,
                    },
                    None => Import::FromImport { where_, symbol },
                }
            }
            _ => unreachable!(),
        };
        self.expect(TokenType::Semicolon)?;
        Ok(import)
    }

    fn definitions(&mut self) -> Result<Vec<Definition<'a>>, ParserError<'a>> {
        let mut result = Vec::new();

        loop {
            let is_exported = self.consume(TokenType::Export).is_some();

            let is_valid_definition_start =
                [TokenType::Struct, TokenType::Function, TokenType::Let]
                    .contains(&self.current().type_);

            match (is_exported, is_valid_definition_start) {
                (false, false) => break,
                _ => result.push(self.definition(is_exported)?),
            }
        }

        Ok(result)
    }

    fn definition(&mut self, is_exported: bool) -> Result<Definition<'a>, ParserError<'a>> {
        match self.current().type_ {
            TokenType::Struct => Ok(Definition::Struct(self.struct_definition(is_exported)?)),
            TokenType::Function => Ok(Definition::Function(self.function_definition(is_exported)?)),
            TokenType::Let => Ok(Definition::GlobalVariable(
                self.global_variable_definition(is_exported)?,
            )),
            _ => Err(ParserError::TokenTypeMismatch {
                expected: &[TokenType::Struct, TokenType::Function],
                actual: self.current(),
            }),
        }
    }

    fn struct_definition(
        &mut self,
        is_exported: bool,
    ) -> Result<StructDefinition<'a>, ParserError<'a>> {
        /*
        StructDefinition:
            'struct' (name=TypeIdentifier) '{'
                (members=StructMembers)
            '}'

        StructMembers:
            (
                members+=StructMember
                (',' members+=StructMember)*
                (',')?
            )?

        StructMember:
            (name=NonTypeIdentifier) ':' (type=DataType)
        */
        assert!(matches!(
            self.current(),
            Token {
                type_: TokenType::Struct,
                ..
            }
        ));

        self.advance(1); // consume 'struct'
        let name = self.type_identifier()?;
        self.expect(TokenType::LeftCurlyBracket)?;
        let mut members = Vec::new();

        while let Some(identifier_token) = self.consume(TokenType::LowercaseIdentifier) {
            self.expect(TokenType::Colon)?;
            let type_ = self.data_type()?;
            members.push(StructMember {
                name: Identifier::NonTypeIdentifier(NonTypeIdentifier(identifier_token)),
                type_,
            });
            let comma_present = self.consume(TokenType::Comma).is_some();
            if !comma_present
                || matches!(
                    self.current(),
                    Token {
                        type_: TokenType::RightCurlyBracket,
                        ..
                    }
                )
            {
                break;
            }
        }

        self.expect(TokenType::RightCurlyBracket)?;

        Ok(StructDefinition {
            is_exported,
            name,
            members: self.bump_allocator.alloc_slice_copy(&members),
        })
    }

    fn function_definition(
        &mut self,
        is_exported: bool,
    ) -> Result<FunctionDefinition<'a>, ParserError<'a>> {
        /*
        FunctionDefinition:
            'function' (name=NonTypeIdentifier) '(' (parameters=ParameterList) ')' ('~>' (return_type=DataType))? '{'
                (statements=Statements)?
            '}'
         */
        assert!(matches!(
            self.current(),
            Token {
                type_: TokenType::Function,
                ..
            }
        ));

        self.advance(1); // consume 'function'
        let name = self.non_type_identifier()?;
        self.expect(TokenType::LeftParenthesis)?;
        let parameters = self.parameter_list()?;
        self.expect(TokenType::RightParenthesis)?;

        let return_type = if self.consume(TokenType::TildeArrow).is_some() {
            Some(self.data_type()?)
        } else {
            None
        };

        let body = self.block()?;

        Ok(FunctionDefinition {
            is_exported,
            name,
            parameters: self.bump_allocator.alloc_slice_copy(&parameters),
            return_type,
            body,
        })
    }

    fn global_variable_definition(
        &mut self,
        is_exported: bool,
    ) -> Result<GlobalVariableDefinition<'a>, ParserError<'a>> {
        /*
        GlobalVariableDefinition:
            (definition=LocalVariableDefinition)
         */
        let definition = self.local_variable_definition()?;
        Ok(GlobalVariableDefinition {
            is_exported,
            mutability: definition.mutability,
            name: definition.name,
            type_: definition.type_,
            initial_value: definition.initial_value,
        })
    }

    fn local_variable_definition(
        &mut self,
    ) -> Result<LocalVariableDefinition<'a>, ParserError<'a>> {
        /*
        LocalVariableDefinition:
            'let' (mutability=Mutability) (name=NonTypeIdentifier) [':' type=DataType]? '=' (value=Expression) ';'
         */
        self.expect(TokenType::Let)?;
        let mutability = if self.consume(TokenType::Mutable).is_some() {
            Mutability::Mutable
        } else {
            self.consume(TokenType::Const);
            Mutability::Constant
        };
        let name = self.non_type_identifier()?;
        let type_ = if self.consume(TokenType::Colon).is_some() {
            Some(self.data_type()?)
        } else {
            None
        };
        self.expect(TokenType::Equals)?;
        let initial_value = self.expression()?;
        self.expect(TokenType::Semicolon)?;
        Ok(LocalVariableDefinition {
            mutability,
            name,
            type_,
            initial_value,
        })
    }

    fn parameter_list(&mut self) -> Result<Vec<FunctionParameter<'a>>, ParserError<'a>> {
        /*
        ParameterList:
            (
                (parameter+=FunctionParameter)
                (',' parameter+=FunctionParameter)*
                (',')?
            )?
         */
        let mut parameters = Vec::new();
        while let Token {
            type_: TokenType::LowercaseIdentifier,
            ..
        } = self.current()
        {
            parameters.push(self.function_parameter()?);
            if self.consume(TokenType::Comma).is_none() {
                break;
            }
        }
        Ok(parameters)
    }

    fn function_parameter(&mut self) -> Result<FunctionParameter<'a>, ParserError<'a>> {
        /*
        FunctionParameter:
            (name=NonTypeIdentifier) ':' (type=DataType)
         */
        let name = self.identifier()?;
        self.expect(TokenType::Colon)?;
        let type_ = self.data_type()?;

        Ok(FunctionParameter { name, type_ })
    }

    fn block(&mut self) -> Result<Block<'a>, ParserError<'a>> {
        self.expect(TokenType::LeftCurlyBracket)?;
        let mut statements: Vec<Statement> = Vec::new();
        while !matches!(
            self.current(),
            Token {
                type_: TokenType::RightCurlyBracket,
                ..
            }
        ) {
            statements.push(self.statement()?);
        }
        assert_eq!(self.current().type_, TokenType::RightCurlyBracket);
        self.advance(1); // consume '}'
        Ok(Block {
            statements: self.bump_allocator.alloc_slice_copy(&statements),
        })
    }

    fn statement(&mut self) -> Result<Statement<'a>, ParserError<'a>> {
        match self.current().type_ {
            TokenType::Yield => self.yield_statement(),
            TokenType::Return => self.return_statement(),
            TokenType::Let => Ok(Statement::VariableDefinition(
                self.local_variable_definition()?,
            )),
            _ => self.expression_statement(),
        }
    }

    fn yield_statement(&mut self) -> Result<Statement<'a>, ParserError<'a>> {
        self.expect(TokenType::Yield)?;
        let expression = self.expression()?;
        self.expect(TokenType::Semicolon)?;
        Ok(Statement::Yield(expression))
    }

    fn return_statement(&mut self) -> Result<Statement<'a>, ParserError<'a>> {
        self.expect(TokenType::Return)?;
        let has_return_value = self.current().type_ != TokenType::Semicolon;
        let return_value = if has_return_value {
            Some(self.expression()?)
        } else {
            None
        };
        self.expect(TokenType::Semicolon)?;
        Ok(Statement::Return(return_value))
    }

    fn expression_statement(&mut self) -> Result<Statement<'a>, ParserError<'a>> {
        let expression = self.expression()?;
        self.expect(TokenType::Semicolon)?;
        Ok(Statement::ExpressionStatement(expression))
    }

    fn expression(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
        self.addition()
    }

    fn addition(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
        let mut accumulator = self.multiplication()?;
        while let Token {
            type_: TokenType::Plus,
            ..
        } = self.current()
        {
            let operator = self.current();
            self.advance(1); // consume operator
            accumulator = Expression::BinaryOperator {
                lhs: self.bump_allocator.alloc(accumulator),
                operator,
                rhs: self.bump_allocator.alloc(self.multiplication()?),
            }
        }
        Ok(accumulator)
    }

    fn multiplication(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
        let mut accumulator = self.primary()?;
        while let Token {
            type_: TokenType::Asterisk,
            ..
        } = self.current()
        {
            let operator = self.current();
            self.advance(1); // consume operator
            accumulator = Expression::BinaryOperator {
                lhs: self.bump_allocator.alloc(accumulator),
                operator,
                rhs: self.bump_allocator.alloc(self.primary()?),
            }
        }
        Ok(accumulator)
    }

    fn primary(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
        if self.consume(TokenType::LeftParenthesis).is_some() {
            let sub_expression = self.expression()?;
            self.expect(TokenType::RightParenthesis)?;
            Ok(sub_expression)
        } else if let Token {
            type_: TokenType::LeftCurlyBracket,
            ..
        } = self.current()
        {
            Ok(Expression::Block(self.block()?))
        } else {
            Ok(Expression::IntegerLiteral(self.expect(TokenType::Integer)?))
        }
    }

    fn mutability(&mut self) -> Mutability {
        /*
        Mutability:
            ('mutable' | 'const')?

        note: if both 'mutable' and 'const' are not present, 'const' is assumed
         */
        if self.consume(TokenType::Mutable).is_some() {
            Mutability::Mutable
        } else {
            self.consume(TokenType::Const);
            Mutability::Constant
        }
    }

    fn type_list(&mut self) -> Result<Vec<DataType<'a>>, ParserError<'a>> {
        let is_valid_type_start = |token: Token| {
            [
                TokenType::ColonColon,
                TokenType::LowercaseIdentifier,
                TokenType::UppercaseIdentifier,
                TokenType::LeftSquareBracket,
                TokenType::Arrow,
                TokenType::CapitalizedFunction,
            ]
            .contains(&token.type_)
        };

        let mut types = Vec::new();
        while is_valid_type_start(self.current()) {
            types.push(self.data_type()?);
            if self.consume(TokenType::Comma).is_none() {
                break;
            }
        }

        Ok(types)
    }

    fn data_type(&mut self) -> Result<DataType<'a>, ParserError<'a>> {
        /*
        DataType:
            (name=QualifiedTypeName)
            | ('[' contained_type=DataType ';' size=Integer ']')
            | ('->' mutability=Mutability pointee_type=DataType)
            | ('Function' '(' parameter_types=TypeList ')' '~>' return_type=DataType)
         */
        match self.current() {
            Token {
                type_: TokenType::LeftSquareBracket,
                ..
            } => {
                // array type
                self.advance(1); // consume '['
                let contained_type = self.bump_allocator.alloc(self.data_type()?);
                self.expect(TokenType::Semicolon)?;
                let size_token = self.expect(TokenType::Integer)?;
                let size = parse_unsigned_int(size_token)?;
                self.expect(TokenType::RightSquareBracket)?;
                Ok(DataType::Array {
                    contained_type,
                    size,
                })
            }
            Token {
                type_: TokenType::Arrow,
                ..
            } => {
                // pointer type
                self.advance(1); // consume '->'

                let mutability = self.mutability();

                let pointee_type = self.bump_allocator.alloc(self.data_type()?);
                Ok(DataType::Pointer {
                    mutability,
                    pointee_type,
                })
            }
            Token {
                type_: TokenType::CapitalizedFunction,
                ..
            } => {
                // function pointer type
                self.advance(1); // consume 'Function'
                self.expect(TokenType::LeftParenthesis)?;
                let parameter_types = self.type_list()?;
                self.expect(TokenType::RightParenthesis)?;
                self.expect(TokenType::TildeArrow)?;
                let return_type = self.bump_allocator.alloc(self.data_type()?);
                Ok(DataType::FunctionPointer {
                    parameter_types: self.bump_allocator.alloc_slice_copy(&parameter_types),
                    return_type,
                })
            }
            _ => {
                // named type
                let name = self.qualified_type_name()?;
                Ok(DataType::Named { name })
            }
        }
    }

    fn type_identifier(&mut self) -> Result<TypeIdentifier<'a>, ParserError<'a>> {
        /*
        TypeIdentifier:
            token=UPPERCASE_IDENTIFIER
         */
        Ok(TypeIdentifier(self.expect(TokenType::UppercaseIdentifier)?))
    }

    fn non_type_identifier(&mut self) -> Result<NonTypeIdentifier<'a>, ParserError<'a>> {
        /*
        TypeIdentifier:
            token=LOWERCASE_IDENTIFIER
         */
        Ok(NonTypeIdentifier(
            self.expect(TokenType::LowercaseIdentifier)?,
        ))
    }

    fn identifier(&mut self) -> Result<Identifier<'a>, ParserError<'a>> {
        /*
        Identifier:
            token=(LOWERCASE_IDENTIFIER | UPPERCASE_IDENTIFIER)
         */
        let token = self.expect_one_of(&[
            TokenType::LowercaseIdentifier,
            TokenType::UppercaseIdentifier,
        ])?;
        match token.type_ {
            TokenType::LowercaseIdentifier => {
                Ok(Identifier::NonTypeIdentifier(NonTypeIdentifier(token)))
            }
            TokenType::UppercaseIdentifier => Ok(Identifier::TypeIdentifier(TypeIdentifier(token))),
            _ => unreachable!(),
        }
    }

    fn consume_until_one_of(&mut self, types: &'static [TokenType]) -> &'a [Token<'a>] {
        let consumable_tokens = self.tokens[self.current_index..]
            .split(|token| types.contains(&token.type_))
            .next()
            .expect("split does always return an iterator with at least one element");
        self.advance(consumable_tokens.len());
        consumable_tokens
    }

    fn consume_until_none_of(&mut self, types: &'static [TokenType]) -> &'a [Token<'a>] {
        let consumable_tokens = self.tokens[self.current_index..]
            .split(|token| !types.contains(&token.type_))
            .next()
            .expect("split does always return an iterator with at least one element");
        self.advance(consumable_tokens.len());
        consumable_tokens
    }

    fn qualified_type_name(&mut self) -> Result<QualifiedTypeName<'a>, ParserError<'a>> {
        /*
        QualifiedTypeName:
            (is_absolute?='::' (tokens+=LOWERCASE_IDENTIFIER) '::')? ((tokens+=LOWERCASE_IDENTIFIER) '::')* tokens+=UPPERCASE_IDENTIFIER
         */
        match self.qualified_name()? {
            QualifiedName::QualifiedTypeName(qualified_type_name) => Ok(qualified_type_name),
            qualified_name => {
                assert_eq!(
                    qualified_name.tokens().last().unwrap().type_,
                    TokenType::LowercaseIdentifier
                );
                Err(ParserError::TokenTypeMismatch {
                    expected: &[TokenType::UppercaseIdentifier],
                    actual: *qualified_name.tokens().last().unwrap(),
                })
            }
        }
    }

    fn qualified_non_type_name(&mut self) -> Result<QualifiedNonTypeName<'a>, ParserError<'a>> {
        /*
        QualifiedNonTypeName:
            (('::')? (tokens+=LOWERCASE_IDENTIFIER '::')* )? tokens+=LOWERCASE_IDENTIFIER
         */
        match self.qualified_name()? {
            QualifiedName::QualifiedNonTypeName(qualified_non_type_name) => {
                Ok(qualified_non_type_name)
            }
            qualified_name => {
                assert_eq!(
                    qualified_name.tokens().last().unwrap().type_,
                    TokenType::UppercaseIdentifier
                );
                Err(ParserError::TokenTypeMismatch {
                    expected: &[TokenType::LowercaseIdentifier],
                    actual: *qualified_name.tokens().last().unwrap(),
                })
            }
        }
    }

    fn qualified_name(&mut self) -> Result<QualifiedName<'a>, ParserError<'a>> {
        /*
        QualifiedName:
            QualifiedTypeName
            | QualifiedNonTypeName
         */
        let rest_of_tokens = &self.tokens[self.current_index..];

        let is_absolute = match self.current() {
            Token {
                type_: TokenType::ColonColon,
                ..
            } => true,
            Token {
                type_: TokenType::LowercaseIdentifier,
                ..
            }
            | Token {
                type_: TokenType::UppercaseIdentifier,
                ..
            } => {
                self.advance(1); // consume first identifier
                false
            }
            token => {
                return Err(ParserError::TokenTypeMismatch {
                    expected: &[
                        TokenType::ColonColon,
                        TokenType::LowercaseIdentifier,
                        TokenType::UppercaseIdentifier,
                    ],
                    actual: token,
                })
            }
        };

        if rest_of_tokens.first().unwrap().type_ == TokenType::UppercaseIdentifier {
            return Ok(QualifiedName::QualifiedTypeName(
                QualifiedTypeName::Relative {
                    tokens: &rest_of_tokens[..1],
                },
            ));
        }

        let mut num_tokens = (!is_absolute).into();

        while self.consume(TokenType::ColonColon).is_some() {
            num_tokens += 1; // for '::'
            let current_token = self.expect_one_of(&[
                TokenType::LowercaseIdentifier,
                TokenType::UppercaseIdentifier,
            ])?;
            num_tokens += 1; // for the identifier
            if current_token.type_ == TokenType::UppercaseIdentifier {
                break;
            }
        }

        let tokens = &rest_of_tokens[..num_tokens];

        let is_type_name = tokens.last().unwrap().type_ == TokenType::UppercaseIdentifier;

        if is_absolute && is_type_name {
            assert_eq!(tokens.first().unwrap().type_, TokenType::ColonColon);
            assert!(tokens.len() >= 2);
            if tokens[1].type_ != TokenType::LowercaseIdentifier {
                return Err(ParserError::TokenTypeMismatch {
                    expected: &[TokenType::LowercaseIdentifier],
                    actual: tokens[1],
                });
            }
        }

        Ok(match (is_absolute, is_type_name) {
            (true, true) => {
                QualifiedName::QualifiedTypeName(QualifiedTypeName::Absolute { tokens })
            }
            (true, false) => {
                QualifiedName::QualifiedNonTypeName(QualifiedNonTypeName::Absolute { tokens })
            }
            (false, true) => {
                QualifiedName::QualifiedTypeName(QualifiedTypeName::Relative { tokens })
            }
            (false, false) => {
                QualifiedName::QualifiedNonTypeName(QualifiedNonTypeName::Relative { tokens })
            }
        })
    }

    fn repeated_tokens(
        &mut self,
        sequence: &'static [TokenType],
    ) -> Result<&'a [Token<'a>], ParserError> {
        assert!(!sequence.is_empty());
        let mut num_tokens = 0;
        let start_index = self.current_index;

        // todo: refactor to use fold (and take_while or something? ¯\_(ツ)_/¯)
        for (token, &expected) in self.tokens[start_index..]
            .iter()
            .zip(sequence.iter().cycle())
        {
            if token.type_ != expected {
                break;
            }
            num_tokens += 1;
        }

        if num_tokens == 0 {
            Err(ParserError::TokenTypeMismatch {
                expected: self.bump_allocator.alloc([sequence[0]]),
                actual: self.current(),
            })
        } else {
            Ok(&self.tokens[start_index..][..num_tokens])
        }
    }
}

fn parse<'a>(
    tokens: &'a [Token<'a>],
    bump_allocator: &'a Bump,
) -> Result<Module<'a>, ParserError<'a>> {
    ParserState::new(tokens, bump_allocator).module()
}

pub(crate) fn parse_module<'a>(
    filename: &'a Path,
    source: &'a str,
    bump_allocator: &'a Bump,
) -> Result<Module<'a>, Box<dyn ErrorReport + 'a>> {
    let tokens = tokenize(filename, source)?;
    let tokens = bump_allocator.alloc_slice_clone(&tokens);
    let module = parse(tokens, bump_allocator)?;
    Ok(module)
}
