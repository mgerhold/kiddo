use std::path::Path;

use bumpalo::Bump;

use crate::lexer::tokenize;
use crate::parser::errors::{ErrorReport, ParserError};
use crate::parser::ir_parsed::{
    DataType, Definition, FunctionDefinition, FunctionParameter, Identifier, Import, Module,
    Mutability, QualifiedName, StructDefinition, StructMember,
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
                expected: vec![type_],
                actual: current_token,
            })
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
                    expected: vec![TokenType::EndOfInput],
                    actual: self.current(),
                })
            }
            _ => Ok(Module {
                imports,
                definitions,
            }),
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
            match self.current().type_ {
                TokenType::Struct => result.push(Definition::Struct(self.struct_definition()?)),
                TokenType::Function => {
                    result.push(Definition::Function(self.function_definition()?))
                }
                _ => break,
            }
        }

        Ok(result)
    }

    fn struct_definition(&mut self) -> Result<StructDefinition<'a>, ParserError<'a>> {
        /*
        StructDefinition:
            'struct' (name=Identifier) '{'
                (members=StructMembers)
            '}'

        StructMembers:
            (
                members+=StructMember
                (',' members+=StructMember)*
                (',')?
            )?

        StructMember:
            (name=Identifier) ':' (type=DataType)
        */
        assert!(matches!(
            self.current(),
            Token {
                type_: TokenType::Struct,
                ..
            }
        ));

        self.advance(1); // consume 'struct'
        let name = self.identifier()?;
        self.expect(TokenType::LeftCurlyBracket)?;
        let mut members = Vec::new();

        while let Some(identifier_token) = self.consume(TokenType::Identifier) {
            self.expect(TokenType::Colon)?;
            let type_ = self.data_type()?;
            members.push(StructMember {
                name: Identifier {
                    token: identifier_token,
                },
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

        Ok(StructDefinition { name, members })
    }

    fn function_definition(&mut self) -> Result<FunctionDefinition<'a>, ParserError<'a>> {
        /*
        FunctionDefinition:
            'function' (name=Identifier) '(' (parameters=ParameterList) ')' ('~>' (return_type=DataType))? '{'
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
        let name = self.identifier()?;
        self.expect(TokenType::LeftParenthesis)?;
        let parameters = self.parameter_list()?;
        self.expect(TokenType::RightParenthesis)?;

        let return_type = if self.consume(TokenType::TildeArrow).is_some() {
            Some(self.data_type()?)
        } else {
            None
        };

        self.expect(TokenType::LeftCurlyBracket)?;

        // todo: function body

        self.expect(TokenType::RightCurlyBracket)?;

        Ok(FunctionDefinition {
            name,
            parameters,
            return_type,
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
            type_: TokenType::Identifier,
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
            (name=Identifier) ':' (type=DataType)
         */
        let name = self.identifier()?;
        self.expect(TokenType::Colon)?;
        let type_ = self.data_type()?;

        Ok(FunctionParameter { name, type_ })
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
                TokenType::Identifier,
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
            (name=QualifiedName)
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
                    parameter_types,
                    return_type,
                })
            }
            _ => {
                // named type
                let name = self.qualified_name()?;
                Ok(DataType::Named { name })
            }
        }
    }

    fn identifier(&mut self) -> Result<Identifier<'a>, ParserError<'a>> {
        /*
        Identifier:
            token=IDENTIFIER
         */
        Ok(Identifier {
            token: self.expect(TokenType::Identifier)?,
        })
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

    fn qualified_name(&mut self) -> Result<QualifiedName<'a>, ParserError<'a>> {
        /*
        QualifiedName:
            ('::')? tokens+=Identifier ('::' tokens+=Identifier)*
         */
        let rest_of_tokens = &self.tokens[self.current_index..];

        let is_absolute = match self.current() {
            Token {
                type_: TokenType::ColonColon,
                ..
            } => true,
            Token {
                type_: TokenType::Identifier,
                ..
            } => {
                self.advance(1); // consume first identifier
                false
            }
            token => {
                return Err(ParserError::TokenTypeMismatch {
                    expected: vec![TokenType::ColonColon, TokenType::Identifier],
                    actual: token,
                })
            }
        };

        let mut num_tokens = (!is_absolute).into();

        while self.consume(TokenType::ColonColon).is_some() {
            num_tokens += 1; // for '::'
            self.expect(TokenType::Identifier)?;
            num_tokens += 1; // for the identifier
        }

        Ok(match is_absolute {
            true => QualifiedName::Absolute {
                tokens: &rest_of_tokens[..num_tokens],
            },
            false => QualifiedName::Relative {
                tokens: &rest_of_tokens[..num_tokens],
            },
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
                expected: vec![sequence[0]],
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
