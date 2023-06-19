use std::path::Path;

use bumpalo::Bump;

use crate::lexer::tokenize;
use crate::parser::errors::{ErrorReport, ParserError};
use crate::parser::ir_parsed::{
    DataType, Definition, Identifier, Import, Module, Mutability, QualifiedName, StructDefinition,
    StructMember,
};
use crate::token::{Token, TokenType};
use crate::utils::parse_unsigned_int;

pub(crate) mod errors;
pub(crate) mod ir_parsed;

fn ends_with_identifier<'a>(tokens: &'a [Token<'a>]) -> Result<(), ParserError<'a>> {
    match tokens {
        [] => Err(ParserError::TokenTypeMismatch {
            expected: vec![TokenType::Identifier],
            actual: None,
        }),
        [.., Token {
            type_: TokenType::Identifier,
            ..
        }] => Ok(()),
        [.., Token { type_, .. }] => Err(ParserError::TokenTypeMismatch {
            expected: vec![TokenType::Identifier],
            actual: Some(*type_),
        }),
    }
}

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

    fn current(&self) -> Option<Token<'a>> {
        self.tokens.get(self.current_index).cloned()
    }

    fn peek(&self) -> Option<Token<'a>> {
        self.tokens.get(self.current_index + 1).cloned()
    }

    fn expect(&mut self, type_: TokenType) -> Result<Token<'a>, ParserError<'a>> {
        let current_token_type = self.current().map(|token| token.type_);
        self.consume(type_)
            .ok_or_else(move || ParserError::TokenTypeMismatch {
                expected: vec![type_],
                actual: current_token_type,
            })
    }

    fn consume(&mut self, type_: TokenType) -> Option<Token<'a>> {
        let result = self.current().expect("should at least be EndOfInput");
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

        assert!(self.current().is_some());
        match self.current() {
            Some(Token { type_, .. }) if type_ != TokenType::EndOfInput => {
                // leftover tokens
                Err(ParserError::TokenTypeMismatch {
                    expected: vec![TokenType::EndOfInput],
                    actual: Some(self.current().expect("EndOfInput not yet reached").type_),
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
        while let Some(Token {
            type_: TokenType::Import | TokenType::From,
            ..
        }) = self.current()
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
            Some(Token {
                type_: TokenType::Import | TokenType::From,
                ..
            })
        ));
        let token = self
            .current()
            .expect("checked existence of token by caller");
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
        while let Some(
            token @ Token {
                type_: TokenType::Struct,
                ..
            },
        ) = self.current()
        {
            match token.type_ {
                TokenType::Struct => {
                    result.push(Definition::Struct(self.struct_definition()?));
                }
                _ => unreachable!(),
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
            Some(Token {
                type_: TokenType::Struct,
                ..
            })
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
                    Some(Token {
                        type_: TokenType::RightCurlyBracket,
                        ..
                    })
                )
            {
                break;
            }
        }

        self.expect(TokenType::RightCurlyBracket)?;

        Ok(StructDefinition { name, members })
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

    fn data_type(&mut self) -> Result<DataType<'a>, ParserError<'a>> {
        /*
        DataType:
            (name=QualifiedName)
            | ('[' contained_type=DataType ';' size=Integer ']')
            | ('->' mutability=Mutability pointee_type=DataType)
            | ('Function' '(' parameter_types=TypeList ')' '~>' return_type=DataType)
         */

        // todo: implement parsing function pointers
        match self.current().expect("should be at least EndOfInput") {
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
        match self.current() {
            Some(Token {
                type_: TokenType::ColonColon | TokenType::Identifier,
                ..
            }) => QualifiedName::try_from(
                self.consume_until_none_of(&[TokenType::ColonColon, TokenType::Identifier]),
            ),
            Some(token) => Err(ParserError::TokenTypeMismatch {
                expected: vec![TokenType::Identifier, TokenType::ColonColon],
                actual: Some(token.type_),
            }),
            None => unreachable!("should at least find the EndOfInput token"),
        }
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
                actual: self.current().map(|token| token.type_),
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
