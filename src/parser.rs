use std::path::Path;

use bumpalo::Bump;

use crate::lexer::tokenize;
use crate::parser::errors::{ErrorReport, ParserError};
use crate::parser::ir_parsed::{
    Definition, Identifier, Import, Module, QualifiedName, StructDefinition, StructMember,
};
use crate::token::{Token, TokenType};

pub(crate) mod errors;
pub(crate) mod ir_parsed;

fn ends_with_identifier(tokens: &[Token]) -> Result<(), ParserError> {
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
}

impl<'a> ParserState<'a> {
    fn new(tokens: &'a [Token<'a>]) -> Self {
        Self {
            tokens,
            current_index: 0,
        }
    }

    fn current(&self) -> Option<Token<'a>> {
        self.tokens.get(self.current_index).cloned()
    }

    fn peek(&self) -> Option<Token<'a>> {
        self.tokens.get(self.current_index + 1).cloned()
    }

    fn expect(&mut self, type_: TokenType) -> Result<Token<'a>, ParserError> {
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

    fn module(&mut self) -> Result<Module<'a>, ParserError> {
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

    fn imports(&mut self) -> Result<Vec<Import<'a>>, ParserError> {
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

    fn import(&mut self) -> Result<Import<'a>, ParserError> {
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

    fn definitions(&mut self) -> Result<Vec<Definition<'a>>, ParserError> {
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

    fn struct_definition(&mut self) -> Result<StructDefinition<'a>, ParserError> {
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
            (name=Identifier) ':' (type=Identifier)
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
        while let Ok(member) = StructMember::try_from(&self.tokens[self.current_index..]) {
            members.push(member);
            self.advance(3);
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

    fn identifier(&mut self) -> Result<Identifier<'a>, ParserError> {
        /*
        Identifier:
            token=IDENTIFIER
         */
        let identifier =
            Identifier::try_from(self.current().expect("must at least be EndOfInput"))?;
        self.advance(1);
        Ok(identifier)
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

    fn qualified_name(&mut self) -> Result<QualifiedName<'a>, ParserError> {
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
                actual: self.current().and_then(|token| Some(token.type_)),
            })
        } else {
            Ok(&self.tokens[start_index..][..num_tokens])
        }
    }
}

fn parse<'a>(tokens: &'a [Token<'a>]) -> Result<Module<'a>, ParserError> {
    ParserState::new(tokens).module()
}

pub(crate) fn parse_module<'a>(
    filename: &'a Path,
    source: &'a str,
    bump_allocator: &'a Bump,
) -> Result<Module<'a>, Box<dyn ErrorReport + 'a>> {
    match tokenize(filename, source) {
        Ok(tokens) => {
            let tokens = bump_allocator.alloc_slice_clone(&tokens);
            let module = parse(tokens)?;
            Ok(module)
        }
        Err(error) => Err(error.into()),
    }
    /*let tokens = tokenize(filename, source)?;
    let tokens = bump_allocator.alloc_slice_clone(&tokens);
    let module = parse(tokens)?;
    Ok(module)*/
}
