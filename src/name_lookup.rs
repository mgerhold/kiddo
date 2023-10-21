use bumpalo::Bump;

use crate::mutual_type_resolution::ir_for_mutual_type_resolution::{
    CompletelyResolvedFunctionDefinition, CompletelyResolvedGlobalVariableDefinition,
    CompletelyResolvedNonTypeDefinition, CompletelyResolvedOverloadSet,
    CompletelyResolvedTypeDefinition, FunctionParameter, LocalVariable, ModuleForNameResolution,
    ProgramWithResolvedTypes,
};
use crate::name_lookup::errors::NameLookupError;
use crate::name_lookup::ir_after_name_lookup::{Scope, ScopeStack};
use crate::parser::ir_parsed::{Expression, LocalVariableDefinition, Statement};

pub(crate) mod errors;
pub(crate) mod ir_after_name_lookup;

fn perform_name_lookup_for_expression<'a>(
    expression: &'a Expression<'a>,
    scope_stack: &mut ScopeStack<'a>,
    bump_allocator: &'a Bump,
) -> Result<(), NameLookupError<'a>> {
    match expression {
        Expression::Literal(_) => {
            // nothing do do since a literal cannot contain a name reference
        }
        Expression::BinaryOperator { lhs, operator, rhs } => {
            perform_name_lookup_for_expression(lhs, scope_stack, bump_allocator)?;
            perform_name_lookup_for_expression(rhs, scope_stack, bump_allocator)?;
        }
        Expression::Block(block) => {
            let number_of_scopes = scope_stack.len();
            scope_stack.push(Scope::default());
            for statement in block.statements {
                perform_name_lookup_for_statement(statement, scope_stack, bump_allocator)?;
            }
            scope_stack.truncate(number_of_scopes);
        }
        Expression::Name(qualified_name) => {
            scope_stack.lookup_non_type(qualified_name)?;
        }
    }
    Ok(())
}

fn perform_name_lookup_for_global_variable_definition<'a>(
    definition: &'a CompletelyResolvedGlobalVariableDefinition<'a>,
    scope_stack: &mut ScopeStack<'a>,
    bump_allocator: &'a Bump,
) -> Result<(), NameLookupError<'a>> {
    perform_name_lookup_for_expression(&definition.initial_value, scope_stack, bump_allocator)
}

fn perform_name_lookup_for_local_variable_definition<'a>(
    definition: &'a LocalVariableDefinition<'a>,
    scope_stack: &mut ScopeStack<'a>,
    bump_allocator: &'a Bump,
) -> Result<(), NameLookupError<'a>> {
    perform_name_lookup_for_expression(&definition.initial_value, scope_stack, bump_allocator)?;

    let must_open_new_scope = scope_stack
        .peek()
        .get_non_type_definition(definition.name.0.lexeme())
        .is_some();

    if must_open_new_scope {
        scope_stack.push(Scope::default());
    }

    let looked_up_type = definition
        .type_
        .map(|type_| scope_stack.lookup_type(type_))
        .transpose()?;

    scope_stack.peek_mut().non_type_definitions.insert(
        definition.name.0.lexeme(),
        &*bump_allocator.alloc(CompletelyResolvedNonTypeDefinition::LocalVariable(
            &*bump_allocator.alloc(LocalVariable {
                mutability: definition.mutability,
                name: definition.name,
                type_: looked_up_type,
            }),
        )),
    );

    println!("{scope_stack}");
    Ok(())
}

fn perform_name_lookup_for_statement<'a>(
    statement: &'a Statement<'a>,
    scope_stack: &mut ScopeStack<'a>,
    bump_allocator: &'a Bump,
) -> Result<(), NameLookupError<'a>> {
    match statement {
        Statement::ExpressionStatement(expression) => {
            perform_name_lookup_for_expression(expression, scope_stack, bump_allocator)
        }
        Statement::Yield(expression) => {
            perform_name_lookup_for_expression(expression, scope_stack, bump_allocator)
        }
        Statement::Return(Some(expression)) => {
            perform_name_lookup_for_expression(expression, scope_stack, bump_allocator)
        }
        Statement::Return(None) => Ok(()),
        Statement::VariableDefinition(definition) => {
            perform_name_lookup_for_local_variable_definition(
                definition,
                scope_stack,
                bump_allocator,
            )
        }
    }
}

fn perform_name_lookup_for_overload<'a>(
    overload: &'a CompletelyResolvedFunctionDefinition<'a>,
    scope_stack: &mut ScopeStack<'a>,
    bump_allocator: &'a Bump,
) -> Result<(), NameLookupError<'a>> {
    println!("name lookup for function '{}'", overload.name.0.lexeme());
    let number_of_scopes = scope_stack.len();
    let function_scope = Scope {
        type_definitions: Default::default(),
        non_type_definitions: overload
            .parameters
            .iter()
            .map(|parameter| {
                (
                    parameter.name.0.lexeme(),
                    &*bump_allocator.alloc(CompletelyResolvedNonTypeDefinition::FunctionParameter(
                        &*bump_allocator.alloc(FunctionParameter {
                            mutability: parameter.mutability,
                            name: parameter.name,
                            type_: parameter.type_,
                        }),
                    )),
                )
            })
            .collect(),
    };
    scope_stack.push(function_scope);
    println!("{scope_stack}");
    for statement in overload.body.statements {
        perform_name_lookup_for_statement(statement, scope_stack, bump_allocator)?;
    }

    assert!(scope_stack.len() >= number_of_scopes);
    scope_stack.truncate(number_of_scopes);

    Ok(())
}

fn perform_name_lookup_for_overload_set<'a>(
    overload_set: CompletelyResolvedOverloadSet<'a>,
    scope_stack: &mut ScopeStack<'a>,
    bump_allocator: &'a Bump,
) -> Result<(), NameLookupError<'a>> {
    for overload in overload_set {
        perform_name_lookup_for_overload(overload, scope_stack, bump_allocator)?;
    }
    Ok(())
}

fn perform_name_lookup_for_module<'a>(
    global_type_table: &'a [&'a CompletelyResolvedTypeDefinition<'a>],
    module: &'a ModuleForNameResolution<'a>,
    bump_allocator: &'a Bump,
) -> Result<(), NameLookupError<'a>> {
    let mut scope_stack = ScopeStack::new(
        global_type_table,
        module.global_scope.clone(),
        bump_allocator,
    );
    for definition in module.non_type_definitions {
        match definition {
            CompletelyResolvedNonTypeDefinition::GlobalVariable(definition) => {
                perform_name_lookup_for_global_variable_definition(
                    definition,
                    &mut scope_stack,
                    bump_allocator,
                )?;
            }
            CompletelyResolvedNonTypeDefinition::Function(overload_set) => {
                perform_name_lookup_for_overload_set(
                    overload_set,
                    &mut scope_stack,
                    bump_allocator,
                )?;
            }
            CompletelyResolvedNonTypeDefinition::FunctionParameter(_) => {
                unreachable!("the global scope of a module must never contain a function parameter")
            }
            CompletelyResolvedNonTypeDefinition::LocalVariable(_) => {
                unreachable!("the global scope of a module must never contain a local variable")
            }
        }
    }
    Ok(())
}

pub(crate) fn perform_name_lookup<'a>(
    program: &'a ProgramWithResolvedTypes<'a>,
    bump_allocator: &'a Bump,
) -> Result<(), NameLookupError<'a>> {
    for module in &program.modules {
        perform_name_lookup_for_module(program.type_table, module, bump_allocator)?;
    }
    Ok(())
}
