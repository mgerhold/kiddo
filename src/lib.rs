#![allow(dead_code)]
#![allow(clippy::result_large_err)]
#![feature(ptr_from_ref)]
#![feature(slice_from_ptr_range)]
#![feature(entry_insert)]
#![feature(iter_collect_into)]
#![feature(iter_intersperse)]

extern crate core;

use bumpalo::Bump;

pub use crate::command_line_arguments::CommandLineArguments;
use crate::helpers::{gather_import_directories, get_canonical_path_to_main_module};
use crate::import_resolution::{
    categorize_names, connect_modules, find_imports, resolve_imports, ModuleWithImports,
};
use crate::name_lookup::ir_after_name_lookup::CompletelyResolvedNonTypeDefinition;
use crate::name_lookup::{completely_resolve_type_definitions, partially_resolve_type_definitions};
use crate::parser::errors::ErrorReport;
use crate::parser::ir_parsed::{Expression, Statement};
use crate::parser::parse_module;
use crate::utils::AllocPath;

mod command_line_arguments;
mod constants;
mod helpers;
mod import_resolution;
mod lexer;
mod name_lookup;
mod parser;
mod token;
mod utils;

#[cfg(test)]
mod test;

pub fn main<'a>(
    bump_allocator: &'a Bump,
    command_line_args: CommandLineArguments,
) -> Result<(), Box<dyn ErrorReport + 'a>> {
    let main_module_canonical_path =
        get_canonical_path_to_main_module(&command_line_args, bump_allocator)?;
    let main_module_directory = bump_allocator.alloc_path(
        main_module_canonical_path
            .parent()
            .expect("the main module is a file and therefore must reside in a directory"),
    );

    let import_directories =
        gather_import_directories(main_module_directory, &command_line_args, bump_allocator)?;

    let main_module_source =
        bump_allocator.alloc_str(&std::fs::read_to_string(main_module_canonical_path).unwrap());

    let main_module = parse_module(
        main_module_canonical_path,
        main_module_source,
        bump_allocator,
    )?;

    // get imports of main module
    let main_module_imports = find_imports(
        main_module_directory,
        import_directories,
        &main_module,
        bump_allocator,
    )?;
    let main_module = ModuleWithImports {
        canonical_path: main_module_canonical_path,
        module: main_module,
        imports: main_module_imports,
    };

    let all_modules = connect_modules(main_module, import_directories, bump_allocator)?;
    let all_modules = resolve_imports(all_modules, bump_allocator)?;

    if let Some(ast_output_path) = command_line_args.ast_output_path {
        let ast = format!("{:#?}", &all_modules);
        std::fs::write(ast_output_path, ast).unwrap();
    }

    let all_modules = all_modules
        .iter()
        .map(|module| categorize_names(module, bump_allocator))
        .collect::<Result<Vec<_>, _>>()?;

    let all_modules = all_modules
        .iter()
        .map(|module| partially_resolve_type_definitions(module, bump_allocator))
        .collect::<Result<Vec<_>, _>>()?;

    let all_modules = &*bump_allocator.alloc_slice_clone(&all_modules);

    let program_with_resolved_types =
        completely_resolve_type_definitions(all_modules, bump_allocator);

    for module in &program_with_resolved_types.modules {
        println!("module '{}'", module.canonical_path.display());
        println!("    non-type definitions in global scope:");
        for name in module.global_scope.non_type_definitions.keys() {
            println!("        {}", name);
        }
        for definition in module.non_type_definitions {
            match definition {
                CompletelyResolvedNonTypeDefinition::GlobalVariable(definition) => {
                    println!(
                        "    let {} {}: {}",
                        definition.mutability,
                        definition.name.0.lexeme(),
                        definition
                            .type_
                            .to_string(program_with_resolved_types.type_table)
                    );
                }
                CompletelyResolvedNonTypeDefinition::Function(overload_set) => {
                    for overload in *overload_set {
                        println!(
                            "    function {}({}){}",
                            overload.name.0.lexeme(),
                            overload
                                .parameters
                                .iter()
                                .map(|parameter| format!(
                                    "{}: {}",
                                    parameter.name.0.lexeme(),
                                    parameter
                                        .type_
                                        .to_string(program_with_resolved_types.type_table)
                                ))
                                .intersperse(", ".to_string())
                                .collect::<String>(),
                            match overload.return_type {
                                None => "".to_string(),
                                Some(type_) => format!(
                                    " ~> {}",
                                    type_.to_string(program_with_resolved_types.type_table)
                                ),
                            }
                        );

                        for statement in overload.body.statements {
                            match statement {
                                Statement::ExpressionStatement(expression) => {
                                    match expression {
                                        Expression::Literal(_) => {}
                                        Expression::BinaryOperator { .. } => {}
                                        Expression::Block(_) => {}
                                        Expression::Name(name) => {
                                            let name = name.tokens().source_location().lexeme();
                                            if module
                                                .global_scope
                                                .non_type_definitions
                                                .contains_key(name)
                                            {
                                                println!("        : name '{name}' found in scope");
                                            } else {
                                                println!("        : name '{name}' __NOT__ found in scope");
                                            }
                                        }
                                    }
                                }
                                Statement::Yield(_) => {}
                                Statement::Return(_) => {}
                                Statement::VariableDefinition(_) => {}
                            }
                        }
                    }
                }
            }
        }
    }

    /*for module in &program_with_resolved_types.modules {
        println!(
            "type definitions available in module '{}'",
            module.canonical_path.display()
        );
        for (name, definition) in &module.global_scope.type_definitions {
            println!(
                "\t{} -> struct {}",
                name,
                match definition {
                    CompletelyResolvedTypeDefinition::Struct(definition) =>
                        definition.name.0.lexeme(),
                }
            );
        }

        println!(
            "non-type definitions available in module '{}'",
            module.canonical_path.display()
        );
        for (name, definition) in &module.global_scope.non_type_definitions {
            println!(
                "\t{} -> {}",
                name,
                definition.to_string(program_with_resolved_types.type_table)
            );
        }

        for definition in module.definitions {
            match definition {
                Definition::Struct(_) => {}
                Definition::Function(definition) => {
                    println!("\tfunction {}", definition.name.0.lexeme());
                }
                Definition::GlobalVariable(definition) => {
                    println!("\t{}", definition);
                }
            }
        }
    }*/

    Ok(())
}
