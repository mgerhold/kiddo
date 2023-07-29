use std::collections::HashMap;

use bumpalo::Bump;

use crate::import_resolution::representations::{
    ModuleWithConnectedImports, ModulesWithConnectedImports,
};
use crate::name_lookup::errors::NameLookupError;
use crate::name_lookup::ir_after_name_lookup as target_ir;
use crate::parser::ir_parsed as source_ir;

pub(crate) mod errors;
pub(crate) mod ir_after_name_lookup;

#[derive(Debug, Clone)]
struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    names: HashMap<&'a str, source_ir::Definition<'a>>,
}

impl<'a> Scope<'a> {
    fn lookup(&self, name: &'a str) -> Option<source_ir::Definition<'a>> {
        if let Some(definition) = self.names.get(name) {
            return Some(*definition);
        }
        self.parent.and_then(|parent| parent.lookup(name))
    }
}

fn module_scope<'a>(
    module: &'a ModuleWithConnectedImports<'a>,
    bump_allocator: &'a Bump,
) -> Scope<'a> {
    let mut names: HashMap<&str, source_ir::Definition> = HashMap::new();

    // imports that do not just import only one symbol, but the whole module
    for import in module.resolved_imports {
        let mut qualified_name_prefix = match import.import {
            source_ir::Import::Import { what } => what.as_string(),
            source_ir::Import::ImportAs { as_, .. } => as_.as_string(),
            source_ir::Import::FromImport { .. } | source_ir::Import::FromImportAs { .. } => {
                continue
            }
        };
        qualified_name_prefix.push_str("::");

        for definition in import.from_module.exported_definitions {
            let mut qualified_name = qualified_name_prefix.clone();
            qualified_name.push_str(&definition.identifier().as_string());
            let qualified_name = bump_allocator.alloc_str(&qualified_name);
            let insertion_result = names.insert(qualified_name, *definition);
            assert!(insertion_result.is_none());
        }
    }

    // imports that import a single symbol (already connected)
    for import in module.connected_imports {
        let imported_name = match import.import {
            source_ir::Import::FromImport { symbol, .. } => symbol.token().lexeme(),
            source_ir::Import::FromImportAs { as_, .. } => as_.token().lexeme(),
            _ => unreachable!(),
        };
        let insertion_result = names.insert(imported_name, import.definition);
        assert!(insertion_result.is_none());
    }

    // add all definitions local to the current module
    for definition in module.module.definitions {
        let definition_name = definition.identifier().token().lexeme();
        let insertion_result = names.insert(definition_name, *definition);
        assert!(insertion_result.is_none());
    }

    Scope {
        parent: None,
        names,
    }
}

pub(crate) fn perform_name_lookup<'a>(
    modules: ModulesWithConnectedImports<'a>,
    bump_allocator: &Bump,
) -> Result<target_ir::Program<'a>, NameLookupError<'a>> {
    for module in modules {
        let scope = module_scope(module, bump_allocator);
        dbg!(&scope);
    }
    todo!()
    /*let mut definitions = Vec::new();
    for module in modules {
        for definition in module.module.definitions {
            let fizzly_blupp = match definition {
                target_ir::Definition::Struct(struct_definition) => {
                    let mut members = Vec::new();
                    for member in struct_definition.members {
                        members.push(target_ir::StructMember {
                            name: member.name,
                            type_: (),
                        })
                    }
                    target_ir::Definition::Struct(target_ir::StructDefinition {
                        name: struct_definition.name,
                        members: &[],
                    })
                }
                Definition::Function(function_definition) => {}
                Definition::GlobalVariable(global_variable_definition) => {}
            };
        }
    }
    todo!()*/
}
