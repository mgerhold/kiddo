use std::fmt::{Debug, Display, Formatter};
use std::path::Path;

use bumpalo::Bump;
use hashbrown::hash_map::DefaultHashBuilder;

use crate::parser::ir_parsed::{
    DataType, Definition, Expression, FunctionDefinition, Import, Module, Mutability,
    NonTypeIdentifier, StructDefinition,
};

pub(crate) type ModuleImports<'a> = &'a [(&'a Import<'a>, &'a Path)];
pub(crate) type ModulesWithImports<'a> = &'a [ModuleWithImports<'a>];

#[derive(Debug, Clone, Copy)]
pub struct ModuleWithImports<'a> {
    pub(crate) canonical_path: &'a Path,
    pub(crate) module: Module<'a>,
    pub(crate) imports: ModuleImports<'a>,
}

pub(crate) type ConnectedModules<'a> = &'a [ConnectedModule<'a>];

#[derive(Clone)]
pub struct ConnectedModule<'a> {
    pub(crate) canonical_path: &'a Path,
    pub(crate) imports: &'a [ConnectedImport<'a>],
    pub(crate) definitions: &'a [&'a Definition<'a>],
}

impl Debug for ConnectedModule<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "ConnectedModule [")?;
        for import in self.imports {
            writeln!(f, "    {import}")?;
        }
        for definition in self.definitions {
            writeln!(f, "    {definition}")?;
        }
        write!(f, "]")
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ConnectedImport<'a> {
    pub(crate) import: &'a Import<'a>,
    pub(crate) target_module: &'a Module<'a>,
    pub(crate) target_module_path: &'a Path,
}

impl Display for ConnectedImport<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.import {
            Import::Import { what, .. } => f.write_fmt(format_args!("import {}", what.tokens())),
            Import::ImportAs { what, as_, .. } => f.write_fmt(format_args!(
                "import {} as {}",
                what.tokens(),
                as_.as_string()
            )),
            Import::FromImport { where_, symbol, .. } => f.write_fmt(format_args!(
                "from {} import {}",
                where_.tokens(),
                symbol.as_string()
            )),
            Import::FromImportAs {
                where_,
                symbol,
                as_,
                ..
            } => f.write_fmt(format_args!(
                "from {} import {} as {}",
                where_.tokens(),
                symbol.as_string(),
                as_.as_string()
            )),
        }?;
        f.write_fmt(format_args!(
            " ({})",
            self.target_module_path
                .file_name()
                .expect("this must name a file")
                .to_string_lossy()
        ))
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ResolvedModule<'a> {
    pub(crate) canonical_path: &'a Path,
    pub(crate) resolved_definitions: &'a [ResolvedDefinition<'a>],
    pub(crate) unresolved_definitions: &'a [&'a Definition<'a>],
}

#[derive(Debug, Clone)]
pub(crate) struct ResolvedDefinition<'a> {
    pub(crate) name: &'a str,
    pub(crate) definition: &'a Definition<'a>,
    pub(crate) origin: Option<&'a Import<'a>>,
}

#[derive(Debug, Clone)]
pub(crate) struct Overload<'a> {
    pub(crate) origin: Option<&'a Import<'a>>,
    pub(crate) definition: &'a FunctionDefinition<'a>,
}

pub(crate) type OverloadSet<'a> = &'a [&'a Overload<'a>];

#[derive(Debug, Clone, Copy)]
pub struct GlobalVariableDefinitionWithOptionalOrigin<'a> {
    pub(crate) origin: Option<&'a Import<'a>>,
    pub(crate) is_exported: bool,
    pub(crate) mutability: Mutability,
    pub(crate) name: NonTypeIdentifier<'a>,
    pub(crate) type_: DataType<'a>,
    pub(crate) initial_value: Expression<'a>,
}

#[derive(Debug, Clone)]
pub(crate) enum NonTypeDefinition<'a> {
    GlobalVariable(&'a GlobalVariableDefinitionWithOptionalOrigin<'a>),
    Function(OverloadSet<'a>),
}

impl<'a> NonTypeDefinition<'a> {
    pub(crate) fn get_first_origin(&self) -> Option<&'a Import<'a>> {
        match self {
            NonTypeDefinition::GlobalVariable(definition) => definition.origin,
            NonTypeDefinition::Function(overload_set) => overload_set
                .iter()
                .filter_map(|overload| overload.origin)
                .next(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum TypeDefinitionKind<'a> {
    Struct(&'a StructDefinition<'a>),
}

#[derive(Debug, Clone)]
pub(crate) struct TypeDefinition<'a> {
    pub(crate) origin: Option<&'a Import<'a>>,
    pub(crate) definition: &'a TypeDefinitionKind<'a>,
}

#[derive(Debug, Clone)]
pub(crate) struct ModuleWithCategorizedNames<'a> {
    pub(crate) canonical_path: &'a Path,
    pub(crate) type_names:
        hashbrown::HashMap<&'a str, &'a TypeDefinition<'a>, DefaultHashBuilder, &'a Bump>,
    pub(crate) non_type_names:
        hashbrown::HashMap<&'a str, &'a NonTypeDefinition<'a>, DefaultHashBuilder, &'a Bump>,
    pub(crate) definitions: &'a [&'a Definition<'a>],
}
