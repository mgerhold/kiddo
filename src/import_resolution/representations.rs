use std::fmt::{Debug, Formatter, Write};
use std::path::Path;

use crate::parser::ir_parsed::{Definition, Import, Module};

pub(crate) type ModuleImports<'a> = &'a [(&'a Import<'a>, &'a Path)];
pub(crate) type ModulesWithImports<'a> = &'a [ModuleWithImports<'a>];

#[derive(Debug, Clone, Copy)]
pub struct ModuleWithImports<'a> {
    pub(crate) canonical_path: &'a Path,
    pub(crate) module: Module<'a>,
    pub(crate) imports: ModuleImports<'a>,
}

pub(crate) type ConnectedModules<'a> = &'a [ConnectedModule<'a>];

#[derive(Debug, Clone, Copy)]
pub(crate) struct ConnectedModule<'a> {
    pub(crate) canonical_path: &'a Path,
    pub(crate) imports: &'a [ConnectedImport<'a>],
    pub(crate) definitions: &'a [Definition<'a>],
}

#[derive(Clone, Copy)]
pub(crate) struct ConnectedImport<'a> {
    pub(crate) import: &'a Import<'a>,
    pub(crate) target_module: &'a Module<'a>,
    pub(crate) target_module_path: &'a Path,
}

impl Debug for ConnectedImport<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.import {
            Import::Import { what } => f.write_fmt(format_args!("import {}", what.tokens())),
            Import::ImportAs { what, as_ } => f.write_fmt(format_args!(
                "import {} as {}",
                what.tokens(),
                as_.as_string()
            )),
            Import::FromImport { where_, symbol } => f.write_fmt(format_args!(
                "from {} import {}",
                where_.tokens(),
                symbol.as_string()
            )),
            Import::FromImportAs {
                where_,
                symbol,
                as_,
            } => f.write_fmt(format_args!(
                "from {} import {} as {}",
                where_.tokens(),
                symbol.as_string(),
                as_.as_string()
            )),
        }?;
        f.write_fmt(format_args!(" ({})", self.target_module_path.display()))
    }
}
