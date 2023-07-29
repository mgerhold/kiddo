use std::fmt::{Debug, Formatter};
use std::path::Path;

use crate::parser::ir_parsed::{Definition, Import, Module};

pub(crate) type ModuleImports<'a> = &'a [(Import<'a>, &'a Path)];
pub(crate) type ModulesWithImports<'a> = &'a [ModuleWithImports<'a>];
pub(crate) type ModulesWithResolvedImportsAndExports<'a> =
    &'a [ModuleWithResolvedImportsAndExports<'a>];
pub(crate) type ModulesWithImportsAndExports<'a> = &'a [ModuleWithImportsAndExports<'a>];
pub type ModulesWithConnectedImports<'a> = &'a [ModuleWithConnectedImports<'a>];

#[derive(Debug, Clone, Copy)]
pub struct ModuleWithImports<'a> {
    pub(crate) canonical_path: &'a Path,
    pub(crate) module: Module<'a>,
    pub(crate) imports: ModuleImports<'a>,
}

#[derive(Debug, Clone, Copy)]
pub struct ModuleWithImportsAndExports<'a> {
    pub(crate) canonical_path: &'a Path,
    pub(crate) module: Module<'a>,
    pub(crate) imports: ModuleImports<'a>,
    pub(crate) exported_definitions: &'a [Definition<'a>],
}

#[derive(Debug, Clone, Copy)]
pub struct ResolvedImport<'a> {
    pub(crate) import: Import<'a>,
    pub(crate) from_module: ModuleWithImportsAndExports<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct ModuleWithResolvedImportsAndExports<'a> {
    pub(crate) canonical_path: &'a Path,
    pub(crate) module: Module<'a>,
    pub(crate) imports: &'a [ResolvedImport<'a>],
    pub(crate) exported_definitions: &'a [Definition<'a>],
}

#[derive(Debug, Clone, Copy)]
pub struct ConnectedImport<'a> {
    pub(crate) import: Import<'a>,
    pub(crate) definition: Definition<'a>,
}

#[derive(Clone, Copy)]
pub struct ModuleWithConnectedImports<'a> {
    pub(crate) canonical_path: &'a Path,
    pub(crate) module: Module<'a>,
    pub(crate) imports: &'a [ConnectedImport<'a>],
}

impl Debug for ModuleWithConnectedImports<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ModuleWithConnectedImports")
            .field(
                "filename",
                &self.canonical_path.file_name().unwrap().to_string_lossy(),
            )
            .field("module", &self.module)
            .field("imports", &self.imports)
            .finish()
    }
}
