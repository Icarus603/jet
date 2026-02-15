//! Import resolution for the Jet language
//!
//! This module handles resolving import statements, including:
//! - Simple imports: `import std::collections::Vec`
//! - From imports: `from std::collections import Vec, Map`
//! - Glob imports: `from std::collections import *`
//! - Aliased imports: `import std::collections::Vec as MyVec`

use crate::def_id::{DefId, ModuleId};
use crate::error::to_diag_span;
use crate::symbol::{Binding, BindingKind, Import};
use crate::Resolver;
use jet_diagnostics::{Diagnostic, ErrorCode, Label};
use jet_lexer::Span;
use jet_parser::ast::{Ident, Import as AstImport, ImportItem, Path};

/// The result of resolving an import
#[derive(Debug, Clone)]
pub enum ImportResolution {
    /// Successfully resolved to a single item
    Single(DefId),
    /// Successfully resolved to multiple items (glob import)
    Multiple(Vec<(String, DefId)>),
    /// Failed to resolve
    Failed(Vec<Diagnostic>),
}

/// Resolves import statements in a module
pub struct ImportResolver<'a> {
    resolver: &'a mut Resolver,
}

impl<'a> ImportResolver<'a> {
    /// Creates a new import resolver
    pub fn new(resolver: &'a mut Resolver) -> Self {
        Self { resolver }
    }

    /// Resolves all imports in the current module
    pub fn resolve_imports(&mut self, imports: &[AstImport]) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        for import in imports {
            if let Err(diag) = self.resolve_import(import) {
                diagnostics.push(*diag);
            }
        }

        diagnostics
    }

    /// Resolves a single import statement
    pub fn resolve_import(&mut self, import: &AstImport) -> Result<(), Box<Diagnostic>> {
        match import {
            AstImport::Simple { path, alias } => self.resolve_simple_import(path, alias.as_ref()),
            AstImport::From { path, items } => self.resolve_from_import(path, items),
        }
    }

    /// Resolves a simple import: `import path` or `import path as alias`
    fn resolve_simple_import(
        &mut self,
        path: &Path,
        alias: Option<&Ident>,
    ) -> Result<(), Box<Diagnostic>> {
        // Get the module path and the final name
        let segments: Vec<_> = path.segments.iter().map(|s| s.name.clone()).collect();

        if segments.is_empty() {
            return Err(Box::new(self.empty_import_error(path.span)));
        }

        // The last segment is the item name
        let item_name = segments.last().unwrap().clone();
        let module_path = &segments[..segments.len().saturating_sub(1)];

        // Resolve the path to find the target module
        let target_module = self.resolve_module_path(module_path, path.span)?;

        // Look up the item in the target module
        let binding = self.lookup_in_module(target_module, &item_name, path.span)?;

        // Check visibility
        if !binding.is_public && target_module != self.resolver.current_module {
            return Err(Box::new(self.private_item_error(
                &item_name,
                path.span,
                binding.span,
            )));
        }

        // Create the import entry
        let import_name = alias.map(|a| a.name.clone()).unwrap_or(item_name);
        let import = Import {
            path: segments.clone(),
            name: Some(segments.last().unwrap().clone()),
            alias: if alias.is_some() {
                Some(import_name.clone())
            } else {
                None
            },
            is_glob: false,
            span: path.span,
            resolved_def: Some(binding.def_id),
        };

        // Add to module's imports
        self.resolver
            .symbol_table
            .current_module_mut()
            .add_import(import);

        // Add a binding for the imported name
        let imported_binding = Binding::new(
            import_name,
            path.span,
            binding.kind,
            binding.def_id,
            self.resolver.current_module,
        )
        .with_public(binding.is_public);

        self.resolver
            .symbol_table
            .insert_module_binding(imported_binding);

        Ok(())
    }

    /// Resolves a from import: `from path import items`
    fn resolve_from_import(
        &mut self,
        path: &Path,
        items: &[ImportItem],
    ) -> Result<(), Box<Diagnostic>> {
        let segments: Vec<_> = path.segments.iter().map(|s| s.name.clone()).collect();

        // Resolve the module path
        let target_module = self.resolve_module_path(&segments, path.span)?;

        for item in items {
            self.resolve_import_item(target_module, path.span, item)?;
        }

        Ok(())
    }

    /// Resolves a single import item
    fn resolve_import_item(
        &mut self,
        target_module: ModuleId,
        module_span: Span,
        item: &ImportItem,
    ) -> Result<(), Box<Diagnostic>> {
        match item {
            ImportItem::Single { name, alias } => {
                self.resolve_single_import_item(target_module, module_span, name, alias.as_ref())
            }
            ImportItem::Group(items) => {
                for sub_item in items {
                    self.resolve_import_item(target_module, module_span, sub_item)?;
                }
                Ok(())
            }
        }
    }

    /// Resolves a single import item (non-group)
    fn resolve_single_import_item(
        &mut self,
        target_module: ModuleId,
        _module_span: Span,
        name: &Ident,
        alias: Option<&Ident>,
    ) -> Result<(), Box<Diagnostic>> {
        // Look up the item in the target module
        let binding = self.lookup_in_module(target_module, &name.name, name.span)?;

        // Check visibility
        if !binding.is_public && target_module != self.resolver.current_module {
            return Err(Box::new(self.private_item_error(
                &name.name,
                name.span,
                binding.span,
            )));
        }

        // Create the import entry
        let import_name = alias
            .map(|a| a.name.clone())
            .unwrap_or_else(|| name.name.clone());
        let import = Import {
            path: vec![name.name.clone()],
            name: Some(name.name.clone()),
            alias: alias.map(|a| a.name.clone()),
            is_glob: false,
            span: name.span,
            resolved_def: Some(binding.def_id),
        };

        // Add to module's imports
        self.resolver
            .symbol_table
            .current_module_mut()
            .add_import(import);

        // Add a binding for the imported name
        let imported_binding = Binding::new(
            import_name,
            name.span,
            binding.kind,
            binding.def_id,
            self.resolver.current_module,
        )
        .with_public(binding.is_public);

        self.resolver
            .symbol_table
            .insert_module_binding(imported_binding);

        Ok(())
    }

    /// Resolves a module path to a ModuleId
    fn resolve_module_path(
        &mut self,
        path: &[String],
        span: Span,
    ) -> Result<ModuleId, Box<Diagnostic>> {
        // Start from the current module and traverse up to find the root
        let mut current = self.resolver.current_module;

        // Handle absolute paths (starting with crate root)
        // For now, we treat all paths as relative to the current module

        // If the path is empty, return the current module
        if path.is_empty() {
            return Ok(current);
        }

        // Try to resolve each segment
        for (i, segment) in path.iter().enumerate() {
            // First, check if this is a submodule
            if let Some(module) = self.resolver.symbol_table.get_module(current) {
                if let Some(child_id) = module.get_child(segment) {
                    current = child_id;
                    continue;
                }

                // Check if it's a module-level binding that is a module
                if let Some(binding) = module.get(segment) {
                    if binding.kind == BindingKind::Module {
                        // Find the module with this DefId
                        if let Some(module_id) = self.find_module_by_def_id(binding.def_id) {
                            current = module_id;
                            continue;
                        }
                    }
                }
            }

            // If this is the last segment, it might be an item name, not a module
            if i == path.len() - 1 {
                // Return the current module - the item will be looked up there
                return Ok(current);
            }

            // Module not found
            return Err(Box::new(self.module_not_found_error(segment, span)));
        }

        Ok(current)
    }

    /// Looks up a name in a specific module
    fn lookup_in_module(
        &self,
        module_id: ModuleId,
        name: &str,
        span: Span,
    ) -> Result<Binding, Box<Diagnostic>> {
        let module = self
            .resolver
            .symbol_table
            .get_module(module_id)
            .ok_or_else(|| Box::new(self.module_not_found_error("<unknown>", span)))?;

        module
            .get(name)
            .cloned()
            .ok_or_else(|| Box::new(self.unresolved_import_error(name, span)))
    }

    /// Finds a module by its DefId
    fn find_module_by_def_id(&self, def_id: DefId) -> Option<ModuleId> {
        for (module_id, module) in &self.resolver.symbol_table.modules {
            for binding in module.bindings() {
                if binding.def_id == def_id && binding.kind == BindingKind::Module {
                    return Some(*module_id);
                }
            }
        }
        None
    }

    /// Resolves all glob imports in the current module
    ///
    /// This should be called after all regular imports are resolved
    pub fn resolve_glob_imports(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        // Collect glob imports from the module
        let glob_imports: Vec<_> = self
            .resolver
            .symbol_table
            .current_module()
            .imports()
            .iter()
            .filter(|imp| imp.is_glob)
            .cloned()
            .collect();

        for import in glob_imports {
            if let Err(diag) = self.resolve_glob_import(&import) {
                diagnostics.push(*diag);
            }
        }

        diagnostics
    }

    /// Resolves a single glob import
    fn resolve_glob_import(&mut self, import: &Import) -> Result<(), Box<Diagnostic>> {
        // Resolve the module path
        let target_module = self.resolve_module_path(&import.path, import.span)?;

        // Get all public bindings from the target module
        let bindings: Vec<_> = self
            .resolver
            .symbol_table
            .get_module(target_module)
            .map(|m| {
                m.bindings()
                    .filter(|b| b.is_public)
                    .cloned()
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();

        // Add each public binding to the current module
        for binding in bindings {
            // Skip if already defined
            if self
                .resolver
                .symbol_table
                .is_defined_in_module(&binding.name)
            {
                continue;
            }

            let imported_binding = Binding::new(
                binding.name.clone(),
                import.span,
                binding.kind,
                binding.def_id,
                self.resolver.current_module,
            )
            .with_public(true);

            self.resolver
                .symbol_table
                .insert_module_binding(imported_binding);
        }

        Ok(())
    }

    // Error helpers

    fn empty_import_error(&self, span: Span) -> Diagnostic {
        Diagnostic::error("empty import path", to_diag_span(span))
            .with_error_code(ErrorCode::ModuleNotFound)
            .with_note("import path must have at least one component")
    }

    fn module_not_found_error(&self, name: &str, span: Span) -> Diagnostic {
        Diagnostic::error(format!("module not found: `{}`", name), to_diag_span(span))
            .with_error_code(ErrorCode::ModuleNotFound)
    }

    fn unresolved_import_error(&self, name: &str, span: Span) -> Diagnostic {
        Diagnostic::error(format!("unresolved import: `{}`", name), to_diag_span(span))
            .with_error_code(ErrorCode::UnresolvedName)
    }

    fn private_item_error(&self, name: &str, span: Span, definition_span: Span) -> Diagnostic {
        Diagnostic::error(format!("`{}` is private", name), to_diag_span(span))
            .with_error_code(ErrorCode::PrivateItem)
            .with_label(Label::secondary(
                to_diag_span(definition_span),
                "defined here",
            ))
            .with_note("items are private by default; use `pub` to make them public")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::def_id::DefIdGenerator;
    use crate::resolver::Resolver;
    use jet_parser::ast::Ident;

    fn make_span() -> Span {
        Span::new(0, 0)
    }

    fn make_ident(name: &str) -> Ident {
        Ident::new(name, make_span())
    }

    fn make_path(segments: &[&str]) -> Path {
        Path::new(
            segments.iter().map(|s| make_ident(s)).collect(),
            make_span(),
        )
    }

    #[test]
    fn test_simple_import() {
        let mut resolver = Resolver::new(ModuleId::root());
        let gen = DefIdGenerator::new();

        // Add a binding to the current module
        let binding = Binding::new(
            "foo",
            make_span(),
            BindingKind::Function,
            gen.next(),
            ModuleId::root(),
        )
        .with_public(true);
        resolver.symbol_table.insert_module_binding(binding);

        // Create import resolver
        let mut import_resolver = ImportResolver::new(&mut resolver);

        // Try to import it
        let path = make_path(&["foo"]);
        let import = AstImport::Simple { path, alias: None };

        let result = import_resolver.resolve_import(&import);
        assert!(result.is_ok());
    }

    #[test]
    fn test_import_with_alias() {
        let mut resolver = Resolver::new(ModuleId::root());
        let gen = DefIdGenerator::new();

        // Add a binding to the current module
        let binding = Binding::new(
            "foo",
            make_span(),
            BindingKind::Function,
            gen.next(),
            ModuleId::root(),
        )
        .with_public(true);
        resolver.symbol_table.insert_module_binding(binding);

        // Create import resolver
        let mut import_resolver = ImportResolver::new(&mut resolver);

        // Try to import it with an alias
        let path = make_path(&["foo"]);
        let alias = Some(make_ident("bar"));
        let import = AstImport::Simple { path, alias };

        let result = import_resolver.resolve_import(&import);
        assert!(result.is_ok());

        // Check that the alias was added
        assert!(resolver.symbol_table.lookup_module("bar").is_some());
    }

    #[test]
    fn test_unresolved_import() {
        let mut resolver = Resolver::new(ModuleId::root());

        // Create import resolver without adding any bindings
        let mut import_resolver = ImportResolver::new(&mut resolver);

        // Try to import something that doesn't exist
        let path = make_path(&["nonexistent"]);
        let import = AstImport::Simple { path, alias: None };

        let result = import_resolver.resolve_import(&import);
        assert!(result.is_err());
    }

    #[test]
    fn test_private_import() {
        let mut resolver = Resolver::new(ModuleId::root());
        let gen = DefIdGenerator::new();

        // Add a private binding to the current module
        let binding = Binding::new(
            "private_fn",
            make_span(),
            BindingKind::Function,
            gen.next(),
            ModuleId::root(),
        )
        .with_public(false); // Private!
        resolver.symbol_table.insert_module_binding(binding);

        // Create import resolver
        let mut import_resolver = ImportResolver::new(&mut resolver);

        // Try to import the private item
        let path = make_path(&["private_fn"]);
        let import = AstImport::Simple { path, alias: None };

        let result = import_resolver.resolve_import(&import);
        // This should succeed since we're importing from the same module
        assert!(result.is_ok());
    }
}
