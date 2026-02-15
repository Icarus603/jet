//! Symbol table for name resolution
//!
//! The symbol table manages hierarchical scopes and bindings for names
//! in the program. It supports nested scopes (modules, functions, blocks, loops)
//! and tracks visibility and mutability of bindings.

use crate::def_id::{DefId, ModuleId};
use indexmap::IndexMap;
use jet_lexer::Span;
use jet_parser::ast::Ident;
use std::fmt;

/// The kind of a binding
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BindingKind {
    /// A variable binding (let, mut)
    Variable,
    /// A function definition
    Function,
    /// A type definition (struct, enum, type alias)
    Type,
    /// A trait definition
    Trait,
    /// A module
    Module,
    /// An effect definition
    Effect,
    /// A constant
    Const,
    /// A static variable
    Static,
    /// A generic parameter
    Generic,
}

impl fmt::Display for BindingKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BindingKind::Variable => write!(f, "variable"),
            BindingKind::Function => write!(f, "function"),
            BindingKind::Type => write!(f, "type"),
            BindingKind::Trait => write!(f, "trait"),
            BindingKind::Module => write!(f, "module"),
            BindingKind::Effect => write!(f, "effect"),
            BindingKind::Const => write!(f, "constant"),
            BindingKind::Static => write!(f, "static"),
            BindingKind::Generic => write!(f, "generic parameter"),
        }
    }
}

/// A binding in the symbol table
#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    /// The name of the binding
    pub name: String,
    /// The span where the binding was defined
    pub span: Span,
    /// The kind of binding
    pub kind: BindingKind,
    /// The unique DefId for this binding
    pub def_id: DefId,
    /// Whether the binding is mutable
    pub is_mutable: bool,
    /// Whether the binding is public
    pub is_public: bool,
    /// The module where this binding is defined
    pub module_id: ModuleId,
}

impl Binding {
    /// Creates a new binding
    pub fn new(
        name: impl Into<String>,
        span: Span,
        kind: BindingKind,
        def_id: DefId,
        module_id: ModuleId,
    ) -> Self {
        Self {
            name: name.into(),
            span,
            kind,
            def_id,
            is_mutable: false,
            is_public: false,
            module_id,
        }
    }

    /// Sets the mutability of the binding
    pub fn with_mutable(mut self, mutable: bool) -> Self {
        self.is_mutable = mutable;
        self
    }

    /// Sets the visibility of the binding
    pub fn with_public(mut self, public: bool) -> Self {
        self.is_public = public;
        self
    }

    /// Creates a binding from an identifier
    pub fn from_ident(
        ident: &Ident,
        kind: BindingKind,
        def_id: DefId,
        module_id: ModuleId,
    ) -> Self {
        Self::new(ident.name.clone(), ident.span, kind, def_id, module_id)
    }
}

/// The kind of scope
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeKind {
    /// Module-level scope
    Module,
    /// Function-level scope
    Function,
    /// Block-level scope (if, match arms, etc.)
    Block,
    /// Loop scope (while, for)
    Loop,
    /// Implementation scope (impl block)
    Impl,
    /// Trait implementation scope
    TraitImpl,
}

impl fmt::Display for ScopeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ScopeKind::Module => write!(f, "module"),
            ScopeKind::Function => write!(f, "function"),
            ScopeKind::Block => write!(f, "block"),
            ScopeKind::Loop => write!(f, "loop"),
            ScopeKind::Impl => write!(f, "impl"),
            ScopeKind::TraitImpl => write!(f, "trait impl"),
        }
    }
}

/// A scope in the symbol table
#[derive(Debug, Clone)]
pub struct Scope {
    /// The kind of scope
    pub kind: ScopeKind,
    /// The bindings in this scope, keyed by name
    bindings: IndexMap<String, Binding>,
    /// The DefId of the item this scope belongs to (if any)
    pub owner: Option<DefId>,
    /// The module this scope belongs to
    pub module_id: ModuleId,
}

impl Scope {
    /// Creates a new scope
    pub fn new(kind: ScopeKind, module_id: ModuleId) -> Self {
        Self {
            kind,
            bindings: IndexMap::new(),
            owner: None,
            module_id,
        }
    }

    /// Creates a new scope with an owner
    pub fn with_owner(kind: ScopeKind, module_id: ModuleId, owner: DefId) -> Self {
        Self {
            kind,
            bindings: IndexMap::new(),
            owner: Some(owner),
            module_id,
        }
    }

    /// Inserts a binding into this scope
    ///
    /// Returns the previous binding with the same name, if any
    pub fn insert(&mut self, binding: Binding) -> Option<Binding> {
        self.bindings.insert(binding.name.clone(), binding)
    }

    /// Looks up a binding by name in this scope only
    pub fn get(&self, name: &str) -> Option<&Binding> {
        self.bindings.get(name)
    }

    /// Checks if a name is defined in this scope
    pub fn contains(&self, name: &str) -> bool {
        self.bindings.contains_key(name)
    }

    /// Returns all bindings in this scope
    pub fn bindings(&self) -> impl Iterator<Item = &Binding> {
        self.bindings.values()
    }

    /// Returns the number of bindings in this scope
    pub fn len(&self) -> usize {
        self.bindings.len()
    }

    /// Returns true if this scope has no bindings
    pub fn is_empty(&self) -> bool {
        self.bindings.is_empty()
    }
}

/// A module scope containing all top-level definitions
#[derive(Debug, Clone)]
pub struct ModuleScope {
    /// The module ID
    pub id: ModuleId,
    /// The name of the module
    pub name: String,
    /// The parent module (if any)
    pub parent: Option<ModuleId>,
    /// The bindings defined at module level
    bindings: IndexMap<String, Binding>,
    /// Child modules
    children: IndexMap<String, ModuleId>,
    /// Imports in this module
    imports: Vec<Import>,
}

/// An import in a module
#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    /// The path being imported
    pub path: Vec<String>,
    /// The name imported (None for glob imports)
    pub name: Option<String>,
    /// The alias (if any)
    pub alias: Option<String>,
    /// Whether this is a glob import
    pub is_glob: bool,
    /// The span of the import
    pub span: Span,
    /// The DefId of the imported item (set after resolution)
    pub resolved_def: Option<DefId>,
}

impl ModuleScope {
    /// Creates a new module scope
    pub fn new(id: ModuleId, name: impl Into<String>) -> Self {
        Self {
            id,
            name: name.into(),
            parent: None,
            bindings: IndexMap::new(),
            children: IndexMap::new(),
            imports: Vec::new(),
        }
    }

    /// Creates a new submodule scope
    pub fn with_parent(id: ModuleId, name: impl Into<String>, parent: ModuleId) -> Self {
        Self {
            id,
            name: name.into(),
            parent: Some(parent),
            bindings: IndexMap::new(),
            children: IndexMap::new(),
            imports: Vec::new(),
        }
    }

    /// Inserts a binding into this module
    ///
    /// Returns the previous binding with the same name, if any
    pub fn insert(&mut self, binding: Binding) -> Option<Binding> {
        self.bindings.insert(binding.name.clone(), binding)
    }

    /// Looks up a binding by name in this module
    pub fn get(&self, name: &str) -> Option<&Binding> {
        self.bindings.get(name)
    }

    /// Adds a child module
    pub fn add_child(&mut self, name: impl Into<String>, id: ModuleId) {
        self.children.insert(name.into(), id);
    }

    /// Gets a child module by name
    pub fn get_child(&self, name: &str) -> Option<ModuleId> {
        self.children.get(name).copied()
    }

    /// Adds an import
    pub fn add_import(&mut self, import: Import) {
        self.imports.push(import);
    }

    /// Returns all imports in this module
    pub fn imports(&self) -> &[Import] {
        &self.imports
    }

    /// Returns all bindings in this module
    pub fn bindings(&self) -> impl Iterator<Item = &Binding> {
        self.bindings.values()
    }

    /// Returns all child modules
    pub fn children(&self) -> impl Iterator<Item = (&String, &ModuleId)> {
        self.children.iter()
    }
}

/// The symbol table managing all scopes and bindings
#[derive(Debug)]
pub struct SymbolTable {
    /// Stack of scopes (innermost last)
    scopes: Vec<Scope>,
    /// All module scopes
    pub(crate) modules: IndexMap<ModuleId, ModuleScope>,
    /// The current module
    current_module: ModuleId,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new(ModuleId::root())
    }
}

impl SymbolTable {
    /// Creates a new symbol table with a root module
    pub fn new(root_module: ModuleId) -> Self {
        let mut modules = IndexMap::new();
        modules.insert(root_module, ModuleScope::new(root_module, "root"));

        Self {
            scopes: Vec::new(),
            modules,
            current_module: root_module,
        }
    }

    /// Creates a new module and returns its ID
    pub fn create_module(&mut self, name: impl Into<String>, parent: Option<ModuleId>) -> ModuleId {
        let id = ModuleId::from_raw(self.modules.len() as u32);
        let module = match parent {
            Some(p) => ModuleScope::with_parent(id, name, p),
            None => ModuleScope::new(id, name),
        };
        self.modules.insert(id, module);
        id
    }

    /// Gets a module by ID
    pub fn get_module(&self, id: ModuleId) -> Option<&ModuleScope> {
        self.modules.get(&id)
    }

    /// Gets a module by ID (mutable)
    pub fn get_module_mut(&mut self, id: ModuleId) -> Option<&mut ModuleScope> {
        self.modules.get_mut(&id)
    }

    /// Gets the current module
    pub fn current_module(&self) -> &ModuleScope {
        self.modules.get(&self.current_module).unwrap()
    }

    /// Gets the current module (mutable)
    pub fn current_module_mut(&mut self) -> &mut ModuleScope {
        self.modules.get_mut(&self.current_module).unwrap()
    }

    /// Sets the current module
    pub fn set_current_module(&mut self, module_id: ModuleId) {
        self.current_module = module_id;
    }

    /// Pushes a new scope onto the stack
    pub fn push_scope(&mut self, kind: ScopeKind) {
        let scope = Scope::new(kind, self.current_module);
        self.scopes.push(scope);
    }

    /// Pushes a new scope with an owner
    pub fn push_scope_with_owner(&mut self, kind: ScopeKind, owner: DefId) {
        let scope = Scope::with_owner(kind, self.current_module, owner);
        self.scopes.push(scope);
    }

    /// Pops the current scope from the stack
    ///
    /// Returns the popped scope, or None if the stack is empty
    pub fn pop_scope(&mut self) -> Option<Scope> {
        self.scopes.pop()
    }

    /// Returns the current scope (innermost)
    pub fn current_scope(&self) -> Option<&Scope> {
        self.scopes.last()
    }

    /// Returns the current scope (mutable)
    pub fn current_scope_mut(&mut self) -> Option<&mut Scope> {
        self.scopes.last_mut()
    }

    /// Returns the number of scopes on the stack
    pub fn scope_depth(&self) -> usize {
        self.scopes.len()
    }

    /// Inserts a binding into the current scope
    ///
    /// Returns the previous binding with the same name in the current scope, if any
    pub fn insert(&mut self, binding: Binding) -> Option<Binding> {
        if let Some(scope) = self.current_scope_mut() {
            scope.insert(binding)
        } else {
            // No active scope, insert into current module
            self.current_module_mut().insert(binding)
        }
    }

    /// Inserts a binding into the current module
    pub fn insert_module_binding(&mut self, binding: Binding) -> Option<Binding> {
        self.current_module_mut().insert(binding)
    }

    /// Looks up a name in all scopes (innermost first)
    pub fn lookup(&self, name: &str) -> Option<&Binding> {
        // First, check all scopes from innermost to outermost
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.get(name) {
                return Some(binding);
            }
        }

        // Then, check the current module
        if let Some(binding) = self.current_module().get(name) {
            return Some(binding);
        }

        // Finally, check imports in the current module
        self.lookup_import(name)
    }

    /// Looks up a name in the current scope only
    pub fn lookup_current(&self, name: &str) -> Option<&Binding> {
        self.current_scope().and_then(|s| s.get(name))
    }

    /// Looks up a name in module-level scope
    pub fn lookup_module(&self, name: &str) -> Option<&Binding> {
        self.current_module().get(name)
    }

    /// Looks up an imported name
    fn lookup_import(&self, name: &str) -> Option<&Binding> {
        // Find an import that matches the name
        let import = self.current_module().imports().iter().find(|imp| {
            if imp.is_glob {
                // For glob imports, we'd need to resolve the module
                // and check if it contains the name
                // For now, return false (will be handled in resolve phase)
                false
            } else {
                imp.alias
                    .as_ref()
                    .map(|a| a == name)
                    .unwrap_or_else(|| imp.name.as_ref().map(|n| n == name).unwrap_or(false))
            }
        })?;

        // Return the resolved DefId if available
        import.resolved_def.map(|def_id| {
            // This is a bit of a hack - we'd need to look up the actual binding
            // For now, create a temporary binding
            // In practice, this should look up the binding in the target module
            self.find_binding_by_def_id(def_id)
        })?
    }

    /// Finds a binding by its DefId
    fn find_binding_by_def_id(&self, def_id: DefId) -> Option<&Binding> {
        // Search all modules and scopes for the binding
        for module in self.modules.values() {
            for binding in module.bindings() {
                if binding.def_id == def_id {
                    return Some(binding);
                }
            }
        }
        None
    }

    /// Checks if a name is defined in the current scope
    pub fn is_defined_in_current(&self, name: &str) -> bool {
        self.current_scope()
            .map(|s| s.contains(name))
            .unwrap_or(false)
    }

    /// Checks if a name is defined at module level
    pub fn is_defined_in_module(&self, name: &str) -> bool {
        self.current_module().get(name).is_some()
    }

    /// Returns all visible bindings at the current position
    pub fn visible_bindings(&self) -> impl Iterator<Item = &Binding> {
        let mut seen = std::collections::HashSet::new();
        let mut bindings: Vec<&Binding> = Vec::new();

        // Add from scopes (innermost first)
        for scope in self.scopes.iter().rev() {
            for binding in scope.bindings() {
                if seen.insert(binding.def_id) {
                    bindings.push(binding);
                }
            }
        }

        // Add from current module
        for binding in self.current_module().bindings() {
            if seen.insert(binding.def_id) {
                bindings.push(binding);
            }
        }

        bindings.into_iter()
    }

    /// Clears all scopes (but keeps modules)
    pub fn clear_scopes(&mut self) {
        self.scopes.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::def_id::DefIdGenerator;

    fn make_span() -> Span {
        Span::new(0, 0)
    }

    #[test]
    fn test_binding_creation() {
        let binding = Binding::new(
            "x",
            make_span(),
            BindingKind::Variable,
            DefId::from_raw(1),
            ModuleId::root(),
        );
        assert_eq!(binding.name, "x");
        assert_eq!(binding.kind, BindingKind::Variable);
        assert!(!binding.is_mutable);
        assert!(!binding.is_public);
    }

    #[test]
    fn test_binding_builder() {
        let binding = Binding::new(
            "x",
            make_span(),
            BindingKind::Variable,
            DefId::from_raw(1),
            ModuleId::root(),
        )
        .with_mutable(true)
        .with_public(true);
        assert!(binding.is_mutable);
        assert!(binding.is_public);
    }

    #[test]
    fn test_scope_operations() {
        let mut scope = Scope::new(ScopeKind::Block, ModuleId::root());
        let binding = Binding::new(
            "x",
            make_span(),
            BindingKind::Variable,
            DefId::from_raw(1),
            ModuleId::root(),
        );

        assert!(scope.is_empty());
        assert_eq!(scope.insert(binding.clone()), None);
        assert_eq!(scope.len(), 1);
        assert!(scope.contains("x"));
        assert_eq!(scope.get("x"), Some(&binding));
    }

    #[test]
    fn test_scope_duplicate() {
        let mut scope = Scope::new(ScopeKind::Block, ModuleId::root());
        let binding1 = Binding::new(
            "x",
            make_span(),
            BindingKind::Variable,
            DefId::from_raw(1),
            ModuleId::root(),
        );
        let binding2 = Binding::new(
            "x",
            make_span(),
            BindingKind::Variable,
            DefId::from_raw(2),
            ModuleId::root(),
        );

        scope.insert(binding1.clone());
        let old = scope.insert(binding2.clone());

        assert_eq!(old, Some(binding1));
        assert_eq!(scope.get("x").map(|b| b.def_id), Some(DefId::from_raw(2)));
    }

    #[test]
    fn test_symbol_table_scopes() {
        let mut table = SymbolTable::new(ModuleId::root());
        let gen = DefIdGenerator::new();

        // Push a scope and add a binding
        table.push_scope(ScopeKind::Block);
        let binding = Binding::new(
            "x",
            make_span(),
            BindingKind::Variable,
            gen.next(),
            ModuleId::root(),
        );
        table.insert(binding);

        assert_eq!(table.scope_depth(), 1);
        assert!(table.lookup("x").is_some());

        // Push another scope and shadow x
        table.push_scope(ScopeKind::Block);
        let binding2 = Binding::new(
            "x",
            make_span(),
            BindingKind::Variable,
            gen.next(),
            ModuleId::root(),
        );
        table.insert(binding2);

        // Should find the inner binding
        let found = table.lookup("x").unwrap();
        assert_eq!(found.def_id.as_raw(), 2);

        // Pop the inner scope
        table.pop_scope();

        // Should find the outer binding
        let found = table.lookup("x").unwrap();
        assert_eq!(found.def_id.as_raw(), 1);
    }

    #[test]
    fn test_symbol_table_module() {
        let mut table = SymbolTable::new(ModuleId::root());
        let gen = DefIdGenerator::new();

        // Add a binding to the module
        let binding = Binding::new(
            "foo",
            make_span(),
            BindingKind::Function,
            gen.next(),
            ModuleId::root(),
        )
        .with_public(true);
        table.insert_module_binding(binding);

        assert!(table.lookup_module("foo").is_some());
        assert!(table.lookup("foo").is_some());
    }

    #[test]
    fn test_module_scope() {
        let mut module = ModuleScope::new(ModuleId::root(), "test");
        let gen = DefIdGenerator::new();

        let binding = Binding::new(
            "x",
            make_span(),
            BindingKind::Variable,
            gen.next(),
            ModuleId::root(),
        );
        module.insert(binding);

        assert!(module.get("x").is_some());
        assert!(module.get("y").is_none());
    }

    #[test]
    fn test_child_modules() {
        let mut parent = ModuleScope::new(ModuleId::root(), "parent");
        let child_id = ModuleId::from_raw(1);

        parent.add_child("child", child_id);
        assert_eq!(parent.get_child("child"), Some(child_id));
        assert_eq!(parent.get_child("other"), None);
    }
}
