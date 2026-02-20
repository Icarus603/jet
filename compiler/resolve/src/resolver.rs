//! The main resolver for name resolution
//!
//! This module contains the `Resolver` struct which orchestrates the name
//! resolution process. It manages the symbol table, tracks the current module,
//! and provides methods for resolving different kinds of names.

use crate::def_id::{DefId, DefIdGenerator, ModuleId, ModuleIdGenerator};
use crate::error::{ErrorHelpers, ResolutionError, ResolutionErrors};
use crate::import::ImportResolver;
use crate::symbol::{Binding, BindingKind, ScopeKind, SymbolTable};
use jet_diagnostics::Diagnostic;
use jet_parser::ast::{
    Block, ConstDef, EffectDef, EnumDef, Expr, Function, Ident, ImplDef, ImplItem, Module,
    ModuleItem, Param, Pattern, StructDef, TraitDef, TraitItem, Type, TypeAlias, VariantBody,
};

/// The result of a resolution pass
#[derive(Debug, Clone)]
pub struct ResolutionResult {
    /// Whether resolution succeeded
    pub success: bool,
    /// Any errors that occurred
    pub errors: ResolutionErrors,
}

impl ResolutionResult {
    /// Creates a successful result
    pub fn ok() -> Self {
        Self {
            success: true,
            errors: ResolutionErrors::new(),
        }
    }

    /// Creates a failed result
    pub fn err(errors: ResolutionErrors) -> Self {
        Self {
            success: errors.is_empty(),
            errors,
        }
    }

    /// Returns true if resolution succeeded
    pub fn is_ok(&self) -> bool {
        self.success && !self.errors.has_errors()
    }

    /// Returns true if resolution failed
    pub fn is_err(&self) -> bool {
        !self.is_ok()
    }

    /// Converts to a Result with diagnostics
    pub fn into_result(self) -> Result<(), Vec<Diagnostic>> {
        if self.is_ok() {
            Ok(())
        } else {
            Err(self.errors.to_diagnostics())
        }
    }

    /// Converts to a Result with both resolution and import diagnostics
    pub fn into_result_with_imports(
        self,
        import_diagnostics: Vec<Diagnostic>,
    ) -> Result<(), Vec<Diagnostic>> {
        if self.is_ok() && import_diagnostics.is_empty() {
            Ok(())
        } else {
            let mut all_diagnostics = self.errors.to_diagnostics();
            all_diagnostics.extend(import_diagnostics);
            Err(all_diagnostics)
        }
    }
}

/// The main resolver struct
#[derive(Debug)]
pub struct Resolver {
    /// The symbol table managing all scopes and bindings
    pub symbol_table: SymbolTable,
    /// Generator for DefIds
    def_id_gen: DefIdGenerator,
    /// Generator for ModuleIds
    module_id_gen: ModuleIdGenerator,
    /// The current module being resolved
    pub current_module: ModuleId,
    /// Collection of errors
    errors: ResolutionErrors,
    /// Diagnostics from import resolution
    pub import_diagnostics: Vec<Diagnostic>,
}

impl Default for Resolver {
    fn default() -> Self {
        Self::new(ModuleId::root())
    }
}

impl Resolver {
    /// Creates a new resolver with a root module
    pub fn new(root_module: ModuleId) -> Self {
        Self {
            symbol_table: SymbolTable::new(root_module),
            def_id_gen: DefIdGenerator::new(),
            module_id_gen: ModuleIdGenerator::new(),
            current_module: root_module,
            errors: ResolutionErrors::new(),
            import_diagnostics: Vec::new(),
        }
    }

    /// Generates a new DefId
    pub fn next_def_id(&self) -> DefId {
        self.def_id_gen.next()
    }

    /// Generates a new ModuleId
    pub fn next_module_id(&self) -> ModuleId {
        self.module_id_gen.next()
    }

    /// Collects module items without resolving imports or references
    ///
    /// This is used in multi-module compilation to collect all top-level
    /// definitions from all modules before resolving cross-module imports.
    pub fn collect_module_items_only(&mut self, module: &Module) -> ResolutionResult {
        self.errors = ResolutionErrors::new();

        // Inject prelude (builtin types and functions) before collecting
        self.inject_prelude();

        // Collect all top-level definitions
        if let Err(err) = self.collect_module_items(module) {
            self.errors.push(err);
        }

        ResolutionResult {
            success: !self.errors.has_errors(),
            errors: self.errors.clone(),
        }
    }

    /// Resolves imports and references for a module
    ///
    /// This assumes that `collect_module_items_only` has already been called
    /// for all modules, so all top-level definitions are available.
    pub fn resolve_module_imports_and_refs(&mut self, module: &Module) -> ResolutionResult {
        self.errors = ResolutionErrors::new();

        // First pass: resolve imports
        let imports: Vec<_> = module
            .items
            .iter()
            .filter_map(|item| {
                if let ModuleItem::Import(import) = item {
                    Some(import.clone())
                } else {
                    None
                }
            })
            .collect();

        if !imports.is_empty() {
            let mut import_resolver = ImportResolver::new(self);
            let import_diagnostics = import_resolver.resolve_imports(&imports);
            // Store import diagnostics as a separate field
            self.import_diagnostics = import_diagnostics;
        }

        // Second pass: resolve all references
        for item in &module.items {
            if let Err(err) = self.resolve_item(item) {
                self.errors.push(err);
            }
        }

        ResolutionResult {
            success: !self.errors.has_errors(),
            errors: self.errors.clone(),
        }
    }

    /// Resolves a module
    ///
    /// This is the main entry point for name resolution.
    /// For single-module compilation, use this method.
    /// For multi-module compilation, use `collect_module_items_only` followed by
    /// `resolve_module_imports_and_refs`.
    pub fn resolve_module(&mut self, module: &Module) -> ResolutionResult {
        self.errors = ResolutionErrors::new();

        // Inject prelude (builtin types and functions) before resolving
        self.inject_prelude();

        // First pass: collect all top-level definitions
        if let Err(err) = self.collect_module_items(module) {
            self.errors.push(err);
        }

        // Second pass: resolve imports
        let imports: Vec<_> = module
            .items
            .iter()
            .filter_map(|item| {
                if let ModuleItem::Import(import) = item {
                    Some(import.clone())
                } else {
                    None
                }
            })
            .collect();

        if !imports.is_empty() {
            let mut import_resolver = ImportResolver::new(self);
            let import_diagnostics = import_resolver.resolve_imports(&imports);
            // Store import diagnostics as a separate field
            self.import_diagnostics = import_diagnostics;
        }

        // Third pass: resolve all references
        for item in &module.items {
            if let Err(err) = self.resolve_item(item) {
                self.errors.push(err);
            }
        }

        ResolutionResult {
            success: !self.errors.has_errors(),
            errors: self.errors.clone(),
        }
    }

    /// Injects prelude bindings (builtin types and functions) into the module scope
    ///
    /// This makes primitive types and builtin functions available without explicit imports.
    fn inject_prelude(&mut self) {
        use jet_lexer::Span;

        let prelude_span = Span::new(0, 0);

        // Inject primitive types
        let primitive_types = [
            "int", "uint", "i8", "i16", "i32", "i64", "isize", "u8", "u16", "u32", "u64", "usize",
            "f32", "f64", "float", "bool", "char", "string", "unit", "never",
        ];

        for ty_name in &primitive_types {
            let def_id = self.next_def_id();
            let binding = Binding::new(
                *ty_name,
                prelude_span,
                BindingKind::Type,
                def_id,
                self.current_module,
            )
            .with_public(true);

            let _ = self.symbol_table.insert_module_binding(binding);
        }

        // Inject builtin functions
        let builtin_functions = [
            "print",
            "println",
            "eprint",
            "eprintln",
            "assert",
            "assert_eq",
            "panic",
            "print_int",
            "print_float",
            "print_bool",
            "len",
            "capacity",
        ];

        for fn_name in &builtin_functions {
            let def_id = self.next_def_id();
            let binding = Binding::new(
                *fn_name,
                prelude_span,
                BindingKind::Function,
                def_id,
                self.current_module,
            )
            .with_public(true);

            let _ = self.symbol_table.insert_module_binding(binding);
        }
    }

    /// Collects all top-level items in a module
    ///
    /// This first pass collects all names defined at module level
    /// so they can be referenced before their definition.
    fn collect_module_items(&mut self, module: &Module) -> Result<(), ResolutionError> {
        for item in &module.items {
            match item {
                ModuleItem::Function(func) => {
                    self.collect_function(func)?;
                }
                ModuleItem::Struct(s) => {
                    self.collect_struct(s)?;
                }
                ModuleItem::Enum(e) => {
                    self.collect_enum(e)?;
                }
                ModuleItem::Trait(t) => {
                    self.collect_trait(t)?;
                }
                ModuleItem::Impl(i) => {
                    // Collect impl methods as module-level bindings
                    // This allows Type::method syntax to work
                    self.collect_impl(i)?;
                }
                ModuleItem::TypeAlias(t) => {
                    self.collect_type_alias(t)?;
                }
                ModuleItem::Const(c) => {
                    self.collect_const(c)?;
                }
                ModuleItem::Import(_) => {
                    // Imports are handled separately
                }
                ModuleItem::Effect(e) => {
                    if let Err(err) = self.collect_effect(e) {
                        self.errors.push(err);
                    }
                }
                ModuleItem::Spec(_) | ModuleItem::Example(_) => {
                    // Spec and Example items don't define new names in the module scope
                }
                ModuleItem::GhostType(_) => {
                    // Ghost types don't define runtime-visible names
                }
            }
        }

        Ok(())
    }

    /// Collects a function definition
    fn collect_function(&mut self, func: &Function) -> Result<(), ResolutionError> {
        let def_id = self.next_def_id();
        let name = func.name.name.clone();

        // Check for duplicates
        if let Some(existing) = self.symbol_table.lookup_module(&name) {
            return Err(ErrorHelpers::duplicate_definition(
                &name,
                func.name.span,
                existing.span,
                BindingKind::Function,
            ));
        }

        let binding = Binding::from_ident(
            &func.name,
            BindingKind::Function,
            def_id,
            self.current_module,
        )
        .with_public(func.public);

        self.symbol_table.insert_module_binding(binding);

        Ok(())
    }

    /// Collects a struct definition
    fn collect_struct(&mut self, s: &StructDef) -> Result<(), ResolutionError> {
        let def_id = self.next_def_id();
        let name = s.name.name.clone();

        // Check for duplicates
        if let Some(existing) = self.symbol_table.lookup_module(&name) {
            return Err(ErrorHelpers::duplicate_definition(
                &name,
                s.name.span,
                existing.span,
                BindingKind::Type,
            ));
        }

        let binding = Binding::from_ident(&s.name, BindingKind::Type, def_id, self.current_module)
            .with_public(s.public);

        self.symbol_table.insert_module_binding(binding);

        Ok(())
    }

    /// Collects an enum definition
    fn collect_enum(&mut self, e: &EnumDef) -> Result<(), ResolutionError> {
        let def_id = self.next_def_id();
        let name = e.name.name.clone();

        // Check for duplicates
        if let Some(existing) = self.symbol_table.lookup_module(&name) {
            return Err(ErrorHelpers::duplicate_definition(
                &name,
                e.name.span,
                existing.span,
                BindingKind::Type,
            ));
        }

        let binding = Binding::from_ident(&e.name, BindingKind::Type, def_id, self.current_module)
            .with_public(e.public);

        self.symbol_table.insert_module_binding(binding);

        // Expose enum variants in module scope for unqualified matching/construction.
        for variant in &e.variants {
            let variant_def = self.next_def_id();
            let variant_binding = Binding::new(
                variant.name.name.clone(),
                variant.name.span,
                BindingKind::Const,
                variant_def,
                self.current_module,
            )
            .with_public(e.public);
            self.symbol_table.insert_module_binding(variant_binding);
        }

        Ok(())
    }

    /// Collects a trait definition
    fn collect_trait(&mut self, t: &TraitDef) -> Result<(), ResolutionError> {
        let def_id = self.next_def_id();
        let name = t.name.name.clone();

        // Check for duplicates
        if let Some(existing) = self.symbol_table.lookup_module(&name) {
            return Err(ErrorHelpers::duplicate_definition(
                &name,
                t.name.span,
                existing.span,
                BindingKind::Trait,
            ));
        }

        let binding = Binding::from_ident(&t.name, BindingKind::Trait, def_id, self.current_module)
            .with_public(t.public);

        self.symbol_table.insert_module_binding(binding);

        Ok(())
    }

    /// Collects a type alias definition
    fn collect_type_alias(&mut self, t: &TypeAlias) -> Result<(), ResolutionError> {
        let def_id = self.next_def_id();
        let name = t.name.name.clone();

        // Check for duplicates
        if let Some(existing) = self.symbol_table.lookup_module(&name) {
            return Err(ErrorHelpers::duplicate_definition(
                &name,
                t.name.span,
                existing.span,
                BindingKind::Type,
            ));
        }

        let binding = Binding::from_ident(&t.name, BindingKind::Type, def_id, self.current_module)
            .with_public(t.public);

        self.symbol_table.insert_module_binding(binding);

        Ok(())
    }

    /// Collects a constant definition
    fn collect_const(&mut self, c: &ConstDef) -> Result<(), ResolutionError> {
        let def_id = self.next_def_id();
        let name = c.name.name.clone();

        // Check for duplicates
        if let Some(existing) = self.symbol_table.lookup_module(&name) {
            return Err(ErrorHelpers::duplicate_definition(
                &name,
                c.name.span,
                existing.span,
                BindingKind::Const,
            ));
        }

        let binding = Binding::from_ident(&c.name, BindingKind::Const, def_id, self.current_module)
            .with_public(c.public);

        self.symbol_table.insert_module_binding(binding);

        Ok(())
    }

    /// Collects an impl block definition
    fn collect_impl(&mut self, i: &ImplDef) -> Result<(), ResolutionError> {
        // Collect methods from the impl block as module-level bindings
        // This allows Type::method syntax to work
        for item in &i.items {
            match item {
                ImplItem::Method(func) => {
                    let def_id = self.next_def_id();
                    let name = func.name.name.clone();

                    // Only insert if no existing binding with this name
                    // (Different types can have methods with the same name)
                    if self.symbol_table.lookup_module(&name).is_none() {
                        let binding = Binding::from_ident(
                            &func.name,
                            BindingKind::Function,
                            def_id,
                            self.current_module,
                        )
                        .with_public(func.public);

                        self.symbol_table.insert_module_binding(binding);
                    }
                }
                ImplItem::Const { name, .. } => {
                    let def_id = self.next_def_id();
                    let const_name = name.name.clone();

                    // Only insert if no existing binding with this name
                    if self.symbol_table.lookup_module(&const_name).is_none() {
                        let binding = Binding::from_ident(
                            name,
                            BindingKind::Const,
                            def_id,
                            self.current_module,
                        )
                        .with_public(true);

                        self.symbol_table.insert_module_binding(binding);
                    }
                }
                ImplItem::TypeAlias(type_alias) => {
                    self.collect_type_alias(type_alias)?;
                }
            }
        }

        Ok(())
    }

    /// Resolves a module item
    fn resolve_item(&mut self, item: &ModuleItem) -> Result<(), ResolutionError> {
        match item {
            ModuleItem::Function(func) => self.resolve_function(func),
            ModuleItem::Struct(s) => self.resolve_struct(s),
            ModuleItem::Enum(e) => self.resolve_enum(e),
            ModuleItem::Trait(t) => self.resolve_trait(t),
            ModuleItem::Impl(i) => self.resolve_impl(i),
            ModuleItem::TypeAlias(t) => self.resolve_type_alias(t),
            ModuleItem::Const(c) => self.resolve_const(c),
            ModuleItem::Import(_) => {
                // Imports are handled separately
                Ok(())
            }
            ModuleItem::Effect(e) => self.resolve_effect_def(e),
            ModuleItem::Spec(_) | ModuleItem::Example(_) => {
                // Spec and Example items don't need resolution
                Ok(())
            }
            ModuleItem::GhostType(_) => {
                // Ghost types are for formal verification and don't need resolution
                Ok(())
            }
        }
    }

    /// Collects an effect definition
    fn collect_effect(&mut self, e: &EffectDef) -> Result<(), ResolutionError> {
        let def_id = self.next_def_id();
        let name = e.name.name.clone();

        // Check for duplicates
        if let Some(existing) = self.symbol_table.lookup_module(&name) {
            return Err(ErrorHelpers::duplicate_definition(
                &name,
                e.name.span,
                existing.span,
                BindingKind::Effect,
            ));
        }

        let binding =
            Binding::from_ident(&e.name, BindingKind::Effect, def_id, self.current_module)
                .with_public(e.public);

        self.symbol_table.insert_module_binding(binding);

        Ok(())
    }

    /// Resolves an effect definition
    fn resolve_effect_def(&mut self, e: &EffectDef) -> Result<(), ResolutionError> {
        // Push a scope for generic parameters
        self.symbol_table.push_scope(ScopeKind::Block);

        // Resolve generic parameters
        for generic in &e.generics {
            let def_id = self.next_def_id();
            let binding = Binding::from_ident(
                &generic.name,
                BindingKind::Generic,
                def_id,
                self.current_module,
            );
            self.symbol_table.insert(binding);
        }

        // Resolve operation parameter types
        for op in &e.operations {
            for param in &op.params {
                self.resolve_type(&param.ty)?;
            }
            if let Some(ret) = &op.return_type {
                self.resolve_type(ret)?;
            }
        }

        self.symbol_table.pop_scope();
        Ok(())
    }

    /// Resolves a function definition
    fn resolve_function(&mut self, func: &Function) -> Result<(), ResolutionError> {
        // Push a function scope
        self.symbol_table.push_scope(ScopeKind::Function);

        // Resolve generic parameters
        for generic in &func.generics {
            let def_id = self.next_def_id();
            let binding = Binding::from_ident(
                &generic.name,
                BindingKind::Generic,
                def_id,
                self.current_module,
            );
            self.symbol_table.insert(binding);
        }

        // Resolve parameters
        for param in &func.params {
            self.resolve_param(param)?;
        }

        // Resolve return type
        if let Some(ret_type) = &func.return_type {
            self.resolve_type(ret_type)?;
        }

        // Resolve effects
        for effect in &func.effects {
            self.resolve_type(effect)?;
        }

        // Resolve where clause
        for bound in &func.where_clause {
            self.resolve_type(&bound.ty)?;
            for b in &bound.bounds {
                self.resolve_type(b)?;
            }
        }

        // Resolve body
        self.resolve_expr(&func.body)?;

        // Pop function scope
        self.symbol_table.pop_scope();

        Ok(())
    }

    /// Resolves a struct definition
    fn resolve_struct(&mut self, s: &StructDef) -> Result<(), ResolutionError> {
        // Push a scope for generic parameters
        self.symbol_table.push_scope(ScopeKind::Block);

        // Resolve generic parameters
        for generic in &s.generics {
            let def_id = self.next_def_id();
            let binding = Binding::from_ident(
                &generic.name,
                BindingKind::Generic,
                def_id,
                self.current_module,
            );
            self.symbol_table.insert(binding);
        }

        // Resolve field types
        for field in &s.fields {
            self.resolve_type(&field.ty)?;
        }

        self.symbol_table.pop_scope();
        Ok(())
    }

    /// Resolves an enum definition
    fn resolve_enum(&mut self, e: &EnumDef) -> Result<(), ResolutionError> {
        // Push a scope for generic parameters
        self.symbol_table.push_scope(ScopeKind::Block);

        // Resolve generic parameters
        for generic in &e.generics {
            let def_id = self.next_def_id();
            let binding = Binding::from_ident(
                &generic.name,
                BindingKind::Generic,
                def_id,
                self.current_module,
            );
            self.symbol_table.insert(binding);
        }

        // Resolve variant types
        for variant in &e.variants {
            match &variant.body {
                VariantBody::Unit => {}
                VariantBody::Tuple(types) => {
                    for ty in types {
                        self.resolve_type(ty)?;
                    }
                }
                VariantBody::Struct(fields) => {
                    for field in fields {
                        self.resolve_type(&field.ty)?;
                    }
                }
                VariantBody::Discriminant(expr) => {
                    self.resolve_expr(expr)?;
                }
            }
        }

        self.symbol_table.pop_scope();
        Ok(())
    }

    /// Resolves a trait definition
    fn resolve_trait(&mut self, t: &TraitDef) -> Result<(), ResolutionError> {
        // Push a scope for generic parameters
        self.symbol_table.push_scope(ScopeKind::Block);

        // Bind `Self` as a type name referring to the trait
        let self_def_id = self.next_def_id();
        let self_binding = Binding::new(
            "Self",
            t.name.span,
            BindingKind::Type,
            self_def_id,
            self.current_module,
        );
        self.symbol_table.insert(self_binding);

        // Resolve generic parameters
        for generic in &t.generics {
            let def_id = self.next_def_id();
            let binding = Binding::from_ident(
                &generic.name,
                BindingKind::Generic,
                def_id,
                self.current_module,
            );
            self.symbol_table.insert(binding);
        }

        // Resolve super traits
        for super_trait in &t.super_traits {
            self.resolve_type(super_trait)?;
        }

        // Resolve trait items
        for item in &t.items {
            match item {
                TraitItem::Method {
                    params,
                    return_type,
                    effects,
                    where_clause,
                    ..
                } => {
                    for param in params {
                        self.resolve_param(param)?;
                    }
                    if let Some(ret) = return_type {
                        self.resolve_type(ret)?;
                    }
                    for effect in effects {
                        self.resolve_type(effect)?;
                    }
                    for bound in where_clause {
                        self.resolve_type(&bound.ty)?;
                        for b in &bound.bounds {
                            self.resolve_type(b)?;
                        }
                    }
                }
                TraitItem::TypeDecl { bounds, .. } => {
                    for bound in bounds {
                        self.resolve_type(bound)?;
                    }
                }
                TraitItem::ConstDecl { ty, .. } => {
                    self.resolve_type(ty)?;
                }
            }
        }

        self.symbol_table.pop_scope();
        Ok(())
    }

    /// Resolves an impl block
    fn resolve_impl(&mut self, i: &ImplDef) -> Result<(), ResolutionError> {
        // Push an impl scope
        self.symbol_table.push_scope(ScopeKind::Impl);

        // Resolve generic parameters
        for generic in &i.generics {
            let def_id = self.next_def_id();
            let binding = Binding::from_ident(
                &generic.name,
                BindingKind::Generic,
                def_id,
                self.current_module,
            );
            self.symbol_table.insert(binding);
        }

        // Resolve trait path if this is a trait impl
        if let Some(trait_path) = &i.trait_path {
            self.resolve_path(trait_path)?;
        }

        // Resolve the type being implemented
        self.resolve_type(&i.ty)?;

        // Bind `Self` as a type name referring to the implemented type
        let self_def_id = self.next_def_id();
        let self_binding = Binding::new(
            "Self",
            i.span,
            BindingKind::Type,
            self_def_id,
            self.current_module,
        );
        self.symbol_table.insert(self_binding);

        // Resolve where clause
        for bound in &i.where_clause {
            self.resolve_type(&bound.ty)?;
            for b in &bound.bounds {
                self.resolve_type(b)?;
            }
        }

        // Resolve impl items
        for item in &i.items {
            match item {
                ImplItem::Method(func) => {
                    self.resolve_function(func)?;
                }
                ImplItem::Const { ty, value, .. } => {
                    self.resolve_type(ty)?;
                    self.resolve_expr(value)?;
                }
                ImplItem::TypeAlias(type_alias) => {
                    self.resolve_type_alias(type_alias)?;
                }
            }
        }

        self.symbol_table.pop_scope();
        Ok(())
    }

    /// Resolves a type alias definition
    fn resolve_type_alias(&mut self, t: &TypeAlias) -> Result<(), ResolutionError> {
        // Push a scope for generic parameters
        self.symbol_table.push_scope(ScopeKind::Block);

        // Resolve generic parameters
        for generic in &t.generics {
            let def_id = self.next_def_id();
            let binding = Binding::from_ident(
                &generic.name,
                BindingKind::Generic,
                def_id,
                self.current_module,
            );
            self.symbol_table.insert(binding);
        }

        // Resolve the aliased type
        self.resolve_type(&t.ty)?;

        self.symbol_table.pop_scope();
        Ok(())
    }

    /// Resolves a constant definition
    fn resolve_const(&mut self, c: &ConstDef) -> Result<(), ResolutionError> {
        // Resolve the type
        self.resolve_type(&c.ty)?;

        // Resolve the value
        self.resolve_expr(&c.value)?;

        Ok(())
    }

    /// Resolves a parameter
    fn resolve_param(&mut self, param: &Param) -> Result<(), ResolutionError> {
        // Resolve the type
        self.resolve_type(&param.ty)?;

        // Bind the parameter name
        self.resolve_pattern(&param.pattern, true)?;

        Ok(())
    }

    /// Resolves a pattern, creating bindings for identifiers
    fn resolve_pattern(
        &mut self,
        pattern: &Pattern,
        is_binding: bool,
    ) -> Result<(), ResolutionError> {
        match pattern {
            Pattern::Wildcard(_) => Ok(()),
            Pattern::Ident { name, mutable } => {
                if is_binding {
                    let def_id = self.next_def_id();
                    let binding = Binding::from_ident(
                        name,
                        BindingKind::Variable,
                        def_id,
                        self.current_module,
                    )
                    .with_mutable(*mutable);
                    self.symbol_table.insert(binding);
                } else {
                    // This is a reference to an existing binding
                    self.resolve_ident(name)?;
                }
                Ok(())
            }
            Pattern::Literal(_) => Ok(()),
            Pattern::Tuple(patterns) => {
                for p in patterns {
                    self.resolve_pattern(p, is_binding)?;
                }
                Ok(())
            }
            Pattern::Struct { path, fields, .. } => {
                self.resolve_path(path)?;
                for field in fields {
                    if let Some(pat) = &field.pattern {
                        self.resolve_pattern(pat, is_binding)?;
                    }
                }
                Ok(())
            }
            Pattern::Enum { path, inner, .. } => {
                self.resolve_path(path)?;
                if let Some(inner_pat) = inner {
                    self.resolve_pattern(inner_pat, is_binding)?;
                }
                Ok(())
            }
            Pattern::Array(patterns) => {
                for p in patterns {
                    self.resolve_pattern(p, is_binding)?;
                }
                Ok(())
            }
            Pattern::Rest(_) => Ok(()),
            Pattern::Or(left, right) => {
                self.resolve_pattern(left, is_binding)?;
                self.resolve_pattern(right, is_binding)?;
                Ok(())
            }
            Pattern::Bind { name, pattern } => {
                let def_id = self.next_def_id();
                let binding =
                    Binding::from_ident(name, BindingKind::Variable, def_id, self.current_module);
                self.symbol_table.insert(binding);
                self.resolve_pattern(pattern, is_binding)?;
                Ok(())
            }
            Pattern::Mut(pat) => {
                self.resolve_pattern(pat, is_binding)?;
                Ok(())
            }
            Pattern::Ref { pattern, .. } => {
                self.resolve_pattern(pattern, is_binding)?;
                Ok(())
            }
        }
    }

    /// Resolves an expression
    fn resolve_expr(&mut self, expr: &Expr) -> Result<(), ResolutionError> {
        match expr {
            Expr::Literal(_) => Ok(()),
            Expr::Variable(ident) => self.resolve_ident(ident),
            Expr::Path(path) => self.resolve_path(path),
            Expr::Call { func, args } => {
                self.resolve_expr(func)?;
                for arg in args {
                    self.resolve_expr(arg)?;
                }
                Ok(())
            }
            Expr::MethodCall {
                receiver,
                method: _,
                args,
            } => {
                self.resolve_expr(receiver)?;
                for arg in args {
                    self.resolve_expr(arg)?;
                }
                Ok(())
            }
            Expr::FieldAccess { object, field: _ } => {
                self.resolve_expr(object)?;
                Ok(())
            }
            Expr::Index { object, index } => {
                self.resolve_expr(object)?;
                self.resolve_expr(index)?;
                Ok(())
            }
            Expr::Unary { expr, .. } => {
                self.resolve_expr(expr)?;
                Ok(())
            }
            Expr::Binary { left, right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
                Ok(())
            }
            Expr::Block(block) => self.resolve_block(block),
            Expr::If {
                cond,
                then_branch,
                else_branch,
            } => {
                self.resolve_expr(cond)?;
                self.resolve_expr(then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.resolve_expr(else_branch)?;
                }
                Ok(())
            }
            Expr::Match { expr, arms } => {
                self.resolve_expr(expr)?;
                for arm in arms {
                    self.symbol_table.push_scope(ScopeKind::Block);
                    self.resolve_pattern(&arm.pattern, true)?;
                    if let Some(guard) = &arm.guard {
                        self.resolve_expr(guard)?;
                    }
                    self.resolve_expr(&arm.body)?;
                    self.symbol_table.pop_scope();
                }
                Ok(())
            }
            Expr::While { cond, body, .. } => {
                self.symbol_table.push_scope(ScopeKind::Loop);
                self.resolve_expr(cond)?;
                self.resolve_expr(body)?;
                self.symbol_table.pop_scope();
                Ok(())
            }
            Expr::For {
                pattern,
                iterable,
                body,
                ..
            } => {
                self.symbol_table.push_scope(ScopeKind::Loop);
                self.resolve_pattern(pattern, true)?;
                self.resolve_expr(iterable)?;
                self.resolve_expr(body)?;
                self.symbol_table.pop_scope();
                Ok(())
            }
            Expr::Loop { body, .. } => {
                self.symbol_table.push_scope(ScopeKind::Loop);
                self.resolve_expr(body)?;
                self.symbol_table.pop_scope();
                Ok(())
            }
            Expr::Lambda {
                params,
                return_type,
                effects,
                body,
            } => {
                self.symbol_table.push_scope(ScopeKind::Function);
                for param in params {
                    self.resolve_param(param)?;
                }
                if let Some(ret) = return_type {
                    self.resolve_type(ret)?;
                }
                for effect in effects {
                    self.resolve_type(effect)?;
                }
                self.resolve_expr(body)?;
                self.symbol_table.pop_scope();
                Ok(())
            }
            Expr::Await(expr) => {
                self.resolve_expr(expr)?;
                Ok(())
            }
            Expr::Try(expr) => {
                self.resolve_expr(expr)?;
                Ok(())
            }
            Expr::Assign { target, value, .. } => {
                self.resolve_expr(target)?;
                self.resolve_expr(value)?;
                Ok(())
            }
            Expr::Break { value, .. } => {
                if let Some(val) = value {
                    self.resolve_expr(val)?;
                }
                Ok(())
            }
            Expr::Continue { .. } => Ok(()),
            Expr::Return(expr) => {
                if let Some(e) = expr {
                    self.resolve_expr(e)?;
                }
                Ok(())
            }
            Expr::Tuple(exprs) => {
                for e in exprs {
                    self.resolve_expr(e)?;
                }
                Ok(())
            }
            Expr::Array(exprs) => {
                for e in exprs {
                    self.resolve_expr(e)?;
                }
                Ok(())
            }
            Expr::StructLiteral { path, fields } => {
                self.resolve_path(path)?;
                for field in fields {
                    if let Some(value) = &field.value {
                        self.resolve_expr(value)?;
                    } else {
                        // Shorthand: field name is also a variable reference
                        self.resolve_ident(&field.name)?;
                    }
                }
                Ok(())
            }
            Expr::Spawn(expr) => {
                self.resolve_expr(expr)?;
                Ok(())
            }
            Expr::Async(block) => self.resolve_block(block),
            Expr::Concurrent(block) => self.resolve_block(block),
            Expr::SelfExpr(_) => Ok(()),
            Expr::Pass => Ok(()),
            Expr::Raise(raise) => {
                // Resolve effect path if present
                if let Some(effect) = &raise.effect {
                    self.resolve_path(effect)?;
                }
                // Resolve operation as identifier
                self.resolve_ident(&raise.operation)?;
                // Resolve arguments
                for arg in &raise.args {
                    self.resolve_expr(arg)?;
                }
                Ok(())
            }
            Expr::Handle(handle) => {
                // Resolve the body
                self.resolve_expr(&handle.body)?;
                // Resolve handler arms
                for arm in &handle.handlers {
                    self.symbol_table.push_scope(ScopeKind::Block);
                    // Bind operation parameters
                    for param in &arm.params {
                        self.resolve_pattern(param, true)?;
                    }
                    // Bind resume function name if present
                    if let Some(resume_name) = &arm.resume_name {
                        let def_id = self.next_def_id();
                        let binding = Binding::from_ident(
                            resume_name,
                            BindingKind::Function,
                            def_id,
                            self.current_module,
                        );
                        self.symbol_table.insert(binding);
                    }
                    // Resolve handler body
                    self.resolve_expr(&arm.body)?;
                    self.symbol_table.pop_scope();
                }
                Ok(())
            }
            Expr::Resume(resume) => {
                // Resolve the value being resumed
                if let Some(value) = &resume.value {
                    self.resolve_expr(value)?;
                }
                Ok(())
            }
            Expr::Hole(_) => {
                // Holes don't need resolution - they are placeholders for type-directed development
                Ok(())
            }
        }
    }

    /// Resolves a block
    fn resolve_block(&mut self, block: &Block) -> Result<(), ResolutionError> {
        self.symbol_table.push_scope(ScopeKind::Block);

        for stmt in &block.stmts {
            self.resolve_stmt(stmt)?;
        }

        if let Some(expr) = &block.expr {
            self.resolve_expr(expr)?;
        }

        self.symbol_table.pop_scope();
        Ok(())
    }

    /// Resolves a statement
    fn resolve_stmt(&mut self, stmt: &jet_parser::ast::Stmt) -> Result<(), ResolutionError> {
        use jet_parser::ast::Stmt;

        match stmt {
            Stmt::Let { pattern, ty, value } => {
                self.resolve_expr(value)?;
                if let Some(t) = ty {
                    self.resolve_type(t)?;
                }
                self.resolve_pattern(pattern, true)?;
                Ok(())
            }
            Stmt::Expr(expr) => self.resolve_expr(expr),
            Stmt::Assign { target, value, .. } => {
                self.resolve_expr(target)?;
                self.resolve_expr(value)?;
                Ok(())
            }
            Stmt::Return(expr) => {
                if let Some(e) = expr {
                    self.resolve_expr(e)?;
                }
                Ok(())
            }
            Stmt::Break { value, .. } => {
                if let Some(val) = value {
                    self.resolve_expr(val)?;
                }
                Ok(())
            }
            Stmt::Continue { .. } => Ok(()),
            Stmt::Handle { body, handlers } => {
                // Resolve the body
                self.resolve_expr(body)?;
                // Resolve handler arms
                for arm in handlers {
                    self.symbol_table.push_scope(ScopeKind::Block);
                    // Bind operation parameters
                    for param in &arm.params {
                        self.resolve_pattern(param, true)?;
                    }
                    // Bind resume function name if present
                    if let Some(resume_name) = &arm.resume_name {
                        let def_id = self.next_def_id();
                        let binding = Binding::from_ident(
                            resume_name,
                            BindingKind::Function,
                            def_id,
                            self.current_module,
                        );
                        self.symbol_table.insert(binding);
                    }
                    // Resolve handler body
                    self.resolve_expr(&arm.body)?;
                    self.symbol_table.pop_scope();
                }
                Ok(())
            }
        }
    }

    /// Resolves a type
    fn resolve_type(&mut self, ty: &Type) -> Result<(), ResolutionError> {
        match ty {
            Type::Path(path) => {
                self.resolve_path(path)?;
                Ok(())
            }
            Type::Generic(base, args) => {
                self.resolve_type(base)?;
                for arg in args {
                    self.resolve_type(arg)?;
                }
                Ok(())
            }
            Type::Tuple(types) => {
                for t in types {
                    self.resolve_type(t)?;
                }
                Ok(())
            }
            Type::Array(elem, size) => {
                self.resolve_type(elem)?;
                if let Some(s) = size {
                    self.resolve_expr(s)?;
                }
                Ok(())
            }
            Type::Function {
                params,
                return_type,
                effects,
            } => {
                for param in params {
                    self.resolve_type(param)?;
                }
                if let Some(ret) = return_type {
                    self.resolve_type(ret)?;
                }
                for effect in effects {
                    self.resolve_type(effect)?;
                }
                Ok(())
            }
            Type::Reference { inner, .. } => {
                self.resolve_type(inner)?;
                Ok(())
            }
            Type::Channel(inner) => {
                self.resolve_type(inner)?;
                Ok(())
            }
            Type::Async(inner) => {
                self.resolve_type(inner)?;
                Ok(())
            }
            Type::Infer => Ok(()),
            Type::SelfType => Ok(()),
        }
    }

    /// Resolves an identifier
    fn resolve_ident(&mut self, ident: &Ident) -> Result<(), ResolutionError> {
        if let Some(binding) = self.symbol_table.lookup(&ident.name) {
            // Check visibility - private items can only be accessed from the same module
            if binding.module_id != self.current_module && !binding.is_public {
                return Err(ErrorHelpers::private_item_access(
                    &ident.name,
                    ident.span,
                    binding.span,
                    binding.def_id,
                ));
            }
            // The identifier is resolved; we could annotate it with the def_id here
            // For now, we just verify it exists and is visible
            Ok(())
        } else {
            Err(ErrorHelpers::unresolved_name(&ident.name, ident.span))
        }
    }

    /// Resolves an identifier and returns the binding
    pub fn resolve_ident_with_binding(
        &mut self,
        ident: &Ident,
    ) -> Result<Binding, ResolutionError> {
        if let Some(binding) = self.symbol_table.lookup(&ident.name) {
            let binding = binding.clone();
            // Check visibility
            if binding.module_id != self.current_module && !binding.is_public {
                return Err(ErrorHelpers::private_item_access(
                    &ident.name,
                    ident.span,
                    binding.span,
                    binding.def_id,
                ));
            }
            Ok(binding)
        } else {
            Err(ErrorHelpers::unresolved_name(&ident.name, ident.span))
        }
    }

    /// Resolves a path
    fn resolve_path(&mut self, path: &jet_parser::ast::Path) -> Result<(), ResolutionError> {
        // For simple paths (single segment), just resolve the identifier
        if path.segments.len() == 1 {
            return self.resolve_ident(&path.segments[0]);
        }

        // For qualified paths, resolve step by step
        let first = &path.segments[0];
        let first_binding = match self.symbol_table.lookup(&first.name) {
            Some(binding) => binding.clone(),
            None => return Err(ErrorHelpers::unresolved_name(&first.name, first.span)),
        };

        // Check visibility for the first segment if it's from another module
        if first_binding.module_id != self.current_module && !first_binding.is_public {
            return Err(ErrorHelpers::private_item_access(
                &first.name,
                first.span,
                first_binding.span,
                first_binding.def_id,
            ));
        }

        // For multi-segment paths, resolve remaining segments in the context of the first
        let mut current_binding = first_binding;

        for segment in &path.segments[1..] {
            match current_binding.kind {
                BindingKind::Module => {
                    // Look up the next segment in the module's bindings
                    // Find the module that contains this binding
                    let module_id = self
                        .symbol_table
                        .modules
                        .iter()
                        .find(|(_, m)| m.bindings().any(|b| b.def_id == current_binding.def_id))
                        .map(|(id, _)| *id);

                    if let Some(module_id) = module_id {
                        if let Some(module) = self.symbol_table.get_module(module_id) {
                            if let Some(binding) = module.get(&segment.name) {
                                // Check visibility
                                if binding.module_id != self.current_module && !binding.is_public {
                                    return Err(ErrorHelpers::private_item_access(
                                        &segment.name,
                                        segment.span,
                                        binding.span,
                                        binding.def_id,
                                    ));
                                }
                                current_binding = binding.clone();
                            } else {
                                return Err(ErrorHelpers::unresolved_name(
                                    &segment.name,
                                    segment.span,
                                ));
                            }
                        } else {
                            return Err(ErrorHelpers::unresolved_name(&segment.name, segment.span));
                        }
                    } else {
                        return Err(ErrorHelpers::unresolved_name(&segment.name, segment.span));
                    }
                }
                BindingKind::Type => {
                    // For types, look up associated items (methods, associated types, etc.)
                    // For now, we just check if the name exists in the current scope
                    // This is a simplified implementation
                    if self.symbol_table.lookup(&segment.name).is_none() {
                        return Err(ErrorHelpers::unresolved_name(&segment.name, segment.span));
                    }
                    // Update current_binding for next iteration
                    if let Some(binding) = self.symbol_table.lookup(&segment.name) {
                        current_binding = binding.clone();
                    }
                }
                _ => {
                    // For other kinds, just check if the name exists
                    if self.symbol_table.lookup(&segment.name).is_none() {
                        return Err(ErrorHelpers::unresolved_name(&segment.name, segment.span));
                    }
                    // Update current_binding for next iteration
                    if let Some(binding) = self.symbol_table.lookup(&segment.name) {
                        current_binding = binding.clone();
                    }
                }
            }
        }

        Ok(())
    }

    /// Resolves a path with full visibility checking
    ///
    /// This is a more sophisticated version that checks visibility at each step
    pub fn resolve_path_with_visibility(
        &mut self,
        path: &jet_parser::ast::Path,
    ) -> Result<Vec<Binding>, ResolutionError> {
        if path.segments.is_empty() {
            return Ok(Vec::new());
        }

        let mut bindings = Vec::new();

        // Resolve the first segment
        let first = &path.segments[0];
        let first_binding = match self.symbol_table.lookup(&first.name) {
            Some(binding) => binding.clone(),
            None => return Err(ErrorHelpers::unresolved_name(&first.name, first.span)),
        };

        // Check visibility
        if first_binding.module_id != self.current_module && !first_binding.is_public {
            return Err(ErrorHelpers::private_item_access(
                &first.name,
                first.span,
                first_binding.span,
                first_binding.def_id,
            ));
        }

        bindings.push(first_binding.clone());

        // Resolve remaining segments
        for segment in &path.segments[1..] {
            // In a full implementation, this would look up in the scope of the previous binding
            // For now, we just check the current module scope
            let binding = match self.symbol_table.lookup(&segment.name) {
                Some(binding) => binding.clone(),
                None => return Err(ErrorHelpers::unresolved_name(&segment.name, segment.span)),
            };

            // Check visibility
            if binding.module_id != self.current_module && !binding.is_public {
                return Err(ErrorHelpers::private_item_access(
                    &segment.name,
                    segment.span,
                    binding.span,
                    binding.def_id,
                ));
            }

            bindings.push(binding);
        }

        Ok(bindings)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_lexer::Span;

    fn make_span() -> Span {
        Span::new(0, 0)
    }

    #[test]
    fn test_resolver_creation() {
        let resolver = Resolver::new(ModuleId::root());
        assert_eq!(resolver.current_module, ModuleId::root());
        assert_eq!(resolver.symbol_table.scope_depth(), 0);
    }

    #[test]
    fn test_def_id_generation() {
        let resolver = Resolver::new(ModuleId::root());
        let id1 = resolver.next_def_id();
        let id2 = resolver.next_def_id();
        assert_ne!(id1, id2);
        assert_eq!(id1.as_raw(), 1);
        assert_eq!(id2.as_raw(), 2);
    }

    #[test]
    fn test_empty_module() {
        let mut resolver = Resolver::new(ModuleId::root());
        let module = Module::new(make_span());
        let result = resolver.resolve_module(&module);
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_path_simple() {
        let mut resolver = Resolver::new(ModuleId::root());

        // Add a binding to the module
        let binding = Binding::new(
            "foo",
            make_span(),
            BindingKind::Function,
            resolver.next_def_id(),
            ModuleId::root(),
        )
        .with_public(true);
        resolver.symbol_table.insert_module_binding(binding);

        // Create a simple path
        let path = jet_parser::ast::Path::single(jet_parser::ast::Ident::new("foo", make_span()));

        // Resolve the path
        let result = resolver.resolve_path(&path);
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_path_qualified() {
        let mut resolver = Resolver::new(ModuleId::root());

        // Add bindings to the module
        let binding1 = Binding::new(
            "std",
            make_span(),
            BindingKind::Module,
            resolver.next_def_id(),
            ModuleId::root(),
        )
        .with_public(true);
        resolver.symbol_table.insert_module_binding(binding1);

        let binding2 = Binding::new(
            "io",
            make_span(),
            BindingKind::Module,
            resolver.next_def_id(),
            ModuleId::root(),
        )
        .with_public(true);
        resolver.symbol_table.insert_module_binding(binding2);

        // Create a qualified path
        let path = jet_parser::ast::Path::new(
            vec![
                jet_parser::ast::Ident::new("std", make_span()),
                jet_parser::ast::Ident::new("io", make_span()),
            ],
            make_span(),
        );

        // Resolve the path
        let result = resolver.resolve_path(&path);
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_path_unresolved() {
        let mut resolver = Resolver::new(ModuleId::root());

        // Create a path to a non-existent item
        let path =
            jet_parser::ast::Path::single(jet_parser::ast::Ident::new("nonexistent", make_span()));

        // Resolve the path - should fail
        let result = resolver.resolve_path(&path);
        assert!(result.is_err());
    }

    #[test]
    fn test_visibility_check_private() {
        let mut resolver = Resolver::new(ModuleId::root());

        // Add a private binding to the root module
        let private_binding = Binding::new(
            "private_fn",
            make_span(),
            BindingKind::Function,
            resolver.next_def_id(),
            ModuleId::root(),
        )
        .with_public(false); // Private!
        resolver.symbol_table.insert_module_binding(private_binding);

        // Create an identifier for the private function
        let ident = jet_parser::ast::Ident::new("private_fn", make_span());

        // Should succeed because we're in the same module (root)
        let result = resolver.resolve_ident(&ident);
        assert!(result.is_ok());
    }

    #[test]
    fn test_visibility_check_public() {
        let mut resolver = Resolver::new(ModuleId::root());

        // Add a public binding to the root module
        let public_binding = Binding::new(
            "public_fn",
            make_span(),
            BindingKind::Function,
            resolver.next_def_id(),
            ModuleId::root(),
        )
        .with_public(true);
        resolver.symbol_table.insert_module_binding(public_binding);

        // Create an identifier for the public function
        let ident = jet_parser::ast::Ident::new("public_fn", make_span());

        // Should succeed
        let result = resolver.resolve_ident(&ident);
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_ident_with_binding() {
        let mut resolver = Resolver::new(ModuleId::root());

        // Add a binding
        let binding = Binding::new(
            "my_var",
            make_span(),
            BindingKind::Variable,
            resolver.next_def_id(),
            ModuleId::root(),
        )
        .with_public(true);
        resolver.symbol_table.insert_module_binding(binding);

        // Resolve and get the binding
        let ident = jet_parser::ast::Ident::new("my_var", make_span());
        let result = resolver.resolve_ident_with_binding(&ident);

        assert!(result.is_ok());
        let resolved_binding = result.unwrap();
        assert_eq!(resolved_binding.name, "my_var");
        assert_eq!(resolved_binding.kind, BindingKind::Variable);
    }

    #[test]
    fn test_resolve_path_with_visibility() {
        let mut resolver = Resolver::new(ModuleId::root());

        // Add bindings
        let binding1 = Binding::new(
            "std",
            make_span(),
            BindingKind::Module,
            resolver.next_def_id(),
            ModuleId::root(),
        )
        .with_public(true);
        resolver.symbol_table.insert_module_binding(binding1);

        let binding2 = Binding::new(
            "print",
            make_span(),
            BindingKind::Function,
            resolver.next_def_id(),
            ModuleId::root(),
        )
        .with_public(true);
        resolver.symbol_table.insert_module_binding(binding2);

        // Create a qualified path
        let path = jet_parser::ast::Path::new(
            vec![
                jet_parser::ast::Ident::new("std", make_span()),
                jet_parser::ast::Ident::new("print", make_span()),
            ],
            make_span(),
        );

        // Resolve the path with visibility checking
        let result = resolver.resolve_path_with_visibility(&path);
        assert!(result.is_ok());

        let bindings = result.unwrap();
        assert_eq!(bindings.len(), 2);
        assert_eq!(bindings[0].name, "std");
        assert_eq!(bindings[1].name, "print");
    }

    #[test]
    fn test_resolve_module_qualified_path() {
        let mut resolver = Resolver::new(ModuleId::root());

        // Create a module binding for "lib"
        let module_def_id = resolver.next_def_id();
        let module_binding = Binding::new(
            "lib",
            make_span(),
            BindingKind::Module,
            module_def_id,
            ModuleId::root(),
        )
        .with_public(true);
        resolver.symbol_table.insert_module_binding(module_binding);

        // Add a function binding "helper" in the lib module
        let helper_def_id = resolver.next_def_id();
        let helper_binding = Binding::new(
            "helper",
            make_span(),
            BindingKind::Function,
            helper_def_id,
            ModuleId::root(),
        )
        .with_public(true);
        // Insert into the module scope directly
        resolver
            .symbol_table
            .current_module_mut()
            .insert(helper_binding);

        // Create a qualified path "lib::helper"
        let path = jet_parser::ast::Path::new(
            vec![
                jet_parser::ast::Ident::new("lib", make_span()),
                jet_parser::ast::Ident::new("helper", make_span()),
            ],
            make_span(),
        );

        // Resolve the path
        let result = resolver.resolve_path(&path);
        assert!(
            result.is_ok(),
            "Failed to resolve module-qualified path: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_self_in_trait_resolution() {
        use jet_parser::ast::{Param, Pattern, TraitDef, TraitItem, Type};

        let mut resolver = Resolver::new(ModuleId::root());

        // Inject prelude so primitive types are available
        resolver.inject_prelude();

        // Create a trait definition with Self reference:
        // trait Comparable:
        //     fn compare(self, other: Self) -> int
        let trait_def = TraitDef {
            public: true,
            name: jet_parser::ast::Ident::new("Comparable", make_span()),
            generics: vec![],
            super_traits: vec![],
            items: vec![TraitItem::Method {
                name: jet_parser::ast::Ident::new("compare", make_span()),
                generics: vec![],
                params: vec![
                    Param {
                        pattern: Pattern::Ident {
                            name: jet_parser::ast::Ident::new("self", make_span()),
                            mutable: false,
                        },
                        ty: Type::Infer,
                    },
                    Param {
                        pattern: Pattern::Ident {
                            name: jet_parser::ast::Ident::new("other", make_span()),
                            mutable: false,
                        },
                        ty: Type::SelfType, // Self type reference
                    },
                ],
                return_type: Some(Type::Path(jet_parser::ast::Path::single(
                    jet_parser::ast::Ident::new("int", make_span()),
                ))),
                effects: vec![],
                where_clause: vec![],
            }],
            span: make_span(),
        };

        // Collect the trait first
        resolver.collect_trait(&trait_def).unwrap();

        // Now resolve the trait - this should succeed with Self bound
        let result = resolver.resolve_trait(&trait_def);
        assert!(
            result.is_ok(),
            "Failed to resolve trait with Self: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_self_in_impl_resolution() {
        use jet_parser::ast::{Expr, Function, ImplDef, ImplItem, Literal};

        let mut resolver = Resolver::new(ModuleId::root());

        // First, define a struct type "Point"
        let struct_def_id = resolver.next_def_id();
        let struct_binding = Binding::new(
            "Point",
            make_span(),
            BindingKind::Type,
            struct_def_id,
            ModuleId::root(),
        )
        .with_public(true);
        resolver.symbol_table.insert_module_binding(struct_binding);

        // Create an impl block:
        // impl Point:
        //     fn new() -> Self:
        //         ...
        let impl_def = ImplDef {
            generics: vec![],
            trait_path: None,
            ty: Type::Path(jet_parser::ast::Path::single(jet_parser::ast::Ident::new(
                "Point",
                make_span(),
            ))),
            where_clause: vec![],
            items: vec![ImplItem::Method(Function {
                public: true,
                attributes: vec![],
                name: jet_parser::ast::Ident::new("new", make_span()),
                generics: vec![],
                params: vec![],
                return_type: Some(Type::SelfType), // Return Self
                effects: vec![],
                where_clause: vec![],
                contract: None,
                body: Expr::Literal(Literal::Unit),
                span: make_span(),
            })],
            span: make_span(),
        };

        // Resolve the impl block - this should succeed with Self bound
        let result = resolver.resolve_impl(&impl_def);
        assert!(
            result.is_ok(),
            "Failed to resolve impl with Self: {:?}",
            result.err()
        );
    }
}
