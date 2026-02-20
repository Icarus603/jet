//! Trait system implementation for Jet
//!
//! This module provides:
//! - Trait resolution: finding trait implementations for types
//! - Method lookup: finding methods on types through traits
//! - Coherence checking: orphan rules and overlapping impl detection
//! - Associated type resolution

use crate::const_eval::ConstValue;
use crate::types::{DefId, EffectSet, TypeContext, TypeId, TypeKind};
use jet_diagnostics::{Diagnostic, ErrorCode, Label, Span};
use std::collections::HashMap;

/// A trait definition in the type system
#[derive(Debug, Clone)]
pub struct Trait {
    pub def_id: DefId,
    pub name: String,
    pub generics: Vec<GenericParam>,
    pub super_traits: Vec<TraitBound>,
    pub items: Vec<TraitItemDef>,
    pub span: Span,
}

/// A generic parameter
#[derive(Debug, Clone)]
pub struct GenericParam {
    pub name: String,
    pub def_id: DefId,
    pub bounds: Vec<TraitBound>,
}

/// A trait bound (e.g., `T: Display`)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitBound {
    pub trait_id: DefId,
    pub args: Vec<TypeId>,
}

/// A trait item definition (method, type, or constant)
#[derive(Debug, Clone)]
pub enum TraitItemDef {
    Method {
        name: String,
        generics: Vec<GenericParam>,
        params: Vec<(String, TypeId)>,
        return_type: TypeId,
        effects: EffectSet,
    },
    Type {
        name: String,
        bounds: Vec<TraitBound>,
    },
    Const {
        name: String,
        ty: TypeId,
    },
}

/// An impl block (trait implementation)
#[derive(Debug, Clone)]
pub struct Impl {
    pub def_id: DefId,
    pub generics: Vec<GenericParam>,
    pub trait_bound: Option<TraitBound>, // None for inherent impls
    pub for_type: TypeId,
    pub where_clauses: Vec<WhereClause>,
    pub items: Vec<ImplItem>,
    pub span: Span,
}

/// An item in an impl block
#[derive(Debug, Clone)]
pub enum ImplItem {
    Method {
        name: String,
        def_id: DefId,
        func_type: TypeId,
    },
    Const {
        name: String,
        def_id: DefId,
        ty: TypeId,
        value: ConstValue,
    },
    Type {
        name: String,
        def_id: DefId,
        ty: TypeId,
    },
}

/// A where clause constraint
#[derive(Debug, Clone)]
pub struct WhereClause {
    pub ty: TypeId,
    pub bounds: Vec<TraitBound>,
}

/// The trait context stores all trait definitions and implementations
#[derive(Debug, Default)]
pub struct TraitContext {
    /// Trait definitions by DefId
    traits: HashMap<DefId, Trait>,
    /// Trait definitions by name
    trait_names: HashMap<String, DefId>,
    /// Impl blocks
    impls: Vec<Impl>,
    /// Impls by trait (for faster lookup)
    impls_by_trait: HashMap<DefId, Vec<usize>>, // indices into impls
    /// Impls by type (for faster lookup)
    impls_by_type: HashMap<TypeId, Vec<usize>>, // indices into impls
    /// Inherent impls (impl Type, not impl Trait for Type)
    inherent_impls: HashMap<TypeId, Vec<usize>>,
}

impl TraitContext {
    /// Create a new trait context
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a trait definition
    pub fn register_trait(&mut self, trait_def: Trait) {
        let def_id = trait_def.def_id;
        let name = trait_def.name.clone();
        self.traits.insert(def_id, trait_def);
        self.trait_names.insert(name, def_id);
    }

    /// Get a trait by its DefId
    pub fn get_trait(&self, def_id: DefId) -> Option<&Trait> {
        self.traits.get(&def_id)
    }

    /// Get a trait by its name
    pub fn get_trait_by_name(&self, name: &str) -> Option<&Trait> {
        self.trait_names
            .get(name)
            .and_then(|id| self.traits.get(id))
    }

    /// Register an impl block
    pub fn register_impl(&mut self, impl_def: Impl) {
        let idx = self.impls.len();

        // Index by trait
        if let Some(ref bound) = impl_def.trait_bound {
            self.impls_by_trait
                .entry(bound.trait_id)
                .or_default()
                .push(idx);
        } else {
            // Inherent impl
            self.inherent_impls
                .entry(impl_def.for_type)
                .or_default()
                .push(idx);
        }

        // Index by type
        self.impls_by_type
            .entry(impl_def.for_type)
            .or_default()
            .push(idx);

        self.impls.push(impl_def);
    }

    /// Check if a type implements a trait
    pub fn implements(&self, ty: TypeId, trait_bound: &TraitBound, tcx: &TypeContext) -> bool {
        self.find_impl(ty, trait_bound, tcx).is_some()
    }

    /// Find an implementation of a trait for a type
    pub fn find_impl(
        &self,
        ty: TypeId,
        trait_bound: &TraitBound,
        tcx: &TypeContext,
    ) -> Option<&Impl> {
        // Get candidates by trait
        let candidates = self.impls_by_trait.get(&trait_bound.trait_id)?;

        for &idx in candidates {
            let impl_def = &self.impls[idx];
            if let Some(ref impl_bound) = impl_def.trait_bound {
                if self.types_match(ty, impl_def.for_type, tcx) {
                    // Check if the trait arguments match
                    if self.trait_args_match(impl_bound, trait_bound, tcx) {
                        return Some(impl_def);
                    }
                }
            }
        }

        None
    }

    /// Find inherent methods for a type
    pub fn find_inherent_methods(&self, ty: TypeId) -> Vec<&ImplItem> {
        let mut items = Vec::new();

        if let Some(indices) = self.inherent_impls.get(&ty) {
            for &idx in indices {
                let impl_def = &self.impls[idx];
                items.extend(impl_def.items.iter());
            }
        }

        items
    }

    /// Look up a method on a type (both inherent and through traits)
    pub fn lookup_method(
        &self,
        ty: TypeId,
        method_name: &str,
        tcx: &TypeContext,
    ) -> Option<MethodCandidate> {
        // First check inherent impls
        if let Some(indices) = self.inherent_impls.get(&ty) {
            for &idx in indices {
                let impl_def = &self.impls[idx];
                for item in &impl_def.items {
                    if let ImplItem::Method {
                        name, func_type, ..
                    } = item
                    {
                        if name == method_name {
                            return Some(MethodCandidate {
                                name: name.clone(),
                                func_type: *func_type,
                                source: MethodSource::Inherent,
                            });
                        }
                    }
                }
            }
        }

        // Then check trait impls
        // We need to find all trait impls for this type and check their methods
        for (trait_id, indices) in &self.impls_by_trait {
            let _trait_def = self.traits.get(trait_id)?;

            for &idx in indices {
                let impl_def = &self.impls[idx];
                if self.types_match(ty, impl_def.for_type, tcx) {
                    for item in &impl_def.items {
                        if let ImplItem::Method {
                            name, func_type, ..
                        } = item
                        {
                            if name == method_name {
                                return Some(MethodCandidate {
                                    name: name.clone(),
                                    func_type: *func_type,
                                    source: MethodSource::Trait(*trait_id),
                                });
                            }
                        }
                    }
                }
            }
        }

        None
    }

    /// Check if two types match (considering type variables)
    fn types_match(&self, t1: TypeId, t2: TypeId, tcx: &TypeContext) -> bool {
        // Simple case: exact match
        if t1 == t2 {
            return true;
        }

        let kind1 = tcx.type_kind(t1);
        let kind2 = tcx.type_kind(t2);

        match (kind1, kind2) {
            // Type variables can match anything (during inference)
            (TypeKind::Var(_), _) | (_, TypeKind::Var(_)) => true,

            // Check structurally
            (TypeKind::Tuple(es1), TypeKind::Tuple(es2)) => {
                if es1.len() != es2.len() {
                    return false;
                }
                es1.iter()
                    .zip(es2.iter())
                    .all(|(a, b)| self.types_match(*a, *b, tcx))
            }
            (TypeKind::Array(e1, s1), TypeKind::Array(e2, s2)) => {
                s1 == s2 && self.types_match(*e1, *e2, tcx)
            }
            (TypeKind::Slice(e1), TypeKind::Slice(e2)) => self.types_match(*e1, *e2, tcx),
            (TypeKind::Ref(i1, m1), TypeKind::Ref(i2, m2)) => {
                m1 == m2 && self.types_match(*i1, *i2, tcx)
            }
            (
                TypeKind::Function {
                    params: p1,
                    ret: r1,
                    ..
                },
                TypeKind::Function {
                    params: p2,
                    ret: r2,
                    ..
                },
            ) => {
                if p1.len() != p2.len() {
                    return false;
                }
                p1.iter()
                    .zip(p2.iter())
                    .all(|(a, b)| self.types_match(*a, *b, tcx))
                    && self.types_match(*r1, *r2, tcx)
            }
            // For concrete types (Int, Bool, etc.), exact match is required
            _ => t1 == t2,
        }
    }

    /// Check if trait arguments match
    fn trait_args_match(
        &self,
        bound1: &TraitBound,
        bound2: &TraitBound,
        tcx: &TypeContext,
    ) -> bool {
        if bound1.trait_id != bound2.trait_id {
            return false;
        }
        if bound1.args.len() != bound2.args.len() {
            return false;
        }

        bound1
            .args
            .iter()
            .zip(bound2.args.iter())
            .all(|(a, b)| self.types_match(*a, *b, tcx))
    }

    /// Check coherence (orphan rules and overlapping impls)
    pub fn check_coherence(&self, tcx: &TypeContext) -> Result<(), Vec<Diagnostic>> {
        let mut errors = Vec::new();

        // Check for overlapping impls
        errors.extend(self.check_overlapping_impls(tcx));

        // Check orphan rules
        errors.extend(self.check_orphan_rules(tcx));

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Check for overlapping implementations
    fn check_overlapping_impls(&self, tcx: &TypeContext) -> Vec<Diagnostic> {
        let mut errors = Vec::new();

        // For each trait, check if any two impls overlap
        for (trait_id, indices) in &self.impls_by_trait {
            for (i, &idx1) in indices.iter().enumerate() {
                for &idx2 in indices.iter().skip(i + 1) {
                    let impl1 = &self.impls[idx1];
                    let impl2 = &self.impls[idx2];

                    if self.impls_overlap(impl1, impl2, tcx) {
                        errors.push(
                            Diagnostic::error(
                                format!(
                                    "conflicting implementations of trait `{}`",
                                    self.get_trait(*trait_id)
                                        .map(|t| t.name.as_str())
                                        .unwrap_or("<unknown>")
                                ),
                                impl2.span,
                            )
                            .with_error_code(ErrorCode::ConflictingImpls)
                            .with_label(Label::primary(impl1.span, "first implementation here"))
                            .with_label(Label::secondary(
                                impl2.span,
                                "conflicting implementation here",
                            ))
                            .with_note("overlapping implementations are not allowed"),
                        );
                    }
                }
            }
        }

        errors
    }

    /// Check if two impls overlap
    fn impls_overlap(&self, impl1: &Impl, impl2: &Impl, tcx: &TypeContext) -> bool {
        // Check if the types could unify
        self.types_could_unify(impl1.for_type, impl2.for_type, tcx)
    }

    /// Check if two types could potentially unify
    fn types_could_unify(&self, t1: TypeId, t2: TypeId, tcx: &TypeContext) -> bool {
        let kind1 = tcx.type_kind(t1);
        let kind2 = tcx.type_kind(t2);

        match (kind1, kind2) {
            // Type variables could unify with anything
            (TypeKind::Var(_), _) | (_, TypeKind::Var(_)) => true,

            // Different constructors can't unify
            (k1, k2) if std::mem::discriminant(k1) != std::mem::discriminant(k2) => false,

            // Check structurally
            (TypeKind::Tuple(es1), TypeKind::Tuple(es2)) => {
                if es1.len() != es2.len() {
                    return false;
                }
                es1.iter()
                    .zip(es2.iter())
                    .all(|(a, b)| self.types_could_unify(*a, *b, tcx))
            }
            (TypeKind::Array(e1, s1), TypeKind::Array(e2, s2)) => {
                s1 == s2 && self.types_could_unify(*e1, *e2, tcx)
            }
            (TypeKind::Slice(e1), TypeKind::Slice(e2)) => self.types_could_unify(*e1, *e2, tcx),
            (TypeKind::Ref(i1, m1), TypeKind::Ref(i2, m2)) => {
                m1 == m2 && self.types_could_unify(*i1, *i2, tcx)
            }
            (
                TypeKind::Function {
                    params: p1,
                    ret: r1,
                    ..
                },
                TypeKind::Function {
                    params: p2,
                    ret: r2,
                    ..
                },
            ) => {
                if p1.len() != p2.len() {
                    return false;
                }
                p1.iter()
                    .zip(p2.iter())
                    .all(|(a, b)| self.types_could_unify(*a, *b, tcx))
                    && self.types_could_unify(*r1, *r2, tcx)
            }
            // Concrete types must be equal
            _ => t1 == t2,
        }
    }

    /// Check orphan rules
    fn check_orphan_rules(&self, _tcx: &TypeContext) -> Vec<Diagnostic> {
        // Orphan rule: At least one of the type or trait must be local
        // For now, we allow all impls (simplified)
        Vec::new()
    }
}

/// A method candidate found during lookup
#[derive(Debug, Clone)]
pub struct MethodCandidate {
    pub name: String,
    pub func_type: TypeId,
    pub source: MethodSource,
}

/// The source of a method
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MethodSource {
    Inherent,
    Trait(DefId),
}

/// Trait resolver for handling trait bounds and constraints
pub struct TraitResolver<'tcx> {
    trait_ctx: &'tcx TraitContext,
    tcx: &'tcx TypeContext,
}

impl<'tcx> TraitResolver<'tcx> {
    /// Create a new trait resolver
    pub fn new(trait_ctx: &'tcx TraitContext, tcx: &'tcx TypeContext) -> Self {
        Self { trait_ctx, tcx }
    }

    /// Resolve a trait bound against a type
    pub fn resolve(&self, ty: TypeId, bound: &TraitBound) -> Result<Resolution, ResolutionError> {
        if let Some(impl_def) = self.trait_ctx.find_impl(ty, bound, self.tcx) {
            Ok(Resolution {
                impl_def_id: impl_def.def_id,
                trait_id: bound.trait_id,
            })
        } else {
            Err(ResolutionError {
                ty,
                bound: bound.clone(),
                message: format!(
                    "the trait bound `{}: {}` is not satisfied",
                    self.tcx.type_to_string(ty),
                    self.trait_name(bound.trait_id)
                ),
            })
        }
    }

    /// Check if all trait bounds are satisfied for a type
    pub fn check_bounds(
        &self,
        ty: TypeId,
        bounds: &[TraitBound],
    ) -> Result<(), Vec<ResolutionError>> {
        let mut errors = Vec::new();

        for bound in bounds {
            if let Err(e) = self.resolve(ty, bound) {
                errors.push(e);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn trait_name(&self, trait_id: DefId) -> String {
        self.trait_ctx
            .get_trait(trait_id)
            .map(|t| t.name.clone())
            .unwrap_or_else(|| format!("<unknown trait {:?}>", trait_id))
    }
}

/// A successful trait resolution
#[derive(Debug, Clone)]
pub struct Resolution {
    pub impl_def_id: DefId,
    pub trait_id: DefId,
}

/// An error during trait resolution
#[derive(Debug, Clone)]
pub struct ResolutionError {
    pub ty: TypeId,
    pub bound: TraitBound,
    pub message: String,
}

impl ResolutionError {
    /// Convert to a diagnostic
    pub fn to_diagnostic(&self, tcx: &TypeContext, span: Span) -> Diagnostic {
        Diagnostic::error(&self.message, span)
            .with_error_code(ErrorCode::TraitBoundNotSatisfied)
            .with_note(format!("type: {}", tcx.type_to_string(self.ty)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::TypeContext;

    #[test]
    fn test_trait_registration() {
        let mut trait_ctx = TraitContext::new();

        let trait_def = Trait {
            def_id: DefId(1),
            name: "Display".to_string(),
            generics: vec![],
            super_traits: vec![],
            items: vec![],
            span: Span::default(),
        };

        trait_ctx.register_trait(trait_def);

        assert!(trait_ctx.get_trait(DefId(1)).is_some());
        assert!(trait_ctx.get_trait_by_name("Display").is_some());
    }

    #[test]
    fn test_impl_lookup() {
        let mut trait_ctx = TraitContext::new();
        let tcx = TypeContext::new();

        let trait_def = Trait {
            def_id: DefId(1),
            name: "Display".to_string(),
            generics: vec![],
            super_traits: vec![],
            items: vec![],
            span: Span::default(),
        };
        trait_ctx.register_trait(trait_def);

        let int_impl = Impl {
            def_id: DefId(2),
            generics: vec![],
            trait_bound: Some(TraitBound {
                trait_id: DefId(1),
                args: vec![],
            }),
            for_type: TypeId::INT,
            where_clauses: vec![],
            items: vec![],
            span: Span::default(),
        };
        trait_ctx.register_impl(int_impl);

        let bound = TraitBound {
            trait_id: DefId(1),
            args: vec![],
        };

        assert!(trait_ctx.implements(TypeId::INT, &bound, &tcx));
        assert!(!trait_ctx.implements(TypeId::BOOL, &bound, &tcx));
    }

    #[test]
    fn test_impl_lookup_with_trait_arg_substitution() {
        let mut trait_ctx = TraitContext::new();
        let mut tcx = TypeContext::new();

        let trait_def = Trait {
            def_id: DefId(10),
            name: "Marker".to_string(),
            generics: vec![],
            super_traits: vec![],
            items: vec![],
            span: Span::default(),
        };
        trait_ctx.register_trait(trait_def);

        let var = tcx.fresh_var(0);
        let generic_impl = Impl {
            def_id: DefId(11),
            generics: vec![],
            trait_bound: Some(TraitBound {
                trait_id: DefId(10),
                args: vec![var],
            }),
            for_type: TypeId::INT,
            where_clauses: vec![],
            items: vec![],
            span: Span::default(),
        };
        trait_ctx.register_impl(generic_impl);

        let query = TraitBound {
            trait_id: DefId(10),
            args: vec![TypeId::STRING],
        };

        assert!(trait_ctx.implements(TypeId::INT, &query, &tcx));
    }
}
