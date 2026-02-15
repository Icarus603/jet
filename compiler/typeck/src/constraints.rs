//! Generic constraint checking for Jet
//!
//! This module handles:
//! - Where clause validation
//! - Generic parameter bounds checking
//! - Constraint solving and propagation

use crate::traits::{TraitBound, TraitContext, TraitResolver};
use crate::types::{DefId, TypeContext, TypeId, TypeKind, VarId};
use jet_diagnostics::{Diagnostic, ErrorCode, Span};
use std::collections::HashMap;

/// A constraint that must be satisfied
#[derive(Debug, Clone)]
pub enum Constraint {
    /// Type must implement a trait
    TraitBound {
        ty: TypeId,
        bound: TraitBound,
        span: Span,
    },
    /// Type equality constraint
    TypeEqual {
        expected: TypeId,
        found: TypeId,
        span: Span,
    },
    /// Subtyping constraint (for variance)
    Subtype {
        sub: TypeId,
        sup: TypeId,
        span: Span,
    },
}

/// A set of constraints to be solved
#[derive(Debug, Default)]
pub struct ConstraintSet {
    constraints: Vec<Constraint>,
}

impl ConstraintSet {
    /// Create a new empty constraint set
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a constraint
    pub fn add(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }

    /// Get all constraints
    pub fn constraints(&self) -> &[Constraint] {
        &self.constraints
    }

    /// Check if the constraint set is empty
    pub fn is_empty(&self) -> bool {
        self.constraints.is_empty()
    }

    /// Merge another constraint set into this one
    pub fn merge(&mut self, other: ConstraintSet) {
        self.constraints.extend(other.constraints);
    }
}

/// Constraint solver for checking generic constraints
pub struct ConstraintSolver<'tcx> {
    trait_ctx: &'tcx TraitContext,
    tcx: &'tcx TypeContext,
    substitutions: HashMap<VarId, TypeId>,
}

impl<'tcx> ConstraintSolver<'tcx> {
    /// Create a new constraint solver
    pub fn new(trait_ctx: &'tcx TraitContext, tcx: &'tcx TypeContext) -> Self {
        Self {
            trait_ctx,
            tcx,
            substitutions: HashMap::new(),
        }
    }

    /// Solve a set of constraints
    pub fn solve(&mut self, constraints: &ConstraintSet) -> Result<(), Vec<Box<Diagnostic>>> {
        let mut errors = Vec::new();

        for constraint in constraints.constraints() {
            if let Err(e) = self.solve_constraint(constraint) {
                errors.push(e);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Solve a single constraint
    fn solve_constraint(&mut self, constraint: &Constraint) -> Result<(), Box<Diagnostic>> {
        match constraint {
            Constraint::TraitBound { ty, bound, span } => self.check_trait_bound(*ty, bound, *span),
            Constraint::TypeEqual {
                expected,
                found,
                span,
            } => self.check_type_equal(*expected, *found, *span),
            Constraint::Subtype { sub, sup, span } => self.check_subtype(*sub, *sup, *span),
        }
    }

    /// Check if a type satisfies a trait bound
    fn check_trait_bound(
        &self,
        ty: TypeId,
        bound: &TraitBound,
        span: Span,
    ) -> Result<(), Box<Diagnostic>> {
        let resolver = TraitResolver::new(self.trait_ctx, self.tcx);

        match resolver.resolve(ty, bound) {
            Ok(_) => Ok(()),
            Err(e) => Err(Box::new(e.to_diagnostic(self.tcx, span))),
        }
    }

    /// Check type equality
    fn check_type_equal(
        &mut self,
        expected: TypeId,
        found: TypeId,
        span: Span,
    ) -> Result<(), Box<Diagnostic>> {
        let expected = self.apply_substitutions(expected);
        let found = self.apply_substitutions(found);

        if self.types_equal(expected, found) {
            Ok(())
        } else {
            Err(Box::new(
                Diagnostic::error(
                    format!(
                        "type mismatch: expected `{}`, found `{}`",
                        self.tcx.type_to_string(expected),
                        self.tcx.type_to_string(found)
                    ),
                    span,
                )
                .with_error_code(ErrorCode::TypeMismatch),
            ))
        }
    }

    /// Check subtyping relationship
    fn check_subtype(&self, sub: TypeId, sup: TypeId, span: Span) -> Result<(), Box<Diagnostic>> {
        if self.is_subtype(sub, sup) {
            Ok(())
        } else {
            Err(Box::new(
                Diagnostic::error(
                    format!(
                        "type mismatch: expected `{}`, found `{}`",
                        self.tcx.type_to_string(sup),
                        self.tcx.type_to_string(sub)
                    ),
                    span,
                )
                .with_error_code(ErrorCode::TypeMismatch),
            ))
        }
    }

    fn is_subtype(&self, sub: TypeId, sup: TypeId) -> bool {
        if self.types_equal(sub, sup) {
            return true;
        }

        let sub = self.apply_substitutions(sub);
        let sup = self.apply_substitutions(sup);

        match (self.tcx.type_kind(sub), self.tcx.type_kind(sup)) {
            // Tuples are covariant in each element.
            (TypeKind::Tuple(sub_es), TypeKind::Tuple(sup_es)) => {
                sub_es.len() == sup_es.len()
                    && sub_es
                        .iter()
                        .zip(sup_es.iter())
                        .all(|(a, b)| self.is_subtype(*a, *b))
            }
            // Arrays are invariant due to mutation/index assignment.
            (TypeKind::Array(sub_e, sub_n), TypeKind::Array(sup_e, sup_n)) => {
                sub_n == sup_n && self.types_equal(*sub_e, *sup_e)
            }
            // Slices are covariant in element type.
            (TypeKind::Slice(sub_e), TypeKind::Slice(sup_e)) => self.is_subtype(*sub_e, *sup_e),
            // Immutable references are covariant.
            (
                TypeKind::Ref(sub_inner, crate::types::Mutability::Immutable),
                TypeKind::Ref(sup_inner, crate::types::Mutability::Immutable),
            ) => self.is_subtype(*sub_inner, *sup_inner),
            // Mutable references are invariant.
            (
                TypeKind::Ref(sub_inner, crate::types::Mutability::Mutable),
                TypeKind::Ref(sup_inner, crate::types::Mutability::Mutable),
            ) => self.types_equal(*sub_inner, *sup_inner),
            // Functions are contravariant in params and covariant in return.
            (
                TypeKind::Function {
                    params: sub_params,
                    ret: sub_ret,
                    effects: sub_effects,
                },
                TypeKind::Function {
                    params: sup_params,
                    ret: sup_ret,
                    effects: sup_effects,
                },
            ) => {
                sub_params.len() == sup_params.len()
                    && sub_params
                        .iter()
                        .zip(sup_params.iter())
                        .all(|(sub_p, sup_p)| self.is_subtype(*sup_p, *sub_p))
                    && self.is_subtype(*sub_ret, *sup_ret)
                    && sub_effects == sup_effects
            }
            _ => false,
        }
    }

    /// Apply current substitutions to a type
    fn apply_substitutions(&self, ty: TypeId) -> TypeId {
        match self.tcx.type_kind(ty) {
            TypeKind::Var(var_id) => {
                if let Some(&subst) = self.substitutions.get(var_id) {
                    self.apply_substitutions(subst)
                } else {
                    ty
                }
            }
            _ => ty,
        }
    }

    /// Check if two types are equal
    fn types_equal(&self, t1: TypeId, t2: TypeId) -> bool {
        let t1 = self.apply_substitutions(t1);
        let t2 = self.apply_substitutions(t2);

        if t1 == t2 {
            return true;
        }

        let kind1 = self.tcx.type_kind(t1);
        let kind2 = self.tcx.type_kind(t2);

        match (kind1, kind2) {
            (TypeKind::Tuple(es1), TypeKind::Tuple(es2)) => {
                if es1.len() != es2.len() {
                    return false;
                }
                es1.iter()
                    .zip(es2.iter())
                    .all(|(a, b)| self.types_equal(*a, *b))
            }
            (TypeKind::Array(e1, s1), TypeKind::Array(e2, s2)) => {
                s1 == s2 && self.types_equal(*e1, *e2)
            }
            (TypeKind::Slice(e1), TypeKind::Slice(e2)) => self.types_equal(*e1, *e2),
            (TypeKind::Ref(i1, m1), TypeKind::Ref(i2, m2)) => {
                m1 == m2 && self.types_equal(*i1, *i2)
            }
            _ => false,
        }
    }

    /// Add a substitution
    pub fn add_substitution(&mut self, var: VarId, ty: TypeId) {
        self.substitutions.insert(var, ty);
    }
}

/// Generic parameter context for tracking bounds
#[derive(Debug, Default)]
pub struct GenericContext {
    /// Generic parameters in scope
    params: HashMap<DefId, GenericParamInfo>,
    /// Parameter order (for error messages)
    param_order: Vec<DefId>,
}

impl GenericContext {
    /// Create a new generic context
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a generic parameter
    pub fn add_param(&mut self, def_id: DefId, name: String, bounds: Vec<TraitBound>) {
        self.params.insert(
            def_id,
            GenericParamInfo {
                name,
                bounds,
                def_id,
            },
        );
        self.param_order.push(def_id);
    }

    /// Get a generic parameter by DefId
    pub fn get_param(&self, def_id: DefId) -> Option<&GenericParamInfo> {
        self.params.get(&def_id)
    }

    /// Get bounds for a generic parameter
    pub fn get_bounds(&self, def_id: DefId) -> Option<&[TraitBound]> {
        self.params.get(&def_id).map(|p| p.bounds.as_slice())
    }

    /// Check if a type parameter has a specific bound
    pub fn has_bound(&self, def_id: DefId, bound: &TraitBound) -> bool {
        if let Some(param) = self.params.get(&def_id) {
            param.bounds.iter().any(|b| b.trait_id == bound.trait_id)
        } else {
            false
        }
    }

    /// Get all parameters
    pub fn params(&self) -> &[DefId] {
        &self.param_order
    }

    /// Clear all parameters
    pub fn clear(&mut self) {
        self.params.clear();
        self.param_order.clear();
    }
}

/// Information about a generic parameter
#[derive(Debug, Clone)]
pub struct GenericParamInfo {
    pub name: String,
    pub bounds: Vec<TraitBound>,
    pub def_id: DefId,
}

/// Where clause checker
pub struct WhereClauseChecker<'tcx> {
    generic_ctx: &'tcx GenericContext,
    trait_ctx: &'tcx TraitContext,
    tcx: &'tcx TypeContext,
}

impl<'tcx> WhereClauseChecker<'tcx> {
    /// Create a new where clause checker
    pub fn new(
        generic_ctx: &'tcx GenericContext,
        trait_ctx: &'tcx TraitContext,
        tcx: &'tcx TypeContext,
    ) -> Self {
        Self {
            generic_ctx,
            trait_ctx,
            tcx,
        }
    }

    /// Check if a where clause is satisfied
    pub fn check_where_clause(
        &self,
        ty: TypeId,
        bounds: &[TraitBound],
        span: Span,
    ) -> Result<(), Vec<Box<Diagnostic>>> {
        let mut errors = Vec::new();
        let _solver = ConstraintSolver::new(self.trait_ctx, self.tcx);

        for bound in bounds {
            // Check if the type is a generic parameter
            if let TypeKind::Param(def_id) = self.tcx.type_kind(ty) {
                // For generic parameters, check declared bounds
                if !self.generic_ctx.has_bound(*def_id, bound) {
                    errors.push(Box::new(
                        Diagnostic::error(
                            format!(
                                "the type parameter `{}` does not satisfy the bound `{}`",
                                self.param_name(*def_id),
                                self.trait_name(bound.trait_id)
                            ),
                            span,
                        )
                        .with_error_code(ErrorCode::TraitBoundNotSatisfied),
                    ));
                }
            } else {
                // For concrete types, check implementations
                if !self.trait_ctx.implements(ty, bound, self.tcx) {
                    errors.push(Box::new(
                        Diagnostic::error(
                            format!(
                                "the trait bound `{}: {}` is not satisfied",
                                self.tcx.type_to_string(ty),
                                self.trait_name(bound.trait_id)
                            ),
                            span,
                        )
                        .with_error_code(ErrorCode::TraitBoundNotSatisfied),
                    ));
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn param_name(&self, def_id: DefId) -> String {
        self.generic_ctx
            .get_param(def_id)
            .map(|p| p.name.clone())
            .unwrap_or_else(|| format!("'p{}", def_id.0))
    }

    fn trait_name(&self, trait_id: DefId) -> String {
        self.trait_ctx
            .get_trait(trait_id)
            .map(|t| t.name.clone())
            .unwrap_or_else(|| "<unknown>".to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{EffectSet, Mutability};

    #[test]
    fn test_constraint_set() {
        let mut constraints = ConstraintSet::new();

        constraints.add(Constraint::TypeEqual {
            expected: TypeId::INT,
            found: TypeId::INT,
            span: Span::default(),
        });

        assert!(!constraints.is_empty());
        assert_eq!(constraints.constraints().len(), 1);
    }

    #[test]
    fn test_generic_context() {
        let mut ctx = GenericContext::new();

        ctx.add_param(DefId(1), "T".to_string(), vec![]);
        ctx.add_param(DefId(2), "U".to_string(), vec![]);

        assert!(ctx.get_param(DefId(1)).is_some());
        assert_eq!(ctx.params().len(), 2);
    }

    #[test]
    fn test_subtype_covariant_immutable_ref() {
        let trait_ctx = TraitContext::new();
        let mut tcx = TypeContext::new();

        let int_ref = tcx.mk_ref(TypeId::INT, Mutability::Immutable);
        let int_ref2 = tcx.mk_ref(TypeId::INT, Mutability::Immutable);
        let mut solver = ConstraintSolver::new(&trait_ctx, &tcx);

        let mut constraints = ConstraintSet::new();
        constraints.add(Constraint::Subtype {
            sub: int_ref,
            sup: int_ref2,
            span: Span::default(),
        });

        assert!(solver.solve(&constraints).is_ok());
    }

    #[test]
    fn test_subtype_invariant_mutable_ref() {
        let trait_ctx = TraitContext::new();
        let mut tcx = TypeContext::new();

        let int_ref_mut = tcx.mk_ref(TypeId::INT, Mutability::Mutable);
        let bool_ref_mut = tcx.mk_ref(TypeId::BOOL, Mutability::Mutable);
        let mut solver = ConstraintSolver::new(&trait_ctx, &tcx);

        let mut constraints = ConstraintSet::new();
        constraints.add(Constraint::Subtype {
            sub: int_ref_mut,
            sup: bool_ref_mut,
            span: Span::default(),
        });

        assert!(solver.solve(&constraints).is_err());
    }

    #[test]
    fn test_subtype_function_variance() {
        let trait_ctx = TraitContext::new();
        let mut tcx = TypeContext::new();

        let f1 = tcx.mk_function(vec![TypeId::INT], TypeId::INT, EffectSet::empty());
        let f2 = tcx.mk_function(vec![TypeId::INT], TypeId::INT, EffectSet::empty());
        let mut solver = ConstraintSolver::new(&trait_ctx, &tcx);

        let mut constraints = ConstraintSet::new();
        constraints.add(Constraint::Subtype {
            sub: f1,
            sup: f2,
            span: Span::default(),
        });

        assert!(solver.solve(&constraints).is_ok());
    }
}
