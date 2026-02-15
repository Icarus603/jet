//! Type unification for Hindley-Milner type inference
//!
//! This module implements the unification algorithm that solves type constraints
//! by finding the most general unifier for two types.

use crate::types::{
    EffectSet, Level, Mutability, TypeContext, TypeId, TypeKind, TypeVarKind, VarId,
};
use jet_diagnostics::{Diagnostic, ErrorCode, Label, Span};
use std::collections::HashSet;

/// Errors that can occur during unification
#[derive(Debug)]
pub enum UnifyError {
    /// Type mismatch between two types
    TypeMismatch {
        expected: TypeId,
        found: TypeId,
        span: Span,
    },
    /// Occurs check failure (infinite type)
    OccursCheck { var: VarId, ty: TypeId, span: Span },
    /// Arity mismatch (e.g., different number of function parameters)
    ArityMismatch {
        expected: usize,
        found: usize,
        span: Span,
    },
    /// Cannot unify mutable with immutable references
    MutabilityMismatch {
        expected: Mutability,
        found: Mutability,
        span: Span,
    },
    /// Cannot unify generic parameters
    CannotUnifyGeneric { def_id: u32, span: Span },
}

impl UnifyError {
    /// Convert the unification error to a diagnostic
    pub fn to_diagnostic(&self, tcx: &TypeContext) -> Diagnostic {
        match self {
            UnifyError::TypeMismatch {
                expected,
                found,
                span,
            } => {
                let mut diag = Diagnostic::error(
                    format!(
                        "type mismatch: expected `{}`, found `{}`",
                        tcx.type_to_string(*expected),
                        tcx.type_to_string(*found)
                    ),
                    *span,
                )
                .with_error_code(ErrorCode::TypeMismatch);

                diag.labels.push(Label::secondary(
                    *span,
                    format!("expected `{}`", tcx.type_to_string(*expected)),
                ));

                diag
            }
            UnifyError::OccursCheck { var, ty, span } => Diagnostic::error(
                format!(
                    "infinite type: type variable `'t{}` occurs in `{}`",
                    var,
                    tcx.type_to_string(*ty)
                ),
                *span,
            )
            .with_error_code(ErrorCode::InfiniteType)
            .with_note("this typically means a recursive type was inferred but not declared"),
            UnifyError::ArityMismatch {
                expected,
                found,
                span,
            } => Diagnostic::error(
                format!(
                    "arity mismatch: expected {} argument{}, found {}",
                    expected,
                    if *expected == 1 { "" } else { "s" },
                    found
                ),
                *span,
            )
            .with_error_code(ErrorCode::ArityMismatch),
            UnifyError::MutabilityMismatch {
                expected,
                found,
                span,
            } => {
                let expected_str = match expected {
                    Mutability::Immutable => "immutable",
                    Mutability::Mutable => "mutable",
                };
                let found_str = match found {
                    Mutability::Immutable => "immutable",
                    Mutability::Mutable => "mutable",
                };
                Diagnostic::error(
                    format!(
                        "mutability mismatch: expected {} reference, found {} reference",
                        expected_str, found_str
                    ),
                    *span,
                )
                .with_error_code(ErrorCode::MutabilityMismatch)
            }
            UnifyError::CannotUnifyGeneric { def_id, span } => Diagnostic::error(
                format!("cannot unify generic parameter `'p{}`", def_id),
                *span,
            )
            .with_error_code(ErrorCode::CannotUnifyGeneric),
        }
    }
}

/// Unification context for solving type constraints
pub struct Unifier<'tcx> {
    tcx: &'tcx mut TypeContext,
}

impl<'tcx> Unifier<'tcx> {
    /// Create a new unifier
    pub fn new(tcx: &'tcx mut TypeContext) -> Self {
        Self { tcx }
    }

    /// Follow type variable links to find the actual type
    pub fn follow_links(&self, id: TypeId) -> TypeId {
        match self.tcx.type_kind(id) {
            TypeKind::Var(var_id) => {
                if let Some(var) = self.tcx.get_var(*var_id) {
                    match &var.kind {
                        TypeVarKind::Link(linked) => self.follow_links(*linked),
                        _ => id,
                    }
                } else {
                    id
                }
            }
            _ => id,
        }
    }

    /// Check if a type variable occurs in a type (occurs check)
    pub fn occurs_in(&self, var_id: VarId, ty: TypeId) -> bool {
        let ty = self.follow_links(ty);

        match self.tcx.type_kind(ty) {
            TypeKind::Var(v) => *v == var_id,
            TypeKind::Tuple(elements) => elements.iter().any(|e| self.occurs_in(var_id, *e)),
            TypeKind::Array(elem, _) => self.occurs_in(var_id, *elem),
            TypeKind::Slice(elem) => self.occurs_in(var_id, *elem),
            TypeKind::Function { params, ret, .. } => {
                params.iter().any(|p| self.occurs_in(var_id, *p)) || self.occurs_in(var_id, *ret)
            }
            TypeKind::Ref(inner, _) | TypeKind::RawPtr(inner, _) => self.occurs_in(var_id, *inner),
            TypeKind::Channel(elem) => self.occurs_in(var_id, *elem),
            TypeKind::Async(inner) => self.occurs_in(var_id, *inner),
            _ => false,
        }
    }

    /// Unify two types, updating the type context
    pub fn unify(&mut self, t1: TypeId, t2: TypeId, span: Span) -> Result<(), UnifyError> {
        let t1 = self.follow_links(t1);
        let t2 = self.follow_links(t2);

        if t1 == t2 {
            return Ok(());
        }
        // `never` can coerce to any type.
        if t1 == TypeId::NEVER || t2 == TypeId::NEVER {
            return Ok(());
        }

        match (
            self.tcx.type_kind(t1).clone(),
            self.tcx.type_kind(t2).clone(),
        ) {
            // Unify type variables
            (TypeKind::Var(v1), TypeKind::Var(v2)) => {
                // Occurs check
                if self.occurs_in(v1, t2) {
                    return Err(UnifyError::OccursCheck {
                        var: v1,
                        ty: t2,
                        span,
                    });
                }

                // Link lower level to higher level (for generalization)
                let level1 = self.tcx.var_level(v1);
                let level2 = self.tcx.var_level(v2);

                if level1 > level2 {
                    self.tcx.set_var_kind(v1, TypeVarKind::Link(t2));
                } else {
                    self.tcx.set_var_kind(v2, TypeVarKind::Link(t1));
                }
                Ok(())
            }

            // Unify type variable with concrete type
            (TypeKind::Var(v), other) | (other, TypeKind::Var(v)) => {
                let concrete = if matches!(other, TypeKind::Var(_)) {
                    t1
                } else {
                    t2
                };

                if self.occurs_in(v, concrete) {
                    return Err(UnifyError::OccursCheck {
                        var: v,
                        ty: concrete,
                        span,
                    });
                }

                self.tcx.set_var_kind(v, TypeVarKind::Link(concrete));
                Ok(())
            }

            // Unify primitive types (they must be equal, checked above)
            (k1, k2) if k1 == k2 => Ok(()),

            // Unify tuples
            (TypeKind::Tuple(ts1), TypeKind::Tuple(ts2)) => {
                if ts1.len() != ts2.len() {
                    return Err(UnifyError::ArityMismatch {
                        expected: ts1.len(),
                        found: ts2.len(),
                        span,
                    });
                }
                for (a, b) in ts1.iter().zip(ts2.iter()) {
                    self.unify(*a, *b, span)?;
                }
                Ok(())
            }

            // Unify arrays
            (TypeKind::Array(e1, s1), TypeKind::Array(e2, s2)) => {
                // Size 0 is used as "unspecified length" in parts of the current frontend.
                if s1 != s2 && s1 != 0 && s2 != 0 {
                    return Err(UnifyError::TypeMismatch {
                        expected: t1,
                        found: t2,
                        span,
                    });
                }
                self.unify(e1, e2, span)
            }

            // Unify slices
            (TypeKind::Slice(e1), TypeKind::Slice(e2)) => self.unify(e1, e2, span),

            // Unify functions
            (
                TypeKind::Function {
                    params: p1,
                    ret: r1,
                    effects: e1,
                },
                TypeKind::Function {
                    params: p2,
                    ret: r2,
                    effects: e2,
                },
            ) => {
                if p1.len() != p2.len() {
                    return Err(UnifyError::ArityMismatch {
                        expected: p1.len(),
                        found: p2.len(),
                        span,
                    });
                }
                for (a, b) in p1.iter().zip(p2.iter()) {
                    self.unify(*a, *b, span)?;
                }
                self.unify(r1, r2, span)?;
                self.unify_effect_sets(&e1, &e2, t1, t2, span)?;
                Ok(())
            }

            // Unify references
            (TypeKind::Ref(i1, m1), TypeKind::Ref(i2, m2)) => {
                if m1 != m2 {
                    return Err(UnifyError::MutabilityMismatch {
                        expected: m1,
                        found: m2,
                        span,
                    });
                }
                self.unify(i1, i2, span)
            }

            // Unify raw pointers
            (TypeKind::RawPtr(i1, m1), TypeKind::RawPtr(i2, m2)) => {
                if m1 != m2 {
                    return Err(UnifyError::MutabilityMismatch {
                        expected: m1,
                        found: m2,
                        span,
                    });
                }
                self.unify(i1, i2, span)
            }

            // Unify channels
            (TypeKind::Channel(e1), TypeKind::Channel(e2)) => self.unify(e1, e2, span),

            // Unify async types
            (TypeKind::Async(i1), TypeKind::Async(i2)) => self.unify(i1, i2, span),

            // Type mismatch
            _ => Err(UnifyError::TypeMismatch {
                expected: t1,
                found: t2,
                span,
            }),
        }
    }

    fn unify_effect_sets(
        &mut self,
        e1: &EffectSet,
        e2: &EffectSet,
        t1: TypeId,
        t2: TypeId,
        span: Span,
    ) -> Result<(), UnifyError> {
        if e1.effects.len() != e2.effects.len() {
            return Err(UnifyError::TypeMismatch {
                expected: t1,
                found: t2,
                span,
            });
        }

        for effect1 in &e1.effects {
            let Some(effect2) = e2
                .effects
                .iter()
                .find(|effect2| effect2.effect == effect1.effect)
            else {
                return Err(UnifyError::TypeMismatch {
                    expected: t1,
                    found: t2,
                    span,
                });
            };

            if effect1.args.len() != effect2.args.len() {
                return Err(UnifyError::TypeMismatch {
                    expected: t1,
                    found: t2,
                    span,
                });
            }

            for (arg1, arg2) in effect1.args.iter().zip(effect2.args.iter()) {
                self.unify(*arg1, *arg2, span)?;
            }
        }

        Ok(())
    }

    /// Update the level of all unbound type variables in a type
    pub fn update_level(&mut self, ty: TypeId, level: Level) {
        let ty = self.follow_links(ty);

        match self.tcx.type_kind(ty) {
            TypeKind::Var(var_id) => {
                if let Some(var) = self.tcx.get_var(*var_id) {
                    if let TypeVarKind::Unbound { level: old_level } = var.kind {
                        if old_level > level {
                            self.tcx
                                .set_var_kind(*var_id, TypeVarKind::Unbound { level });
                        }
                    }
                }
            }
            TypeKind::Tuple(elements) => {
                let elems: Vec<_> = elements.clone();
                for elem in elems {
                    self.update_level(elem, level);
                }
            }
            TypeKind::Array(elem, _) => {
                let e = *elem;
                self.update_level(e, level);
            }
            TypeKind::Slice(elem) => {
                let e = *elem;
                self.update_level(e, level);
            }
            TypeKind::Function { params, ret, .. } => {
                let ps: Vec<_> = params.clone();
                let r = *ret;
                for param in ps {
                    self.update_level(param, level);
                }
                self.update_level(r, level);
            }
            TypeKind::Ref(inner, _) | TypeKind::RawPtr(inner, _) => {
                let i = *inner;
                self.update_level(i, level);
            }
            TypeKind::Channel(elem) => {
                let e = *elem;
                self.update_level(e, level);
            }
            TypeKind::Async(inner) => {
                let i = *inner;
                self.update_level(i, level);
            }
            _ => {}
        }
    }

    /// Collect all free (unbound) type variables in a type
    pub fn free_vars(&self, ty: TypeId) -> HashSet<VarId> {
        let mut vars = HashSet::new();
        self.collect_free_vars(ty, &mut vars);
        vars
    }

    fn collect_free_vars(&self, ty: TypeId, vars: &mut HashSet<VarId>) {
        let ty = self.follow_links(ty);

        match self.tcx.type_kind(ty) {
            TypeKind::Var(var_id) => {
                if let Some(var) = self.tcx.get_var(*var_id) {
                    if matches!(var.kind, TypeVarKind::Unbound { .. }) {
                        vars.insert(*var_id);
                    }
                }
            }
            TypeKind::Tuple(elements) => {
                let elems: Vec<_> = elements.clone();
                for elem in elems {
                    self.collect_free_vars(elem, vars);
                }
            }
            TypeKind::Array(elem, _) => {
                let e = *elem;
                self.collect_free_vars(e, vars);
            }
            TypeKind::Slice(elem) => {
                let e = *elem;
                self.collect_free_vars(e, vars);
            }
            TypeKind::Function { params, ret, .. } => {
                let ps: Vec<_> = params.clone();
                for param in ps {
                    self.collect_free_vars(param, vars);
                }
                let r = *ret;
                self.collect_free_vars(r, vars);
            }
            TypeKind::Ref(inner, _) | TypeKind::RawPtr(inner, _) => {
                let i = *inner;
                self.collect_free_vars(i, vars);
            }
            TypeKind::Channel(elem) => {
                let e = *elem;
                self.collect_free_vars(e, vars);
            }
            TypeKind::Async(inner) => {
                let i = *inner;
                self.collect_free_vars(i, vars);
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{DefId, EffectInstance, EffectSet, TypeContext};

    #[test]
    fn test_unify_same_type() {
        let mut tcx = TypeContext::new();
        let mut unifier = Unifier::new(&mut tcx);
        let span = Span::new(0, 1);

        assert!(unifier.unify(TypeId::INT, TypeId::INT, span).is_ok());
    }

    #[test]
    fn test_unify_type_mismatch() {
        let mut tcx = TypeContext::new();
        let mut unifier = Unifier::new(&mut tcx);
        let span = Span::new(0, 1);

        let result = unifier.unify(TypeId::INT, TypeId::BOOL, span);
        assert!(matches!(result, Err(UnifyError::TypeMismatch { .. })));
    }

    #[test]
    fn test_unify_type_var_with_concrete() {
        let mut tcx = TypeContext::new();
        let var = tcx.fresh_var(0);
        let mut unifier = Unifier::new(&mut tcx);
        let span = Span::new(0, 1);

        assert!(unifier.unify(var, TypeId::INT, span).is_ok());

        // After unification, the variable should link to int
        let resolved = unifier.follow_links(var);
        assert_eq!(resolved, TypeId::INT);
    }

    #[test]
    fn test_unify_two_type_vars() {
        let mut tcx = TypeContext::new();
        let var1 = tcx.fresh_var(0);
        let var2 = tcx.fresh_var(0);
        let mut unifier = Unifier::new(&mut tcx);
        let span = Span::new(0, 1);

        assert!(unifier.unify(var1, var2, span).is_ok());

        // After unification, both should link to the same thing
        let resolved1 = unifier.follow_links(var1);
        let resolved2 = unifier.follow_links(var2);
        assert_eq!(resolved1, resolved2);
    }

    #[test]
    fn test_occurs_check() {
        let mut tcx = TypeContext::new();
        let var = tcx.fresh_var(0);
        // Create a recursive type: var = (var, int) - should fail occurs check
        let tuple = tcx.mk_tuple(vec![var, TypeId::INT]);

        let mut unifier = Unifier::new(&mut tcx);
        let span = Span::new(0, 1);
        let result = unifier.unify(var, tuple, span);

        assert!(matches!(result, Err(UnifyError::OccursCheck { .. })));
    }

    #[test]
    fn test_unify_tuples() {
        let mut tcx = TypeContext::new();
        let t1 = tcx.mk_tuple(vec![TypeId::INT, TypeId::BOOL]);
        let t2 = tcx.mk_tuple(vec![TypeId::INT, TypeId::BOOL]);

        let mut unifier = Unifier::new(&mut tcx);
        let span = Span::new(0, 1);

        assert!(unifier.unify(t1, t2, span).is_ok());
    }

    #[test]
    fn test_unify_tuple_arity_mismatch() {
        let mut tcx = TypeContext::new();
        let t1 = tcx.mk_tuple(vec![TypeId::INT, TypeId::BOOL]);
        let t2 = tcx.mk_tuple(vec![TypeId::INT]);

        let mut unifier = Unifier::new(&mut tcx);
        let span = Span::new(0, 1);

        let result = unifier.unify(t1, t2, span);
        assert!(matches!(
            result,
            Err(UnifyError::ArityMismatch {
                expected: 2,
                found: 1,
                ..
            })
        ));
    }

    #[test]
    fn test_unify_functions() {
        let mut tcx = TypeContext::new();
        let f1 = tcx.mk_function(
            vec![TypeId::INT, TypeId::INT],
            TypeId::BOOL,
            EffectSet::empty(),
        );
        let f2 = tcx.mk_function(
            vec![TypeId::INT, TypeId::INT],
            TypeId::BOOL,
            EffectSet::empty(),
        );

        let mut unifier = Unifier::new(&mut tcx);
        let span = Span::new(0, 1);

        assert!(unifier.unify(f1, f2, span).is_ok());
    }

    #[test]
    fn test_unify_function_with_type_var() {
        let mut tcx = TypeContext::new();
        let var = tcx.fresh_var(0);
        // fn(int) -> var  unified with  fn(int) -> bool
        let f1 = tcx.mk_function(vec![TypeId::INT], var, EffectSet::empty());
        let f2 = tcx.mk_function(vec![TypeId::INT], TypeId::BOOL, EffectSet::empty());

        let mut unifier = Unifier::new(&mut tcx);
        let span = Span::new(0, 1);

        assert!(unifier.unify(f1, f2, span).is_ok());

        // The variable should now be linked to bool
        let resolved = unifier.follow_links(var);
        assert_eq!(resolved, TypeId::BOOL);
    }

    #[test]
    fn test_unify_function_effect_args() {
        let mut tcx = TypeContext::new();
        let var = tcx.fresh_var(0);

        let f1 = tcx.mk_function(
            vec![TypeId::INT],
            TypeId::BOOL,
            EffectSet::singleton(EffectInstance {
                effect: DefId(1),
                args: vec![var],
            }),
        );
        let f2 = tcx.mk_function(
            vec![TypeId::INT],
            TypeId::BOOL,
            EffectSet::singleton(EffectInstance {
                effect: DefId(1),
                args: vec![TypeId::STRING],
            }),
        );

        let mut unifier = Unifier::new(&mut tcx);
        let span = Span::new(0, 1);
        assert!(unifier.unify(f1, f2, span).is_ok());
        assert_eq!(unifier.follow_links(var), TypeId::STRING);
    }

    #[test]
    fn test_unify_function_effect_mismatch() {
        let mut tcx = TypeContext::new();

        let f1 = tcx.mk_function(
            vec![TypeId::INT],
            TypeId::BOOL,
            EffectSet::singleton(EffectInstance {
                effect: DefId(1),
                args: vec![],
            }),
        );
        let f2 = tcx.mk_function(
            vec![TypeId::INT],
            TypeId::BOOL,
            EffectSet::singleton(EffectInstance {
                effect: DefId(2),
                args: vec![],
            }),
        );

        let mut unifier = Unifier::new(&mut tcx);
        let span = Span::new(0, 1);
        let result = unifier.unify(f1, f2, span);
        assert!(matches!(result, Err(UnifyError::TypeMismatch { .. })));
    }

    #[test]
    fn test_free_vars() {
        let mut tcx = TypeContext::new();
        let var1 = tcx.fresh_var(0);
        let var2 = tcx.fresh_var(0);
        let tuple = tcx.mk_tuple(vec![var1, var2, TypeId::INT]);

        let unifier = Unifier::new(&mut tcx);
        let free = unifier.free_vars(tuple);

        assert!(free.contains(&0)); // var1
        assert!(free.contains(&1)); // var2
        assert_eq!(free.len(), 2);
    }
}
