//! Effect inference for the Jet language.
//!
//! This module implements effect inference, which determines what effects
//! an expression can perform. It supports effect polymorphism and handles
//! the propagation of effects through function calls.

use std::collections::HashMap;

use crate::effect::{DefId, EffectSet, EffectVar};
use crate::error::{EffectError, EffectResult};

/// Effect inference engine.
#[derive(Debug, Clone, Default)]
pub struct EffectInference {
    /// Maps expression IDs to their inferred effects.
    expr_effects: HashMap<DefId, EffectSet>,
    /// Maps effect variables to their solutions.
    var_solutions: HashMap<EffectVar, EffectSet>,
    /// Substitution for effect variables.
    #[allow(dead_code)]
    substitutions: HashMap<EffectVar, EffectVar>,
    /// Next expression ID.
    next_expr_id: u32,
}

/// An effect constraint for inference.
#[derive(Debug, Clone)]
pub enum EffectConstraint {
    /// An expression has exactly these effects.
    Exact(DefId, EffectSet),
    /// An expression's effects include at least these.
    AtLeast(DefId, EffectSet),
    /// An expression's effects are a subset of these.
    AtMost(DefId, EffectSet),
    /// Two expressions have the same effects.
    Equal(DefId, DefId),
    /// Effect variable equals a set.
    VarEquals(EffectVar, EffectSet),
    /// Effect variable is at least a set.
    VarAtLeast(EffectVar, EffectSet),
    /// Effect variable is at most a set.
    VarAtMost(EffectVar, EffectSet),
}

/// The solution to an effect inference problem.
#[derive(Debug, Clone)]
pub struct EffectSolution {
    /// The inferred effects for each expression.
    pub effects: HashMap<DefId, EffectSet>,
    /// Solutions for effect variables.
    pub var_solutions: HashMap<EffectVar, EffectSet>,
}

impl EffectInference {
    /// Creates a new effect inference engine.
    pub fn new() -> Self {
        Self {
            expr_effects: HashMap::new(),
            var_solutions: HashMap::new(),
            substitutions: HashMap::new(),
            next_expr_id: 0,
        }
    }

    /// Creates a fresh expression ID.
    pub fn fresh_expr_id(&mut self) -> DefId {
        let id = DefId(self.next_expr_id);
        self.next_expr_id += 1;
        id
    }

    /// Infers effects for an expression from its subexpressions.
    pub fn infer_from_subexprs(&mut self, expr_id: DefId, subexprs: &[DefId]) -> EffectSet {
        let mut effects = EffectSet::empty();
        for subexpr in subexprs {
            if let Some(sub_effects) = self.expr_effects.get(subexpr) {
                effects.union(sub_effects);
            }
        }
        self.expr_effects.insert(expr_id, effects.clone());
        effects
    }

    /// Infers effects for a function call.
    pub fn infer_call(
        &mut self,
        expr_id: DefId,
        func_effects: &EffectSet,
        arg_effects: &[EffectSet],
    ) -> EffectSet {
        let mut effects = func_effects.clone();
        for arg in arg_effects {
            effects.union(arg);
        }
        self.expr_effects.insert(expr_id, effects.clone());
        effects
    }

    /// Infers effects for a conditional (if/match).
    pub fn infer_conditional(
        &mut self,
        expr_id: DefId,
        cond_effects: &EffectSet,
        branch_effects: &[EffectSet],
    ) -> EffectSet {
        let mut effects = cond_effects.clone();
        for branch in branch_effects {
            effects.union(branch);
        }
        self.expr_effects.insert(expr_id, effects.clone());
        effects
    }

    /// Infers effects for a loop.
    pub fn infer_loop(
        &mut self,
        expr_id: DefId,
        cond_effects: &EffectSet,
        body_effects: &EffectSet,
    ) -> EffectSet {
        let mut effects = cond_effects.clone();
        effects.union(body_effects);
        // Loops can diverge
        self.expr_effects.insert(expr_id, effects.clone());
        effects
    }

    /// Infers effects for a lambda/closure.
    pub fn infer_lambda(
        &mut self,
        expr_id: DefId,
        body_effects: &EffectSet,
        declared_effects: &EffectSet,
    ) -> EffectResult<EffectSet> {
        // Check that body effects are covered by declared effects
        let unhandled: Vec<_> = body_effects
            .iter()
            .filter(|e| !declared_effects.contains(e))
            .cloned()
            .collect();

        if !unhandled.is_empty() {
            // Lambda body has effects not in its signature
            // This is an error
            return Err(EffectError::InferenceFailure {
                message: format!(
                    "lambda body has unhandled effects: {}",
                    unhandled
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                span: jet_diagnostics::Span::default(),
            });
        }

        // Lambda itself is pure (effects are latent)
        let effects = EffectSet::empty();
        self.expr_effects.insert(expr_id, effects.clone());
        Ok(effects)
    }

    /// Generalizes effects into a polymorphic scheme.
    pub fn generalize(&self, effects: &EffectSet, bound_vars: &[EffectVar]) -> EffectScheme {
        // Find free effect variables
        let _free_vars: Vec<_> = effects
            .iter()
            .filter_map(|_e| {
                // Check if this effect contains variables
                // For now, simplified - just check for polymorphic effects
                if effects.is_polymorphic() {
                    effects.effect_var()
                } else {
                    None
                }
            })
            .collect();

        EffectScheme {
            bound_vars: bound_vars.to_vec(),
            effects: effects.clone(),
        }
    }

    /// Instantiates a polymorphic effect scheme with fresh variables.
    pub fn instantiate(&mut self, scheme: &EffectScheme) -> EffectSet {
        // Create fresh variables for bound variables
        let mut subst = HashMap::new();
        for var in &scheme.bound_vars {
            let fresh = EffectVar(self.next_expr_id);
            self.next_expr_id += 1;
            subst.insert(*var, fresh);
        }

        // Apply substitution to effects
        self.apply_subst_to_set(&scheme.effects, &subst)
    }

    /// Applies a substitution to an effect set.
    fn apply_subst_to_set(
        &self,
        set: &EffectSet,
        subst: &HashMap<EffectVar, EffectVar>,
    ) -> EffectSet {
        if let Some(var) = set.effect_var() {
            if let Some(&new_var) = subst.get(&var) {
                return EffectSet::polymorphic(new_var);
            }
        }
        set.clone()
    }

    /// Solves a set of effect constraints.
    pub fn solve_constraints(
        &mut self,
        constraints: &[EffectConstraint],
    ) -> EffectResult<EffectSolution> {
        for constraint in constraints {
            self.solve_constraint(constraint)?;
        }

        Ok(EffectSolution {
            effects: self.expr_effects.clone(),
            var_solutions: self.var_solutions.clone(),
        })
    }

    /// Solves a single constraint.
    fn solve_constraint(&mut self, constraint: &EffectConstraint) -> EffectResult<()> {
        match constraint {
            EffectConstraint::Exact(expr, effects) => {
                self.expr_effects.insert(*expr, effects.clone());
            }

            EffectConstraint::AtLeast(expr, min_effects) => {
                let current = self
                    .expr_effects
                    .entry(*expr)
                    .or_insert_with(EffectSet::empty);
                let mut new_effects = current.clone();
                new_effects.union(min_effects);
                *current = new_effects;
            }

            EffectConstraint::AtMost(expr, max_effects) => {
                let current = self
                    .expr_effects
                    .entry(*expr)
                    .or_insert_with(EffectSet::empty);
                // Check that current is subset of max
                if !max_effects.contains_set(current) {
                    return Err(EffectError::InferenceFailure {
                        message: "effect constraint violated: effects exceed maximum".to_string(),
                        span: jet_diagnostics::Span::default(),
                    });
                }
            }

            EffectConstraint::Equal(expr1, expr2) => {
                let e1 = self
                    .expr_effects
                    .get(expr1)
                    .cloned()
                    .unwrap_or_else(EffectSet::empty);
                let e2 = self
                    .expr_effects
                    .get(expr2)
                    .cloned()
                    .unwrap_or_else(EffectSet::empty);
                let merged = e1.merged(&e2);
                self.expr_effects.insert(*expr1, merged.clone());
                self.expr_effects.insert(*expr2, merged);
            }

            EffectConstraint::VarEquals(var, effects) => {
                self.var_solutions.insert(*var, effects.clone());
            }

            EffectConstraint::VarAtLeast(var, min_effects) => {
                let current = self
                    .var_solutions
                    .entry(*var)
                    .or_insert_with(EffectSet::empty);
                let mut new_effects = current.clone();
                new_effects.union(min_effects);
                *current = new_effects;
            }

            EffectConstraint::VarAtMost(var, max_effects) => {
                let current = self
                    .var_solutions
                    .entry(*var)
                    .or_insert_with(EffectSet::empty);
                if !max_effects.contains_set(current) {
                    return Err(EffectError::InferenceFailure {
                        message: format!("effect variable {:?} exceeds maximum", var),
                        span: jet_diagnostics::Span::default(),
                    });
                }
            }
        }

        Ok(())
    }

    /// Gets the inferred effects for an expression.
    pub fn get_effects(&self, expr_id: DefId) -> Option<&EffectSet> {
        self.expr_effects.get(&expr_id)
    }

    /// Sets the effects for an expression.
    pub fn set_effects(&mut self, expr_id: DefId, effects: EffectSet) {
        self.expr_effects.insert(expr_id, effects);
    }

    /// Unifies two effect sets.
    pub fn unify(&mut self, set1: &EffectSet, set2: &EffectSet) -> EffectResult<EffectSet> {
        // If one is top, result is top
        if set1.is_top() || set2.is_top() {
            return Ok(EffectSet::top());
        }

        // Union of both sets
        let mut result = set1.clone();
        result.union(set2);

        // Handle polymorphic variables
        match (set1.effect_var(), set2.effect_var()) {
            (Some(v1), Some(v2)) if v1 == v2 => {
                // Same variable, keep it
            }
            (Some(v), _) | (_, Some(v)) => {
                // Different variables or one concrete
                // For now, just keep the variable
                if result.effect_var().is_none() {
                    result = EffectSet::polymorphic(v);
                }
            }
            _ => {}
        }

        Ok(result)
    }

    /// Propagates effects through a function call, handling polymorphism.
    pub fn propagate_call_effects(
        &mut self,
        callee_effects: &EffectSet,
        arg_effects: &[EffectSet],
        return_effects: &EffectSet,
    ) -> EffectSet {
        let mut result = callee_effects.clone();

        // Add argument effects
        for arg in arg_effects {
            result.union(arg);
        }

        // Add return effects
        result.union(return_effects);

        result
    }

    /// Performs effect subsumption check: can `sub` be used where `sup` is expected?
    pub fn subsumes(&self, sup: &EffectSet, sub: &EffectSet) -> bool {
        // sup subsumes sub if sub's effects are a subset of sup's effects
        sup.contains_set(sub)
    }

    /// Computes the least upper bound (join) of two effect sets.
    pub fn join(&self, set1: &EffectSet, set2: &EffectSet) -> EffectSet {
        set1.merged(set2)
    }

    /// Computes the greatest lower bound (meet) of two effect sets.
    pub fn meet(&self, set1: &EffectSet, set2: &EffectSet) -> EffectSet {
        // Intersection of effects
        let mut result = EffectSet::empty();
        for effect in set1.iter() {
            if set2.contains(effect) {
                result.insert(effect.clone());
            }
        }
        result
    }
}

/// A polymorphic effect scheme with bound variables.
#[derive(Debug, Clone)]
pub struct EffectScheme {
    /// The bound effect variables.
    pub bound_vars: Vec<EffectVar>,
    /// The effects (may contain bound variables).
    pub effects: EffectSet,
}

/// Effect variable substitution.
#[derive(Debug, Clone, Default)]
pub struct EffectSubst {
    /// Maps effect variables to their replacements.
    mapping: HashMap<EffectVar, EffectSet>,
}

impl EffectSubst {
    /// Creates a new empty substitution.
    pub fn new() -> Self {
        Self {
            mapping: HashMap::new(),
        }
    }

    /// Adds a mapping to the substitution.
    pub fn insert(&mut self, var: EffectVar, effects: EffectSet) {
        self.mapping.insert(var, effects);
    }

    /// Looks up a variable in the substitution.
    pub fn get(&self, var: EffectVar) -> Option<&EffectSet> {
        self.mapping.get(&var)
    }

    /// Applies the substitution to an effect set.
    pub fn apply(&self, effects: &EffectSet) -> EffectSet {
        if let Some(var) = effects.effect_var() {
            if let Some(subst) = self.mapping.get(&var) {
                return subst.clone();
            }
        }
        effects.clone()
    }

    /// Composes two substitutions: self after other.
    pub fn compose(&self, other: &EffectSubst) -> EffectSubst {
        let mut result = EffectSubst::new();

        // Apply other, then self
        for (var, effects) in &other.mapping {
            result.insert(*var, self.apply(effects));
        }

        // Add self's mappings that aren't in other
        for (var, effects) in &self.mapping {
            if !other.mapping.contains_key(var) {
                result.insert(*var, effects.clone());
            }
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::effect::BuiltinEffect;

    #[test]
    fn test_inference_fresh_id() {
        let mut inference = EffectInference::new();
        let id1 = inference.fresh_expr_id();
        let id2 = inference.fresh_expr_id();
        assert_ne!(id1, id2);
    }

    #[test]
    fn test_inference_from_subexprs() {
        let mut inference = EffectInference::new();

        let sub1 = inference.fresh_expr_id();
        let sub2 = inference.fresh_expr_id();
        let parent = inference.fresh_expr_id();

        inference.set_effects(sub1, EffectSet::singleton(BuiltinEffect::Io.instance()));
        inference.set_effects(sub2, EffectSet::singleton(BuiltinEffect::Async.instance()));

        let effects = inference.infer_from_subexprs(parent, &[sub1, sub2]);

        assert!(effects.contains(&BuiltinEffect::Io.instance()));
        assert!(effects.contains(&BuiltinEffect::Async.instance()));
        assert_eq!(effects.len(), 2);
    }

    #[test]
    fn test_inference_call() {
        let mut inference = EffectInference::new();

        let call_id = inference.fresh_expr_id();
        let func_effects = EffectSet::singleton(BuiltinEffect::Io.instance());
        let arg_effects = vec![
            EffectSet::singleton(BuiltinEffect::Async.instance()),
            EffectSet::empty(),
        ];

        let effects = inference.infer_call(call_id, &func_effects, &arg_effects);

        assert!(effects.contains(&BuiltinEffect::Io.instance()));
        assert!(effects.contains(&BuiltinEffect::Async.instance()));
    }

    #[test]
    fn test_inference_conditional() {
        let mut inference = EffectInference::new();

        let if_id = inference.fresh_expr_id();
        let cond = EffectSet::empty();
        let branches = vec![
            EffectSet::singleton(BuiltinEffect::Io.instance()),
            EffectSet::singleton(BuiltinEffect::Async.instance()),
        ];

        let effects = inference.infer_conditional(if_id, &cond, &branches);

        assert!(effects.contains(&BuiltinEffect::Io.instance()));
        assert!(effects.contains(&BuiltinEffect::Async.instance()));
    }

    #[test]
    fn test_inference_lambda_ok() {
        let mut inference = EffectInference::new();

        let lambda_id = inference.fresh_expr_id();
        let body_effects = EffectSet::singleton(BuiltinEffect::Io.instance());
        let declared = EffectSet::singleton(BuiltinEffect::Io.instance());

        let result = inference.infer_lambda(lambda_id, &body_effects, &declared);
        assert!(result.is_ok());
        assert!(result.unwrap().is_empty());
    }

    #[test]
    fn test_inference_lambda_error() {
        let mut inference = EffectInference::new();

        let lambda_id = inference.fresh_expr_id();
        let body_effects = EffectSet::singleton(BuiltinEffect::Io.instance());
        let declared = EffectSet::empty();

        let result = inference.infer_lambda(lambda_id, &body_effects, &declared);
        assert!(result.is_err());
    }

    #[test]
    fn test_unify() {
        let mut inference = EffectInference::new();

        let set1 = EffectSet::singleton(BuiltinEffect::Io.instance());
        let set2 = EffectSet::singleton(BuiltinEffect::Async.instance());

        let result = inference.unify(&set1, &set2).unwrap();
        assert!(result.contains(&BuiltinEffect::Io.instance()));
        assert!(result.contains(&BuiltinEffect::Async.instance()));
    }

    #[test]
    fn test_subsumes() {
        let inference = EffectInference::new();

        let io = EffectSet::singleton(BuiltinEffect::Io.instance());
        let async_eff = EffectSet::singleton(BuiltinEffect::Async.instance());
        let both = io.merged(&async_eff);

        assert!(inference.subsumes(&both, &io));
        assert!(inference.subsumes(&both, &async_eff));
        assert!(!inference.subsumes(&io, &both));
    }

    #[test]
    fn test_join() {
        let inference = EffectInference::new();

        let io = EffectSet::singleton(BuiltinEffect::Io.instance());
        let async_eff = EffectSet::singleton(BuiltinEffect::Async.instance());

        let joined = inference.join(&io, &async_eff);
        assert!(joined.contains(&BuiltinEffect::Io.instance()));
        assert!(joined.contains(&BuiltinEffect::Async.instance()));
    }

    #[test]
    fn test_meet() {
        let inference = EffectInference::new();

        let io = EffectSet::singleton(BuiltinEffect::Io.instance());
        let async_eff = EffectSet::singleton(BuiltinEffect::Async.instance());
        let both = io.merged(&async_eff);

        let meet1 = inference.meet(&both, &io);
        assert!(meet1.contains(&BuiltinEffect::Io.instance()));
        assert!(!meet1.contains(&BuiltinEffect::Async.instance()));

        let meet2 = inference.meet(&io, &async_eff);
        assert!(meet2.is_empty());
    }

    #[test]
    fn test_subst() {
        let mut subst = EffectSubst::new();
        let var = EffectVar(0);
        let io = EffectSet::singleton(BuiltinEffect::Io.instance());

        subst.insert(var, io.clone());

        let poly = EffectSet::polymorphic(var);
        let result = subst.apply(&poly);

        assert!(result.contains(&BuiltinEffect::Io.instance()));
    }

    #[test]
    fn test_solve_constraints() {
        let mut inference = EffectInference::new();

        let expr1 = inference.fresh_expr_id();
        let expr2 = inference.fresh_expr_id();

        let constraints = vec![
            EffectConstraint::Exact(expr1, EffectSet::singleton(BuiltinEffect::Io.instance())),
            EffectConstraint::Equal(expr1, expr2),
        ];

        let solution = inference.solve_constraints(&constraints).unwrap();

        assert!(solution
            .effects
            .get(&expr2)
            .unwrap()
            .contains(&BuiltinEffect::Io.instance()));
    }

    #[test]
    fn test_generalize_instantiate() {
        let mut inference = EffectInference::new();

        let io = EffectSet::singleton(BuiltinEffect::Io.instance());
        let var = EffectVar(0);
        let poly = EffectSet::polymorphic(var);
        let combined = io.merged(&poly);

        let scheme = inference.generalize(&combined, &[var]);
        assert_eq!(scheme.bound_vars.len(), 1);

        let instantiated = inference.instantiate(&scheme);
        // Should have a fresh variable
        assert!(instantiated.effect_var().is_some());
    }
}
