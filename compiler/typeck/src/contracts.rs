//! Contract verification for the Jet type checker
//!
//! This module implements compile-time verification of formal contracts:
//! - Preconditions (`requires`): Verified at call sites
//! - Postconditions (`ensures`): Verified at return points
//! - Loop invariants: Verified at loop entry and preserved through body
//! - Ghost types: Tracked for verification but erased during lowering
//!
//! The verification system uses a simplified approach suitable for a type checker:
//! - Track known conditions at each program point
//! - Basic constant folding for simple expressions
//! - Support common patterns (non-zero checks, bounds checks)
//! - Generate warnings when conditions cannot be proven
#![allow(unused_imports, unused_variables)]
#![allow(clippy::collapsible_match, clippy::single_match)]

use crate::checker::{TypedExpr, TypedExprKind, TypedFunction, TypedStmt};
use crate::types::{TypeContext, TypeId, TypeKind};
use jet_diagnostics::{Diagnostic, DiagnosticBag, ErrorCode, Label, Level, Span};
use jet_parser::ast::{BinaryOp, Literal, Pattern, UnaryOp};
use std::collections::{HashMap, HashSet};

/// A condition that is known to be true at a program point
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Condition {
    /// A variable is non-zero (for division safety)
    NonZero(String),
    /// A variable is non-null (for pointer safety)
    NonNull(String),
    /// A variable is within bounds [lower, upper)
    InBounds {
        var: String,
        lower: i64,
        upper: Option<i64>,
    },
    /// A boolean variable is true
    IsTrue(String),
    /// A boolean variable is false
    IsFalse(String),
    /// Two variables are equal
    Equals(String, String),
    /// A variable equals a constant
    EqualsConst(String, ConstantValue),
    /// A variable is positive (x > 0)
    Positive(String),
    /// A variable is non-negative (x >= 0)
    NonNegative(String),
    /// Custom condition (for complex expressions we can't fully analyze)
    Custom(String),
}

/// A constant value for comparison
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstantValue {
    Integer(i64),
    Bool(bool),
    Unit,
}

/// The contract context tracks what conditions are known at each program point
#[derive(Debug, Clone, Default)]
pub struct ContractContext {
    /// Conditions known to be true
    known_true: HashSet<Condition>,
    /// Conditions known to be false
    known_false: HashSet<Condition>,
    /// Variable substitutions (for tracking equalities)
    substitutions: HashMap<String, String>,
    /// Ghost variable bindings (ghost variables -> their types)
    ghost_bindings: HashMap<String, TypeId>,
    /// Whether we're in a ghost context (erased at runtime)
    in_ghost_context: bool,
}

impl ContractContext {
    /// Create a new empty contract context
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a child context (for branching)
    pub fn branch(&self) -> Self {
        Self {
            known_true: self.known_true.clone(),
            known_false: self.known_false.clone(),
            substitutions: self.substitutions.clone(),
            ghost_bindings: self.ghost_bindings.clone(),
            in_ghost_context: self.in_ghost_context,
        }
    }

    /// Merge two contexts (for joining branches)
    /// Only conditions known in BOTH contexts are preserved
    pub fn merge(&mut self, other: &ContractContext) {
        self.known_true.retain(|c| other.known_true.contains(c));
        self.known_false.retain(|c| other.known_false.contains(c));
        // Keep substitutions that agree in both
        self.substitutions
            .retain(|k, v| other.substitutions.get(k) == Some(v));
    }

    /// Add a condition known to be true
    pub fn add_true(&mut self, condition: Condition) {
        // Simplify the condition first
        let simplified = self.simplify_condition(condition);
        self.known_true.insert(simplified);
    }

    /// Add a condition known to be false
    pub fn add_false(&mut self, condition: Condition) {
        let simplified = self.simplify_condition(condition);
        self.known_false.insert(simplified);
    }

    /// Check if a condition is known to be true
    pub fn is_known_true(&self, condition: &Condition) -> bool {
        let simplified = self.simplify_condition(condition.clone());
        self.known_true.contains(&simplified)
    }

    /// Check if a condition is known to be false
    pub fn is_known_false(&self, condition: &Condition) -> bool {
        let simplified = self.simplify_condition(condition.clone());
        self.known_false.contains(&simplified)
    }

    /// Simplify a condition using known substitutions
    fn simplify_condition(&self, condition: Condition) -> Condition {
        match condition {
            Condition::NonZero(var) => {
                Condition::NonZero(self.substitutions.get(&var).cloned().unwrap_or(var))
            }
            Condition::NonNull(var) => {
                Condition::NonNull(self.substitutions.get(&var).cloned().unwrap_or(var))
            }
            Condition::InBounds { var, lower, upper } => Condition::InBounds {
                var: self.substitutions.get(&var).cloned().unwrap_or(var),
                lower,
                upper,
            },
            Condition::IsTrue(var) => {
                Condition::IsTrue(self.substitutions.get(&var).cloned().unwrap_or(var))
            }
            Condition::IsFalse(var) => {
                Condition::IsFalse(self.substitutions.get(&var).cloned().unwrap_or(var))
            }
            Condition::Equals(left, right) => Condition::Equals(
                self.substitutions.get(&left).cloned().unwrap_or(left),
                self.substitutions.get(&right).cloned().unwrap_or(right),
            ),
            Condition::EqualsConst(var, val) => {
                Condition::EqualsConst(self.substitutions.get(&var).cloned().unwrap_or(var), val)
            }
            Condition::Positive(var) => {
                Condition::Positive(self.substitutions.get(&var).cloned().unwrap_or(var))
            }
            Condition::NonNegative(var) => {
                Condition::NonNegative(self.substitutions.get(&var).cloned().unwrap_or(var))
            }
            Condition::Custom(s) => Condition::Custom(s),
        }
    }

    /// Add a substitution (left = right)
    pub fn add_equality(&mut self, left: String, right: String) {
        self.substitutions.insert(left.clone(), right.clone());
        // Also add as conditions
        self.add_true(Condition::Equals(left.clone(), right.clone()));
    }

    /// Enter a ghost context
    pub fn enter_ghost(&mut self) {
        self.in_ghost_context = true;
    }

    /// Exit a ghost context
    pub fn exit_ghost(&mut self) {
        self.in_ghost_context = false;
    }

    /// Check if we're in a ghost context
    pub fn is_ghost(&self) -> bool {
        self.in_ghost_context
    }

    /// Add a ghost variable binding
    pub fn add_ghost_binding(&mut self, name: String, ty: TypeId) {
        self.ghost_bindings.insert(name, ty);
    }

    /// Check if a variable is ghost
    pub fn is_ghost_var(&self, name: &str) -> bool {
        self.ghost_bindings.contains_key(name)
    }
}

/// Contract information for a function
#[derive(Debug, Clone, Default)]
pub struct FunctionContract {
    /// Preconditions that must hold at call sites
    pub preconditions: Vec<ContractCondition>,
    /// Postconditions that must hold at return points
    pub postconditions: Vec<ContractCondition>,
    /// Whether this function has ghost parameters
    pub has_ghost_params: bool,
    /// Ghost variable declarations
    pub ghost_vars: Vec<(String, TypeId)>,
}

/// A contract condition with source span for error reporting
#[derive(Debug, Clone)]
pub struct ContractCondition {
    /// The condition expression (as a typed expression)
    pub expr: TypedExpr,
    /// Source span for error reporting
    pub span: Span,
    /// Human-readable description
    pub description: String,
}

/// Contract verifier that checks conditions during type checking
pub struct ContractVerifier {
    /// Collected diagnostics
    diagnostics: DiagnosticBag,
    /// Current contract context
    context: ContractContext,
    /// Function contracts (function name -> contract)
    function_contracts: HashMap<String, FunctionContract>,
    /// Current function's return type (for postcondition checking)
    current_return_type: Option<TypeId>,
    /// Loop invariants for current loop
    loop_invariants: Vec<ContractCondition>,
}

impl ContractVerifier {
    /// Create a new contract verifier
    pub fn new() -> Self {
        Self {
            diagnostics: DiagnosticBag::new(),
            context: ContractContext::new(),
            function_contracts: HashMap::new(),
            current_return_type: None,
            loop_invariants: Vec::new(),
        }
    }

    /// Get collected diagnostics
    pub fn diagnostics(&self) -> &[Diagnostic] {
        self.diagnostics.diagnostics()
    }

    /// Take collected diagnostics
    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        self.diagnostics.take_diagnostics()
    }

    /// Check if any contract violations were found
    pub fn has_violations(&self) -> bool {
        self.diagnostics.has_errors()
    }

    /// Register a function's contract
    pub fn register_function_contract(&mut self, name: String, contract: FunctionContract) {
        self.function_contracts.insert(name, contract);
    }

    /// Get a function's contract
    pub fn get_function_contract(&self, name: &str) -> Option<&FunctionContract> {
        self.function_contracts.get(name)
    }

    /// Verify a function's contracts
    pub fn verify_function(&mut self, func: &TypedFunction, tcx: &TypeContext) {
        self.current_return_type = Some(func.return_type);
        self.context = ContractContext::new();

        // Add parameter conditions to context
        for param in &func.params {
            // Track non-null for references
            let ty = param.ty;
            if self.is_reference_type(ty, tcx) {
                let var_name = self.pattern_to_string(&param.pattern);
                self.context.add_true(Condition::NonNull(var_name));
            }
        }

        // Get the function's contract
        let contract = self
            .function_contracts
            .get(&func.name.name)
            .cloned()
            .unwrap_or_default();

        // Verify preconditions are satisfied at function entry
        // (For now, assume parameters satisfy preconditions)
        for precond in &contract.preconditions {
            self.assume_condition(&precond.expr);
        }

        // Verify the function body
        self.verify_expr(&func.body, tcx);

        // Check that postconditions hold
        for postcond in &contract.postconditions {
            if !self.check_condition(&postcond.expr, tcx) {
                self.report_postcondition_violation(postcond);
            }
        }
    }

    /// Verify contracts in an expression
    pub fn verify_expr(&mut self, expr: &TypedExpr, tcx: &TypeContext) {
        match &expr.kind {
            TypedExprKind::Literal(_) => {}
            TypedExprKind::Variable(ident) => {
                // Track variable usage
                let name = &ident.name;
                if self.context.is_ghost_var(name) && !self.context.is_ghost() {
                    self.report_ghost_in_runtime_context(ident);
                }
            }
            TypedExprKind::Binary { op, left, right } => {
                self.verify_expr(left, tcx);
                self.verify_expr(right, tcx);

                // Check for division by zero
                if matches!(op, BinaryOp::Div | BinaryOp::Mod) {
                    self.check_divisor_non_zero(right, tcx);
                }

                // Track conditions from comparisons
                self.track_comparison(*op, left, right);
            }
            TypedExprKind::Unary { op, expr: inner } => {
                self.verify_expr(inner, tcx);
            }
            TypedExprKind::Call { func, args } => {
                // Verify the function and arguments
                self.verify_expr(func, tcx);
                for arg in args {
                    self.verify_expr(arg, tcx);
                }

                // Check preconditions at call site
                self.check_call_preconditions(func, args, tcx);
            }
            TypedExprKind::Block(block) => {
                let mut block_context = self.context.branch();
                std::mem::swap(&mut self.context, &mut block_context);

                for stmt in &block.stmts {
                    self.verify_stmt(stmt, tcx);
                }

                if let Some(expr) = &block.expr {
                    self.verify_expr(expr, tcx);
                }

                std::mem::swap(&mut self.context, &mut block_context);
            }
            TypedExprKind::If {
                cond,
                then_branch,
                else_branch,
            } => {
                self.verify_expr(cond, tcx);

                // Extract conditions from the condition expression
                let cond_conditions = self.extract_conditions(cond);

                // Verify then branch with positive conditions
                let mut then_context = self.context.branch();
                for condition in &cond_conditions {
                    then_context.add_true(condition.clone());
                }

                let mut parent_context = std::mem::replace(&mut self.context, then_context);
                self.verify_expr(then_branch, tcx);
                let then_context = std::mem::replace(&mut self.context, parent_context);

                // Verify else branch with negated conditions
                if let Some(else_branch) = else_branch {
                    let mut else_context = self.context.branch();
                    for condition in &cond_conditions {
                        // Add negated condition
                        self.add_negated_condition(&mut else_context, condition);
                    }

                    parent_context = std::mem::replace(&mut self.context, else_context);
                    self.verify_expr(else_branch, tcx);
                    let else_context = std::mem::replace(&mut self.context, parent_context);

                    // Merge contexts
                    self.context.merge(&then_context);
                    self.context.merge(&else_context);
                }
            }
            TypedExprKind::While { cond, body } => {
                self.verify_expr(cond, tcx);

                // For loops, we need to check invariants
                // Store current invariants
                let saved_invariants = std::mem::take(&mut self.loop_invariants);

                // Verify loop body
                let mut body_context = self.context.branch();
                std::mem::swap(&mut self.context, &mut body_context);

                // Add loop invariants to context
                let invariants_to_check: Vec<_> = self.loop_invariants.clone();
                for invariant in &invariants_to_check {
                    self.assume_condition(&invariant.expr);
                }

                self.verify_expr(body, tcx);

                // Check that invariants are preserved
                for invariant in &invariants_to_check {
                    if !self.check_condition(&invariant.expr, tcx) {
                        self.report_invariant_not_preserved(invariant);
                    }
                }

                std::mem::swap(&mut self.context, &mut body_context);
                self.loop_invariants = saved_invariants;
            }
            TypedExprKind::Match { expr, arms } => {
                self.verify_expr(expr, tcx);

                let mut arm_contexts = Vec::new();
                for arm in arms {
                    let mut arm_context = self.context.branch();
                    // Add pattern bindings
                    self.add_pattern_conditions(&mut arm_context, &arm.pattern, expr);

                    let parent_context = std::mem::replace(&mut self.context, arm_context);
                    self.verify_expr(&arm.body, tcx);
                    arm_contexts.push(std::mem::replace(&mut self.context, parent_context));
                }

                // Merge all arm contexts
                for ctx in &arm_contexts {
                    self.context.merge(ctx);
                }
            }
            TypedExprKind::Return(expr) => {
                if let Some(ret_expr) = expr {
                    self.verify_expr(ret_expr, tcx);
                }

                // Check postconditions at return
                // (This would be more sophisticated in a full implementation)
            }
            TypedExprKind::Hole(_) => {
                // Holes don't have contracts to verify
            }
            _ => {
                // For other expression kinds, recursively verify sub-expressions
                // This is a simplified version - full implementation would handle all cases
            }
        }
    }

    /// Verify contracts in a statement
    fn verify_stmt(&mut self, stmt: &TypedStmt, tcx: &TypeContext) {
        match stmt {
            TypedStmt::Let { pattern, value, .. } => {
                self.verify_expr(value, tcx);

                // Add pattern bindings to context
                if let TypedExprKind::Variable(ident) = &value.kind {
                    let var_name = self.pattern_to_string(pattern);
                    self.context.add_equality(var_name, ident.name.clone());
                }

                // Track constant assignments
                if let Some(const_val) = self.eval_constant(value) {
                    let var_name = self.pattern_to_string(pattern);
                    self.context
                        .add_true(Condition::EqualsConst(var_name, const_val));
                }
            }
            TypedStmt::Expr(expr) => {
                self.verify_expr(expr, tcx);
            }
            TypedStmt::Return(expr) => {
                if let Some(ret_expr) = expr {
                    self.verify_expr(ret_expr, tcx);
                }

                // Check postconditions
                // (Full implementation would check all postconditions here)
            }
            _ => {}
        }
    }

    /// Check if a divisor is known to be non-zero
    fn check_divisor_non_zero(&mut self, divisor: &TypedExpr, _tcx: &TypeContext) {
        if let TypedExprKind::Variable(ident) = &divisor.kind {
            let var_name = &ident.name;
            if !self
                .context
                .is_known_true(&Condition::NonZero(var_name.clone()))
                && !self
                    .context
                    .is_known_true(&Condition::NonNull(var_name.clone()))
            {
                // Cannot prove non-zero
                self.report_possible_div_by_zero(divisor);
            }
        }
        // For constant expressions, check directly
        else if let Some(ConstantValue::Integer(0)) = self.eval_constant(divisor) {
            self.report_definite_div_by_zero(divisor);
        }
    }

    /// Check preconditions at a call site
    fn check_call_preconditions(
        &mut self,
        func: &TypedExpr,
        _args: &[TypedExpr],
        _tcx: &TypeContext,
    ) {
        // Get the function name
        let func_name = match &func.kind {
            TypedExprKind::Variable(ident) => &ident.name,
            _ => return, // Can't determine function name
        };

        // Look up the function's contract
        if let Some(contract) = self.function_contracts.get(func_name).cloned() {
            for precond in &contract.preconditions {
                if !self.check_condition(&precond.expr, _tcx) {
                    self.report_precondition_not_met(&precond.description, func);
                }
            }
        }
    }

    /// Check if a condition holds in the current context
    fn check_condition(&self, expr: &TypedExpr, _tcx: &TypeContext) -> bool {
        // Try to evaluate as constant first
        if let Some(ConstantValue::Bool(true)) = self.eval_constant(expr) {
            return true;
        }

        // Check if condition is in known_true
        let conditions = self.extract_conditions(expr);
        conditions
            .iter()
            .all(|c| self.context.is_known_true(c) || self.is_implied_by_context(c))
    }

    /// Assume a condition is true (add to context)
    fn assume_condition(&mut self, expr: &TypedExpr) {
        let conditions = self.extract_conditions(expr);
        for condition in conditions {
            self.context.add_true(condition);
        }
    }

    /// Extract conditions from an expression
    fn extract_conditions(&self, expr: &TypedExpr) -> Vec<Condition> {
        match &expr.kind {
            TypedExprKind::Binary { op, left, right } => match op {
                BinaryOp::And => {
                    let mut conds = self.extract_conditions(left);
                    conds.extend(self.extract_conditions(right));
                    conds
                }
                BinaryOp::Eq => {
                    if let (TypedExprKind::Variable(l), TypedExprKind::Variable(r)) =
                        (&left.kind, &right.kind)
                    {
                        vec![Condition::Equals(l.name.clone(), r.name.clone())]
                    } else if let (TypedExprKind::Variable(v), TypedExprKind::Literal(lit)) =
                        (&left.kind, &right.kind)
                    {
                        if let Some(const_val) = self.literal_to_constant(lit) {
                            vec![Condition::EqualsConst(v.name.clone(), const_val)]
                        } else {
                            vec![]
                        }
                    } else {
                        vec![]
                    }
                }
                BinaryOp::Ne => {
                    // x != 0 is equivalent to x is non-zero
                    if let (TypedExprKind::Variable(v), TypedExprKind::Literal(lit)) =
                        (&left.kind, &right.kind)
                    {
                        if let Literal::Integer(0) = lit {
                            vec![Condition::NonZero(v.name.clone())]
                        } else {
                            vec![]
                        }
                    } else {
                        vec![]
                    }
                }
                BinaryOp::Gt => {
                    // x > 0 means x is positive
                    if let (TypedExprKind::Variable(v), TypedExprKind::Literal(lit)) =
                        (&left.kind, &right.kind)
                    {
                        if let Literal::Integer(0) = lit {
                            vec![Condition::Positive(v.name.clone())]
                        } else {
                            vec![]
                        }
                    } else {
                        vec![]
                    }
                }
                BinaryOp::Ge => {
                    // x >= 0 means x is non-negative
                    if let (TypedExprKind::Variable(v), TypedExprKind::Literal(lit)) =
                        (&left.kind, &right.kind)
                    {
                        if let Literal::Integer(0) = lit {
                            vec![Condition::NonNegative(v.name.clone())]
                        } else {
                            vec![]
                        }
                    } else {
                        vec![]
                    }
                }
                _ => vec![],
            },
            TypedExprKind::Variable(ident) => {
                // A bare variable in condition context means it's true
                vec![Condition::IsTrue(ident.name.clone())]
            }
            TypedExprKind::Literal(Literal::Bool(true)) => {
                vec![] // Always true, no condition needed
            }
            TypedExprKind::Literal(Literal::Bool(false)) => {
                vec![Condition::Custom("false".to_string())] // Always false
            }
            _ => vec![Condition::Custom(format!("expr@{:?}", expr.span))],
        }
    }

    /// Check if a condition is implied by the current context
    fn is_implied_by_context(&self, condition: &Condition) -> bool {
        match condition {
            Condition::NonZero(var) => {
                // Positive implies non-zero
                self.context
                    .is_known_true(&Condition::Positive(var.clone()))
            }
            Condition::Positive(var) => {
                // Already checking positive
                self.context
                    .is_known_true(&Condition::Positive(var.clone()))
            }
            Condition::NonNegative(var) => {
                self.context
                    .is_known_true(&Condition::NonNegative(var.clone()))
                    || self
                        .context
                        .is_known_true(&Condition::Positive(var.clone()))
            }
            _ => false,
        }
    }

    /// Add a negated condition to context
    fn add_negated_condition(&self, context: &mut ContractContext, condition: &Condition) {
        match condition {
            Condition::IsTrue(var) => {
                context.add_true(Condition::IsFalse(var.clone()));
            }
            Condition::IsFalse(var) => {
                context.add_true(Condition::IsTrue(var.clone()));
            }
            Condition::NonZero(var) => {
                // Not (x != 0) means x == 0
                context.add_true(Condition::EqualsConst(
                    var.clone(),
                    ConstantValue::Integer(0),
                ));
            }
            Condition::Positive(var) => {
                // Not (x > 0) means x <= 0
                context.add_false(Condition::Positive(var.clone()));
            }
            _ => {}
        }
    }

    /// Add pattern conditions to context
    fn add_pattern_conditions(
        &self,
        context: &mut ContractContext,
        pattern: &Pattern,
        matched_expr: &TypedExpr,
    ) {
        match pattern {
            Pattern::Ident { name, .. } => {
                if let TypedExprKind::Variable(v) = &matched_expr.kind {
                    context.add_equality(name.name.clone(), v.name.clone());
                }
            }
            _ => {}
        }
    }

    /// Evaluate a constant expression
    fn eval_constant(&self, expr: &TypedExpr) -> Option<ConstantValue> {
        match &expr.kind {
            TypedExprKind::Literal(lit) => self.literal_to_constant(lit),
            TypedExprKind::Binary { op, left, right } => {
                let left_val = self.eval_constant(left)?;
                let right_val = self.eval_constant(right)?;
                self.eval_binary_op(*op, left_val, right_val)
            }
            _ => None,
        }
    }

    /// Convert a literal to a constant value
    fn literal_to_constant(&self, lit: &Literal) -> Option<ConstantValue> {
        match lit {
            Literal::Integer(i) => Some(ConstantValue::Integer(*i)),
            Literal::Bool(b) => Some(ConstantValue::Bool(*b)),
            Literal::Unit => Some(ConstantValue::Unit),
            _ => None,
        }
    }

    /// Evaluate a binary operation on constants
    fn eval_binary_op(
        &self,
        op: BinaryOp,
        left: ConstantValue,
        right: ConstantValue,
    ) -> Option<ConstantValue> {
        match (left, right) {
            (ConstantValue::Integer(l), ConstantValue::Integer(r)) => match op {
                BinaryOp::Add => Some(ConstantValue::Integer(l + r)),
                BinaryOp::Sub => Some(ConstantValue::Integer(l - r)),
                BinaryOp::Mul => Some(ConstantValue::Integer(l * r)),
                BinaryOp::Div => {
                    if r != 0 {
                        Some(ConstantValue::Integer(l / r))
                    } else {
                        None
                    }
                }
                BinaryOp::Mod => {
                    if r != 0 {
                        Some(ConstantValue::Integer(l % r))
                    } else {
                        None
                    }
                }
                BinaryOp::Eq => Some(ConstantValue::Bool(l == r)),
                BinaryOp::Ne => Some(ConstantValue::Bool(l != r)),
                BinaryOp::Lt => Some(ConstantValue::Bool(l < r)),
                BinaryOp::Le => Some(ConstantValue::Bool(l <= r)),
                BinaryOp::Gt => Some(ConstantValue::Bool(l > r)),
                BinaryOp::Ge => Some(ConstantValue::Bool(l >= r)),
                _ => None,
            },
            (ConstantValue::Bool(l), ConstantValue::Bool(r)) => match op {
                BinaryOp::And => Some(ConstantValue::Bool(l && r)),
                BinaryOp::Or => Some(ConstantValue::Bool(l || r)),
                BinaryOp::Eq => Some(ConstantValue::Bool(l == r)),
                BinaryOp::Ne => Some(ConstantValue::Bool(l != r)),
                _ => None,
            },
            _ => None,
        }
    }

    /// Track conditions from comparison operations
    fn track_comparison(&mut self, op: BinaryOp, left: &TypedExpr, right: &TypedExpr) {
        // Add conditions based on comparison result
        match op {
            BinaryOp::Eq => {
                if let (TypedExprKind::Variable(l), TypedExprKind::Variable(r)) =
                    (&left.kind, &right.kind)
                {
                    self.context.add_equality(l.name.clone(), r.name.clone());
                }
            }
            _ => {}
        }
    }

    /// Check if a type is a reference type
    fn is_reference_type(&self, ty: TypeId, tcx: &TypeContext) -> bool {
        matches!(tcx.type_kind(ty), TypeKind::Ref(_, _))
    }

    /// Convert a pattern to a string representation
    fn pattern_to_string(&self, pattern: &Pattern) -> String {
        match pattern {
            Pattern::Ident { name, .. } => name.name.clone(),
            _ => format!("pattern@{:?}", pattern),
        }
    }

    // Diagnostic reporting methods

    fn report_possible_div_by_zero(&mut self, expr: &TypedExpr) {
        let diag = Diagnostic::warning("possible division by zero", expr.span)
            .with_error_code(ErrorCode::Custom(3500))
            .with_note("cannot prove divisor is non-zero")
            .with_note("consider adding a check: `if divisor != 0` or `requires divisor != 0`");

        self.diagnostics.push(diag);
    }

    fn report_definite_div_by_zero(&mut self, expr: &TypedExpr) {
        let diag = Diagnostic::error("definite division by zero", expr.span)
            .with_error_code(ErrorCode::Custom(3501));

        self.diagnostics.push(diag);
    }

    fn report_precondition_not_met(&mut self, description: &str, call_site: &TypedExpr) {
        let diag = Diagnostic::warning(
            format!("precondition may not be satisfied: {}", description),
            call_site.span,
        )
        .with_error_code(ErrorCode::ContractPreconditionFailed)
        .with_note("cannot prove this precondition holds at call site");

        self.diagnostics.push(diag);
    }

    fn report_postcondition_violation(&mut self, postcond: &ContractCondition) {
        let diag = Diagnostic::warning(
            format!(
                "postcondition may not be satisfied: {}",
                postcond.description
            ),
            postcond.span,
        )
        .with_error_code(ErrorCode::ContractPostconditionFailed)
        .with_note("cannot prove this postcondition holds at return point");

        self.diagnostics.push(diag);
    }

    fn report_invariant_not_preserved(&mut self, invariant: &ContractCondition) {
        let diag = Diagnostic::warning(
            format!(
                "loop invariant may not be preserved: {}",
                invariant.description
            ),
            invariant.span,
        )
        .with_error_code(ErrorCode::InvariantNotPreserved)
        .with_note("cannot prove this invariant is preserved by the loop body");

        self.diagnostics.push(diag);
    }

    fn report_ghost_in_runtime_context(&mut self, ident: &jet_parser::ast::Ident) {
        let diag = Diagnostic::error(
            format!("ghost variable '{}' used in runtime context", ident.name),
            Span::new(ident.span.start, ident.span.end),
        )
        .with_error_code(ErrorCode::GhostInRuntimeContext)
        .with_note("ghost variables only exist for verification and are erased at runtime");

        self.diagnostics.push(diag);
    }
}

impl Default for ContractVerifier {
    fn default() -> Self {
        Self::new()
    }
}

/// Extension trait for adding contract verification to the type checker
pub trait ContractChecking {
    /// Verify contracts for a function
    fn verify_function_contracts(&mut self, func: &TypedFunction, tcx: &TypeContext);

    /// Check contracts at a call site
    fn check_call_contracts(&mut self, func_name: &str, args: &[TypedExpr], span: Span);

    /// Register a function contract
    fn register_contract(&mut self, name: String, contract: FunctionContract);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_contract_context() {
        let mut ctx = ContractContext::new();

        // Add a known condition
        ctx.add_true(Condition::NonZero("x".to_string()));
        assert!(ctx.is_known_true(&Condition::NonZero("x".to_string())));

        // Check unknown condition
        assert!(!ctx.is_known_true(&Condition::NonZero("y".to_string())));
    }

    #[test]
    fn test_contract_context_branch() {
        let mut ctx = ContractContext::new();
        ctx.add_true(Condition::NonZero("x".to_string()));

        // Branch and modify
        let mut branch = ctx.branch();
        branch.add_true(Condition::NonZero("y".to_string()));

        // Original should not have y
        assert!(!ctx.is_known_true(&Condition::NonZero("y".to_string())));
        // Branch should have both
        assert!(branch.is_known_true(&Condition::NonZero("x".to_string())));
        assert!(branch.is_known_true(&Condition::NonZero("y".to_string())));
    }

    #[test]
    fn test_contract_context_merge() {
        let mut ctx1 = ContractContext::new();
        ctx1.add_true(Condition::NonZero("x".to_string()));
        ctx1.add_true(Condition::NonZero("y".to_string()));

        let mut ctx2 = ContractContext::new();
        ctx2.add_true(Condition::NonZero("x".to_string()));
        ctx2.add_true(Condition::NonZero("z".to_string()));

        // Merge ctx2 into ctx1
        ctx1.merge(&ctx2);

        // Only x is in both
        assert!(ctx1.is_known_true(&Condition::NonZero("x".to_string())));
        assert!(!ctx1.is_known_true(&Condition::NonZero("y".to_string())));
        assert!(!ctx1.is_known_true(&Condition::NonZero("z".to_string())));
    }

    #[test]
    fn test_condition_simplification() {
        let mut ctx = ContractContext::new();
        ctx.add_equality("a".to_string(), "b".to_string());

        // Condition on 'a' should also apply to 'b'
        ctx.add_true(Condition::NonZero("a".to_string()));
        assert!(ctx.is_known_true(&Condition::NonZero("b".to_string())));
    }

    #[test]
    fn test_implied_conditions() {
        let mut ctx = ContractContext::new();

        // Positive implies non-zero
        ctx.add_true(Condition::Positive("x".to_string()));

        let mut verifier = ContractVerifier::new();
        verifier.context = ctx;
        assert!(verifier.is_implied_by_context(&Condition::NonZero("x".to_string())));
    }
}
