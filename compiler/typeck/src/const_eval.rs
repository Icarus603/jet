//! Constant expression evaluation for contract verification
//!
//! This module provides compile-time evaluation of constant expressions,
//! used for contract verification and other compile-time computations.
#![allow(unused_imports)]

use crate::checker::{TypedExpr, TypedExprKind, TypedStmt};
use crate::types::{TypeContext, TypeId, TypeKind};
use jet_diagnostics::{Diagnostic, ErrorCode, Span as DiagSpan};
use jet_parser::ast::{BinaryOp, Literal, Pattern, UnaryOp};
use std::collections::HashMap;
type Span = DiagSpan;

/// Convert a lexer span to a diagnostics span
fn to_diag_span(span: jet_lexer::Span) -> DiagSpan {
    DiagSpan::new(span.start, span.end)
}

/// A constant value that can be computed at compile time
#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    /// Integer constant
    Integer(i128),
    /// Unsigned integer constant
    Unsigned(u128),
    /// Floating point constant
    Float(f64),
    /// Boolean constant
    Bool(bool),
    /// String constant
    String(String),
    /// Unit constant
    Unit,
    /// Array constant
    Array(Vec<ConstValue>),
    /// Tuple constant
    Tuple(Vec<ConstValue>),
    /// Unknown/unevaluable constant
    Unknown,
}

impl ConstValue {
    /// Convert to boolean if possible
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            ConstValue::Bool(b) => Some(*b),
            ConstValue::Integer(0) | ConstValue::Unsigned(0) => Some(false),
            ConstValue::Integer(_) | ConstValue::Unsigned(_) => Some(true),
            _ => None,
        }
    }

    /// Convert to integer if possible
    pub fn as_integer(&self) -> Option<i128> {
        match self {
            ConstValue::Integer(i) => Some(*i),
            ConstValue::Unsigned(u) => i128::try_from(*u).ok(),
            _ => None,
        }
    }

    /// Convert to unsigned if possible
    pub fn as_unsigned(&self) -> Option<u128> {
        match self {
            ConstValue::Unsigned(u) => Some(*u),
            ConstValue::Integer(i) => u128::try_from(*i).ok(),
            _ => None,
        }
    }

    /// Convert to float if possible
    pub fn as_float(&self) -> Option<f64> {
        match self {
            ConstValue::Float(f) => Some(*f),
            ConstValue::Integer(i) => Some(*i as f64),
            ConstValue::Unsigned(u) => Some(*u as f64),
            _ => None,
        }
    }

    /// Check if this is a truthy value
    pub fn is_truthy(&self) -> bool {
        match self {
            ConstValue::Bool(b) => *b,
            ConstValue::Integer(i) => *i != 0,
            ConstValue::Unsigned(u) => *u != 0,
            ConstValue::Float(f) => *f != 0.0,
            ConstValue::String(s) => !s.is_empty(),
            ConstValue::Array(a) => !a.is_empty(),
            ConstValue::Tuple(t) => !t.is_empty(),
            ConstValue::Unit => false,
            ConstValue::Unknown => false,
        }
    }

    /// Get the type of this constant value
    pub fn type_kind(&self) -> Option<TypeKind> {
        match self {
            ConstValue::Integer(_) => Some(TypeKind::Int(crate::types::IntSize::I64)),
            ConstValue::Unsigned(_) => Some(TypeKind::Uint(crate::types::IntSize::I64)),
            ConstValue::Float(_) => Some(TypeKind::Float(crate::types::FloatSize::F64)),
            ConstValue::Bool(_) => Some(TypeKind::Bool),
            ConstValue::String(_) => Some(TypeKind::String),
            ConstValue::Unit => Some(TypeKind::Unit),
            ConstValue::Array(_) => None, // Would need element type
            ConstValue::Tuple(_) => None, // Would need element types
            ConstValue::Unknown => None,
        }
    }
}

impl std::fmt::Display for ConstValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstValue::Integer(i) => write!(f, "{}", i),
            ConstValue::Unsigned(u) => write!(f, "{}", u),
            ConstValue::Float(fl) => write!(f, "{}", fl),
            ConstValue::Bool(b) => write!(f, "{}", b),
            ConstValue::String(s) => write!(f, "\"{}\"", s),
            ConstValue::Unit => write!(f, "()"),
            ConstValue::Array(a) => {
                write!(f, "[")?;
                for (i, v) in a.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            ConstValue::Tuple(t) => {
                write!(f, "(")?;
                for (i, v) in t.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, ")")
            }
            ConstValue::Unknown => write!(f, "<unknown>"),
        }
    }
}

/// Context for constant evaluation
#[derive(Debug, Clone)]
pub struct ConstEvalContext {
    /// Known constant bindings (variable name -> value)
    bindings: HashMap<String, ConstValue>,
    /// Maximum evaluation depth to prevent stack overflow
    max_depth: usize,
    /// Current evaluation depth
    current_depth: usize,
}

impl ConstEvalContext {
    /// Create a new constant evaluation context
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            max_depth: 100,
            current_depth: 0,
        }
    }

    /// Create a context with custom max depth
    pub fn with_max_depth(max_depth: usize) -> Self {
        Self {
            bindings: HashMap::new(),
            max_depth,
            current_depth: 0,
        }
    }

    /// Add a constant binding
    pub fn bind(&mut self, name: impl Into<String>, value: ConstValue) {
        self.bindings.insert(name.into(), value);
    }

    /// Look up a constant binding
    pub fn lookup(&self, name: &str) -> Option<&ConstValue> {
        self.bindings.get(name)
    }

    /// Enter a nested evaluation context
    pub fn enter(&mut self) -> bool {
        if self.current_depth >= self.max_depth {
            return false;
        }
        self.current_depth += 1;
        true
    }

    /// Exit a nested evaluation context
    pub fn exit(&mut self) {
        self.current_depth = self.current_depth.saturating_sub(1);
    }

    /// Check if we've exceeded the maximum depth
    pub fn depth_exceeded(&self) -> bool {
        self.current_depth >= self.max_depth
    }

    /// Create a child context with inherited bindings
    pub fn child(&self) -> Self {
        Self {
            bindings: self.bindings.clone(),
            max_depth: self.max_depth,
            current_depth: self.current_depth,
        }
    }
}

impl Default for ConstEvalContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Result of constant evaluation
pub type ConstEvalResult = Result<ConstValue, ConstEvalError>;

/// Error during constant evaluation
#[derive(Debug, Clone)]
pub enum ConstEvalError {
    /// Division by zero
    DivisionByZero { span: Span },
    /// Integer overflow
    IntegerOverflow { span: Span, op: String },
    /// Unknown variable
    UnknownVariable { name: String, span: Span },
    /// Not a constant expression
    NotConstant { span: Span },
    /// Maximum evaluation depth exceeded
    DepthExceeded { span: Span },
    /// Type mismatch in operation
    TypeMismatch {
        expected: String,
        found: String,
        span: Span,
    },
    /// Unsupported operation
    UnsupportedOperation { op: String, span: Span },
}

impl ConstEvalError {
    /// Convert to a diagnostic message
    pub fn to_diagnostic(&self) -> Diagnostic {
        match self {
            ConstEvalError::DivisionByZero { span } => {
                Diagnostic::error("division by zero in constant expression", *span)
                    .with_error_code(ErrorCode::Custom(3600))
            }
            ConstEvalError::IntegerOverflow { span, op } => Diagnostic::error(
                format!("integer overflow in constant expression: {}", op),
                *span,
            )
            .with_error_code(ErrorCode::Custom(3601)),
            ConstEvalError::UnknownVariable { name, span } => Diagnostic::error(
                format!("unknown variable in constant expression: '{}'", name),
                *span,
            )
            .with_error_code(ErrorCode::UnresolvedName),
            ConstEvalError::NotConstant { span } => {
                Diagnostic::error("expression is not constant", *span)
                    .with_error_code(ErrorCode::Custom(3602))
            }
            ConstEvalError::DepthExceeded { span } => {
                Diagnostic::error("constant evaluation depth exceeded", *span)
                    .with_error_code(ErrorCode::Custom(3603))
            }
            ConstEvalError::TypeMismatch {
                expected,
                found,
                span,
            } => Diagnostic::error(
                format!("type mismatch: expected {}, found {}", expected, found),
                *span,
            )
            .with_error_code(ErrorCode::TypeMismatch),
            ConstEvalError::UnsupportedOperation { op, span } => Diagnostic::error(
                format!("unsupported operation in constant expression: {}", op),
                *span,
            )
            .with_error_code(ErrorCode::UnsupportedFeature),
        }
    }
}

/// Evaluate a typed expression to a constant value
pub fn eval_const_expr(expr: &TypedExpr, ctx: &mut ConstEvalContext) -> ConstEvalResult {
    if !ctx.enter() {
        return Err(ConstEvalError::DepthExceeded { span: expr.span });
    }

    let result = eval_const_expr_inner(expr, ctx);
    ctx.exit();
    result
}

fn eval_const_expr_inner(expr: &TypedExpr, ctx: &mut ConstEvalContext) -> ConstEvalResult {
    match &expr.kind {
        TypedExprKind::Literal(lit) => eval_literal(lit),

        TypedExprKind::Variable(ident) => {
            if let Some(value) = ctx.lookup(&ident.name) {
                Ok(value.clone())
            } else {
                Err(ConstEvalError::UnknownVariable {
                    name: ident.name.clone(),
                    span: to_diag_span(ident.span),
                })
            }
        }

        TypedExprKind::Binary { op, left, right } => {
            let left_val = eval_const_expr(left, ctx)?;
            let right_val = eval_const_expr(right, ctx)?;
            eval_binary_op(*op, left_val, right_val, expr.span)
        }

        TypedExprKind::Unary { op, expr: inner } => {
            let val = eval_const_expr(inner, ctx)?;
            eval_unary_op(*op, val, expr.span)
        }

        TypedExprKind::Block(block) => {
            // Evaluate block - only works if the block has a final expression
            // and all statements are constant
            let mut child_ctx = ctx.child();

            for stmt in &block.stmts {
                eval_const_stmt(stmt, &mut child_ctx)?;
            }

            if let Some(result_expr) = &block.expr {
                eval_const_expr(result_expr, &mut child_ctx)
            } else {
                Ok(ConstValue::Unit)
            }
        }

        TypedExprKind::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond_val = eval_const_expr(cond, ctx)?;

            if let Some(b) = cond_val.as_bool() {
                if b {
                    eval_const_expr(then_branch, ctx)
                } else if let Some(else_expr) = else_branch {
                    eval_const_expr(else_expr, ctx)
                } else {
                    Ok(ConstValue::Unit)
                }
            } else {
                Err(ConstEvalError::TypeMismatch {
                    expected: "boolean".to_string(),
                    found: format!("{:?}", cond_val),
                    span: cond.span,
                })
            }
        }

        TypedExprKind::Tuple(elements) => {
            let mut values = Vec::new();
            for elem in elements {
                values.push(eval_const_expr(elem, ctx)?);
            }
            Ok(ConstValue::Tuple(values))
        }

        TypedExprKind::Array(elements) => {
            let mut values = Vec::new();
            for elem in elements {
                values.push(eval_const_expr(elem, ctx)?);
            }
            Ok(ConstValue::Array(values))
        }

        // These expression types cannot be evaluated at compile time
        TypedExprKind::Call { .. }
        | TypedExprKind::Match { .. }
        | TypedExprKind::While { .. }
        | TypedExprKind::For { .. }
        | TypedExprKind::Loop { .. }
        | TypedExprKind::Lambda { .. }
        | TypedExprKind::Await(_)
        | TypedExprKind::Try(_)
        | TypedExprKind::Spawn(_)
        | TypedExprKind::Async(_)
        | TypedExprKind::Concurrent(_)
        | TypedExprKind::Hole(_) => Err(ConstEvalError::NotConstant { span: expr.span }),

        // Other cases
        _ => Err(ConstEvalError::NotConstant { span: expr.span }),
    }
}

/// Evaluate a literal to a constant value
fn eval_literal(lit: &Literal) -> ConstEvalResult {
    match lit {
        Literal::Integer(i) => Ok(ConstValue::Integer(*i as i128)),
        Literal::Float(f) => Ok(ConstValue::Float(*f)),
        Literal::String(s) => Ok(ConstValue::String(s.clone())),
        Literal::Char(c) => Ok(ConstValue::Integer(*c as i128)),
        Literal::Bool(b) => Ok(ConstValue::Bool(*b)),
        Literal::Unit => Ok(ConstValue::Unit),
    }
}

/// Evaluate a binary operation
fn eval_binary_op(
    op: jet_parser::ast::BinaryOp,
    left: ConstValue,
    right: ConstValue,
    span: Span,
) -> ConstEvalResult {
    use jet_parser::ast::BinaryOp;

    match op {
        // Arithmetic operations
        BinaryOp::Add => match (left, right) {
            (ConstValue::Integer(l), ConstValue::Integer(r)) => l
                .checked_add(r)
                .map(ConstValue::Integer)
                .ok_or_else(|| ConstEvalError::IntegerOverflow {
                    span,
                    op: "addition".to_string(),
                }),
            (ConstValue::Unsigned(l), ConstValue::Unsigned(r)) => l
                .checked_add(r)
                .map(ConstValue::Unsigned)
                .ok_or_else(|| ConstEvalError::IntegerOverflow {
                    span,
                    op: "addition".to_string(),
                }),
            (ConstValue::Float(l), ConstValue::Float(r)) => Ok(ConstValue::Float(l + r)),
            (ConstValue::Integer(l), ConstValue::Float(r)) => Ok(ConstValue::Float(l as f64 + r)),
            (ConstValue::Float(l), ConstValue::Integer(r)) => Ok(ConstValue::Float(l + r as f64)),
            (l, r) => Err(ConstEvalError::TypeMismatch {
                expected: "numeric".to_string(),
                found: format!("{:?}, {:?}", l, r),
                span,
            }),
        },

        BinaryOp::Sub => match (left, right) {
            (ConstValue::Integer(l), ConstValue::Integer(r)) => l
                .checked_sub(r)
                .map(ConstValue::Integer)
                .ok_or_else(|| ConstEvalError::IntegerOverflow {
                    span,
                    op: "subtraction".to_string(),
                }),
            (ConstValue::Unsigned(l), ConstValue::Unsigned(r)) => l
                .checked_sub(r)
                .map(ConstValue::Unsigned)
                .ok_or_else(|| ConstEvalError::IntegerOverflow {
                    span,
                    op: "subtraction".to_string(),
                }),
            (ConstValue::Float(l), ConstValue::Float(r)) => Ok(ConstValue::Float(l - r)),
            (ConstValue::Integer(l), ConstValue::Float(r)) => Ok(ConstValue::Float(l as f64 - r)),
            (ConstValue::Float(l), ConstValue::Integer(r)) => Ok(ConstValue::Float(l - r as f64)),
            (l, r) => Err(ConstEvalError::TypeMismatch {
                expected: "numeric".to_string(),
                found: format!("{:?}, {:?}", l, r),
                span,
            }),
        },

        BinaryOp::Mul => match (left, right) {
            (ConstValue::Integer(l), ConstValue::Integer(r)) => l
                .checked_mul(r)
                .map(ConstValue::Integer)
                .ok_or_else(|| ConstEvalError::IntegerOverflow {
                    span,
                    op: "multiplication".to_string(),
                }),
            (ConstValue::Unsigned(l), ConstValue::Unsigned(r)) => l
                .checked_mul(r)
                .map(ConstValue::Unsigned)
                .ok_or_else(|| ConstEvalError::IntegerOverflow {
                    span,
                    op: "multiplication".to_string(),
                }),
            (ConstValue::Float(l), ConstValue::Float(r)) => Ok(ConstValue::Float(l * r)),
            (ConstValue::Integer(l), ConstValue::Float(r)) => Ok(ConstValue::Float(l as f64 * r)),
            (ConstValue::Float(l), ConstValue::Integer(r)) => Ok(ConstValue::Float(l * r as f64)),
            (l, r) => Err(ConstEvalError::TypeMismatch {
                expected: "numeric".to_string(),
                found: format!("{:?}, {:?}", l, r),
                span,
            }),
        },

        BinaryOp::Div => match (left, right) {
            (ConstValue::Integer(l), ConstValue::Integer(r)) => {
                if r == 0 {
                    Err(ConstEvalError::DivisionByZero { span })
                } else {
                    l.checked_div(r).map(ConstValue::Integer).ok_or_else(|| {
                        ConstEvalError::IntegerOverflow {
                            span,
                            op: "division".to_string(),
                        }
                    })
                }
            }
            (ConstValue::Unsigned(l), ConstValue::Unsigned(r)) => {
                if r == 0 {
                    Err(ConstEvalError::DivisionByZero { span })
                } else {
                    l.checked_div(r).map(ConstValue::Unsigned).ok_or_else(|| {
                        ConstEvalError::IntegerOverflow {
                            span,
                            op: "division".to_string(),
                        }
                    })
                }
            }
            (ConstValue::Float(l), ConstValue::Float(r)) => {
                if r == 0.0 {
                    Err(ConstEvalError::DivisionByZero { span })
                } else {
                    Ok(ConstValue::Float(l / r))
                }
            }
            (l, r) => Err(ConstEvalError::TypeMismatch {
                expected: "numeric".to_string(),
                found: format!("{:?}, {:?}", l, r),
                span,
            }),
        },

        BinaryOp::Mod => match (left, right) {
            (ConstValue::Integer(l), ConstValue::Integer(r)) => {
                if r == 0 {
                    Err(ConstEvalError::DivisionByZero { span })
                } else {
                    Ok(ConstValue::Integer(l % r))
                }
            }
            (ConstValue::Unsigned(l), ConstValue::Unsigned(r)) => {
                if r == 0 {
                    Err(ConstEvalError::DivisionByZero { span })
                } else {
                    Ok(ConstValue::Unsigned(l % r))
                }
            }
            (l, r) => Err(ConstEvalError::TypeMismatch {
                expected: "integer".to_string(),
                found: format!("{:?}, {:?}", l, r),
                span,
            }),
        },

        // Comparison operations
        BinaryOp::Eq => Ok(ConstValue::Bool(left == right)),
        BinaryOp::Ne => Ok(ConstValue::Bool(left != right)),

        BinaryOp::Lt => match (left, right) {
            (ConstValue::Integer(l), ConstValue::Integer(r)) => Ok(ConstValue::Bool(l < r)),
            (ConstValue::Unsigned(l), ConstValue::Unsigned(r)) => Ok(ConstValue::Bool(l < r)),
            (ConstValue::Float(l), ConstValue::Float(r)) => Ok(ConstValue::Bool(l < r)),
            (ConstValue::String(l), ConstValue::String(r)) => Ok(ConstValue::Bool(l < r)),
            (l, r) => Err(ConstEvalError::TypeMismatch {
                expected: "comparable".to_string(),
                found: format!("{:?}, {:?}", l, r),
                span,
            }),
        },

        BinaryOp::Le => match (left, right) {
            (ConstValue::Integer(l), ConstValue::Integer(r)) => Ok(ConstValue::Bool(l <= r)),
            (ConstValue::Unsigned(l), ConstValue::Unsigned(r)) => Ok(ConstValue::Bool(l <= r)),
            (ConstValue::Float(l), ConstValue::Float(r)) => Ok(ConstValue::Bool(l <= r)),
            (ConstValue::String(l), ConstValue::String(r)) => Ok(ConstValue::Bool(l <= r)),
            (l, r) => Err(ConstEvalError::TypeMismatch {
                expected: "comparable".to_string(),
                found: format!("{:?}, {:?}", l, r),
                span,
            }),
        },

        BinaryOp::Gt => match (left, right) {
            (ConstValue::Integer(l), ConstValue::Integer(r)) => Ok(ConstValue::Bool(l > r)),
            (ConstValue::Unsigned(l), ConstValue::Unsigned(r)) => Ok(ConstValue::Bool(l > r)),
            (ConstValue::Float(l), ConstValue::Float(r)) => Ok(ConstValue::Bool(l > r)),
            (ConstValue::String(l), ConstValue::String(r)) => Ok(ConstValue::Bool(l > r)),
            (l, r) => Err(ConstEvalError::TypeMismatch {
                expected: "comparable".to_string(),
                found: format!("{:?}, {:?}", l, r),
                span,
            }),
        },

        BinaryOp::Ge => match (left, right) {
            (ConstValue::Integer(l), ConstValue::Integer(r)) => Ok(ConstValue::Bool(l >= r)),
            (ConstValue::Unsigned(l), ConstValue::Unsigned(r)) => Ok(ConstValue::Bool(l >= r)),
            (ConstValue::Float(l), ConstValue::Float(r)) => Ok(ConstValue::Bool(l >= r)),
            (ConstValue::String(l), ConstValue::String(r)) => Ok(ConstValue::Bool(l >= r)),
            (l, r) => Err(ConstEvalError::TypeMismatch {
                expected: "comparable".to_string(),
                found: format!("{:?}, {:?}", l, r),
                span,
            }),
        },

        // Logical operations
        BinaryOp::And => match (left.as_bool(), right.as_bool()) {
            (Some(l), Some(r)) => Ok(ConstValue::Bool(l && r)),
            _ => Err(ConstEvalError::TypeMismatch {
                expected: "boolean".to_string(),
                found: format!("{:?}, {:?}", left, right),
                span,
            }),
        },

        BinaryOp::Or => match (left.as_bool(), right.as_bool()) {
            (Some(l), Some(r)) => Ok(ConstValue::Bool(l || r)),
            _ => Err(ConstEvalError::TypeMismatch {
                expected: "boolean".to_string(),
                found: format!("{:?}, {:?}", left, right),
                span,
            }),
        },

        // Unsupported operations
        _ => Err(ConstEvalError::UnsupportedOperation {
            op: format!("{:?}", op),
            span,
        }),
    }
}

/// Evaluate a unary operation
fn eval_unary_op(op: jet_parser::ast::UnaryOp, val: ConstValue, span: Span) -> ConstEvalResult {
    use jet_parser::ast::UnaryOp;

    match op {
        UnaryOp::Not => match val {
            ConstValue::Bool(b) => Ok(ConstValue::Bool(!b)),
            ConstValue::Integer(i) => Ok(ConstValue::Integer(!i)),
            ConstValue::Unsigned(u) => Ok(ConstValue::Unsigned(!u)),
            _ => Err(ConstEvalError::TypeMismatch {
                expected: "boolean or integer".to_string(),
                found: format!("{:?}", val),
                span,
            }),
        },

        UnaryOp::Neg => match val {
            ConstValue::Integer(i) => i.checked_neg().map(ConstValue::Integer).ok_or_else(|| {
                ConstEvalError::IntegerOverflow {
                    span,
                    op: "negation".to_string(),
                }
            }),
            ConstValue::Float(f) => Ok(ConstValue::Float(-f)),
            _ => Err(ConstEvalError::TypeMismatch {
                expected: "numeric".to_string(),
                found: format!("{:?}", val),
                span,
            }),
        },

        UnaryOp::BitNot => match val {
            ConstValue::Integer(i) => Ok(ConstValue::Integer(!i)),
            ConstValue::Unsigned(u) => Ok(ConstValue::Unsigned(!u)),
            _ => Err(ConstEvalError::TypeMismatch {
                expected: "integer".to_string(),
                found: format!("{:?}", val),
                span,
            }),
        },

        _ => Err(ConstEvalError::UnsupportedOperation {
            op: format!("{:?}", op),
            span,
        }),
    }
}

/// Evaluate a constant statement
fn eval_const_stmt(
    stmt: &crate::checker::TypedStmt,
    ctx: &mut ConstEvalContext,
) -> Result<(), ConstEvalError> {
    use crate::checker::TypedStmt;

    match stmt {
        TypedStmt::Let { pattern, value, .. } => {
            let val = eval_const_expr(value, ctx)?;

            // Extract variable name from pattern
            if let Pattern::Ident { name, .. } = pattern {
                ctx.bind(name.name.clone(), val);
            }

            Ok(())
        }

        TypedStmt::Expr(expr) => {
            // Evaluate for side effects (there shouldn't be any in constant context)
            eval_const_expr(expr, ctx)?;
            Ok(())
        }

        _ => Err(ConstEvalError::NotConstant {
            span: Span::default(), // Would need to extract span from stmt
        }),
    }
}

/// Try to evaluate an expression to a constant boolean
pub fn eval_const_bool(expr: &TypedExpr, ctx: &mut ConstEvalContext) -> Option<bool> {
    eval_const_expr(expr, ctx).ok()?.as_bool()
}

/// Try to evaluate an expression to a constant integer
pub fn eval_const_integer(expr: &TypedExpr, ctx: &mut ConstEvalContext) -> Option<i128> {
    eval_const_expr(expr, ctx).ok()?.as_integer()
}

/// Check if an expression is a constant true
pub fn is_const_true(expr: &TypedExpr, ctx: &mut ConstEvalContext) -> bool {
    eval_const_bool(expr, ctx) == Some(true)
}

/// Check if an expression is a constant false
pub fn is_const_false(expr: &TypedExpr, ctx: &mut ConstEvalContext) -> bool {
    eval_const_bool(expr, ctx) == Some(false)
}

/// Check if an expression evaluates to zero
pub fn is_const_zero(expr: &TypedExpr, ctx: &mut ConstEvalContext) -> bool {
    eval_const_integer(expr, ctx) == Some(0)
}

/// Check if an expression evaluates to a non-zero value
pub fn is_const_nonzero(expr: &TypedExpr, ctx: &mut ConstEvalContext) -> bool {
    eval_const_integer(expr, ctx)
        .map(|i| i != 0)
        .unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_diagnostics::Span;

    fn make_int_expr(i: i64) -> TypedExpr {
        TypedExpr {
            kind: TypedExprKind::Literal(Literal::Integer(i)),
            ty: TypeId::INT,
            span: Span::default(),
        }
    }

    fn make_bool_expr(b: bool) -> TypedExpr {
        TypedExpr {
            kind: TypedExprKind::Literal(Literal::Bool(b)),
            ty: TypeId::BOOL,
            span: Span::default(),
        }
    }

    #[test]
    fn test_eval_literal() {
        let mut ctx = ConstEvalContext::new();

        let expr = make_int_expr(42);
        let result = eval_const_expr(&expr, &mut ctx).unwrap();
        assert_eq!(result, ConstValue::Integer(42));
    }

    #[test]
    fn test_eval_arithmetic() {
        let mut ctx = ConstEvalContext::new();

        // 2 + 3
        let expr = TypedExpr {
            kind: TypedExprKind::Binary {
                op: BinaryOp::Add,
                left: Box::new(make_int_expr(2)),
                right: Box::new(make_int_expr(3)),
            },
            ty: TypeId::INT,
            span: Span::default(),
        };

        let result = eval_const_expr(&expr, &mut ctx).unwrap();
        assert_eq!(result, ConstValue::Integer(5));
    }

    #[test]
    fn test_eval_division_by_zero() {
        let mut ctx = ConstEvalContext::new();

        // 10 / 0
        let expr = TypedExpr {
            kind: TypedExprKind::Binary {
                op: BinaryOp::Div,
                left: Box::new(make_int_expr(10)),
                right: Box::new(make_int_expr(0)),
            },
            ty: TypeId::INT,
            span: Span::default(),
        };

        let result = eval_const_expr(&expr, &mut ctx);
        assert!(matches!(result, Err(ConstEvalError::DivisionByZero { .. })));
    }

    #[test]
    fn test_eval_comparison() {
        let mut ctx = ConstEvalContext::new();

        // 5 < 10
        let expr = TypedExpr {
            kind: TypedExprKind::Binary {
                op: BinaryOp::Lt,
                left: Box::new(make_int_expr(5)),
                right: Box::new(make_int_expr(10)),
            },
            ty: TypeId::BOOL,
            span: Span::default(),
        };

        let result = eval_const_expr(&expr, &mut ctx).unwrap();
        assert_eq!(result, ConstValue::Bool(true));
    }

    #[test]
    fn test_eval_logical() {
        let mut ctx = ConstEvalContext::new();

        // true and false
        let expr = TypedExpr {
            kind: TypedExprKind::Binary {
                op: BinaryOp::And,
                left: Box::new(make_bool_expr(true)),
                right: Box::new(make_bool_expr(false)),
            },
            ty: TypeId::BOOL,
            span: Span::default(),
        };

        let result = eval_const_expr(&expr, &mut ctx).unwrap();
        assert_eq!(result, ConstValue::Bool(false));
    }

    #[test]
    fn test_eval_variable() {
        let mut ctx = ConstEvalContext::new();
        ctx.bind("x", ConstValue::Integer(100));

        let expr = TypedExpr {
            kind: TypedExprKind::Variable(jet_parser::ast::Ident {
                name: "x".to_string(),
                span: jet_lexer::Span::default(),
            }),
            ty: TypeId::INT,
            span: Span::default(),
        };

        let result = eval_const_expr(&expr, &mut ctx).unwrap();
        assert_eq!(result, ConstValue::Integer(100));
    }

    #[test]
    fn test_eval_unknown_variable() {
        let mut ctx = ConstEvalContext::new();

        let expr = TypedExpr {
            kind: TypedExprKind::Variable(jet_parser::ast::Ident {
                name: "unknown".to_string(),
                span: jet_lexer::Span::default(),
            }),
            ty: TypeId::INT,
            span: Span::default(),
        };

        let result = eval_const_expr(&expr, &mut ctx);
        assert!(matches!(
            result,
            Err(ConstEvalError::UnknownVariable { .. })
        ));
    }

    #[test]
    fn test_is_const_true() {
        let mut ctx = ConstEvalContext::new();

        assert!(is_const_true(&make_bool_expr(true), &mut ctx));
        assert!(!is_const_true(&make_bool_expr(false), &mut ctx));
    }

    #[test]
    fn test_is_const_nonzero() {
        let mut ctx = ConstEvalContext::new();

        assert!(is_const_nonzero(&make_int_expr(42), &mut ctx));
        assert!(!is_const_nonzero(&make_int_expr(0), &mut ctx));
    }
}
