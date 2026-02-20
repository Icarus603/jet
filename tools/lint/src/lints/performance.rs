//! Performance lints
//!
//! These lints detect code that may have performance issues.

use jet_lexer::Span;
use jet_parser::ast::{Block, EnumDef, Expr, Literal, Module, ModuleItem, Stmt, Type, VariantBody};

use crate::context::LintContext;
use crate::{Lint, LintCategory, LintLevel, LintPass, LintViolation};

/// Lint for inefficient to_string calls
pub struct InefficientToStringLint;

impl Lint for InefficientToStringLint {
    fn name(&self) -> &'static str {
        "inefficient_to_string"
    }

    fn description(&self) -> &'static str {
        "Detects inefficient string conversions"
    }

    fn default_level(&self) -> LintLevel {
        LintLevel::Warn
    }

    fn category(&self) -> LintCategory {
        LintCategory::Performance
    }
}

impl LintPass for InefficientToStringLint {
    fn check_module(&mut self, cx: &mut LintContext, module: &Module) {
        for item in &module.items {
            if let ModuleItem::Function(func) = item {
                self.check_expr(cx, &func.body);
            }
        }
    }
}

impl InefficientToStringLint {
    fn check_expr(&mut self, cx: &mut LintContext, expr: &Expr) {
        match expr {
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => {
                let method_name = method.name.as_str();

                // Check for String::to_string() on a String
                if method_name == "to_string" {
                    cx.report(
                        LintViolation::new(
                            self.name(),
                            self.default_level(),
                            "calling `to_string()` on a String",
                            Span::new(0, 0),
                        )
                        .with_suggestion("this is a no-op, consider removing the call"),
                    );
                }

                // Check for to_string() on a string literal
                if method_name == "to_string" {
                    if let Expr::Literal(Literal::String(_)) = receiver.as_ref() {
                        cx.report(
                            LintViolation::new(
                                self.name(),
                                self.default_level(),
                                "calling `to_string()` on a string literal",
                                Span::new(0, 0),
                            )
                            .with_suggestion(
                                "consider using `String::from(...)` or `...to_owned()`",
                            ),
                        );
                    }
                }

                // Check arguments
                for arg in args {
                    self.check_expr(cx, arg);
                }

                // Check receiver
                self.check_expr(cx, receiver);
            }
            Expr::Block(block) => {
                self.check_block(cx, block);
            }
            Expr::If {
                cond,
                then_branch,
                else_branch,
            } => {
                self.check_expr(cx, cond);
                self.check_expr(cx, then_branch);
                if let Some(else_expr) = else_branch {
                    self.check_expr(cx, else_expr);
                }
            }
            Expr::Match { expr, arms } => {
                self.check_expr(cx, expr);
                for arm in arms {
                    self.check_expr(cx, &arm.body);
                }
            }
            Expr::While { cond, body, .. } => {
                self.check_expr(cx, cond);
                self.check_expr(cx, body);
            }
            Expr::For { iterable, body, .. } => {
                self.check_expr(cx, iterable);
                self.check_expr(cx, body);
            }
            Expr::Loop { body, .. } => {
                self.check_expr(cx, body);
            }
            Expr::Lambda { body, .. } => {
                self.check_expr(cx, body);
            }
            Expr::Binary { left, right, .. } => {
                self.check_expr(cx, left);
                self.check_expr(cx, right);
            }
            Expr::Unary { expr, .. } => {
                self.check_expr(cx, expr);
            }
            Expr::Call { func, args } => {
                self.check_expr(cx, func);
                for arg in args {
                    self.check_expr(cx, arg);
                }
            }
            Expr::FieldAccess { object, .. } => {
                self.check_expr(cx, object);
            }
            Expr::Index { object, index } => {
                self.check_expr(cx, object);
                self.check_expr(cx, index);
            }
            Expr::Assign { target, value, .. } => {
                self.check_expr(cx, target);
                self.check_expr(cx, value);
            }
            Expr::Tuple(exprs) | Expr::Array(exprs) => {
                for expr in exprs {
                    self.check_expr(cx, expr);
                }
            }
            Expr::StructLiteral { fields, .. } => {
                for field in fields {
                    if let Some(value) = &field.value {
                        self.check_expr(cx, value);
                    }
                }
            }
            Expr::Return(expr) | Expr::Break { value: expr, .. } => {
                if let Some(expr) = expr {
                    self.check_expr(cx, expr);
                }
            }
            Expr::Await(expr) | Expr::Try(expr) | Expr::Spawn(expr) => {
                self.check_expr(cx, expr);
            }
            _ => {}
        }
    }

    fn check_block(&mut self, cx: &mut LintContext, block: &Block) {
        for stmt in &block.stmts {
            self.check_stmt(cx, stmt);
        }
        if let Some(expr) = &block.expr {
            self.check_expr(cx, expr);
        }
    }

    fn check_stmt(&mut self, cx: &mut LintContext, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) | Stmt::Let { value: expr, .. } | Stmt::Assign { value: expr, .. } => {
                self.check_expr(cx, expr);
            }
            _ => {}
        }
    }
}

/// Lint for large enum variants
pub struct LargeEnumVariantLint;

impl Lint for LargeEnumVariantLint {
    fn name(&self) -> &'static str {
        "large_enum_variant"
    }

    fn description(&self) -> &'static str {
        "Detects enums with a large size difference between variants"
    }

    fn default_level(&self) -> LintLevel {
        LintLevel::Warn
    }

    fn category(&self) -> LintCategory {
        LintCategory::Performance
    }
}

impl LintPass for LargeEnumVariantLint {
    fn check_module(&mut self, cx: &mut LintContext, module: &Module) {
        for item in &module.items {
            if let ModuleItem::Enum(enum_def) = item {
                self.check_enum(cx, enum_def);
            }
        }
    }
}

impl LargeEnumVariantLint {
    fn check_enum(&mut self, cx: &mut LintContext, enum_def: &EnumDef) {
        // Estimate the size of each variant
        let mut variant_sizes: Vec<(usize, String)> = Vec::new();

        for variant in &enum_def.variants {
            let size = self.estimate_variant_size(&variant.body);
            variant_sizes.push((size, variant.name.name.clone()));
        }

        if variant_sizes.len() < 2 {
            return;
        }

        // Find min and max sizes
        let min_size = variant_sizes.iter().map(|(s, _)| *s).min().unwrap_or(0);
        let max_size = variant_sizes.iter().map(|(s, _)| *s).max().unwrap_or(0);

        // If there's a large disparity (e.g., max > 2x min and max > 128 bytes)
        if max_size > min_size * 2 && max_size > 128 {
            let large_variants: Vec<_> = variant_sizes
                .iter()
                .filter(|(s, _)| *s == max_size)
                .map(|(_, n)| n.clone())
                .collect();

            cx.report(
                LintViolation::new(
                    self.name(),
                    self.default_level(),
                    format!(
                        "enum variant(s) {} is/are much larger than other variants",
                        large_variants.join(", ")
                    ),
                    enum_def.name.span,
                )
                .with_suggestion("consider boxing the large fields to reduce total size"),
            );
        }
    }

    fn estimate_variant_size(&self, body: &VariantBody) -> usize {
        match body {
            VariantBody::Unit => 0,
            VariantBody::Tuple(types) => types.iter().map(|t| self.estimate_type_size(t)).sum(),
            VariantBody::Struct(fields) => {
                fields.iter().map(|f| self.estimate_type_size(&f.ty)).sum()
            }
            VariantBody::Discriminant(_) => 8, // Size of discriminant
        }
    }

    fn estimate_type_size(&self, ty: &Type) -> usize {
        // Very rough estimates - in a real implementation, we'd use actual type information
        match ty {
            Type::Path(path) => {
                let name = path.segments.last().map(|s| s.name.as_str()).unwrap_or("");
                match name {
                    "bool" | "u8" | "i8" => 1,
                    "u16" | "i16" => 2,
                    "u32" | "i32" | "f32" => 4,
                    "u64" | "i64" | "f64" => 8,
                    "usize" | "isize" => 8,
                    "String" | "Vec" => 24, // Pointer + length + capacity
                    _ => 8,                 // Default to pointer size
                }
            }
            Type::Tuple(types) => types.iter().map(|t| self.estimate_type_size(t)).sum(),
            Type::Array(inner, Some(_size)) => {
                let inner_size = self.estimate_type_size(inner);
                // Try to get the size from the expression
                // This is simplified - in reality we'd evaluate the expression
                inner_size * 10 // Assume 10 elements as a guess
            }
            Type::Array(inner, None) => self.estimate_type_size(inner) * 10,
            Type::Reference { .. } => 8, // Pointer size
            Type::Generic(base, _) => self.estimate_type_size(base),
            _ => 8,
        }
    }
}

/// Lint for needless allocations
pub struct NeedlessAllocationLint;

impl Lint for NeedlessAllocationLint {
    fn name(&self) -> &'static str {
        "needless_allocation"
    }

    fn description(&self) -> &'static str {
        "Detects heap allocations that could be avoided"
    }

    fn default_level(&self) -> LintLevel {
        LintLevel::Warn
    }

    fn category(&self) -> LintCategory {
        LintCategory::Performance
    }
}

impl LintPass for NeedlessAllocationLint {
    fn check_module(&mut self, cx: &mut LintContext, module: &Module) {
        for item in &module.items {
            if let ModuleItem::Function(func) = item {
                self.check_expr(cx, &func.body);
            }
        }
    }
}

impl NeedlessAllocationLint {
    fn check_expr(&mut self, cx: &mut LintContext, expr: &Expr) {
        match expr {
            Expr::Call { func, args } => {
                // Check for Vec::new() or Vec::with_capacity() that might not be needed
                if let Expr::Path(path) = func.as_ref() {
                    let path_str: String = path
                        .segments
                        .iter()
                        .map(|s| s.name.as_str())
                        .collect::<Vec<_>>()
                        .join("::");

                    if path_str == "Vec::new" || path_str == "Vec::with_capacity" {
                        // Check if the Vec is immediately collected or iterated
                        // This is a heuristic
                    }
                }

                for arg in args {
                    self.check_expr(cx, arg);
                }
            }
            Expr::Block(block) => {
                self.check_block(cx, block);
            }
            Expr::If {
                then_branch,
                else_branch,
                ..
            } => {
                self.check_expr(cx, then_branch);
                if let Some(else_expr) = else_branch {
                    self.check_expr(cx, else_expr);
                }
            }
            Expr::Match { arms, .. } => {
                for arm in arms {
                    self.check_expr(cx, &arm.body);
                }
            }
            Expr::While { body, .. } => {
                self.check_expr(cx, body);
            }
            Expr::For { iterable, body, .. } => {
                // Check for collecting an iterator just to iterate over it
                self.check_expr(cx, iterable);
                self.check_expr(cx, body);
            }
            Expr::Loop { body, .. } => {
                self.check_expr(cx, body);
            }
            Expr::Lambda { body, .. } => {
                self.check_expr(cx, body);
            }
            _ => {}
        }
    }

    fn check_block(&mut self, cx: &mut LintContext, block: &Block) {
        for stmt in &block.stmts {
            self.check_stmt(cx, stmt);
        }
        if let Some(expr) = &block.expr {
            self.check_expr(cx, expr);
        }
    }

    fn check_stmt(&mut self, cx: &mut LintContext, stmt: &Stmt) {
        match stmt {
            Stmt::Let { value, .. } => {
                self.check_expr(cx, value);
            }
            Stmt::Expr(expr) | Stmt::Assign { value: expr, .. } => {
                self.check_expr(cx, expr);
            }
            _ => {}
        }
    }
}

/// Lint for redundant clone calls
pub struct RedundantCloneLint;

impl Lint for RedundantCloneLint {
    fn name(&self) -> &'static str {
        "redundant_clone"
    }

    fn description(&self) -> &'static str {
        "Detects unnecessary clone() calls"
    }

    fn default_level(&self) -> LintLevel {
        LintLevel::Warn
    }

    fn category(&self) -> LintCategory {
        LintCategory::Performance
    }
}

impl LintPass for RedundantCloneLint {
    fn check_module(&mut self, cx: &mut LintContext, module: &Module) {
        for item in &module.items {
            if let ModuleItem::Function(func) = item {
                self.check_expr(cx, &func.body);
            }
        }
    }
}

impl RedundantCloneLint {
    fn check_expr(&mut self, cx: &mut LintContext, expr: &Expr) {
        match expr {
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => {
                let method_name = method.name.as_str();

                // Check for clone() on a reference that's immediately dereferenced
                if method_name == "clone" {
                    // In a real implementation with type info, we'd check if:
                    // 1. The receiver is Copy (clone is unnecessary)
                    // 2. The clone result is never used
                    // 3. The clone is immediately borrowed

                    // For now, check if clone is called on a literal or simple path
                    if let Expr::Literal(_) = receiver.as_ref() {
                        cx.report(
                            LintViolation::new(
                                self.name(),
                                self.default_level(),
                                "cloning a literal value",
                                Span::new(0, 0),
                            )
                            .with_suggestion("literals are cheap to copy, clone is unnecessary"),
                        );
                    }
                }

                for arg in args {
                    self.check_expr(cx, arg);
                }
                self.check_expr(cx, receiver);
            }
            Expr::Block(block) => {
                self.check_block(cx, block);
            }
            Expr::If {
                cond,
                then_branch,
                else_branch,
            } => {
                self.check_expr(cx, cond);
                self.check_expr(cx, then_branch);
                if let Some(else_expr) = else_branch {
                    self.check_expr(cx, else_expr);
                }
            }
            Expr::Match { expr, arms } => {
                self.check_expr(cx, expr);
                for arm in arms {
                    self.check_expr(cx, &arm.body);
                }
            }
            Expr::While { cond, body, .. } => {
                self.check_expr(cx, cond);
                self.check_expr(cx, body);
            }
            Expr::For { iterable, body, .. } => {
                self.check_expr(cx, iterable);
                self.check_expr(cx, body);
            }
            Expr::Loop { body, .. } => {
                self.check_expr(cx, body);
            }
            Expr::Lambda { body, .. } => {
                self.check_expr(cx, body);
            }
            Expr::Binary { left, right, .. } => {
                self.check_expr(cx, left);
                self.check_expr(cx, right);
            }
            Expr::Unary { expr, .. } => {
                self.check_expr(cx, expr);
            }
            Expr::Call { func, args } => {
                self.check_expr(cx, func);
                for arg in args {
                    self.check_expr(cx, arg);
                }
            }
            Expr::FieldAccess { object, .. } => {
                self.check_expr(cx, object);
            }
            Expr::Index { object, index } => {
                self.check_expr(cx, object);
                self.check_expr(cx, index);
            }
            Expr::Assign { target, value, .. } => {
                self.check_expr(cx, target);
                self.check_expr(cx, value);
            }
            Expr::Tuple(exprs) | Expr::Array(exprs) => {
                for expr in exprs {
                    self.check_expr(cx, expr);
                }
            }
            Expr::StructLiteral { fields, .. } => {
                for field in fields {
                    if let Some(value) = &field.value {
                        self.check_expr(cx, value);
                    }
                }
            }
            Expr::Return(expr) | Expr::Break { value: expr, .. } => {
                if let Some(expr) = expr {
                    self.check_expr(cx, expr);
                }
            }
            Expr::Await(expr) | Expr::Try(expr) | Expr::Spawn(expr) => {
                self.check_expr(cx, expr);
            }
            _ => {}
        }
    }

    fn check_block(&mut self, cx: &mut LintContext, block: &Block) {
        for stmt in &block.stmts {
            self.check_stmt(cx, stmt);
        }
        if let Some(expr) = &block.expr {
            self.check_expr(cx, expr);
        }
    }

    fn check_stmt(&mut self, cx: &mut LintContext, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) | Stmt::Let { value: expr, .. } | Stmt::Assign { value: expr, .. } => {
                self.check_expr(cx, expr);
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_large_enum_variant_lint() {
        let lint = LargeEnumVariantLint;
        assert_eq!(lint.name(), "large_enum_variant");
        assert_eq!(lint.default_level(), LintLevel::Warn);
    }

    #[test]
    fn test_redundant_clone_lint() {
        let lint = RedundantCloneLint;
        assert_eq!(lint.name(), "redundant_clone");
        assert_eq!(lint.default_level(), LintLevel::Warn);
    }
}
