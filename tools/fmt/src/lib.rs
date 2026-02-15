//! Jet Code Formatter
//!
//! An opinionated, zero-configuration code formatter for the Jet programming language.

use jet_parser::ast::*;

mod config;

pub use config::FormatConfig;

/// Formatter state and configuration
pub struct Formatter {
    config: FormatConfig,
    output: String,
    indent_level: usize,
}

impl Formatter {
    /// Create a new formatter with default configuration
    pub fn new() -> Self {
        Self::with_config(FormatConfig::default())
    }

    /// Create a formatter with custom configuration
    pub fn with_config(config: FormatConfig) -> Self {
        Self {
            config,
            output: String::new(),
            indent_level: 0,
        }
    }

    /// Format an entire module
    pub fn format_module(&mut self, module: &Module) -> String {
        self.output.clear();
        self.indent_level = 0;

        // Format module items with blank line separation
        for (i, item) in module.items.iter().enumerate() {
            if i > 0 {
                self.write_blank_line();
            }
            self.format_module_item(item);
        }

        // Ensure trailing newline
        if !self.output.ends_with('\n') {
            self.output.push('\n');
        }

        self.output.clone()
    }

    /// Format a single module item
    fn format_module_item(&mut self, item: &ModuleItem) {
        match item {
            ModuleItem::Function(func) => self.format_function(func),
            ModuleItem::Struct(struct_def) => self.format_struct(struct_def),
            ModuleItem::Enum(enum_def) => self.format_enum(enum_def),
            ModuleItem::Trait(trait_def) => self.format_trait(trait_def),
            ModuleItem::Impl(impl_def) => self.format_impl(impl_def),
            ModuleItem::Import(import) => self.format_import(import),
            ModuleItem::Const(const_def) => self.format_const(const_def),
            ModuleItem::TypeAlias(type_alias) => self.format_type_alias(type_alias),
            ModuleItem::Effect(effect_def) => self.format_effect(effect_def),
        }
    }

    /// Format a function definition
    fn format_function(&mut self, func: &Function) {
        // Visibility
        if func.public {
            self.write_str("pub ");
        }

        // Function keyword and name
        self.write_str("fn ");
        self.write_str(&func.name.name);

        // Generic parameters
        if !func.generics.is_empty() {
            self.write_str("[");
            for (i, param) in func.generics.iter().enumerate() {
                if i > 0 {
                    self.write_str(", ");
                }
                self.write_str(&param.name.name);
            }
            self.write_str("]");
        }

        // Parameters
        self.write_str("(");
        for (i, param) in func.params.iter().enumerate() {
            if i > 0 {
                self.write_str(", ");
            }
            self.format_pattern(&param.pattern);
            self.write_str(": ");
            self.format_type(&param.ty);
        }
        self.write_str(")");

        // Return type
        if let Some(ret) = &func.return_type {
            self.write_str(" -> ");
            self.format_type(ret);
        }

        // Effects
        if !func.effects.is_empty() {
            self.write_str(" ! ");
            self.write_separated(&func.effects, " | ", |f, eff| {
                f.format_type(eff);
            });
        }

        // Body
        self.write_str(":");
        self.indent_level += 1;
        self.format_expr(&func.body);
        self.indent_level -= 1;
    }

    /// Format a struct definition
    fn format_struct(&mut self, struct_def: &StructDef) {
        if struct_def.public {
            self.write_str("pub ");
        }

        self.write_str("struct ");
        self.write_str(&struct_def.name.name);

        // Generic parameters
        if !struct_def.generics.is_empty() {
            self.write_str("[");
            for (i, param) in struct_def.generics.iter().enumerate() {
                if i > 0 {
                    self.write_str(", ");
                }
                self.write_str(&param.name.name);
            }
            self.write_str("]");
        }

        self.write_str(":");
        self.indent_level += 1;

        for field in &struct_def.fields {
            self.write_newline();
            if field.public {
                self.write_str("pub ");
            }
            self.write_str(&field.name.name);
            self.write_str(": ");
            self.format_type(&field.ty);
        }

        self.indent_level -= 1;
        self.write_newline();
    }

    /// Format an enum definition
    fn format_enum(&mut self, enum_def: &EnumDef) {
        if enum_def.public {
            self.write_str("pub ");
        }

        self.write_str("enum ");
        self.write_str(&enum_def.name.name);

        // Generic parameters
        if !enum_def.generics.is_empty() {
            self.write_str("[");
            for (i, param) in enum_def.generics.iter().enumerate() {
                if i > 0 {
                    self.write_str(", ");
                }
                self.write_str(&param.name.name);
            }
            self.write_str("]");
        }

        self.write_str(":");
        self.indent_level += 1;

        for variant in &enum_def.variants {
            self.write_newline();
            self.write_str("| ");
            self.write_str(&variant.name.name);

            match &variant.body {
                VariantBody::Unit => {}
                VariantBody::Tuple(types) => {
                    self.write_str("(");
                    for (i, ty) in types.iter().enumerate() {
                        if i > 0 {
                            self.write_str(", ");
                        }
                        self.format_type(ty);
                    }
                    self.write_str(")");
                }
                VariantBody::Struct(fields) => {
                    self.write_str("(");
                    for (i, field) in fields.iter().enumerate() {
                        if i > 0 {
                            self.write_str(", ");
                        }
                        self.write_str(&field.name.name);
                        self.write_str(": ");
                        self.format_type(&field.ty);
                    }
                    self.write_str(")");
                }
                VariantBody::Discriminant(expr) => {
                    self.write_str(" = ");
                    self.format_expr(expr);
                }
            }
        }

        self.indent_level -= 1;
        self.write_newline();
    }

    /// Format a trait definition
    fn format_trait(&mut self, trait_def: &TraitDef) {
        if trait_def.public {
            self.write_str("pub ");
        }

        self.write_str("trait ");
        self.write_str(&trait_def.name.name);

        // Generic parameters
        if !trait_def.generics.is_empty() {
            self.write_str("[");
            for (i, param) in trait_def.generics.iter().enumerate() {
                if i > 0 {
                    self.write_str(", ");
                }
                self.write_str(&param.name.name);
            }
            self.write_str("]");
        }

        // Super traits
        if !trait_def.super_traits.is_empty() {
            self.write_str(": ");
            let super_traits: Vec<String> = trait_def
                .super_traits
                .iter()
                .map(|t| self.type_to_string(t))
                .collect();
            self.write_str(&super_traits.join(" + "));
        }

        self.write_str(":");
        self.indent_level += 1;

        for item in &trait_def.items {
            self.write_newline();
            match item {
                TraitItem::Method {
                    name,
                    params,
                    return_type,
                    ..
                } => {
                    self.write_str("fn ");
                    self.write_str(&name.name);
                    self.write_str("(");
                    for (i, param) in params.iter().enumerate() {
                        if i > 0 {
                            self.write_str(", ");
                        }
                        self.format_pattern(&param.pattern);
                        self.write_str(": ");
                        self.format_type(&param.ty);
                    }
                    self.write_str(")");

                    if let Some(ret) = return_type {
                        self.write_str(" -> ");
                        self.format_type(ret);
                    }
                }
                TraitItem::TypeDecl { name, .. } => {
                    self.write_str("type ");
                    self.write_str(&name.name);
                }
                TraitItem::ConstDecl { name, ty } => {
                    self.write_str("const ");
                    self.write_str(&name.name);
                    self.write_str(": ");
                    self.format_type(ty);
                }
            }
        }

        self.indent_level -= 1;
        self.write_newline();
    }

    /// Format an impl block
    fn format_impl(&mut self, impl_def: &ImplDef) {
        self.write_str("impl");

        if !impl_def.generics.is_empty() {
            self.write_str("[");
            for (i, param) in impl_def.generics.iter().enumerate() {
                if i > 0 {
                    self.write_str(", ");
                }
                self.write_str(&param.name.name);
            }
            self.write_str("]");
        }

        if let Some(trait_path) = &impl_def.trait_path {
            self.write_str(" ");
            self.format_path(trait_path);
            self.write_str(" for ");
        } else {
            self.write_str(" ");
        }
        self.format_type(&impl_def.ty);

        self.write_str(":");
        self.indent_level += 1;

        for item in &impl_def.items {
            self.write_newline();
            match item {
                ImplItem::Method(func) => {
                    self.write_str("fn ");
                    self.write_str(&func.name.name);
                    self.write_str("(...): ...");
                }
                ImplItem::Const { name, ty, .. } => {
                    self.write_str("const ");
                    self.write_str(&name.name);
                    self.write_str(": ");
                    self.format_type(ty);
                    self.write_str(" = ...");
                }
                ImplItem::TypeAlias(type_alias) => {
                    self.write_str("type ");
                    self.write_str(&type_alias.name.name);
                    self.write_str(" = ");
                    self.format_type(&type_alias.ty);
                }
            }
        }

        self.indent_level -= 1;
        self.write_newline();
    }

    /// Format an import statement
    fn format_import(&mut self, import: &Import) {
        match import {
            Import::Simple { path, alias } => {
                self.write_str("import ");
                self.format_path(path);
                if let Some(alias) = alias {
                    self.write_str(" as ");
                    self.write_str(&alias.name);
                }
            }
            Import::From { path, items } => {
                self.write_str("from ");
                self.format_path(path);
                self.write_str(" import ");
                self.format_import_items(items);
            }
        }
    }

    /// Format import items
    fn format_import_items(&mut self, items: &[ImportItem]) {
        for (i, item) in items.iter().enumerate() {
            if i > 0 {
                self.write_str(", ");
            }
            match item {
                ImportItem::Single { name, alias } => {
                    self.write_str(&name.name);
                    if let Some(alias) = alias {
                        self.write_str(" as ");
                        self.write_str(&alias.name);
                    }
                }
                ImportItem::Group(group) => {
                    self.write_str("{");
                    self.format_import_items(group);
                    self.write_str("}");
                }
            }
        }
    }

    /// Format a const definition
    fn format_const(&mut self, const_def: &ConstDef) {
        if const_def.public {
            self.write_str("pub ");
        }

        self.write_str("let ");
        self.write_str(&const_def.name.name);
        self.write_str(": ");
        self.format_type(&const_def.ty);
        self.write_str(" = ");
        self.format_expr(&const_def.value);
    }

    /// Format a type alias
    fn format_type_alias(&mut self, type_alias: &TypeAlias) {
        if type_alias.public {
            self.write_str("pub ");
        }

        self.write_str("type ");
        self.write_str(&type_alias.name.name);

        if !type_alias.generics.is_empty() {
            self.write_str("[");
            for (i, param) in type_alias.generics.iter().enumerate() {
                if i > 0 {
                    self.write_str(", ");
                }
                self.write_str(&param.name.name);
            }
            self.write_str("]");
        }

        self.write_str(" = ");
        self.format_type(&type_alias.ty);
    }

    /// Format an effect definition
    fn format_effect(&mut self, effect_def: &EffectDef) {
        if effect_def.public {
            self.write_str("pub ");
        }

        self.write_str("effect ");
        self.write_str(&effect_def.name.name);

        if !effect_def.generics.is_empty() {
            self.write_str("[");
            for (i, param) in effect_def.generics.iter().enumerate() {
                if i > 0 {
                    self.write_str(", ");
                }
                self.write_str(&param.name.name);
            }
            self.write_str("]");
        }

        self.write_str(":");
        self.indent_level += 1;

        for op in &effect_def.operations {
            self.write_newline();
            self.write_str("fn ");
            self.write_str(&op.name.name);
            self.write_str("(");
            for (i, param) in op.params.iter().enumerate() {
                if i > 0 {
                    self.write_str(", ");
                }
                self.format_pattern(&param.pattern);
                self.write_str(": ");
                self.format_type(&param.ty);
            }
            self.write_str(")");

            if let Some(ret) = &op.return_type {
                self.write_str(" -> ");
                self.format_type(ret);
            }
        }

        self.indent_level -= 1;
        self.write_newline();
    }

    /// Format a pattern
    fn format_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Ident { mutable, name } => {
                if *mutable {
                    self.write_str("mut ");
                }
                self.write_str(&name.name);
            }
            Pattern::Wildcard(_) => {
                self.write_str("_");
            }
            Pattern::Literal(lit) => {
                self.format_literal(lit);
            }
            Pattern::Tuple(patterns) => {
                self.write_str("(");
                for (i, p) in patterns.iter().enumerate() {
                    if i > 0 {
                        self.write_str(", ");
                    }
                    self.format_pattern(p);
                }
                self.write_str(")");
            }
            Pattern::Struct { path, fields, .. } => {
                self.format_path(path);
                self.write_str("(");
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        self.write_str(", ");
                    }
                    self.write_str(&field.name.name);
                    if let Some(pat) = &field.pattern {
                        self.write_str(": ");
                        self.format_pattern(pat);
                    }
                }
                self.write_str(")");
            }
            Pattern::Enum {
                path,
                variant,
                inner,
            } => {
                self.format_path(path);
                self.write_str("::");
                self.write_str(&variant.name);
                if let Some(inner_pat) = inner {
                    self.write_str("(");
                    self.format_pattern(inner_pat);
                    self.write_str(")");
                }
            }
            Pattern::Array(patterns) => {
                self.write_str("[");
                for (i, p) in patterns.iter().enumerate() {
                    if i > 0 {
                        self.write_str(", ");
                    }
                    self.format_pattern(p);
                }
                self.write_str("]");
            }
            Pattern::Rest(ident) => {
                self.write_str("..");
                if let Some(name) = ident {
                    self.write_str(&name.name);
                }
            }
            Pattern::Or(left, right) => {
                self.format_pattern(left);
                self.write_str(" | ");
                self.format_pattern(right);
            }
            Pattern::Bind { name, pattern } => {
                self.write_str(&name.name);
                self.write_str(" @ ");
                self.format_pattern(pattern);
            }
            Pattern::Mut(inner) => {
                self.write_str("mut ");
                self.format_pattern(inner);
            }
            Pattern::Ref { mutable, pattern } => {
                self.write_str("&");
                if *mutable {
                    self.write_str("mut ");
                }
                self.format_pattern(pattern);
            }
        }
    }

    /// Format a type expression
    fn format_type(&mut self, ty: &Type) {
        let s = self.type_to_string(ty);
        self.write_str(&s);
    }

    /// Convert type to string
    fn type_to_string(&self, ty: &Type) -> String {
        match ty {
            Type::Path(path) => path
                .segments
                .iter()
                .map(|s| s.name.as_str())
                .collect::<Vec<_>>()
                .join("::"),
            Type::Generic(base, args) => {
                let base_str = self.type_to_string(base);
                let args_str: Vec<String> = args.iter().map(|t| self.type_to_string(t)).collect();
                format!("{}[{}]", base_str, args_str.join(", "))
            }
            Type::Tuple(types) => {
                let inner: Vec<String> = types.iter().map(|t| self.type_to_string(t)).collect();
                format!("({})", inner.join(", "))
            }
            Type::Array(inner, size) => {
                let inner_str = self.type_to_string(inner);
                if let Some(size_expr) = size {
                    format!("[{}; {}]", inner_str, self.expr_to_string(size_expr))
                } else {
                    format!("[{}]", inner_str)
                }
            }
            Type::Function {
                params,
                return_type,
                effects,
            } => {
                let params_str: Vec<String> =
                    params.iter().map(|t| self.type_to_string(t)).collect();
                let mut result = format!("fn({})", params_str.join(", "));

                if let Some(ret) = return_type {
                    result.push_str(&format!(" -> {}", self.type_to_string(ret)));
                }

                if !effects.is_empty() {
                    result.push_str(" ! ");
                    let effects_str: Vec<String> =
                        effects.iter().map(|e| self.type_to_string(e)).collect();
                    result.push_str(&effects_str.join(" | "));
                }
                result
            }
            Type::Reference { mutable, inner } => {
                if *mutable {
                    format!("&mut {}", self.type_to_string(inner))
                } else {
                    format!("&{}", self.type_to_string(inner))
                }
            }
            Type::Channel(inner) => format!("chan[{}]", self.type_to_string(inner)),
            Type::Async(inner) => format!("async {}", self.type_to_string(inner)),
            Type::Infer => "_".to_string(),
            Type::SelfType => "Self".to_string(),
        }
    }

    /// Format an expression
    fn format_expr(&mut self, expr: &Expr) {
        let s = self.expr_to_string(expr);
        self.write_str(&s);
    }

    /// Convert expression to string
    fn expr_to_string(&self, expr: &Expr) -> String {
        match expr {
            Expr::Literal(lit) => self.literal_to_string(lit),
            Expr::Variable(ident) => ident.name.clone(),
            Expr::Path(path) => path
                .segments
                .iter()
                .map(|s| s.name.as_str())
                .collect::<Vec<_>>()
                .join("::"),
            Expr::Binary { left, op, right } => {
                let left_str = self.expr_to_string(left);
                let right_str = self.expr_to_string(right);
                let op_str = format!("{:?}", op).to_lowercase();
                format!("{} {} {}", left_str, op_str, right_str)
            }
            Expr::Unary { op, expr } => {
                let expr_str = self.expr_to_string(expr);
                let op_str = match op {
                    UnaryOp::Not => "not ",
                    UnaryOp::Neg => "-",
                    UnaryOp::BitNot => "~",
                    UnaryOp::Deref => "*",
                    UnaryOp::Ref => "&",
                    UnaryOp::RefMut => "&mut ",
                };
                format!("{}{}", op_str, expr_str)
            }
            Expr::Call { func, args } => {
                let func_str = self.expr_to_string(func);
                let args_str: Vec<String> = args.iter().map(|a| self.expr_to_string(a)).collect();
                format!("{}({})", func_str, args_str.join(", "))
            }
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => {
                let recv_str = self.expr_to_string(receiver);
                let args_str: Vec<String> = args.iter().map(|a| self.expr_to_string(a)).collect();
                format!("{}.{}({})", recv_str, method.name, args_str.join(", "))
            }
            Expr::FieldAccess { object, field } => {
                let obj_str = self.expr_to_string(object);
                format!("{}.{}", obj_str, field.name)
            }
            Expr::Index { object, index } => {
                let obj_str = self.expr_to_string(object);
                let idx_str = self.expr_to_string(index);
                format!("{}[{}]", obj_str, idx_str)
            }
            Expr::Block(block) => {
                let stmts_str: Vec<String> =
                    block.stmts.iter().map(|s| self.stmt_to_string(s)).collect();
                let expr_str = block
                    .expr
                    .as_ref()
                    .map(|e| self.expr_to_string(e))
                    .unwrap_or_default();
                if stmts_str.is_empty() && expr_str.is_empty() {
                    "{}".to_string()
                } else {
                    format!("{{ {} {} }}", stmts_str.join("; "), expr_str)
                }
            }
            Expr::If {
                cond,
                then_branch,
                else_branch,
            } => {
                let cond_str = self.expr_to_string(cond);
                let then_str = self.expr_to_string(then_branch);
                let mut result = format!("if {} then {}", cond_str, then_str);
                if let Some(else_expr) = else_branch {
                    result.push_str(&format!(" else {}", self.expr_to_string(else_expr)));
                }
                result
            }
            Expr::Match { expr, arms } => {
                let expr_str = self.expr_to_string(expr);
                let arms_str: Vec<String> = arms
                    .iter()
                    .map(|a| {
                        let pat_str = self.pattern_to_string(&a.pattern);
                        let body_str = self.expr_to_string(&a.body);
                        format!("| {} => {}", pat_str, body_str)
                    })
                    .collect();
                format!("match {} {{ {} }}", expr_str, arms_str.join(" "))
            }
            Expr::While { cond, body, .. } => {
                let cond_str = self.expr_to_string(cond);
                let body_str = self.expr_to_string(body);
                format!("while {} {}", cond_str, body_str)
            }
            Expr::For {
                pattern,
                iterable,
                body,
                ..
            } => {
                let pat_str = self.pattern_to_string(pattern);
                let iter_str = self.expr_to_string(iterable);
                let body_str = self.expr_to_string(body);
                format!("for {} in {} {}", pat_str, iter_str, body_str)
            }
            Expr::Loop { body, .. } => {
                format!("loop {}", self.expr_to_string(body))
            }
            Expr::Lambda { params, body, .. } => {
                let params_str: Vec<String> = params
                    .iter()
                    .map(|p| self.pattern_to_string(&p.pattern))
                    .collect();
                format!("|{}| {}", params_str.join(", "), self.expr_to_string(body))
            }
            Expr::Await(expr) => self.expr_to_string(expr),
            Expr::Try(expr) => {
                format!("{}?", self.expr_to_string(expr))
            }
            Expr::Assign { target, op, value } => {
                let target_str = self.expr_to_string(target);
                let value_str = self.expr_to_string(value);
                let op_str = match op {
                    AssignOp::Assign => "=",
                    AssignOp::AddAssign => "+=",
                    AssignOp::SubAssign => "-=",
                    AssignOp::MulAssign => "*=",
                    AssignOp::DivAssign => "/=",
                    AssignOp::ModAssign => "%=",
                    AssignOp::BitAndAssign => "&=",
                    AssignOp::BitOrAssign => "|=",
                    AssignOp::BitXorAssign => "^=",
                    AssignOp::ShlAssign => "<<=",
                    AssignOp::ShrAssign => ">>=",
                };
                format!("{} {} {}", target_str, op_str, value_str)
            }
            Expr::Return(expr) => {
                if let Some(e) = expr {
                    format!("return {}", self.expr_to_string(e))
                } else {
                    "return".to_string()
                }
            }
            Expr::Break { value, .. } => {
                if let Some(val) = value {
                    format!("break {}", self.expr_to_string(val))
                } else {
                    "break".to_string()
                }
            }
            Expr::Continue { .. } => "continue".to_string(),
            Expr::Tuple(exprs) => {
                let inner: Vec<String> = exprs.iter().map(|e| self.expr_to_string(e)).collect();
                format!("({})", inner.join(", "))
            }
            Expr::Array(exprs) => {
                let inner: Vec<String> = exprs.iter().map(|e| self.expr_to_string(e)).collect();
                format!("[{}]", inner.join(", "))
            }
            Expr::StructLiteral { path, fields } => {
                let path_str = self.path_to_string(path);
                let fields_str: Vec<String> = fields
                    .iter()
                    .map(|f| {
                        if let Some(val) = &f.value {
                            format!("{}: {}", f.name.name, self.expr_to_string(val))
                        } else {
                            f.name.name.clone()
                        }
                    })
                    .collect();
                format!("{} {{ {} }}", path_str, fields_str.join(", "))
            }
            Expr::Spawn(expr) => format!("spawn {}", self.expr_to_string(expr)),
            Expr::Async(_) => "async { ... }".to_string(),
            Expr::Concurrent(_) => "concurrent { ... }".to_string(),
            Expr::SelfExpr(_) => "self".to_string(),
            Expr::Pass => "pass".to_string(),
            Expr::Raise(_) => "raise ...".to_string(),
            Expr::Handle(_) => "handle ...".to_string(),
            Expr::Resume(_) => "resume ...".to_string(),
        }
    }

    /// Convert pattern to string
    fn pattern_to_string(&self, pattern: &Pattern) -> String {
        match pattern {
            Pattern::Ident { mutable, name } => {
                if *mutable {
                    format!("mut {}", name.name)
                } else {
                    name.name.clone()
                }
            }
            Pattern::Wildcard(_) => "_".to_string(),
            Pattern::Literal(lit) => self.literal_to_string(lit),
            Pattern::Tuple(patterns) => {
                let inner: Vec<String> =
                    patterns.iter().map(|p| self.pattern_to_string(p)).collect();
                format!("({})", inner.join(", "))
            }
            Pattern::Struct { path, fields, .. } => {
                let path_str = self.path_to_string(path);
                let fields_str: Vec<String> = fields
                    .iter()
                    .map(|f| {
                        if let Some(pat) = &f.pattern {
                            format!("{}: {}", f.name.name, self.pattern_to_string(pat))
                        } else {
                            f.name.name.clone()
                        }
                    })
                    .collect();
                format!("{} {{ {} }}", path_str, fields_str.join(", "))
            }
            Pattern::Enum {
                path,
                variant,
                inner,
            } => {
                let path_str = self.path_to_string(path);
                if let Some(inner_pat) = inner {
                    format!(
                        "{}::{}({})",
                        path_str,
                        variant.name,
                        self.pattern_to_string(inner_pat)
                    )
                } else {
                    format!("{}::{}", path_str, variant.name)
                }
            }
            Pattern::Array(patterns) => {
                let inner: Vec<String> =
                    patterns.iter().map(|p| self.pattern_to_string(p)).collect();
                format!("[{}]", inner.join(", "))
            }
            Pattern::Rest(ident) => {
                if let Some(name) = ident {
                    format!("..{}", name.name)
                } else {
                    "..".to_string()
                }
            }
            Pattern::Or(left, right) => {
                format!(
                    "{} | {}",
                    self.pattern_to_string(left),
                    self.pattern_to_string(right)
                )
            }
            Pattern::Bind { name, pattern } => {
                format!("{} @ {}", name.name, self.pattern_to_string(pattern))
            }
            Pattern::Mut(inner) => {
                format!("mut {}", self.pattern_to_string(inner))
            }
            Pattern::Ref { mutable, pattern } => {
                if *mutable {
                    format!("&mut {}", self.pattern_to_string(pattern))
                } else {
                    format!("&{}", self.pattern_to_string(pattern))
                }
            }
        }
    }

    /// Convert literal to string
    fn literal_to_string(&self, lit: &Literal) -> String {
        match lit {
            Literal::Integer(n) => n.to_string(),
            Literal::Float(f) => f.to_string(),
            Literal::String(s) => format!("\"{}\"", s),
            Literal::Char(c) => format!("'{}'", c),
            Literal::Bool(b) => b.to_string(),
            Literal::Unit => "unit".to_string(),
        }
    }

    /// Convert path to string
    fn path_to_string(&self, path: &Path) -> String {
        path.segments
            .iter()
            .map(|s| s.name.as_str())
            .collect::<Vec<_>>()
            .join("::")
    }

    /// Convert statement to string
    fn stmt_to_string(&self, stmt: &Stmt) -> String {
        match stmt {
            Stmt::Expr(expr) => self.expr_to_string(expr),
            Stmt::Let { pattern, ty, value } => {
                let mut result = format!("let {}", self.pattern_to_string(pattern));
                if let Some(t) = ty {
                    result.push_str(&format!(": {}", self.type_to_string(t)));
                }
                result.push_str(&format!(" = {}", self.expr_to_string(value)));
                result
            }
            Stmt::Assign { target, op, value } => {
                let target_str = self.expr_to_string(target);
                let value_str = self.expr_to_string(value);
                let op_str = match op {
                    AssignOp::Assign => "=",
                    AssignOp::AddAssign => "+=",
                    AssignOp::SubAssign => "-=",
                    AssignOp::MulAssign => "*=",
                    AssignOp::DivAssign => "/=",
                    AssignOp::ModAssign => "%=",
                    AssignOp::BitAndAssign => "&=",
                    AssignOp::BitOrAssign => "|=",
                    AssignOp::BitXorAssign => "^=",
                    AssignOp::ShlAssign => "<<=",
                    AssignOp::ShrAssign => ">>=",
                };
                format!("{} {} {}", target_str, op_str, value_str)
            }
            Stmt::Return(expr) => {
                if let Some(e) = expr {
                    format!("return {}", self.expr_to_string(e))
                } else {
                    "return".to_string()
                }
            }
            Stmt::Break { value, .. } => {
                if let Some(val) = value {
                    format!("break {}", self.expr_to_string(val))
                } else {
                    "break".to_string()
                }
            }
            Stmt::Continue { .. } => "continue".to_string(),
            Stmt::Handle { body, .. } => format!("handle {}", self.expr_to_string(body)),
        }
    }

    /// Format a path
    fn format_path(&mut self, path: &Path) {
        self.write_str(&self.path_to_string(path));
    }

    /// Format a literal value
    fn format_literal(&mut self, lit: &Literal) {
        self.write_str(&self.literal_to_string(lit));
    }

    /// Write a string to output
    fn write_str(&mut self, s: &str) {
        self.output.push_str(s);
    }

    /// Write a newline with proper indentation
    fn write_newline(&mut self) {
        self.output.push('\n');
        let indent = self.config.indent_string.repeat(self.indent_level);
        self.output.push_str(&indent);
    }

    /// Write a blank line
    fn write_blank_line(&mut self) {
        // Avoid multiple consecutive blank lines
        if !self.output.ends_with("\n\n") && !self.output.is_empty() {
            self.output.push('\n');
        }
    }

    /// Write items separated by a delimiter
    fn write_separated<T>(&mut self, items: &[T], sep: &str, mut f: impl FnMut(&mut Self, &T)) {
        for (i, item) in items.iter().enumerate() {
            if i > 0 {
                self.write_str(sep);
            }
            f(self, item);
        }
    }
}

impl Default for Formatter {
    fn default() -> Self {
        Self::new()
    }
}

/// Format a Jet source file
pub fn format_source(source: &str) -> Result<String, String> {
    use jet_lexer::Lexer;
    use jet_parser::Parser;

    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize();

    let mut parser = Parser::new(tokens);
    let module = parser.parse_module().map_err(|e| e.to_string())?;

    let mut formatter = Formatter::new();
    Ok(formatter.format_module(&module))
}

/// Format a Jet source file with custom configuration
pub fn format_source_with_config(source: &str, config: FormatConfig) -> Result<String, String> {
    use jet_lexer::Lexer;
    use jet_parser::Parser;

    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize();

    let mut parser = Parser::new(tokens);
    let module = parser.parse_module().map_err(|e| e.to_string())?;

    let mut formatter = Formatter::with_config(config);
    Ok(formatter.format_module(&module))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_simple_function() {
        let source = r#"fn main():
    let x = 42
"#;

        let result = format_source(source).unwrap();
        assert!(result.contains("fn main():"));
    }

    #[test]
    fn test_format_function_with_params() {
        let source = r#"fn add(a: int, b: int) -> int:
    return a + b
"#;

        let result = format_source(source).unwrap();
        assert!(result.contains("fn add"));
    }

    #[test]
    fn test_format_struct() {
        let source = r#"struct Point:
    x: int
    y: int
"#;

        let result = format_source(source).unwrap();
        assert!(result.contains("struct Point:"));
    }
}
