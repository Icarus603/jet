//! Expression evaluator for the REPL
//!
//! This module handles parsing, type checking, and evaluation of Jet expressions.

use anyhow::{anyhow, Result};
use jet_lexer::Lexer;
use jet_parser::Parser;
use std::collections::HashMap;

/// A value in the REPL
#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Char(char),
    Tuple(Vec<Value>),
    Array(Vec<Value>),
    Function(String),
    Type(String),
}

impl Value {
    /// Check if the value is unit
    pub fn is_unit(&self) -> bool {
        matches!(self, Value::Unit)
    }

    /// Display the value
    pub fn display(&self) -> String {
        match self {
            Value::Unit => "()".to_string(),
            Value::Integer(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::String(s) => format!("\"{}\"", s),
            Value::Char(c) => format!("'{}'", c),
            Value::Tuple(t) => {
                let items: Vec<String> = t.iter().map(|v| v.display()).collect();
                format!("({})", items.join(", "))
            }
            Value::Array(a) => {
                let items: Vec<String> = a.iter().map(|v| v.display()).collect();
                format!("[{}]", items.join(", "))
            }
            Value::Function(name) => format!("<function {}>", name),
            Value::Type(name) => format!("<type {}>", name),
        }
    }
}

/// Variable binding in the REPL session
#[derive(Debug, Clone)]
pub struct Binding {
    pub name: String,
    pub ty: String,
    pub value: Value,
    pub mutable: bool,
}

/// Module import in the REPL session
#[derive(Debug, Clone)]
pub struct Import {
    pub path: String,
    pub alias: Option<String>,
}

/// The REPL evaluator
pub struct Evaluator {
    bindings: HashMap<String, Binding>,
    imports: Vec<Import>,
    current_module: Option<String>,
}

impl Evaluator {
    /// Create a new evaluator
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            imports: Vec::new(),
            current_module: None,
        }
    }

    /// Evaluate a code snippet
    pub fn evaluate(&mut self, code: &str) -> Result<Value> {
        // First, try to parse as an expression
        match self.parse_and_evaluate_expression(code) {
            Ok(value) => Ok(value),
            Err(_) => {
                // Try to parse as a statement or declaration
                self.parse_and_evaluate_statement(code)
            }
        }
    }

    /// Get the type of an expression
    pub fn get_type(&mut self, expr: &str) -> Result<String> {
        // Parse the expression
        let tokens = self.tokenize(expr)?;
        let _ast = self.parse_expr(&tokens)?;

        // For now, return a placeholder type
        // In a full implementation, this would run the type checker
        self.infer_type(expr)
    }

    /// Get documentation for a symbol
    pub fn get_documentation(&self, symbol: &str) -> Result<String> {
        // Look up in bindings
        if let Some(binding) = self.bindings.get(symbol) {
            let mut doc = format!("{}: {}", binding.name, binding.ty);
            if binding.mutable {
                doc.push_str(" (mutable)");
            }
            return Ok(doc);
        }

        // Look up in stdlib documentation
        if let Some(doc) = self.get_stdlib_documentation(symbol) {
            return Ok(doc);
        }

        Err(anyhow!("Symbol '{}' not found", symbol))
    }

    /// Load a module from file
    pub fn load_module(&mut self, path: &str) -> Result<()> {
        let source =
            std::fs::read_to_string(path).map_err(|e| anyhow!("Failed to read file: {}", e))?;

        // Parse the module
        let tokens = self.tokenize(&source)?;
        let ast = self.parse_module(&tokens)?;

        // Store as current module
        self.current_module = Some(path.to_string());

        // Process module items
        for item in &ast.items {
            self.process_module_item(item)?;
        }

        Ok(())
    }

    /// Reload the current module
    pub fn reload_current_module(&mut self) -> Result<()> {
        if let Some(path) = &self.current_module {
            let path = path.clone();
            self.load_module(&path)
        } else {
            Err(anyhow!("No module currently loaded"))
        }
    }

    /// List all imports
    pub fn list_imports(&self) -> Vec<String> {
        self.imports
            .iter()
            .map(|i| {
                if let Some(alias) = &i.alias {
                    format!("{} as {}", i.path, alias)
                } else {
                    i.path.clone()
                }
            })
            .collect()
    }

    /// List all defined variables
    pub fn list_variables(&self) -> Vec<(String, String, String)> {
        self.bindings
            .values()
            .map(|b| (b.name.clone(), b.ty.clone(), b.value.display()))
            .collect()
    }

    // Private helper methods

    fn tokenize(&self, source: &str) -> Result<Vec<jet_lexer::SpannedToken>> {
        let mut lexer = Lexer::new(source);
        Ok(lexer.tokenize())
    }

    fn parse_expr(&self, tokens: &[jet_lexer::SpannedToken]) -> Result<jet_parser::ast::Expr> {
        let mut parser = Parser::new(tokens.to_vec());
        parser
            .parse_expr()
            .map_err(|e| anyhow!("Parse error: {:?}", e))
    }

    fn parse_module(&self, tokens: &[jet_lexer::SpannedToken]) -> Result<jet_parser::ast::Module> {
        let mut parser = Parser::new(tokens.to_vec());
        parser
            .parse_module()
            .map_err(|e| anyhow!("Parse error: {:?}", e))
    }

    fn parse_and_evaluate_expression(&mut self, code: &str) -> Result<Value> {
        let tokens = self.tokenize(code)?;
        let _ast = self.parse_expr(&tokens)?;

        // For now, do simple evaluation based on the code
        // In a full implementation, this would:
        // 1. Resolve names
        // 2. Type check
        // 3. Lower to IR
        // 4. Compile and execute
        self.evaluate_simple(code)
    }

    fn parse_and_evaluate_statement(&mut self, code: &str) -> Result<Value> {
        // Try to parse as a let binding
        if code.trim().starts_with("let ") {
            return self.evaluate_let_binding(code);
        }

        // Try to parse as an import
        if code.trim().starts_with("import ") {
            return self.evaluate_import(code);
        }

        // Try to parse as a function definition
        if code.trim().starts_with("fn ") {
            return self.evaluate_function_def(code);
        }

        Err(anyhow!("Unable to parse statement"))
    }

    fn evaluate_let_binding(&mut self, code: &str) -> Result<Value> {
        // Simplified let binding evaluation
        let trimmed = code.trim();
        let without_let = trimmed.strip_prefix("let ").unwrap_or(trimmed);

        // Try to find the equals sign
        if let Some(eq_pos) = without_let.find('=') {
            let name_part = &without_let[..eq_pos].trim();
            let value_part = &without_let[eq_pos + 1..].trim();

            // Check for mut keyword
            let (name, mutable): (&str, bool) = if name_part.starts_with("mut ") {
                let name = name_part.strip_prefix("mut ").unwrap().trim();
                (name, true)
            } else {
                (name_part, false)
            };

            // Evaluate the value
            let value = self.evaluate_simple(value_part)?;
            let ty = self.infer_type(value_part)?;

            // Store the binding
            let binding = Binding {
                name: name.to_string(),
                ty: ty.clone(),
                value: value.clone(),
                mutable,
            };

            self.bindings.insert(name.to_string(), binding);

            // Print the binding
            println!("{}: {} = {}", name, ty, value.display());
        }

        Ok(Value::Unit)
    }

    fn evaluate_import(&mut self, code: &str) -> Result<Value> {
        // Simplified import handling
        let trimmed = code.trim();
        let path = trimmed
            .strip_prefix("import ")
            .unwrap_or(trimmed)
            .trim()
            .to_string();

        self.imports.push(Import { path, alias: None });
        Ok(Value::Unit)
    }

    fn evaluate_function_def(&mut self, code: &str) -> Result<Value> {
        // Simplified function definition
        let trimmed = code.trim();
        let without_fn = trimmed.strip_prefix("fn ").unwrap_or(trimmed);

        // Extract function name (simplified)
        if let Some(name_end) = without_fn.find(|c: char| c == '(' || c.is_whitespace()) {
            let name = &without_fn[..name_end];
            let func_value = Value::Function(name.to_string());

            let binding = Binding {
                name: name.to_string(),
                ty: "fn(...)".to_string(),
                value: func_value.clone(),
                mutable: false,
            };

            self.bindings.insert(name.to_string(), binding);

            println!("{}: fn(...) = <function>", name);
        }

        Ok(Value::Unit)
    }

    fn evaluate_simple(&self, code: &str) -> Result<Value> {
        // Simple evaluation for literals
        let trimmed = code.trim();

        // Try integer
        if let Ok(i) = trimmed.parse::<i64>() {
            return Ok(Value::Integer(i));
        }

        // Try float
        if let Ok(f) = trimmed.parse::<f64>() {
            return Ok(Value::Float(f));
        }

        // Try bool
        if trimmed == "true" {
            return Ok(Value::Bool(true));
        }
        if trimmed == "false" {
            return Ok(Value::Bool(false));
        }

        // Try string literal
        if trimmed.starts_with('"') && trimmed.ends_with('"') && trimmed.len() > 1 {
            let content = &trimmed[1..trimmed.len() - 1];
            return Ok(Value::String(content.to_string()));
        }

        // Try char literal
        if trimmed.starts_with('\'') && trimmed.ends_with('\'') && trimmed.len() == 3 {
            let ch = trimmed.chars().nth(1).unwrap();
            return Ok(Value::Char(ch));
        }

        // Check for variable reference
        if let Some(binding) = self.bindings.get(trimmed) {
            return Ok(binding.value.clone());
        }

        // Simple binary operations
        if let Some(result) = self.evaluate_binary_op(trimmed) {
            return Ok(result);
        }

        Ok(Value::Unit)
    }

    fn evaluate_binary_op(&self, code: &str) -> Option<Value> {
        // Handle simple binary operations for integers
        for op in [" + ", " - ", " * ", " / ", " % "] {
            if let Some(pos) = code.find(op) {
                let left = code[..pos].trim();
                let right = code[pos + op.len()..].trim();

                let left_val = self.evaluate_simple(left).ok()?;
                let right_val = self.evaluate_simple(right).ok()?;

                match (left_val, right_val) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        return match op.trim() {
                            "+" => Some(Value::Integer(l + r)),
                            "-" => Some(Value::Integer(l - r)),
                            "*" => Some(Value::Integer(l * r)),
                            "/" => Some(Value::Integer(l / r)),
                            "%" => Some(Value::Integer(l % r)),
                            _ => None,
                        };
                    }
                    (Value::Float(l), Value::Float(r)) => {
                        return match op.trim() {
                            "+" => Some(Value::Float(l + r)),
                            "-" => Some(Value::Float(l - r)),
                            "*" => Some(Value::Float(l * r)),
                            "/" => Some(Value::Float(l / r)),
                            _ => None,
                        };
                    }
                    _ => {}
                }
            }
        }
        None
    }

    fn infer_type(&self, expr: &str) -> Result<String> {
        // Simplified type inference
        let trimmed = expr.trim();

        if trimmed.parse::<i64>().is_ok() {
            Ok("i32".to_string())
        } else if trimmed.parse::<f64>().is_ok() {
            Ok("float".to_string())
        } else if trimmed == "true" || trimmed == "false" {
            Ok("bool".to_string())
        } else if trimmed.starts_with('"') && trimmed.ends_with('"') {
            Ok("string".to_string())
        } else if trimmed.starts_with('\'') && trimmed.ends_with('\'') {
            Ok("char".to_string())
        } else if self.bindings.contains_key(trimmed) {
            Ok(self.bindings[trimmed].ty.clone())
        } else {
            Ok("<inferred>".to_string())
        }
    }

    fn process_module_item(&mut self, item: &jet_parser::ast::ModuleItem) -> Result<()> {
        match item {
            jet_parser::ast::ModuleItem::Function(func) => {
                let name = func.name.name.clone();
                let value = Value::Function(name.clone());
                let binding = Binding {
                    name: name.clone(),
                    ty: self.format_function_type(func),
                    value,
                    mutable: false,
                };
                self.bindings.insert(name, binding);
            }
            jet_parser::ast::ModuleItem::Const(const_def) => {
                let name = const_def.name.name.clone();
                let ty = self.format_type(&const_def.ty);
                let binding = Binding {
                    name: name.clone(),
                    ty,
                    value: Value::Unit,
                    mutable: false,
                };
                self.bindings.insert(name, binding);
            }
            _ => {
                // Other items are handled differently
            }
        }
        Ok(())
    }

    fn format_function_type(&self, func: &jet_parser::ast::Function) -> String {
        let params: Vec<String> = func
            .params
            .iter()
            .map(|p| self.format_type(&p.ty))
            .collect();

        let ret = func
            .return_type
            .as_ref()
            .map(|t| self.format_type(t))
            .unwrap_or_else(|| "()".to_string());

        format!("fn({}) -> {}", params.join(", "), ret)
    }

    fn format_type(&self, ty: &jet_parser::ast::Type) -> String {
        use jet_parser::ast::Type;

        match ty {
            Type::Path(path) => path
                .segments
                .last()
                .map(|s| s.name.clone())
                .unwrap_or_else(|| "<unknown>".to_string()),
            Type::Generic(base, args) => {
                let base_str = self.format_type(base);
                let args_str: Vec<String> = args.iter().map(|t| self.format_type(t)).collect();
                format!("{}[{}]", base_str, args_str.join(", "))
            }
            Type::Tuple(types) => {
                let inner: Vec<String> = types.iter().map(|t| self.format_type(t)).collect();
                format!("({})", inner.join(", "))
            }
            Type::Array(inner, _) => format!("[{}]", self.format_type(inner)),
            Type::Function {
                params,
                return_type,
                ..
            } => {
                let params_str: Vec<String> = params.iter().map(|t| self.format_type(t)).collect();
                let ret = return_type
                    .as_ref()
                    .map(|t| self.format_type(t))
                    .unwrap_or_else(|| "()".to_string());
                format!("fn({}) -> {}", params_str.join(", "), ret)
            }
            Type::Reference { mutable, inner } => {
                if *mutable {
                    format!("&mut {}", self.format_type(inner))
                } else {
                    format!("&{}", self.format_type(inner))
                }
            }
            Type::Channel(inner) => format!("chan[{}]", self.format_type(inner)),
            Type::Async(inner) => format!("async {}", self.format_type(inner)),
            Type::Infer => "_".to_string(),
            Type::SelfType => "Self".to_string(),
        }
    }

    fn get_stdlib_documentation(&self, symbol: &str) -> Option<String> {
        // Built-in documentation for common types and functions
        match symbol {
            "int" => Some("Type: Signed integer (platform-dependent size)".to_string()),
            "i32" => Some("Type: 32-bit signed integer".to_string()),
            "i64" => Some("Type: 64-bit signed integer".to_string()),
            "uint" => Some("Type: Unsigned integer (platform-dependent size)".to_string()),
            "u32" => Some("Type: 32-bit unsigned integer".to_string()),
            "u64" => Some("Type: 64-bit unsigned integer".to_string()),
            "float" => Some("Type: 64-bit floating-point number".to_string()),
            "f32" => Some("Type: 32-bit floating-point number".to_string()),
            "f64" => Some("Type: 64-bit floating-point number".to_string()),
            "bool" => Some("Type: Boolean (true or false)".to_string()),
            "string" => Some("Type: UTF-8 encoded string".to_string()),
            "char" => Some("Type: Unicode character".to_string()),
            "unit" => Some("Type: Unit type (empty tuple, represented as ())".to_string()),
            "print" => Some("Function: Print a value to stdout".to_string()),
            "println" => Some("Function: Print a value to stdout with a newline".to_string()),
            _ => None,
        }
    }
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}
