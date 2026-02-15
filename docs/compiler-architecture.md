# Jet Compiler Architecture

## Overview

Jet is a statically-typed, compiled programming language featuring Python-like syntax with significant indentation, Rust-level type safety, and Hindley-Milner type inference. The compiler is implemented in Rust and uses LLVM as its backend via the `inkwell` crate.

## Design Principles

1. **Modularity**: Each compiler phase is a separate crate with well-defined interfaces
2. **Error Recovery**: Continue compilation past errors to report multiple issues
3. **Performance**: Streaming lexer, incremental parsing, and parallel type checking
4. **Diagnostics**: Rich error messages with source location tracking
5. **Testability**: Each phase independently testable with golden file tests

---

## 1. Compiler Pipeline

```
Source File (.jet)
       |
       v
┌─────────────────┐
│   Lexer         │  jet-lexer crate
│   (Streaming)   │  Output: TokenStream
└────────┬────────┘
         |
         v
┌─────────────────┐
│   Parser        │  jet-parser crate
│   (Recursive    │  Output: AST (Concrete Syntax Tree)
│    Descent)     │
└────────┬────────┘
         |
         v
┌─────────────────┐
│   Name Resolver │  jet-resolve crate
│                 │  Output: AST with resolved names
└────────┬────────┘
         |
         v
┌─────────────────┐
│   Type Checker  │  jet-typeck crate
│   (Hindley-     │  Output: Typed AST (TAST)
│    Milner)      │  Side effect: Type constraints solved
└────────┬────────┘
         |
         v
┌─────────────────┐
│   Effect Check  │  jet-effect crate
│                 │  Output: Effect-annotated TAST
└────────┬────────┘
         |
         v
┌─────────────────┐
│   IR Lowering   │  jet-ir crate
│                 │  Output: Jet IR (SSA-based)
└────────┬────────┘
         |
         v
┌─────────────────┐
│   Optimizations │  jet-opt crate
│                 │  Output: Optimized Jet IR
└────────┬────────┘
         |
         v
┌─────────────────┐
│   LLVM Codegen  │  jet-codegen crate
│   (inkwell)     │  Output: LLVM IR
└────────┬────────┘
         |
         v
┌─────────────────┐
│   LLVM Backend  │  LLVM toolchain
│                 │  Output: Object file / Binary
└─────────────────┘
```

### Data Structures Between Phases

| Phase | Input | Output |
|-------|-------|--------|
| Lexer | `&str` (source) | `TokenStream` |
| Parser | `TokenStream` | `ast::Module` |
| Name Resolver | `ast::Module` | `ast::Module` + `SymbolTable` |
| Type Checker | `ast::Module` + `SymbolTable` | `tast::Module` |
| Effect Check | `tast::Module` | `tast::Module` (with effects) |
| IR Lowering | `tast::Module` | `ir::Module` |
| Optimizations | `ir::Module` | `ir::Module` |
| Codegen | `ir::Module` | `inkwell::module::Module` |

### Error Handling Strategy

All phases use a unified error type:

```rust
// jet-diagnostics crate
pub struct Diagnostic {
    pub level: Level,           // Error, Warning, Note
    pub message: String,
    pub span: SourceSpan,
    pub labels: Vec<Label>,
    pub notes: Vec<String>,
    pub error_code: ErrorCode,  // e.g., E0001
}

pub struct SourceSpan {
    pub file: FileId,
    pub start: ByteOffset,
    pub end: ByteOffset,
}

pub struct Label {
    pub span: SourceSpan,
    pub message: String,
    pub style: LabelStyle,  // Primary, Secondary
}
```

Each phase returns a `Result<T, Vec<Diagnostic>>`. The driver collects all diagnostics and continues where possible:

```rust
pub enum PhaseResult<T> {
    Ok(T),
    Partial(T, Vec<Diagnostic>),  // Continue with partial result
    Err(Vec<Diagnostic>),         // Fatal, cannot continue
}
```

---

## 2. Phase 1: Lexical Analysis

### Crate: `jet-lexer`

**Dependencies:**
- `jet-diagnostics` (for error reporting)
- `logos` (optional, for regex-based tokenization)

### Design

The lexer produces a **token stream with explicit indentation tokens** rather than using an indentation stack in the parser. This simplifies the parser significantly.

### Token Types

```rust
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Indentation (synthesized)
    Indent,           // Increase indentation level
    Dedent,           // Decrease indentation level
    Newline,          // Line break (significant)

    // Keywords
    Fn, Let, Mut, If, Else, Elif, Match, While, For, In,
    Return, Break, Continue, Struct, Enum, Trait, Impl,
    Pub, Priv, Async, Await, Effect, Handle, With, Resume,
    Type, Where, As, Use, Mod, Const, Static, True, False,

    // Literals
    Integer(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),

    // Identifiers
    Identifier(String),

    // Operators
    Plus, Minus, Star, Slash, Percent,
    Eq, EqEq, NotEq, Lt, Gt, Le, Ge,
    And, Or, Not,
    Ampersand, Pipe, Caret, Tilde, Shl, Shr,
    PlusEq, MinusEq, StarEq, SlashEq, PercentEq,
    AndEq, OrEq, XorEq, ShlEq, ShrEq,

    // Delimiters
    LParen, RParen,
    LBracket, RBracket,
    LBrace, RBrace,

    // Punctuation
    Comma, Dot, DotDot, DotDotEq, Colon, ColonColon,
    Arrow, FatArrow, Semi, Underscore,

    // Type-related
    Question,      // ? for error types
    Bang,          // ! for never type

    // End of file
    Eof,
}
```

### Indentation Handling

```rust
pub struct Lexer<'a> {
    source: &'a str,
    chars: Peekable<Chars<'a>>,
    position: ByteOffset,
    indent_stack: Vec<usize>,      // Stack of indentation levels
    pending_dedents: usize,        // Dedent tokens to emit
    at_line_start: bool,
}

impl<'a> Lexer<'a> {
    pub fn next_token(&mut self) -> TokenWithSpan {
        // Handle pending dedents from previous line
        if self.pending_dedents > 0 {
            self.pending_dedents -= 1;
            self.indent_stack.pop();
            return self.token(Token::Dedent);
        }

        // Skip whitespace, track indentation at line start
        if self.at_line_start {
            let indent = self.consume_indentation();
            let current_indent = *self.indent_stack.last().unwrap_or(&0);

            if indent > current_indent {
                self.indent_stack.push(indent);
                return self.token(Token::Indent);
            } else if indent < current_indent {
                // Calculate how many dedents needed
                while self.indent_stack.last().map(|&i| i > indent).unwrap_or(false) {
                    self.pending_dedents += 1;
                    self.indent_stack.pop();
                }
                if self.pending_dedents > 0 {
                    self.pending_dedents -= 1;
                    return self.token(Token::Dedent);
                }
                // Indentation doesn't match any level - error
                return self.error(Diagnostic::invalid_indentation(indent, &self.indent_stack));
            }
        }

        // ... rest of tokenization
    }
}
```

### Token Stream

```rust
pub struct TokenStream {
    tokens: Vec<TokenWithSpan>,
}

pub struct TokenWithSpan {
    pub token: Token,
    pub span: SourceSpan,
}
```

### Performance Considerations

1. **Streaming**: The lexer operates as an iterator, not loading all tokens at once
2. **Zero-copy**: String literals reference the source string until interning
3. **Lookahead**: The parser needs 2-token lookahead for some constructs

---

## 3. Phase 2: Parsing

### Crate: `jet-parser`

**Dependencies:**
- `jet-lexer`
- `jet-diagnostics`
- `jet-ast`

### Parser Type: Recursive Descent with Pratt Parsing

Recursive descent is chosen for:
- Easy to write and maintain
- Excellent error messages
- Natural handling of Python-style indentation
- Straightforward recovery strategies

Pratt parsing (top-down operator precedence) handles expressions.

### AST Structure

```rust
// jet-ast crate

pub struct Module {
    pub span: Span,
    pub items: Vec<Item>,
}

pub enum Item {
    Function(Function),
    Struct(StructDef),
    Enum(EnumDef),
    Trait(TraitDef),
    Impl(ImplBlock),
    TypeAlias(TypeAlias),
    Import(Import),
    Module(ModuleDecl),
    Effect(EffectDef),
}

pub struct Function {
    pub span: Span,
    pub name: Ident,
    pub generics: Vec<GenericParam>,
    pub params: Vec<Param>,
    pub ret_type: Option<Type>,
    pub effects: Vec<Effect>,
    pub body: Block,
    pub is_pub: bool,
    pub is_async: bool,
}

pub struct Param {
    pub span: Span,
    pub name: Ident,
    pub ty: Option<Type>,  // Optional: can be inferred
    pub is_mut: bool,
}

pub struct Block {
    pub span: Span,
    pub stmts: Vec<Stmt>,
    pub expr: Option<Box<Expr>>,
}

pub enum Stmt {
    Let(LetStmt),
    Expr(Expr),
    Assign(AssignStmt),
    While(WhileStmt),
    For(ForStmt),
    Return(Option<Expr>),
    Break,
    Continue,
    Handle(HandleStmt),
}

pub struct LetStmt {
    pub span: Span,
    pub pattern: Pattern,
    pub ty: Option<Type>,
    pub init: Expr,
    pub is_mut: bool,
}

pub enum Expr {
    Literal(Literal),
    Identifier(Ident),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Call(CallExpr),
    MethodCall(MethodCallExpr),
    FieldAccess(FieldAccessExpr),
    Index(IndexExpr),
    If(IfExpr),
    Match(MatchExpr),
    Block(Block),
    Lambda(LambdaExpr),
    Tuple(Vec<Expr>),
    Array(Vec<Expr>),
    StructLiteral(StructLiteral),
    Await(Box<Expr>),
    Resume(ResumeExpr),
    Grouped(Box<Expr>),
}

pub enum Pattern {
    Wildcard,
    Identifier(Ident, bool),  // name, is_mut
    Literal(Literal),
    Tuple(Vec<Pattern>),
    Struct(StructPattern),
    Enum(EnumPattern),
    Or(Vec<Pattern>),
    Range(Box<Pattern>, Box<Pattern>),
}

pub enum Type {
    Named(Path, Vec<Type>),     // Vec<T>, Option<T>
    Tuple(Vec<Type>),
    Function(FunctionType),
    Reference(Box<Type>, bool), // &T or &mut T
    Array(Box<Type>, Option<usize>),
    Slice(Box<Type>),
    Infer,                      // _
    SelfType,
    Never,                      // !
}

pub struct Effect {
    pub span: Span,
    pub name: Path,
    pub args: Vec<Type>,
}

pub struct EffectDef {
    pub span: Span,
    pub name: Ident,
    pub generics: Vec<GenericParam>,
    pub operations: Vec<Operation>,
}

pub struct Operation {
    pub span: Span,
    pub name: Ident,
    pub params: Vec<Param>,
    pub ret_type: Type,
}
```

### Parser Implementation

```rust
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: TokenWithSpan,
    peek: TokenWithSpan,
    diagnostics: Vec<Diagnostic>,
}

impl<'a> Parser<'a> {
    pub fn parse_module(&mut self) -> Result<Module, Vec<Diagnostic>> {
        let mut items = Vec::new();
        while !self.is_at_end() {
            match self.parse_item() {
                Ok(item) => items.push(item),
                Err(diag) => {
                    self.diagnostics.push(diag);
                    self.synchronize();
                }
            }
        }
        if self.diagnostics.is_empty() {
            Ok(Module { span: self.span(), items })
        } else {
            Err(self.diagnostics.clone())
        }
    }

    // Pratt parser for expressions
    pub fn parse_expr(&mut self, min_precedence: u8) -> Result<Expr, Diagnostic> {
        let mut left = self.parse_prefix()?;

        while self.get_precedence() >= min_precedence {
            let op = self.parse_infix_op()?;
            let right = self.parse_expr(self.get_precedence() + 1)?;
            left = Expr::Binary(BinaryExpr {
                span: self.span_from(left.span()),
                left: Box::new(left),
                op,
                right: Box::new(right),
            });
        }

        Ok(left)
    }

    // Block parsing with indentation awareness
    fn parse_block(&mut self) -> Result<Block, Diagnostic> {
        self.expect(Token::Colon)?;
        self.expect(Token::Indent)?;

        let start = self.current.span.start;
        let mut stmts = Vec::new();

        while !self.check(Token::Dedent) && !self.is_at_end() {
            stmts.push(self.parse_stmt()?);
            // Statements are newline-separated
            if self.check(Token::Newline) {
                self.advance();
            }
        }

        self.expect(Token::Dedent)?;

        Ok(Block {
            span: Span::new(start, self.previous.span.end),
            stmts,
            expr: None,  // Last statement might be expression
        })
    }
}
```

### Error Recovery

```rust
impl<'a> Parser<'a> {
    fn synchronize(&mut self) {
        // Skip tokens until we find a likely statement boundary
        while !self.is_at_end() {
            match self.current.token {
                Token::Fn | Token::Let | Token::Struct |
                Token::Enum | Token::Trait | Token::Impl |
                Token::Pub | Token::Priv | Token::Indent => return,
                Token::Newline => {
                    self.advance();
                    return;
                }
                _ => self.advance(),
            }
        }
    }
}
```

---

## 4. Phase 3: Type Checking

### Crate: `jet-typeck`

**Dependencies:**
- `jet-ast`
- `jet-diagnostics`
- `jet-types` (shared type definitions)

### Type Representation

```rust
// jet-types crate

pub type TypeId = u32;

pub enum TypeKind {
    // Concrete types
    Int(IntSize),           // i8, i16, i32, i64, isize
    Uint(IntSize),          // u8, u16, u32, u64, usize
    Float(FloatSize),       // f32, f64
    Bool,
    Char,
    String,
    Unit,                   // ()
    Never,                  // !

    // Composite types
    Tuple(Vec<TypeId>),
    Array(TypeId, usize),
    Slice(TypeId),
    Struct(DefId),
    Enum(DefId),

    // Function types with effects
    Function {
        params: Vec<TypeId>,
        ret: TypeId,
        effects: EffectSet,
    },

    // Reference types
    Ref(TypeId, Mutability),
    RawPtr(TypeId, Mutability),

    // Type variables for inference
    Var(VarId),

    // Generic parameters
    Param(DefId),
}

pub struct Type {
    pub kind: TypeKind,
    pub span: Option<Span>,
}

pub type VarId = u32;

pub struct TypeVar {
    pub id: VarId,
    pub kind: TypeVarKind,
}

pub enum TypeVarKind {
    Unbound { level: Level },
    Link(TypeId),
    Generic(DefId),
}

pub type Level = u32;  // For let-generalization
```

### Hindley-Milner Type Inference

```rust
pub struct TypeChecker<'tcx> {
    tcx: &'tcx mut TypeContext,
    substitutions: Substitution,
    var_counter: VarId,
    current_level: Level,
    diagnostics: Vec<Diagnostic>,
}

impl<'tcx> TypeChecker<'tcx> {
    /// Infer type of expression, returning a typed AST node
    pub fn infer_expr(&mut self, expr: &ast::Expr) -> Result<(tast::Expr, TypeId), Diagnostic> {
        match expr {
            ast::Expr::Literal(lit) => {
                let ty = self.type_of_literal(lit);
                Ok((tast::Expr::Literal(lit.clone(), ty), ty))
            }

            ast::Expr::Identifier(ident) => {
                let ty = self.lookup_variable(ident)?;
                Ok((tast::Expr::Variable(ident.clone(), ty), ty))
            }

            ast::Expr::Binary(bin) => {
                let (left, left_ty) = self.infer_expr(&bin.left)?;
                let (right, right_ty) = self.infer_expr(&bin.right)?;

                // Get expected types for this operator
                let (expected_lhs, expected_rhs, result_ty) =
                    self.operator_types(bin.op);

                self.unify(left_ty, expected_lhs)?;
                self.unify(right_ty, expected_rhs)?;

                Ok((tast::Expr::Binary {
                    op: bin.op,
                    left: Box::new(left),
                    right: Box::new(right),
                    ty: result_ty,
                }, result_ty))
            }

            ast::Expr::Call(call) => {
                let (func, func_ty) = self.infer_expr(&call.func)?;

                // Create fresh type variable for return
                let ret_var = self.fresh_var();

                // Build expected function type
                let mut arg_types = Vec::new();
                for arg in &call.args {
                    let (typed_arg, arg_ty) = self.infer_expr(arg)?;
                    arg_types.push((typed_arg, arg_ty));
                }

                let expected_func = self.tcx.mk_function(
                    arg_types.iter().map(|(_, t)| *t).collect(),
                    ret_var,
                    EffectSet::default(),
                );

                self.unify(func_ty, expected_func)?;

                Ok((tast::Expr::Call {
                    func: Box::new(func),
                    args: arg_types.into_iter().map(|(e, _)| e).collect(),
                    ty: ret_var,
                }, ret_var))
            }

            ast::Expr::Lambda(lambda) => {
                // Enter new scope for lambda parameters
                self.enter_scope();

                // Create type variables for parameters
                let mut param_types = Vec::new();
                let mut params = Vec::new();

                for param in &lambda.params {
                    let ty = if let Some(annot) = &param.ty {
                        self.resolve_type(annot)?
                    } else {
                        self.fresh_var()
                    };
                    param_types.push(ty);
                    params.push(tast::Param {
                        name: param.name.clone(),
                        ty,
                        is_mut: param.is_mut,
                    });
                    self.bind_variable(param.name.clone(), ty);
                }

                // Infer body type
                let (body, body_ty) = self.infer_expr(&lambda.body)?;

                self.exit_scope();

                let lambda_ty = self.tcx.mk_function(param_types, body_ty, lambda.effects.clone());

                Ok((tast::Expr::Lambda {
                    params,
                    body: Box::new(body),
                    ty: lambda_ty,
                }, lambda_ty))
            }

            // ... other cases
        }
    }

    /// Unify two types, updating substitutions
    pub fn unify(&mut self, t1: TypeId, t2: TypeId) -> Result<(), Diagnostic> {
        let t1 = self.follow_links(t1);
        let t2 = self.follow_links(t2);

        if t1 == t2 {
            return Ok(());
        }

        match (self.tcx.type_kind(t1), self.tcx.type_kind(t2)) {
            (TypeKind::Var(v1), TypeKind::Var(v2)) => {
                // Occurs check
                if self.occurs_in(v1, t2) {
                    return Err(self.infinite_type_error(v1, t2));
                }
                // Link lower level to higher level
                self.link_vars(v1, v2);
                Ok(())
            }

            (TypeKind::Var(v), other) | (other, TypeKind::Var(v)) => {
                if self.occurs_in(v, if matches!(other, TypeKind::Var(_)) { t1 } else { t2 }) {
                    return Err(self.infinite_type_error(v, t1));
                }
                self.substitutions.insert(v, t1);
                Ok(())
            }

            (TypeKind::Function { params: p1, ret: r1, effects: e1 },
             TypeKind::Function { params: p2, ret: r2, effects: e2 }) => {
                if p1.len() != p2.len() {
                    return Err(self.arity_mismatch(p1.len(), p2.len()));
                }
                for (a, b) in p1.iter().zip(p2.iter()) {
                    self.unify(*a, *b)?;
                }
                self.unify(r1, r2)?;
                self.unify_effects(e1, e2)?;
                Ok(())
            }

            (TypeKind::Tuple(ts1), TypeKind::Tuple(ts2)) => {
                if ts1.len() != ts2.len() {
                    return Err(self.tuple_size_mismatch(ts1.len(), ts2.len()));
                }
                for (a, b) in ts1.iter().zip(ts2.iter()) {
                    self.unify(*a, *b)?;
                }
                Ok(())
            }

            (k1, k2) if k1 == k2 => Ok(()),

            _ => Err(self.type_mismatch(t1, t2)),
        }
    }

    /// Let-generalization: generalize free type variables at current level
    fn generalize(&mut self, ty: TypeId) -> Scheme {
        let free_vars = self.free_vars(ty);
        let gen_vars: Vec<_> = free_vars
            .into_iter()
            .filter(|&v| self.var_level(v) > self.current_level)
            .collect();

        Scheme {
            vars: gen_vars,
            ty,
        }
    }

    /// Instantiate a polymorphic type with fresh variables
    fn instantiate(&mut self, scheme: &Scheme) -> TypeId {
        let subst: HashMap<VarId, TypeId> = scheme.vars
            .iter()
            .map(|&v| (v, self.fresh_var()))
            .collect();

        self.apply_subst(ty, &subst)
    }
}
```

### Effect Tracking

```rust
#[derive(Clone, Default, PartialEq, Eq)]
pub struct EffectSet {
    effects: Vec<EffectInstance>,
}

pub struct EffectInstance {
    pub effect: DefId,           // Which effect type
    pub args: Vec<TypeId>,       // Effect type arguments
    pub source: EffectSource,    // Where it comes from
}

pub enum EffectSource {
    Explicit,        // Declared in function signature
    FromCall(DefId), // Inherited from another function call
    FromHandler,     // Introduced by handler
}

impl TypeChecker<'_> {
    /// Check that effects in expression are handled or declared
    fn check_effects(&mut self, expr: &tast::Expr, allowed: &EffectSet) -> Result<(), Diagnostic> {
        match expr {
            tast::Expr::Call { func, .. } => {
                let func_effects = self.get_function_effects(func);
                for effect in func_effects.iter() {
                    if !allowed.contains(effect) {
                        return Err(self.unhandled_effect(effect, expr.span()));
                    }
                }
                Ok(())
            }

            tast::Expr::Handle { body, handlers, .. } => {
                // Effects in handlers are allowed within the handler bodies
                let mut handled = allowed.clone();
                for h in handlers {
                    handled.add(h.effect);
                }
                self.check_effects(body, &handled)?;

                // Handler bodies must handle their own effects
                for h in handlers {
                    self.check_effects(&h.body, allowed)?;
                }
                Ok(())
            }

            tast::Expr::Resume { .. } => {
                // Resume re-introduces the effect being handled
                Ok(())
            }

            _ => {
                // Recursively check sub-expressions
                for child in expr.children() {
                    self.check_effects(child, allowed)?;
                }
                Ok(())
            }
        }
    }
}
```

### Error Messages

```rust
impl TypeChecker<'_> {
    fn type_mismatch(&self, expected: TypeId, found: TypeId) -> Diagnostic {
        Diagnostic {
            level: Level::Error,
            message: format!("type mismatch: expected `{}`, found `{}`",
                self.type_to_string(expected),
                self.type_to_string(found)),
            span: self.current_span(),
            labels: vec![
                Label {
                    span: self.type_span(expected),
                    message: "expected due to this".to_string(),
                    style: LabelStyle::Secondary,
                }
            ],
            notes: vec![],
            error_code: ErrorCode::E0308,
        }
    }

    fn infinite_type_error(&self, var: VarId, ty: TypeId) -> Diagnostic {
        Diagnostic {
            level: Level::Error,
            message: format!("infinite type: type variable `{}` occurs in `{}`",
                self.var_to_string(var),
                self.type_to_string(ty)),
            span: self.current_span(),
            labels: vec![],
            notes: vec![
                "this typically means a recursive type was inferred but not declared".to_string()
            ],
            error_code: ErrorCode::E0309,
        }
    }
}
```

---

## 5. Phase 4: IR Generation

### Crate: `jet-ir`

**Dependencies:**
- `jet-types`
- `jet-tast` (typed AST)

### IR Design: Custom SSA-based IR

We use a custom IR rather than LLVM IR directly because:
1. Easier to implement Jet-specific optimizations
2. Better preservation of high-level semantics
3. Easier effect system representation
4. Platform-independent optimizations

### IR Structure

```rust
pub struct Module {
    pub name: String,
    pub functions: Vec<Function>,
    pub globals: Vec<Global>,
    pub types: TypeRegistry,
}

pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub ret_type: Type,
    pub locals: Vec<Local>,
    pub blocks: Vec<BasicBlock>,
    pub effects: EffectSet,
}

pub struct BasicBlock {
    pub id: BlockId,
    pub params: Vec<Param>,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

pub enum Instruction {
    // Constants and literals
    Const { dest: ValueId, value: Constant },

    // Arithmetic
    Add { dest: ValueId, lhs: ValueId, rhs: ValueId },
    Sub { dest: ValueId, lhs: ValueId, rhs: ValueId },
    Mul { dest: ValueId, lhs: ValueId, rhs: ValueId },
    Div { dest: ValueId, lhs: ValueId, rhs: ValueId, checked: bool },
    Rem { dest: ValueId, lhs: ValueId, rhs: ValueId },

    // Comparisons
    Eq { dest: ValueId, lhs: ValueId, rhs: ValueId },
    Ne { dest: ValueId, lhs: ValueId, rhs: ValueId },
    Lt { dest: ValueId, lhs: ValueId, rhs: ValueId },
    Le { dest: ValueId, lhs: ValueId, rhs: ValueId },
    Gt { dest: ValueId, lhs: ValueId, rhs: ValueId },
    Ge { dest: ValueId, lhs: ValueId, rhs: ValueId },

    // Memory
    Alloc { dest: ValueId, ty: Type },
    Load { dest: ValueId, ptr: ValueId },
    Store { ptr: ValueId, value: ValueId },
    GetElementPtr { dest: ValueId, ptr: ValueId, indices: Vec<u32> },

    // Control flow (within block)
    Phi { dest: ValueId, incoming: Vec<(BlockId, ValueId)> },

    // Calls
    Call { dest: Option<ValueId>, func: FuncId, args: Vec<ValueId> },
    CallIndirect { dest: Option<ValueId>, ptr: ValueId, args: Vec<ValueId> },

    // Effects
    Perform { dest: ValueId, effect: EffectId, op: OpId, args: Vec<ValueId> },
    Resume { value: ValueId },

    // Concurrency
    Spawn { dest: ValueId, func: FuncId, args: Vec<ValueId> },
    Channel { dest: ValueId, ty: Type },
    Send { channel: ValueId, value: ValueId },
    Recv { dest: ValueId, channel: ValueId },
}

pub enum Terminator {
    Return { value: Option<ValueId> },
    Branch { target: BlockId },
    CondBranch { cond: ValueId, then_block: BlockId, else_block: BlockId },
    Switch { value: ValueId, cases: Vec<(Constant, BlockId)>, default: BlockId },
    Unreachable,
}

pub struct Param {
    pub id: ValueId,
    pub ty: Type,
}

pub struct Local {
    pub id: ValueId,
    pub ty: Type,
    pub mutable: bool,
}

pub type ValueId = u32;
pub type BlockId = u32;
pub type FuncId = u32;
```

### Effect Representation in IR

Effects are transformed into explicit operations:

```rust
// Before (source):
//   with handler {
//     let x = perform Get();
//     resume x + 1
//   }

// After (IR):
//   %handler = create_handler { Get: block_handler_get }
//   %x = perform Get, %handler
//   br block_resume
//
// block_handler_get:
//   ; handler body
//   resume %value
//
// block_resume:
//   %result = add %x, 1
```

### Lowering from AST

```rust
pub struct LoweringContext<'a> {
    tcx: &'a TypeContext,
    module: Module,
    current_function: Option<FuncId>,
    current_block: Option<BlockId>,
    value_counter: ValueId,
    block_counter: BlockId,
    scope_stack: Vec<Scope>,
}

impl<'a> LoweringContext<'a> {
    pub fn lower_module(&mut self, tast: &tast::Module) -> Module {
        for item in &tast.items {
            match item {
                tast::Item::Function(f) => self.lower_function(f),
                // ...
            }
        }
        self.module.clone()
    }

    fn lower_function(&mut self, func: &tast::Function) -> FuncId {
        let func_id = self.module.add_function(Function {
            name: func.name.to_string(),
            params: Vec::new(),
            ret_type: self.lower_type(func.ret_type),
            locals: Vec::new(),
            blocks: Vec::new(),
            effects: func.effects.clone(),
        });

        self.current_function = Some(func_id);

        // Create entry block
        let entry = self.new_block();
        self.current_block = Some(entry);

        // Lower parameters
        for param in &func.params {
            let param_id = self.new_value();
            // ...
        }

        // Lower body
        let body_val = self.lower_block(&func.body);

        // Add return terminator
        self.terminate(Terminator::Return { value: Some(body_val) });

        func_id
    }

    fn lower_expr(&mut self, expr: &tast::Expr) -> ValueId {
        match expr {
            tast::Expr::Literal(lit, ty) => {
                let dest = self.new_value();
                let const_val = self.lower_literal(lit, *ty);
                self.emit(Instruction::Const { dest, value: const_val });
                dest
            }

            tast::Expr::Binary { op, left, right, ty } => {
                let lhs = self.lower_expr(left);
                let rhs = self.lower_expr(right);
                let dest = self.new_value();

                let inst = match op {
                    BinOp::Add => Instruction::Add { dest, lhs, rhs },
                    BinOp::Sub => Instruction::Sub { dest, lhs, rhs },
                    // ...
                };
                self.emit(inst);
                dest
            }

            tast::Expr::If { cond, then_branch, else_branch, ty } => {
                let cond_val = self.lower_expr(cond);

                let then_block = self.new_block();
                let else_block = self.new_block();
                let merge_block = self.new_block();

                self.terminate(Terminator::CondBranch {
                    cond: cond_val,
                    then_block,
                    else_block,
                });

                // Then branch
                self.current_block = Some(then_block);
                let then_val = self.lower_block(then_branch);
                self.terminate(Terminator::Branch { target: merge_block });

                // Else branch
                self.current_block = Some(else_block);
                let else_val = else_branch
                    .as_ref()
                    .map(|e| self.lower_block(e))
                    .unwrap_or_else(|| self.unit_value());
                self.terminate(Terminator::Branch { target: merge_block });

                // Merge
                self.current_block = Some(merge_block);
                let dest = self.new_value();
                self.emit(Instruction::Phi {
                    dest,
                    incoming: vec![
                        (then_block, then_val),
                        (else_block, else_val),
                    ],
                });
                dest
            }

            tast::Expr::Handle { body, handlers, .. } => {
                self.lower_handle(body, handlers)
            }

            // ...
        }
    }

    fn lower_handle(&mut self, body: &tast::Expr, handlers: &[tast::Handler]) -> ValueId {
        // Generate handler dispatch table
        // Transform perform operations into indirect calls through handler

        let resume_block = self.new_block();
        let mut handler_blocks = Vec::new();

        for handler in handlers {
            let handler_block = self.new_block();
            handler_blocks.push((handler.operation, handler_block));
        }

        // Lower body with handler context
        let old_handlers = self.push_handler_context(&handler_blocks, resume_block);
        let body_val = self.lower_expr(body);
        self.pop_handler_context(old_handlers);

        // Lower handler bodies
        for (handler, block_id) in handlers.iter().zip(handler_blocks.iter()) {
            self.current_block = Some(block_id.1);
            let handler_val = self.lower_expr(&handler.body);
            // Handler body ends with resume
        }

        self.current_block = Some(resume_block);
        body_val
    }
}
```

---

## 6. Phase 5: Code Generation

### Crate: `jet-codegen`

**Dependencies:**
- `jet-ir`
- `inkwell` (LLVM bindings)

### LLVM Integration

```rust
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module as LlvmModule;
use inkwell::passes::PassManager;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue};
use inkwell::OptimizationLevel;

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: LlvmModule<'ctx>,
    builder: Builder<'ctx>,
    function_pass_manager: PassManager<FunctionValue<'ctx>>,

    // Mappings from IR to LLVM
    functions: HashMap<FuncId, FunctionValue<'ctx>>,
    values: HashMap<ValueId, BasicValueEnum<'ctx>>,
    blocks: HashMap<BlockId, inkwell::basic_block::BasicBlock<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        let fpm = PassManager::create(&module);
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.initialize();

        Self {
            context,
            module,
            builder,
            function_pass_manager: fpm,
            functions: HashMap::new(),
            values: HashMap::new(),
            blocks: HashMap::new(),
        }
    }

    pub fn compile_module(&mut self, ir: &ir::Module) -> Result<(), String> {
        // First pass: declare all functions and types
        for func in &ir.functions {
            self.declare_function(func)?;
        }

        // Second pass: compile function bodies
        for func in &ir.functions {
            self.compile_function(func)?;
        }

        // Verify module
        self.module.verify().map_err(|e| e.to_string())
    }
}
```

### Type Mapping

```rust
impl<'ctx> CodeGen<'ctx> {
    fn llvm_type(&self, ty: &ir::Type) -> BasicTypeEnum<'ctx> {
        match ty {
            ir::Type::Int(8) => self.context.i8_type().into(),
            ir::Type::Int(16) => self.context.i16_type().into(),
            ir::Type::Int(32) => self.context.i32_type().into(),
            ir::Type::Int(64) => self.context.i64_type().into(),
            ir::Type::Int(_) => self.context.i64_type().into(),

            ir::Type::Uint(8) => self.context.i8_type().into(),
            ir::Type::Uint(16) => self.context.i16_type().into(),
            ir::Type::Uint(32) => self.context.i32_type().into(),
            ir::Type::Uint(64) => self.context.i64_type().into(),
            ir::Type::Uint(_) => self.context.i64_type().into(),

            ir::Type::Float(32) => self.context.f32_type().into(),
            ir::Type::Float(64) => self.context.f64_type().into(),

            ir::Type::Bool => self.context.bool_type().into(),
            ir::Type::Unit => self.context.struct_type(&[], false).into(),

            ir::Type::Ref(inner, _) => {
                let inner_ty = self.llvm_type(inner);
                inner_ty.ptr_type(inkwell::AddressSpace::default()).into()
            }

            ir::Type::Tuple(elements) => {
                let llvm_elements: Vec<_> = elements
                    .iter()
                    .map(|e| self.llvm_type(e).into())
                    .collect();
                self.context.struct_type(&llvm_elements, false).into()
            }

            ir::Type::Function { params, ret, .. } => {
                let param_types: Vec<_> = params
                    .iter()
                    .map(|p| self.llvm_type(p).into())
                    .collect();
                let ret_type = self.llvm_type(ret);
                let fn_type = ret_type.fn_type(&param_types, false);
                fn_type.ptr_type(inkwell::AddressSpace::default()).into()
            }

            // ...
        }
    }
}
```

### Effect Handling in Codegen

Effects are compiled using one of these strategies:

1. **Direct calls** for pure functions (no effects)
2. **CPS transformation** for effectful functions
3. **Handler dispatch tables** for dynamically handled effects

```rust
impl<'ctx> CodeGen<'ctx> {
    fn compile_effect_perform(
        &mut self,
        dest: ir::ValueId,
        effect: ir::EffectId,
        op: ir::OpId,
        args: &[ir::ValueId],
    ) {
        // In CPS-transformed code, performing an effect
        // involves calling the current continuation with
        // the effect tag and arguments

        let effect_tag = self.context.i32_type().const_int(effect as u64, false);
        let op_tag = self.context.i32_type().const_int(op as u64, false);

        // Pack arguments into effect payload
        let llvm_args: Vec<_> = args
            .iter()
            .map(|&a| self.values[&a])
            .collect();

        // Call effect handler (passed as implicit parameter)
        let handler = self.get_current_handler();

        // Store result
        let result = self.builder.build_call(
            handler,
            &[effect_tag.into(), op_tag.into(), /* args */],
            "effect_result",
        ).unwrap();

        self.values.insert(dest, result.try_as_basic_value().left().unwrap());
    }
}
```

### Concurrency Primitives

```rust
impl<'ctx> CodeGen<'ctx> {
    fn compile_spawn(&mut self, func: ir::FuncId, args: &[ir::ValueId]) -> BasicValueEnum<'ctx> {
        // Map to pthread_create or async runtime
        let spawn_fn = self.module.get_function("jet_rt_spawn").unwrap_or_else(|| {
            let fn_type = self.context.i64_type().fn_type(
                &[self.context.i8_type().ptr_type(inkwell::AddressSpace::default()).into()],
                false,
            );
            self.module.add_function("jet_rt_spawn", fn_type, None)
        });

        let closure = self.build_closure(func, args);
        self.builder.build_call(spawn_fn, &[closure.into()], "thread_id")
            .unwrap()
            .try_as_basic_value()
            .left()
            .unwrap()
    }

    fn compile_channel(&mut self, ty: &ir::Type) -> BasicValueEnum<'ctx> {
        let channel_fn = self.module.get_function("jet_rt_channel").unwrap_or_else(|| {
            let fn_type = self.context.i8_type().ptr_type(inkwell::AddressSpace::default())
                .fn_type(&[self.context.i64_type().into()], false);
            self.module.add_function("jet_rt_channel", fn_type, None)
        });

        let type_id = self.get_type_id(ty);
        self.builder.build_call(channel_fn, &[type_id.into()], "channel")
            .unwrap()
            .try_as_basic_value()
            .left()
            .unwrap()
    }
}
```

---

## 7. Supporting Infrastructure

### 7.1 Symbol Tables

```rust
// jet-resolve crate

pub struct Resolver {
    scopes: Vec<Scope>,
    modules: HashMap<ModuleId, ModuleScope>,
    current_module: ModuleId,
}

pub struct Scope {
    bindings: HashMap<String, Binding>,
    kind: ScopeKind,
}

pub enum ScopeKind {
    Module,
    Function,
    Block,
    Loop,
}

pub struct Binding {
    pub name: Ident,
    pub kind: BindingKind,
    pub def_id: DefId,
    pub is_mutable: bool,
}

pub enum BindingKind {
    Variable,
    Function,
    Type,
    Trait,
    Module,
    Effect,
}

impl Resolver {
    pub fn resolve_module(&mut self, ast: &mut ast::Module) -> Result<(), Vec<Diagnostic>> {
        // First pass: collect all top-level names
        self.collect_items(ast)?;

        // Second pass: resolve references
        for item in &mut ast.items {
            self.resolve_item(item)?;
        }

        Ok(())
    }

    fn resolve_expr(&mut self, expr: &mut ast::Expr) -> Result<(), Diagnostic> {
        match expr {
            ast::Expr::Identifier(ident) => {
                if let Some(binding) = self.lookup(&ident.name) {
                    *ident = Ident::with_def_id(ident.clone(), binding.def_id);
                    Ok(())
                } else {
                    Err(self.unresolved_name(ident))
                }
            }
            // ...
        }
    }
}
```

### 7.2 Source Location Tracking

```rust
// jet-diagnostics crate

pub struct SourceMap {
    files: Vec<SourceFile>,
}

pub struct SourceFile {
    pub id: FileId,
    pub name: String,
    pub source: String,
    pub line_starts: Vec<ByteOffset>,
}

pub struct SourceSpan {
    pub file: FileId,
    pub start: ByteOffset,
    pub end: ByteOffset,
}

impl SourceMap {
    pub fn line_column(&self, span: SourceSpan) -> (usize, usize) {
        let file = &self.files[span.file.0 as usize];
        let line = file.line_starts.binary_search(&span.start)
            .unwrap_or_else(|i| i);
        let line_start = if line == 0 { 0 } else { file.line_starts[line - 1] };
        let column = span.start.0 - line_start.0;
        (line + 1, column + 1)
    }

    pub fn snippet(&self, span: SourceSpan) -> &str {
        let file = &self.files[span.file.0 as usize];
        &file.source[span.start.0..span.end.0]
    }
}
```

### 7.3 Dead Code Elimination

```rust
// jet-opt crate

pub fn eliminate_dead_code(module: &mut ir::Module) {
    let mut used = HashSet::new();
    let mut worklist = Vec::new();

    // Mark all exported functions as used
    for (id, func) in module.functions.iter().enumerate() {
        if func.is_exported {
            worklist.push(FuncId(id as u32));
        }
    }

    // Traverse from roots
    while let Some(func_id) = worklist.pop() {
        if used.insert(func_id) {
            let func = &module.functions[func_id.0 as usize];
            for block in &func.blocks {
                for inst in &block.instructions {
                    if let Instruction::Call { func: callee, .. } = inst {
                        if callee.0 != func_id.0 {
                            worklist.push(*callee);
                        }
                    }
                }
            }
        }
    }

    // Remove unused functions
    module.functions.retain(|f| used.contains(&f.id));
}
```

---

## 8. Build System Integration

### Crate: `jet-build`

### jet build Command Behavior

```rust
pub struct BuildCommand {
    pub target: BuildTarget,
    pub profile: Profile,
    pub features: Vec<String>,
    pub jobs: Option<usize>,
}

pub enum BuildTarget {
    Binary(String),      // Build specific binary
    Library,             // Build library
    All,                 // Build all targets
}

pub enum Profile {
    Debug,
    Release,
}

impl BuildCommand {
    pub fn execute(&self) -> Result<(), BuildError> {
        // 1. Load jet.toml configuration
        let config = Config::load("jet.toml")?;

        // 2. Determine compilation units
        let units = self.collect_units(&config)?;

        // 3. Check cache for each unit
        let mut to_compile = Vec::new();
        for unit in units {
            if !self.is_cached(&unit)? {
                to_compile.push(unit);
            }
        }

        // 4. Compile in parallel
        let pool = ThreadPool::new(self.jobs.unwrap_or(num_cpus::get()));
        let results: Vec<_> = to_compile
            .into_par_iter()
            .map(|unit| self.compile_unit(unit))
            .collect();

        // 5. Link
        self.link(&results)?;

        Ok(())
    }
}
```

### Incremental Compilation Strategy

```rust
pub struct IncrementalCache {
    /// Directory for cache files
    cache_dir: PathBuf,
    /// Dependency graph
    deps: DependencyGraph,
}

pub struct DependencyGraph {
    /// File -> hash of content
    file_hashes: HashMap<PathBuf, u64>,
    /// Module -> dependencies
    module_deps: HashMap<ModuleId, Vec<ModuleId>>,
}

impl IncrementalCache {
    /// Check if a module needs recompilation
    pub fn needs_rebuild(&self, module: &Module) -> bool {
        let current_hash = hash_file(&module.source_file);

        match self.file_hashes.get(&module.source_file.path) {
            Some(old_hash) if *old_hash == current_hash => {
                // File unchanged, check dependencies
                for dep in &self.module_deps[&module.id] {
                    if self.needs_rebuild_by_id(*dep) {
                        return true;
                    }
                }
                false
            }
            _ => true, // File changed or not in cache
        }
    }

    /// Save compilation artifacts
    pub fn save_artifact(&mut self, module: &Module, artifact: &Artifact) {
        let path = self.cache_dir.join(format!("{}.o", module.id.0));
        std::fs::write(&path, &artifact.object_code).unwrap();

        self.file_hashes.insert(
            module.source_file.path.clone(),
            hash_file(&module.source_file),
        );
    }
}
```

### Caching

```rust
pub struct CacheKey {
    /// Hash of source file content
    source_hash: u64,
    /// Hash of compiler version
    compiler_version: u64,
    /// Hash of compilation flags
    flags_hash: u64,
    /// Hash of dependency artifacts
    deps_hash: u64,
}

impl CacheKey {
    pub fn compute(module: &Module, flags: &CompileFlags) -> Self {
        Self {
            source_hash: hash(&module.source),
            compiler_version: hash(env!("JET_COMPILER_VERSION")),
            flags_hash: hash(flags),
            deps_hash: hash_deps(&module.dependencies),
        }
    }
}
```

---

## 9. Testing Strategy

### Unit Tests per Phase

```rust
// jet-lexer/tests/lexer_tests.rs

#[test]
fn test_indentation() {
    let source = r#"
def foo():
    if true:
        return 1
    return 2
"#;
    let lexer = Lexer::new(source);
    let tokens: Vec<_> = lexer.collect();

    assert!(tokens.contains(&Token::Indent));
    assert!(tokens.contains(&Token::Dedent));
}

// jet-parser/tests/parser_tests.rs

#[test]
fn test_function_parsing() {
    let source = r#"
def add(x: int, y: int) -> int:
    return x + y
"#;
    let ast = parse(source).unwrap();
    assert_eq!(ast.items.len(), 1);
}

// jet-typeck/tests/typeck_tests.rs

#[test]
fn test_type_inference() {
    let source = r#"
def identity(x):
    return x

let a = identity(5)      # a: int
let b = identity("hi")   # b: string
"#;
    let tast = typecheck(source).unwrap();
    // Verify types are correctly inferred
}
```

### Golden File Tests for Diagnostics

```rust
// tests/diagnostics/
//   type_mismatch.jet
//   type_mismatch.stderr

// type_mismatch.jet:
// def foo() -> int:
//     return "hello"

// type_mismatch.stderr:
// error[E0308]: type mismatch
//  --> tests/diagnostics/type_mismatch.jet:2:12
//   |
// 2 |     return "hello"
//   |            ^^^^^^^ expected `int`, found `string`

#[test]
fn test_diagnostics() {
    let test_dir = Path::new("tests/diagnostics");

    for entry in fs::read_dir(test_dir).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();

        if path.extension().unwrap() == "jet" {
            let source = fs::read_to_string(&path).unwrap();
            let expected_path = path.with_extension("stderr");
            let expected = fs::read_to_string(&expected_path).unwrap();

            let result = compile(&source);
            let actual = format_diagnostics(result.unwrap_err());

            assert_eq!(actual.trim(), expected.trim(),
                "diagnostic mismatch for {}", path.display());
        }
    }
}
```

### Integration Tests

```rust
// tests/integration/fibonacci.jet
// tests/integration/fibonacci.stdout

#[test]
fn test_fibonacci() {
    let source = r#"
def fib(n: int) -> int:
    if n <= 1:
        return n
    return fib(n - 1) + fib(n - 2)

print(fib(10))
"#;

    let output = run(source).unwrap();
    assert_eq!(output.trim(), "55");
}
```

### Property-Based Testing

```rust
// jet-typeck/tests/property_tests.rs

use proptest::prelude::*;

proptest! {
    #[test]
    fn type_inference_is_deterministic(source in "[a-zA-Z0-9_\s\n]{0,100}") {
        // Parse and typecheck should be deterministic
        let ast1 = parse(&source);
        let ast2 = parse(&source);

        match (ast1, ast2) {
            (Ok(a1), Ok(a2)) => {
                let tast1 = typecheck(&a1);
                let tast2 = typecheck(&a2);
                assert_eq!(tast1.is_ok(), tast2.is_ok());
            }
            (Err(_), Err(_)) => {}
            _ => panic!("non-deterministic parsing"),
        }
    }
}
```

---

## Crate Structure

```
jet/
├── Cargo.toml
├── crates/
│   ├── jet-diagnostics/     # Error types, source maps, pretty printing
│   ├── jet-lexer/            # Tokenizer with indentation handling
│   ├── jet-ast/              # AST definitions
│   ├── jet-parser/           # Recursive descent parser
│   ├── jet-resolve/          # Name resolution
│   ├── jet-types/            # Type system definitions
│   ├── jet-typeck/           # Hindley-Milner type inference
│   ├── jet-effect/           # Effect system checking
│   ├── jet-ir/               # Intermediate representation
│   ├── jet-opt/              # Optimizations
│   ├── jet-codegen/          # LLVM code generation
│   ├── jet-build/            # Build system, incremental compilation
│   └── jet-driver/           # Main compiler driver
└── tests/
    ├── lexer/
    ├── parser/
    ├── typeck/
    ├── codegen/
    └── integration/
```

---

## Key Dependencies

| Crate | Purpose | Version |
|-------|---------|---------|
| `inkwell` | LLVM bindings | 0.4 |
| `logos` | Regex-based lexer (optional) | 0.14 |
| `indexmap` | Ordered maps/sets | 2.0 |
| `rustc-hash` | Fast hash map | 1.1 |
| `smol_str` | Small string optimization | 0.2 |
| `bumpalo` | Arena allocator | 3.14 |
| `crossbeam` | Concurrent data structures | 0.8 |
| `rayon` | Data parallelism | 1.8 |
| `proptest` | Property-based testing | 1.4 |
| `insta` | Snapshot testing | 1.34 |

---

## Future Considerations

1. **MIR (Mid-level IR)**: Add a second IR between Jet IR and LLVM for more aggressive optimizations
2. **LTO**: Link-time optimization support
3. **WASM Backend**: Alternative backend for WebAssembly
4. **JIT Compilation**: REPL and scripting support via LLVM ORC JIT
5. **Incremental Type Checking**: Only re-typecheck changed modules
6. **IDE Integration**: LSP server using the same compiler infrastructure
