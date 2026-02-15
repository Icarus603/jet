//! Abstract Syntax Tree for the Jet Language

use jet_lexer::Span;

/// Identifier
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

impl Ident {
    pub fn new(name: impl Into<String>, span: Span) -> Self {
        Self {
            name: name.into(),
            span,
        }
    }
}

/// Literal values
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    Unit,
}

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Not,    // not
    Neg,    // -
    BitNot, // ~
    Deref,  // *
    Ref,    // &
    RefMut, // &mut
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %
    Pow, // **

    // Bitwise
    BitAnd, // &
    BitOr,  // |
    BitXor, // ^
    Shl,    // <<
    Shr,    // >>

    // Comparison
    Eq, // ==
    Ne, // !=
    Lt, // <
    Gt, // >
    Le, // <=
    Ge, // >=

    // Logical
    And, // and
    Or,  // or

    // Range
    Range,          // ..
    RangeInclusive, // ...
}

/// Assignment operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignOp {
    Assign,       // =
    AddAssign,    // +=
    SubAssign,    // -=
    MulAssign,    // *=
    DivAssign,    // /=
    ModAssign,    // %=
    BitAndAssign, // &=
    BitOrAssign,  // |=
    BitXorAssign, // ^=
    ShlAssign,    // <<=
    ShrAssign,    // >>=
}

/// Type expressions
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Named type (e.g., `int`, `Vec`, `Option`)
    Path(Path),
    /// Generic type (e.g., `Vec<T>`, `Map<K, V>`)
    Generic(Box<Type>, Vec<Type>),
    /// Tuple type (e.g., `(int, string)`)
    Tuple(Vec<Type>),
    /// Array type (e.g., `[int; 5]` or `[int]`)
    Array(Box<Type>, Option<Box<Expr>>),
    /// Function type (e.g., `fn(int) -> int ! Effect`)
    Function {
        params: Vec<Type>,
        return_type: Option<Box<Type>>,
        effects: Vec<Type>,
    },
    /// Reference type (e.g., `&T` or `&mut T`)
    Reference { mutable: bool, inner: Box<Type> },
    /// Channel type (e.g., `chan[T]`)
    Channel(Box<Type>),
    /// Async type (e.g., `async T`)
    Async(Box<Type>),
    /// Inferred type placeholder
    Infer,
    /// Self type
    SelfType,
}

/// A path (e.g., `std::collections::Vec`)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path {
    pub segments: Vec<Ident>,
    pub span: Span,
}

impl Path {
    pub fn new(segments: Vec<Ident>, span: Span) -> Self {
        Self { segments, span }
    }

    pub fn single(ident: Ident) -> Self {
        let span = ident.span;
        Self {
            segments: vec![ident],
            span,
        }
    }
}

/// Pattern for matching
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// Wildcard pattern `_`
    Wildcard(Span),
    /// Identifier pattern (with optional mut)
    Ident { mutable: bool, name: Ident },
    /// Literal pattern
    Literal(Literal),
    /// Tuple pattern
    Tuple(Vec<Pattern>),
    /// Struct pattern
    Struct {
        path: Path,
        fields: Vec<FieldPattern>,
        rest: bool,
    },
    /// Enum variant pattern
    Enum {
        path: Path,
        variant: Ident,
        inner: Option<Box<Pattern>>,
    },
    /// Array pattern
    Array(Vec<Pattern>),
    /// Rest pattern `..` or `..rest`
    Rest(Option<Ident>),
    /// Or pattern `p1 | p2`
    Or(Box<Pattern>, Box<Pattern>),
    /// Bind pattern `name @ pattern`
    Bind { name: Ident, pattern: Box<Pattern> },
    /// Mut pattern wrapper
    Mut(Box<Pattern>),
    /// Ref pattern wrapper
    Ref {
        mutable: bool,
        pattern: Box<Pattern>,
    },
}

impl Pattern {
    /// Get the span of this pattern
    pub fn span(&self) -> Span {
        match self {
            Pattern::Wildcard(span) => *span,
            Pattern::Ident { name, .. } => name.span,
            Pattern::Literal(lit) => match lit {
                Literal::Integer(_) => Span::default(),
                Literal::Float(_) => Span::default(),
                Literal::String(_) => Span::default(),
                Literal::Char(_) => Span::default(),
                Literal::Bool(_) => Span::default(),
                Literal::Unit => Span::default(),
            },
            Pattern::Tuple(patterns) => {
                if let (Some(first), Some(last)) = (patterns.first(), patterns.last()) {
                    Span::new(first.span().start, last.span().end)
                } else {
                    Span::default()
                }
            }
            Pattern::Struct { path, .. } => path.span,
            Pattern::Enum { path, variant, .. } => Span::new(path.span.start, variant.span.end),
            Pattern::Array(patterns) => {
                if let (Some(first), Some(last)) = (patterns.first(), patterns.last()) {
                    Span::new(first.span().start, last.span().end)
                } else {
                    Span::default()
                }
            }
            Pattern::Rest(ident) => ident.as_ref().map(|i| i.span).unwrap_or_default(),
            Pattern::Or(left, right) => Span::new(left.span().start, right.span().end),
            Pattern::Bind { name, pattern } => Span::new(name.span.start, pattern.span().end),
            Pattern::Mut(pattern) => pattern.span(),
            Pattern::Ref { pattern, .. } => pattern.span(),
        }
    }
}

/// Field pattern in struct patterns
#[derive(Debug, Clone, PartialEq)]
pub struct FieldPattern {
    pub name: Ident,
    pub pattern: Option<Pattern>,
}

/// Match arm
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Box<Expr>>,
    pub body: Box<Expr>,
}

/// Expression
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Literal value
    Literal(Literal),
    /// Variable reference
    Variable(Ident),
    /// Path expression (for qualified names)
    Path(Path),
    /// Function call
    Call { func: Box<Expr>, args: Vec<Expr> },
    /// Method call
    MethodCall {
        receiver: Box<Expr>,
        method: Ident,
        args: Vec<Expr>,
    },
    /// Field access
    FieldAccess { object: Box<Expr>, field: Ident },
    /// Index access
    Index { object: Box<Expr>, index: Box<Expr> },
    /// Unary operation
    Unary { op: UnaryOp, expr: Box<Expr> },
    /// Binary operation
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    /// Block expression
    Block(Block),
    /// If expression
    If {
        cond: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
    },
    /// Match expression
    Match {
        expr: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    /// While loop
    While {
        label: Option<Ident>,
        cond: Box<Expr>,
        body: Box<Expr>,
    },
    /// For loop
    For {
        label: Option<Ident>,
        pattern: Pattern,
        iterable: Box<Expr>,
        body: Box<Expr>,
    },
    /// Loop (infinite)
    Loop {
        label: Option<Ident>,
        body: Box<Expr>,
    },
    /// Lambda expression
    Lambda {
        params: Vec<Param>,
        return_type: Option<Type>,
        effects: Vec<Type>,
        body: Box<Expr>,
    },
    /// Await expression
    Await(Box<Expr>),
    /// Try operator `?`
    Try(Box<Expr>),
    /// Assignment (as expression)
    Assign {
        target: Box<Expr>,
        op: AssignOp,
        value: Box<Expr>,
    },
    /// Break expression
    Break {
        label: Option<Ident>,
        value: Option<Box<Expr>>,
    },
    /// Continue expression
    Continue { label: Option<Ident> },
    /// Return expression
    Return(Option<Box<Expr>>),
    /// Tuple expression
    Tuple(Vec<Expr>),
    /// Array expression
    Array(Vec<Expr>),
    /// Struct literal
    StructLiteral { path: Path, fields: Vec<FieldInit> },
    /// Spawn expression
    Spawn(Box<Expr>),
    /// Async block
    Async(Block),
    /// Concurrent block
    Concurrent(Block),
    /// Self expression
    SelfExpr(Span),
    /// Pass expression (no-op, like Python's pass)
    Pass,
    /// Raise expression (effect operation)
    Raise(RaiseExpr),
    /// Handle expression (effect handler)
    Handle(HandleExpr),
    /// Resume expression (continue from effect handler)
    Resume(ResumeExpr),
}

/// Block of statements
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub expr: Option<Box<Expr>>,
    pub span: Span,
}

/// Field initialization in struct literals
#[derive(Debug, Clone, PartialEq)]
pub struct FieldInit {
    pub name: Ident,
    pub value: Option<Expr>, // None means shorthand: `x` means `x: x`
}

/// Statement
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    /// Let binding
    Let {
        pattern: Pattern,
        ty: Option<Type>,
        value: Box<Expr>,
    },
    /// Expression statement
    Expr(Box<Expr>),
    /// Assignment statement
    Assign {
        target: Box<Expr>,
        op: AssignOp,
        value: Box<Expr>,
    },
    /// Return statement
    Return(Option<Box<Expr>>),
    /// Break statement
    Break {
        label: Option<Ident>,
        value: Option<Box<Expr>>,
    },
    /// Continue statement
    Continue { label: Option<Ident> },
    /// Handle statement (effect handling)
    Handle {
        body: Box<Expr>,
        handlers: Vec<HandlerArm>,
    },
}

/// Function parameter
#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub pattern: Pattern,
    pub ty: Type,
}

/// Generic parameter
#[derive(Debug, Clone, PartialEq)]
pub struct GenericParam {
    pub name: Ident,
    pub bounds: Vec<Type>,
}

/// Where clause bound
#[derive(Debug, Clone, PartialEq)]
pub struct WhereBound {
    pub ty: Type,
    pub bounds: Vec<Type>,
}

/// Function definition
#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub public: bool,
    pub name: Ident,
    pub generics: Vec<GenericParam>,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub effects: Vec<Type>,
    pub where_clause: Vec<WhereBound>,
    pub body: Expr,
    pub span: Span,
}

/// Struct field definition
#[derive(Debug, Clone, PartialEq)]
pub struct FieldDef {
    pub public: bool,
    pub name: Ident,
    pub ty: Type,
}

/// Struct definition
#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub public: bool,
    pub name: Ident,
    pub generics: Vec<GenericParam>,
    pub fields: Vec<FieldDef>,
    pub span: Span,
}

/// Enum variant definition
#[derive(Debug, Clone, PartialEq)]
pub enum VariantBody {
    /// Unit variant (e.g., `None`)
    Unit,
    /// Tuple variant (e.g., `Some(T)`)
    Tuple(Vec<Type>),
    /// Struct variant (e.g., `Error { code: int }`)
    Struct(Vec<FieldDef>),
    /// Variant with discriminant
    Discriminant(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: Ident,
    pub body: VariantBody,
}

/// Enum definition
#[derive(Debug, Clone, PartialEq)]
pub struct EnumDef {
    pub public: bool,
    pub name: Ident,
    pub generics: Vec<GenericParam>,
    pub variants: Vec<EnumVariant>,
    pub span: Span,
}

/// Trait item (method signature or type alias)
#[derive(Debug, Clone, PartialEq)]
pub enum TraitItem {
    /// Method signature
    Method {
        name: Ident,
        generics: Vec<GenericParam>,
        params: Vec<Param>,
        return_type: Option<Type>,
        effects: Vec<Type>,
        where_clause: Vec<WhereBound>,
    },
    /// Type alias declaration (without =)
    TypeDecl {
        name: Ident,
        generics: Vec<GenericParam>,
        bounds: Vec<Type>,
    },
    /// Constant declaration (without =)
    ConstDecl { name: Ident, ty: Type },
}

/// Trait definition
#[derive(Debug, Clone, PartialEq)]
pub struct TraitDef {
    pub public: bool,
    pub name: Ident,
    pub generics: Vec<GenericParam>,
    pub super_traits: Vec<Type>,
    pub items: Vec<TraitItem>,
    pub span: Span,
}

/// Impl item (method, constant, or type alias)
#[derive(Debug, Clone, PartialEq)]
#[allow(clippy::large_enum_variant)]
pub enum ImplItem {
    Method(Function),
    Const {
        name: Ident,
        ty: Type,
        value: Box<Expr>,
    },
    TypeAlias(TypeAlias),
}

/// Impl definition
#[derive(Debug, Clone, PartialEq)]
pub struct ImplDef {
    pub generics: Vec<GenericParam>,
    pub trait_path: Option<Path>,
    pub ty: Type,
    pub where_clause: Vec<WhereBound>,
    pub items: Vec<ImplItem>,
    pub span: Span,
}

/// Type alias
#[derive(Debug, Clone, PartialEq)]
pub struct TypeAlias {
    pub public: bool,
    pub name: Ident,
    pub generics: Vec<GenericParam>,
    pub ty: Type,
    pub span: Span,
}

/// Constant definition
#[derive(Debug, Clone, PartialEq)]
pub struct ConstDef {
    pub public: bool,
    pub name: Ident,
    pub ty: Type,
    pub value: Box<Expr>,
    pub span: Span,
}

/// Import item
#[derive(Debug, Clone, PartialEq)]
pub enum ImportItem {
    /// Single import `name` or `name as alias`
    Single { name: Ident, alias: Option<Ident> },
    /// Group import `{ a, b, c }`
    Group(Vec<ImportItem>),
}

/// Import statement
#[derive(Debug, Clone, PartialEq)]
pub enum Import {
    /// `import path` or `import path as name`
    Simple { path: Path, alias: Option<Ident> },
    /// `from path import items`
    From { path: Path, items: Vec<ImportItem> },
}

/// Module-level item
#[derive(Debug, Clone, PartialEq)]
pub enum ModuleItem {
    Import(Import),
    Function(Function),
    Struct(StructDef),
    Enum(EnumDef),
    Trait(TraitDef),
    Impl(ImplDef),
    TypeAlias(TypeAlias),
    Const(ConstDef),
    Effect(EffectDef),
}

/// Handler arm for effect handlers
#[derive(Debug, Clone, PartialEq)]
pub struct HandlerArm {
    pub operation: Ident,           // Operation name being handled
    pub params: Vec<Pattern>,       // Operation parameters
    pub resume_name: Option<Ident>, // Name for resume function (optional)
    pub body: Expr,
    pub span: Span,
}

/// Effect definition
#[derive(Debug, Clone, PartialEq)]
pub struct EffectDef {
    pub public: bool,
    pub name: Ident,
    pub generics: Vec<GenericParam>,
    pub operations: Vec<OperationDef>,
    pub span: Span,
}

/// Operation definition within an effect
#[derive(Debug, Clone, PartialEq)]
pub struct OperationDef {
    pub name: Ident,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub span: Span,
}

/// Raise expression for effect operations
#[derive(Debug, Clone, PartialEq)]
pub struct RaiseExpr {
    pub effect: Option<Path>, // Optional effect type
    pub operation: Ident,
    pub args: Vec<Expr>,
    pub span: Span,
}

/// Handle expression for effect handling
#[derive(Debug, Clone, PartialEq)]
pub struct HandleExpr {
    pub body: Box<Expr>,
    pub handlers: Vec<HandlerArm>,
    pub span: Span,
}

/// Resume expression for continuing from effect handler
#[derive(Debug, Clone, PartialEq)]
pub struct ResumeExpr {
    pub value: Option<Box<Expr>>,
    pub span: Span,
}

/// A complete module
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub items: Vec<ModuleItem>,
    pub span: Span,
}

impl Module {
    pub fn new(span: Span) -> Self {
        Self {
            items: Vec::new(),
            span,
        }
    }
}
