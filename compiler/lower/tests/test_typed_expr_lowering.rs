//! Tests for typed expression lowering.
//!
//! These tests verify that the `lower_typed_expr()` function correctly
//! converts typed AST expressions (TypedExpr) to Jet IR instructions.

use jet_diagnostics::Span as DiagnosticsSpan;
use jet_ir::{Instruction, Ty, ValueId};
use jet_lexer::Span as LexerSpan;
use jet_lower::{lower_module, lower_typed_expression, LoweringContext};
use jet_parser::ast::{BinaryOp, Ident, Literal, Pattern, UnaryOp};
use jet_typeck::{
    EffectSet, TypeContext, TypeId, TypedExpr, TypedExprKind, TypedFunction, TypedModule,
    TypedModuleItem, TypedParam,
};

fn make_ident(name: &str) -> Ident {
    Ident::new(name, LexerSpan::new(0, 0))
}

fn make_typed_expr(kind: TypedExprKind, ty: TypeId) -> TypedExpr {
    TypedExpr {
        kind,
        ty,
        span: DiagnosticsSpan::new(0, 0),
    }
}

/// Setup a test context with a function and entry block ready for lowering.
fn setup_test_context<'tcx>(tcx: &'tcx TypeContext) -> LoweringContext<'tcx> {
    let mut ctx = LoweringContext::new("test", tcx);
    // Create a dummy function to hold the instructions
    let func = jet_ir::Function::new("test_func", vec![], Ty::I64);
    ctx.module.add_function(func);
    ctx.start_function(0);
    let entry = ctx.create_block("entry");
    ctx.set_current_block(entry);
    ctx
}

#[test]
fn test_lower_typed_literal_int() {
    let tcx = TypeContext::new();
    let mut ctx = setup_test_context(&tcx);

    // Create a typed integer literal (TypeId(3) is I64 based on pre-interned types)
    let int_ty = TypeId(3); // I64
    let lit = make_typed_expr(TypedExprKind::Literal(Literal::Integer(42)), int_ty);

    let result = lower_typed_expression(&mut ctx, &lit);

    // Verify we got a valid ValueId
    assert_eq!(result, ValueId::new(0));

    // Verify the instruction was emitted
    let func = &ctx.module.functions[0]; // First function is the one we're building
    let block = &func.blocks[0];
    assert_eq!(block.instructions.len(), 1);

    match &block.instructions[0] {
        Instruction::Const { result, value } => {
            assert_eq!(*result, ValueId::new(0));
            match value {
                jet_ir::ConstantValue::Int(n, _) => assert_eq!(*n, 42),
                _ => panic!("Expected integer constant"),
            }
        }
        _ => panic!("Expected Const instruction"),
    }
}

#[test]
fn test_lower_typed_literal_bool() {
    let tcx = TypeContext::new();
    let mut ctx = setup_test_context(&tcx);

    // Create a typed boolean literal (TypeId(2) is Bool)
    let bool_ty = TypeId::BOOL;
    let lit = make_typed_expr(TypedExprKind::Literal(Literal::Bool(true)), bool_ty);

    let result = lower_typed_expression(&mut ctx, &lit);

    assert_eq!(result, ValueId::new(0));

    let func = &ctx.module.functions[0];
    let block = &func.blocks[0];

    match &block.instructions[0] {
        Instruction::Const { value, .. } => match value {
            jet_ir::ConstantValue::Bool(b) => assert!(*b),
            _ => panic!("Expected boolean constant"),
        },
        _ => panic!("Expected Const instruction"),
    }
}

#[test]
fn test_lower_typed_binary_op() {
    let tcx = TypeContext::new();
    let mut ctx = setup_test_context(&tcx);

    let int_ty = TypeId(3); // I64

    // Create: 1 + 2
    let left = Box::new(make_typed_expr(
        TypedExprKind::Literal(Literal::Integer(1)),
        int_ty,
    ));
    let right = Box::new(make_typed_expr(
        TypedExprKind::Literal(Literal::Integer(2)),
        int_ty,
    ));

    let binary = make_typed_expr(
        TypedExprKind::Binary {
            op: BinaryOp::Add,
            left,
            right,
        },
        int_ty,
    );

    let result = lower_typed_expression(&mut ctx, &binary);

    // Should have 3 values: two constants and one binary result
    assert_eq!(result, ValueId::new(2));

    let func = &ctx.module.functions[0];
    let block = &func.blocks[0];

    // Should have 3 instructions: const 1, const 2, binary add
    assert_eq!(block.instructions.len(), 3);

    match &block.instructions[2] {
        Instruction::Binary { op, lhs, rhs, .. } => {
            assert!(matches!(op, jet_ir::BinaryOp::Add));
            assert_eq!(*lhs, ValueId::new(0));
            assert_eq!(*rhs, ValueId::new(1));
        }
        _ => panic!("Expected Binary instruction"),
    }
}

#[test]
fn test_lower_typed_unary_op() {
    let tcx = TypeContext::new();
    let mut ctx = setup_test_context(&tcx);

    let int_ty = TypeId(3); // I64

    // Create: -5
    let operand = Box::new(make_typed_expr(
        TypedExprKind::Literal(Literal::Integer(5)),
        int_ty,
    ));

    let unary = make_typed_expr(
        TypedExprKind::Unary {
            op: UnaryOp::Neg,
            expr: operand,
        },
        int_ty,
    );

    let result = lower_typed_expression(&mut ctx, &unary);

    assert_eq!(result, ValueId::new(1));

    let func = &ctx.module.functions[0];
    let block = &func.blocks[0];

    match &block.instructions[1] {
        Instruction::Unary { op, operand, .. } => {
            assert!(matches!(op, jet_ir::UnaryOp::Neg));
            assert_eq!(*operand, ValueId::new(0));
        }
        _ => panic!("Expected Unary instruction"),
    }
}

#[test]
fn test_lower_typed_if() {
    let tcx = TypeContext::new();
    let mut ctx = setup_test_context(&tcx);

    let bool_ty = TypeId::BOOL;
    let int_ty = TypeId(3); // I64

    // Create: if true then 1 else 2
    let if_expr = make_typed_expr(
        TypedExprKind::If {
            cond: Box::new(make_typed_expr(
                TypedExprKind::Literal(Literal::Bool(true)),
                bool_ty,
            )),
            then_branch: Box::new(make_typed_expr(
                TypedExprKind::Literal(Literal::Integer(1)),
                int_ty,
            )),
            else_branch: Some(Box::new(make_typed_expr(
                TypedExprKind::Literal(Literal::Integer(2)),
                int_ty,
            ))),
        },
        int_ty,
    );

    let _result = lower_typed_expression(&mut ctx, &if_expr);

    // Verify blocks were created (entry, then, else, merge)
    let func = &ctx.module.functions[0];
    assert!(func.blocks.len() >= 4);
}

#[test]
fn test_lower_typed_while() {
    let tcx = TypeContext::new();
    let mut ctx = setup_test_context(&tcx);

    let bool_ty = TypeId::BOOL;

    // Create: while false { () }
    let while_expr = make_typed_expr(
        TypedExprKind::While {
            cond: Box::new(make_typed_expr(
                TypedExprKind::Literal(Literal::Bool(false)),
                bool_ty,
            )),
            body: Box::new(make_typed_expr(
                TypedExprKind::Literal(Literal::Unit),
                TypeId::UNIT,
            )),
        },
        TypeId::UNIT,
    );

    let _result = lower_typed_expression(&mut ctx, &while_expr);

    // Verify loop blocks were created
    let func = &ctx.module.functions[0];
    assert!(func.blocks.len() >= 3); // entry, cond, body, exit
}

#[test]
fn test_lower_typed_function_call() {
    let tcx = TypeContext::new();
    let mut ctx = setup_test_context(&tcx);

    let int_ty = TypeId(3); // I64

    // Create: add(1, 2)
    let args = vec![
        make_typed_expr(TypedExprKind::Literal(Literal::Integer(1)), int_ty),
        make_typed_expr(TypedExprKind::Literal(Literal::Integer(2)), int_ty),
    ];

    let call = make_typed_expr(
        TypedExprKind::Call {
            func: Box::new(make_typed_expr(
                TypedExprKind::Variable(make_ident("add")),
                int_ty, // simplified type
            )),
            args,
        },
        int_ty,
    );

    let _result = lower_typed_expression(&mut ctx, &call);

    let func = &ctx.module.functions[0];
    let block = &func.blocks[0];

    // Should have const 1, const 2, and call
    assert!(block.instructions.len() >= 3);

    match &block.instructions[2] {
        Instruction::Call {
            func: name, args, ..
        } => {
            assert_eq!(name, "add");
            assert_eq!(args.len(), 2);
        }
        _ => panic!("Expected Call instruction"),
    }
}

#[test]
fn test_lower_typed_module_with_function() {
    let tcx = TypeContext::new();

    let int_ty = TypeId(3); // I64

    // Create a simple function: fn add(x: int, y: int) -> int: x + y
    let func = TypedFunction {
        name: make_ident("add"),
        generics: vec![],
        params: vec![
            TypedParam {
                pattern: Pattern::Ident {
                    mutable: false,
                    name: make_ident("x"),
                },
                ty: int_ty,
            },
            TypedParam {
                pattern: Pattern::Ident {
                    mutable: false,
                    name: make_ident("y"),
                },
                ty: int_ty,
            },
        ],
        return_type: int_ty,
        effects: EffectSet::empty(),
        body: make_typed_expr(
            TypedExprKind::Binary {
                op: BinaryOp::Add,
                left: Box::new(make_typed_expr(
                    TypedExprKind::Variable(make_ident("x")),
                    int_ty,
                )),
                right: Box::new(make_typed_expr(
                    TypedExprKind::Variable(make_ident("y")),
                    int_ty,
                )),
            },
            int_ty,
        ),
        public: true,
        span: DiagnosticsSpan::new(0, 0),
        where_clause: vec![],
    };

    let module = TypedModule {
        items: vec![TypedModuleItem::Function(func)],
        span: DiagnosticsSpan::new(0, 0),
    };

    let ir_module = lower_module(&module, &tcx, "test");

    // Should have builtins (8) + our function (1) = 9 functions
    assert!(ir_module.functions.len() >= 9);

    // Find our function
    let add_func = ir_module
        .functions
        .iter()
        .find(|f| f.name == "add")
        .expect("add function should exist");

    assert_eq!(add_func.params.len(), 2);
    assert!(add_func.is_exported);
}
