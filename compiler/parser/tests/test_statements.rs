//! Tests for statement parsing

use jet_lexer::Lexer;
use jet_parser::Parser;

fn parse_stmt(input: &str) -> Result<jet_parser::ast::Stmt, String> {
    let wrapped = format!("fn __test__():\n    {}", input);
    let mut lexer = Lexer::new(&wrapped);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);

    match parser.parse_module() {
        Ok(module) => {
            if let Some(jet_parser::ast::ModuleItem::Function(func)) = module.items.first() {
                match &func.body {
                    jet_parser::ast::Expr::Block(block) => block
                        .stmts
                        .first()
                        .cloned()
                        .ok_or_else(|| "No statement found".to_string()),
                    _ => Err("Function body is not a block".to_string()),
                }
            } else {
                Err("Failed to extract statement".to_string())
            }
        }
        Err(e) => Err(format!("{:?}", e)),
    }
}

fn assert_parses(input: &str) {
    match parse_stmt(input) {
        Ok(_) => {}
        Err(e) => panic!("Expected '{}' to parse, but got error: {}", input, e),
    }
}

fn assert_parse_fails(input: &str) {
    if parse_stmt(input).is_ok() {
        panic!("Expected '{}' to fail parsing, but it succeeded", input);
    }
}

#[test]
fn test_let_simple() {
    assert_parses("let x = 5");
}

#[test]
fn test_let_with_type() {
    assert_parses("let x: int = 5");
}

#[test]
fn test_let_mutable() {
    assert_parses("let mut x = 5");
}

#[test]
fn test_assign_simple() {
    assert_parses("x = 5");
}

#[test]
fn test_assign_ops() {
    assert_parses("x += 1");
    assert_parses("x -= 1");
    assert_parses("x *= 2");
    assert_parses("x /= 2");
}

#[test]
fn test_return() {
    assert_parses("return");
    assert_parses("return 42");
}

#[test]
fn test_break() {
    assert_parses("break");
    assert_parses("break 'outer");
}

#[test]
fn test_continue() {
    assert_parses("continue");
    assert_parses("continue 'outer");
}

#[test]
fn test_expr_stmt() {
    assert_parses("foo()");
}

#[test]
fn test_let_missing_equals() {
    assert_parse_fails("let x 5");
}

#[test]
fn test_let_missing_expression() {
    assert_parse_fails("let x =");
}
