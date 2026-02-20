//! Tests for contract syntax parsing (requires/ensures/invariant)
#![allow(clippy::needless_range_loop, clippy::assertions_on_constants)]

use jet_lexer::Lexer;
use jet_parser::Parser;

fn parse_module(input: &str) -> Result<jet_parser::ast::Module, String> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    parser.parse_module().map_err(|e| format!("{:?}", e))
}

fn assert_parses(input: &str) {
    match parse_module(input) {
        Ok(_) => {}
        Err(e) => panic!("Expected parsing to succeed, but got error: {}", e),
    }
}

fn assert_parse_fails(input: &str) {
    if parse_module(input).is_ok() {
        panic!("Expected parsing to fail, but it succeeded");
    }
}

#[test]
fn debug_tokenize_contract() {
    let input =
        "fn divide(a: float, b: float) -> float\n    requires b != 0.0\n:\n    return a / b\n";

    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();

    for i in 0..tokens.len() {
        let token = &tokens[i];
        println!("{}: {:?} at {:?}", i, token.token, token.span);
    }

    assert!(true);
}

#[test]
fn debug_tokenize_result_ensures() {
    let input = "fn abs(x: int) -> int\n    ensures result >= 0\n    ensures result == x or result == -x\n:\n    return if x >= 0:\n        x\n    else:\n        -x\n";

    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();

    for i in 0..tokens.len() {
        let token = &tokens[i];
        println!("{}: {:?} at {:?}", i, token.token, token.span);
    }

    assert!(true);
}

// =============================================================================
// Function Contracts
// =============================================================================

#[test]
fn test_function_with_requires() {
    assert_parses(
        r#"
fn divide(a: float, b: float) -> float
    requires b != 0.0
:
    return a / b
"#,
    );
}

#[test]
fn test_function_with_ensures() {
    assert_parses(
        r#"
fn add(a: int, b: int) -> int
    ensures result == a + b
:
    return a + b
"#,
    );
}

#[test]
fn test_function_with_requires_and_ensures() {
    assert_parses(
        r#"
fn divide(a: float, b: float) -> float
    requires b != 0.0
    ensures result * b == a
:
    return a / b
"#,
    );
}

#[test]
fn test_function_with_multiple_requires() {
    assert_parses(
        r#"
fn binary_search(arr: [int], target: int) -> int
    requires arr.len() > 0
    requires is_sorted(arr)
:
    return 0
"#,
    );
}

#[test]
fn test_function_with_multiple_ensures() {
    assert_parses(
        r#"
fn clamp(x: int, min: int, max: int) -> int
    ensures result >= min
    ensures result <= max
:
    return x
"#,
    );
}

#[test]
fn test_function_with_complex_contract() {
    assert_parses(
        r#"
fn sqrt(x: float) -> float
    requires x >= 0.0
    ensures result >= 0.0
    ensures result * result == x
:
    return 0.0
"#,
    );
}

#[test]
fn test_function_with_contract_and_where_clause() {
    assert_parses(
        r#"
fn max[T](a: T, b: T) -> T
    where T: Ord
    requires true
    ensures result == a or result == b
:
    return a
"#,
    );
}

// =============================================================================
// Loop Invariants
// =============================================================================

#[test]
fn test_while_with_invariant() {
    assert_parses(
        r#"
fn test():
    let mut i = 0
    while i < 10
        invariant i >= 0 && i <= 10
    :
        i = i + 1
"#,
    );
}

#[test]
fn test_while_with_multiple_invariants() {
    assert_parses(
        r#"
fn test():
    let mut i = 0
    let mut j = 0
    while i < 10
        invariant i >= 0
        invariant j == i * 2
    :
        i = i + 1
        j = j + 2
"#,
    );
}

#[test]
fn test_for_with_invariant() {
    assert_parses(
        r#"
fn test():
    let mut sum = 0
    for x in items
        invariant sum >= 0
    :
        sum = sum + x
"#,
    );
}

#[test]
fn test_labeled_while_with_invariant() {
    assert_parses(
        r#"
fn test():
    'outer: while true
        invariant true
    :
        pass
"#,
    );
}

// =============================================================================
// Ghost Types
// =============================================================================

#[test]
fn test_ghost_type_simple() {
    assert_parses(
        r#"
ghost type SortedList[T] = List[T]
"#,
    );
}

#[test]
fn test_ghost_type_with_where() {
    assert_parse_fails(
        r#"
ghost type NonEmptyVec[T] = Vec[T] where self.len() > 0
"#,
    );
}

#[test]
fn test_public_ghost_type() {
    assert_parses(
        r#"
pub ghost type ValidIndex = int
"#,
    );
}

#[test]
fn test_ghost_type_no_generics() {
    assert_parses(
        r#"
ghost type PositiveInt = int
"#,
    );
}

// =============================================================================
// Result keyword in postconditions
// =============================================================================

#[test]
fn test_result_in_ensures() {
    assert_parses(
        r#"
fn abs(x: int) -> int
    ensures result >= 0
    ensures result == x or result == -x
:
    return if x >= 0:
        x
    else:
        -x
"#,
    );
}

// =============================================================================
// Combined Features
// =============================================================================

#[test]
fn test_function_with_contract_loop_and_ghost() {
    assert_parses(
        r#"
ghost type SortedArray[T] = [T]

fn find(arr: SortedArray[int], target: int) -> int
    requires is_sorted(arr)
    ensures result >= -1
:
    let mut i = 0
    while i < arr.len()
        invariant i >= 0
    :
        if arr[i] == target:
            return i
        i = i + 1
    return -1
"#,
    );
}

// =============================================================================
// Error Cases
// =============================================================================

#[test]
fn test_requires_without_expression() {
    assert_parse_fails(
        r#"
fn foo() -> int
    requires
:
    return 0
"#,
    );
}

#[test]
fn test_ensures_without_expression() {
    assert_parse_fails(
        r#"
fn foo() -> int
    ensures
:
    return 0
"#,
    );
}

#[test]
fn test_invariant_without_expression() {
    assert_parse_fails(
        r#"
fn test():
    while true
        invariant
    :
        pass
"#,
    );
}

#[test]
fn test_ghost_type_without_type_keyword() {
    assert_parse_fails(
        r#"
ghost SortedList[T] = List[T]
"#,
    );
}

#[test]
fn test_ghost_type_without_name() {
    assert_parse_fails(
        r#"
ghost type = int
"#,
    );
}

#[test]
fn test_ghost_type_without_definition() {
    assert_parse_fails(
        r#"
ghost type MyType
"#,
    );
}
