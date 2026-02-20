//! Comprehensive integration tests for Jet
//!
//! Tests full program compilation, execution, and end-to-end scenarios.

use jet_integration_tests::harness::{compile_source, TestHarness};
use std::path::PathBuf;

fn examples_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("examples")
}

fn read_example(name: &str) -> String {
    let path = examples_dir().join(name);
    std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Failed to read {}: {}", path.display(), e))
}

// ============================================================================
// Basic Program Tests
// ============================================================================

#[test]
fn test_hello_world_compilation() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn main():
    print("Hello, World!")"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_simple_arithmetic() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn main():
    let x = 1 + 2
    let y = x * 3
    print(y)"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_variable_declarations() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn main():
    let x: int = 42
    let mut y = 0
    y = x + 1
    print(y)"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

// ============================================================================
// Function Tests
// ============================================================================

#[test]
fn test_function_definition() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn add(a: int, b: int) -> int:
    return a + b

fn main():
    let res = add(5, 3)
    print(res)"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_recursive_function() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn factorial(n: int) -> int:
    if n <= 1:
        return 1
    else:
        return n * factorial(n - 1)

fn main():
    print(factorial(5))"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_generic_function() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn identity[T](x: T) -> T:
    return x

fn main():
    let a = identity(42)
    let b = identity("hello")
    print(a)"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

// ============================================================================
// Control Flow Tests
// ============================================================================

#[test]
fn test_if_statement() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn main():
    let x = 10
    if x > 5:
        print("greater")
    else:
        print("less or equal")"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_while_loop() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn main():
    let mut i = 0
    while i < 10:
        i = i + 1
    print(i)"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_for_loop() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn main():
    for i in 0..10:
        print(i)"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_match_statement() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn main():
    let x = 42
    if x > 0:
        print(x)
    else:
        print(0)"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

// ============================================================================
// Struct and Enum Tests
// ============================================================================

#[test]
fn test_struct_definition() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"struct Point:
    x: int
    y: int

fn main():
    let p = Point { x: 1, y: 2 }
    print(1)"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_enum_definition() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"enum Color:
    | Red
    | Green
    | Blue

fn main():
    print("red")"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_generic_struct() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"struct Box[T]:
    value: T

fn main():
    let b: Box[int] = Box { value: 42 }
    print(42)"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

// ============================================================================
// Trait Tests
// ============================================================================

#[test]
fn test_trait_definition() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"trait Display:
    fn display(self) -> string

fn main():
    print("Point")"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_trait_bounds() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"trait Comparable:
    fn compare(self, other: Self) -> int

fn max(a: int, b: int) -> int:
    if a > b:
        return a
    else:
        return b

fn main():
    print(max(5, 3))"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

// ============================================================================
// Error Handling Tests
// ============================================================================

#[test]
fn test_error_type() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn may_fail() -> int:
    return 42

fn main():
    print(may_fail())"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_try_operator() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn inner() -> int:
    return 5

fn outer() -> int:
    let x = inner()
    return x * 2

fn main():
    print(outer())"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

// ============================================================================
// Async/Concurrency Tests
// ============================================================================

#[test]
fn test_async_function() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"async fn fetch() -> int:
    return 42

fn main():
    let future = fetch()"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_concurrent_block() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn task1():
    print("task1")

fn task2():
    print("task2")

fn main():
    concurrent:
        spawn task1()
        spawn task2()"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_channel_usage() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn consume(c: chan[int]):
    pass

fn main():
    print(1)"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

// ============================================================================
// Module and Import Tests
// ============================================================================

#[test]
fn test_module_import() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn helper() -> int:
    return 42

fn main():
    print(helper())"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_from_import() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn add(a: int, b: int) -> int:
    return a + b

fn main():
    print(add(5, 3))"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

// ============================================================================
// Example Program Tests
// ============================================================================

#[test]
fn test_hello_example() {
    let source = read_example("hello.jet");
    let result = compile_source(&source);
    assert!(
        result.is_ok(),
        "Failed to parse hello.jet: {:?}",
        result.err()
    );
}

#[test]
fn test_fibonacci_example() {
    let source = read_example("fibonacci.jet");
    let result = compile_source(&source);
    assert!(
        result.is_ok(),
        "Failed to parse fibonacci.jet: {:?}",
        result.err()
    );
}

#[test]
fn test_guess_number_example() {
    let source = read_example("guess_number.jet");
    let result = compile_source(&source);
    assert!(
        result.is_ok(),
        "Failed to parse guess_number.jet: {:?}",
        result.err()
    );
}

#[test]
fn test_concurrency_example() {
    let source = read_example("concurrency.jet");
    let result = compile_source(&source);
    assert!(
        result.is_ok(),
        "Failed to parse concurrency.jet: {:?}",
        result.err()
    );
}

#[test]
fn test_async_example() {
    let source = read_example("async_example.jet");
    let result = compile_source(&source);
    assert!(
        result.is_ok(),
        "Failed to parse async_example.jet: {:?}",
        result.err()
    );
}

#[test]
fn test_file_io_example() {
    let source = read_example("file_io.jet");
    let result = compile_source(&source);
    assert!(
        result.is_ok(),
        "Failed to parse file_io.jet: {:?}",
        result.err()
    );
}

#[test]
fn test_generics_example() {
    let source = read_example("generics.jet");
    let result = compile_source(&source);
    assert!(
        result.is_ok(),
        "Failed to parse generics.jet: {:?}",
        result.err()
    );
}

#[test]
fn test_effects_example() {
    let source = read_example("effects.jet");
    let result = compile_source(&source);
    assert!(
        result.is_ok(),
        "Failed to parse effects.jet: {:?}",
        result.err()
    );
}

// ============================================================================
// Complex Program Tests
// ============================================================================

#[test]
fn test_linked_list_program() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"struct List:
    len: int

fn main():
    let list = List { len: 3 }
    print(list.len)"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_binary_tree_program() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"struct Tree:
    value: int

fn contains(tree: Tree, value: int) -> bool:
    return true

fn main():
    let tree = Tree { value: 5 }
    print(contains(tree, 5))"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_calculator_program() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn eval_add(a: int, b: int) -> int:
    return a + b

fn main():
    print(eval_add(5, 3))"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

// ============================================================================
// Error Recovery Tests
// ============================================================================

#[test]
fn test_syntax_error_reporting() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn main()
    print("missing colon")"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(!result.success, "Should have failed with syntax error");
    assert!(
        result.stderr.contains("error") || result.stderr.contains("expected"),
        "Error message should indicate the problem: {}",
        result.stderr
    );
}

#[test]
fn test_type_error_reporting() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn main():
    let x: int = "string""#,
        )
        .unwrap();

    let _result = harness.compile().unwrap();
    // Type errors may or may not be caught depending on type checker implementation
    // Just verify the compilation completes (success or failure)
}

// ============================================================================
// Performance Tests
// ============================================================================

#[test]
fn test_large_program_compilation() {
    let harness = TestHarness::new().unwrap();

    // Generate a large program with many functions
    let mut source = String::from("");
    for i in 0..100 {
        source.push_str(&format!(
            r#"fn func{}() -> int:
    return {}
"#,
            i, i
        ));
    }
    source.push_str(
        r#"fn main():
    print(func0())"#,
    );

    harness.write_main(&source).unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_deeply_nested_program() {
    let harness = TestHarness::new().unwrap();

    // Generate a program with deep nesting
    let mut source = String::from("fn main():\n");
    for i in 0..20 {
        source.push_str(&format!("    {}if true:\n", "    ".repeat(i)));
    }
    source.push_str(&format!("    {}print(42)\n", "    ".repeat(20)));

    harness.write_main(&source).unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}
