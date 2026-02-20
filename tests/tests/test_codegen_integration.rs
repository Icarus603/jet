//! Codegen integration tests for the Jet compiler
//!
//! These tests verify end-to-end code generation for various language features.

use jet_integration_tests::harness::{compile_source, TestHarness};

// ============================================================================
// Basic Code Generation Tests
// ============================================================================

#[test]
fn test_codegen_hello_world() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn main():
    print("Hello, World!")
"#,
        )
        .unwrap();

    let result = harness.compile();
    // Compilation should succeed
    assert!(result.is_ok(), "Compilation should not panic");
}

#[test]
fn test_codegen_arithmetic() {
    let source = r#"fn main():
    let a = 1 + 2
    let b = 10 - 3
    let c = 4 * 5
    let d = 20 / 4
    print(a)
"#;

    let result = compile_source(source);
    assert!(result.is_ok(), "Arithmetic compilation should succeed");
}

#[test]
fn test_codegen_variables() {
    let source = r#"fn main():
    let x = 42
    let y = x + 1
    print(y)
"#;

    let result = compile_source(source);
    assert!(result.is_ok(), "Variable compilation should succeed");
}

#[test]
fn test_codegen_function_calls() {
    let source = r#"fn add(a: int, b: int) -> int:
    return a + b

fn main():
    let result = add(1, 2)
    print(result)
"#;

    let result = compile_source(source);
    assert!(result.is_ok(), "Function call compilation should succeed");
}

// ============================================================================
// Control Flow Code Generation Tests
// ============================================================================

#[test]
fn test_codegen_if_expression() {
    let source = r#"fn main():
    let x = 10
    if x > 5:
        print("big")
    else:
        print("small")
"#;

    let result = compile_source(source);
    assert!(result.is_ok(), "If expression compilation should succeed");
}

#[test]
fn test_codegen_while_loop() {
    let source = r#"fn main():
    let i = 0
    while i < 10:
        i = i + 1
    print(i)
"#;

    let result = compile_source(source);
    assert!(result.is_ok(), "While loop compilation should succeed");
}

#[test]
fn test_codegen_for_loop() {
    let source = r#"fn main():
    for i in 0..10:
        print(i)
"#;

    let result = compile_source(source);
    // For loop syntax might vary, just check it doesn't panic
    assert!(result.is_ok(), "For loop compilation should not panic");
}

#[test]
fn test_codegen_match_expression() {
    let source = r#"fn main():
    let x = Some(42)
    match x:
        | Some(n) => print(n)
        | None => print(0)
"#;

    let result = compile_source(source);
    assert!(
        result.is_ok(),
        "Match expression compilation should succeed"
    );
}

// ============================================================================
// Data Structure Code Generation Tests
// ============================================================================

#[test]
fn test_codegen_struct() {
    let source = r#"struct Point:
    x: int
    y: int

fn main():
    let p = Point { x: 1, y: 2 }
    print(p.x)
"#;

    let result = compile_source(source);
    assert!(result.is_ok(), "Struct compilation should succeed");
}

#[test]
fn test_codegen_enum() {
    let source = r#"enum Option[T]:
    | Some(T)
    | None

fn main():
    let x = Some(42)
    print(x)
"#;

    let result = compile_source(source);
    assert!(result.is_ok(), "Enum compilation should succeed");
}

#[test]
fn test_codegen_generic_function() {
    let source = r#"fn identity[T](x: T) -> T:
    return x

fn main():
    let x = identity(42)
    let y = identity("hello")
    print(x)
"#;

    let result = compile_source(source);
    assert!(
        result.is_ok(),
        "Generic function compilation should succeed"
    );
}

// ============================================================================
// Trait Code Generation Tests
// ============================================================================

#[test]
fn test_codegen_trait() {
    let source = r#"trait Display:
    fn display(self) -> string

struct MyType:
    value: int

impl Display for MyType:
    fn display(self) -> string:
        return "MyType"

fn main():
    let t = MyType { value: 42 }
    print(t.display())
"#;

    let result = compile_source(source);
    assert!(result.is_ok(), "Trait compilation should succeed");
}

#[test]
fn test_codegen_trait_with_default() {
    let source = r#"trait Greet:
    fn greet(self) -> string:
        return "Hello!"

struct Person:
    name: string

impl Greet for Person:
    pass

fn main():
    let p = Person { name: "Alice" }
    print(p.greet())
"#;

    let result = compile_source(source);
    assert!(
        result.is_ok(),
        "Trait with default compilation should succeed"
    );
}

// ============================================================================
// Effect System Code Generation Tests
// ============================================================================

#[test]
fn test_codegen_with_effects() {
    let source = r#"fn may_fail() -> int ! Error:
    return 42

fn main():
    let result = may_fail()
    print(result)
"#;

    let result = compile_source(source);
    assert!(
        result.is_ok(),
        "Effect declaration compilation should succeed"
    );
}

#[test]
fn test_codegen_effect_handler() {
    let source = r#"effect Error:
    fn raise(msg: string) -> never

fn main():
    with handler:
        | Error.raise(msg) => print(msg)
    in:
        print("handled")
"#;

    let result = compile_source(source);
    // Effect handlers are complex, just check it doesn't panic
    assert!(
        result.is_ok(),
        "Effect handler compilation should not panic"
    );
}

// ============================================================================
// String and Collection Code Generation Tests
// ============================================================================

#[test]
fn test_codegen_string_operations() {
    let source = r#"fn main():
    let s = "hello"
    let t = "world"
    let combined = s + " " + t
    print(combined)
"#;

    let result = compile_source(source);
    assert!(
        result.is_ok(),
        "String operations compilation should succeed"
    );
}

#[test]
fn test_codegen_list() {
    let source = r#"fn main():
    let list = [1, 2, 3, 4, 5]
    print(list[0])
"#;

    let result = compile_source(source);
    assert!(result.is_ok(), "List compilation should succeed");
}

#[test]
fn test_codegen_list_comprehension() {
    let source = r#"fn main():
    let squares = [x * x for x in 1..10]
    print(squares[0])
"#;

    let result = compile_source(source);
    assert!(
        result.is_ok(),
        "List comprehension compilation should succeed"
    );
}

// ============================================================================
// Closure and Higher-Order Function Tests
// ============================================================================

#[test]
fn test_codegen_closure() {
    let source = r#"fn main():
    let x = 10
    let f = fn(y: int) => x + y
    print(f(5))
"#;

    let result = compile_source(source);
    assert!(result.is_ok(), "Closure compilation should succeed");
}

#[test]
fn test_codegen_higher_order_function() {
    let source = r#"fn map(f: fn(int) -> int, list: [int]) -> [int]:
    return [f(x) for x in list]

fn main():
    let result = map(fn(x) => x * 2, [1, 2, 3])
    print(result)
"#;

    let result = compile_source(source);
    assert!(
        result.is_ok(),
        "Higher-order function compilation should succeed"
    );
}

// ============================================================================
// Pattern Matching Code Generation Tests
// ============================================================================

#[test]
fn test_codegen_pattern_matching_destructuring() {
    let source = r#"struct Point:
    x: int
    y: int

fn main():
    let p = Point { x: 1, y: 2 }
    let Point { x, y } = p
    print(x)
"#;

    let result = compile_source(source);
    assert!(result.is_ok(), "Destructuring compilation should succeed");
}

#[test]
fn test_codegen_pattern_matching_nested() {
    let source = r#"enum Option[T]:
    | Some(T)
    | None

fn main():
    let x = Some(Some(42))
    match x:
        | Some(Some(n)) => print(n)
        | _ => print(0)
"#;

    let result = compile_source(source);
    assert!(
        result.is_ok(),
        "Nested pattern matching compilation should succeed"
    );
}

// ============================================================================
// Module and Import Tests
// ============================================================================

#[test]
fn test_codegen_module() {
    let harness = TestHarness::new().unwrap();

    // Write a module file
    harness
        .write_source(
            "math.jet",
            r#"pub fn add(a: int, b: int) -> int:
    return a + b

pub fn sub(a: int, b: int) -> int:
    return a - b
"#,
        )
        .unwrap();

    // Write main file that imports the module
    harness
        .write_main(
            r#"import math

fn main():
    let result = math.add(1, 2)
    print(result)
"#,
        )
        .unwrap();

    let result = harness.compile();
    assert!(result.is_ok(), "Module compilation should succeed");
}

// ============================================================================
// Edge Case Code Generation Tests
// ============================================================================

#[test]
fn test_codegen_empty_function() {
    let source = r#"fn do_nothing():
    pass

fn main():
    do_nothing()
    print("done")
"#;

    let result = compile_source(source);
    assert!(result.is_ok(), "Empty function compilation should succeed");
}

#[test]
fn test_codegen_deeply_nested_blocks() {
    let source = r#"fn main():
    let x =
        let y =
            let z =
                let w = 42
                w + 1
            z + 1
        y + 1
    print(x)
"#;

    let result = compile_source(source);
    assert!(
        result.is_ok(),
        "Deeply nested blocks compilation should succeed"
    );
}

#[test]
fn test_codegen_recursion() {
    let source = r#"fn factorial(n: int) -> int:
    if n <= 1:
        return 1
    else:
        return n * factorial(n - 1)

fn main():
    print(factorial(5))
"#;

    let result = compile_source(source);
    assert!(result.is_ok(), "Recursion compilation should succeed");
}

#[test]
fn test_codegen_mutual_recursion() {
    let source = r#"fn is_even(n: int) -> bool:
    if n == 0:
        return true
    else:
        return is_odd(n - 1)

fn is_odd(n: int) -> bool:
    if n == 0:
        return false
    else:
        return is_even(n - 1)

fn main():
    print(is_even(10))
"#;

    let result = compile_source(source);
    assert!(
        result.is_ok(),
        "Mutual recursion compilation should succeed"
    );
}

// ============================================================================
// Type Inference Code Generation Tests
// ============================================================================

#[test]
fn test_codegen_type_inference() {
    let source = r#"fn main():
    # Type should be inferred as int
    let x = 42
    # Type should be inferred as string
    let y = "hello"
    # Type should be inferred from context
    let z = [1, 2, 3]
    print(x)
"#;

    let result = compile_source(source);
    assert!(result.is_ok(), "Type inference compilation should succeed");
}

#[test]
fn test_codegen_polymorphic_inference() {
    let source = r#"fn identity(x):
    return x

fn main():
    let a = identity(42)
    let b = identity("hello")
    print(a)
"#;

    let result = compile_source(source);
    assert!(
        result.is_ok(),
        "Polymorphic inference compilation should succeed"
    );
}

// ============================================================================
// Optimization Pass Tests
// ============================================================================

#[test]
fn test_codegen_constant_folding() {
    let source = r#"fn main():
    # These should be constant folded
    let x = 1 + 2 + 3
    let y = 10 * 5
    print(x)
"#;

    let result = compile_source(source);
    assert!(
        result.is_ok(),
        "Constant folding compilation should succeed"
    );
}

#[test]
fn test_codegen_dead_code_elimination() {
    let source = r#"fn main():
    let x = 42
    let y = 100  # This might be eliminated if unused
    print(x)
"#;

    let result = compile_source(source);
    assert!(
        result.is_ok(),
        "Dead code elimination should not break compilation"
    );
}

// ============================================================================
// Error Handling Code Generation Tests
// ============================================================================

#[test]
fn test_codegen_try_catch() {
    let source = r#"fn main():
    try:
        may_fail()
    catch e:
        print("caught")
"#;

    let result = compile_source(source);
    assert!(result.is_ok(), "Try-catch compilation should succeed");
}

#[test]
fn test_codegen_result_type() {
    let source = r#"enum Result[T, E]:
    | Ok(T)
    | Err(E)

fn main():
    let result: Result[int, string] = Ok(42)
    match result:
        | Ok(n) => print(n)
        | Err(e) => print(e)
"#;

    let result = compile_source(source);
    assert!(result.is_ok(), "Result type compilation should succeed");
}

// ============================================================================
// Async/Await Code Generation Tests
// ============================================================================

#[test]
fn test_codegen_async_function() {
    let source = r#"async fn fetch_data() -> string:
    return "data"

fn main():
    let future = fetch_data()
    print("created future")
"#;

    let result = compile_source(source);
    assert!(result.is_ok(), "Async function compilation should succeed");
}

#[test]
fn test_codegen_await() {
    let source = r#"async fn get_value() -> int:
    return 42

async fn main():
    let value = await get_value()
    print(value)
"#;

    let result = compile_source(source);
    assert!(result.is_ok(), "Await compilation should succeed");
}
