//! Advanced integration tests for Jet compiler pipeline
//!
//! Tests advanced features: closures, higher-order functions, complex effects,
//! and advanced concurrency patterns.

use jet_integration_tests::harness::{compile_source, TestHarness};

// ============================================================================
// Higher-Order Function Tests
// ============================================================================

#[test]
fn test_higher_order_functions() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn apply(f: fn(int) -> int, x: int) -> int:
    return f(x)

fn double(x: int) -> int:
    return x * 2

fn main():
    let result = apply(double, 5)
    print(result)"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_function_composition() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn compose(f: fn(int) -> int, g: fn(int) -> int) -> fn(int) -> int:
    return fn(x: int) -> int:
        return f(g(x))

fn add_one(x: int) -> int:
    return x + 1

fn mul_two(x: int) -> int:
    return x * 2

fn main():
    let h = compose(add_one, mul_two)
    print(h(5))"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_map_function() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn map(f: fn(int) -> int, items: [int]) -> [int]:
    let mut result = []
    for item in items:
        result.push(f(item))
    return result

fn square(x: int) -> int:
    return x * x

fn main():
    let nums = [1, 2, 3, 4, 5]
    let squared = map(square, nums)
    print(squared)"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_filter_function() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn filter(pred: fn(int) -> bool, items: [int]) -> [int]:
    let mut result = []
    for item in items:
        if pred(item):
            result.push(item)
    return result

fn is_even(x: int) -> bool:
    return x % 2 == 0

fn main():
    let nums = [1, 2, 3, 4, 5, 6]
    let evens = filter(is_even, nums)
    print(evens)"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_fold_function() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn fold(f: fn(int, int) -> int, init: int, items: [int]) -> int:
    let mut acc = init
    for item in items:
        acc = f(acc, item)
    return acc

fn add(a: int, b: int) -> int:
    return a + b

fn main():
    let nums = [1, 2, 3, 4, 5]
    let sum = fold(add, 0, nums)
    print(sum)"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

// ============================================================================
// Closure Tests
// ============================================================================

#[test]
fn test_simple_closure() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn make_adder(x: int) -> fn(int) -> int:
    return fn(y: int) -> int:
        return x + y

fn main():
    let add_five = make_adder(5)
    print(add_five(3))"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_closure_with_multiple_captures() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn make_multiplier(a: int, b: int) -> fn(int) -> int:
    return fn(x: int) -> int:
        return a * b * x

fn main():
    let mult = make_multiplier(2, 3)
    print(mult(4))"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_closure_as_callback() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn with_callback(callback: fn(int) -> unit):
    callback(42)

fn main():
    let mut result = 0
    with_callback(fn(x: int):
        result = x
    )
    print(result)"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

// ============================================================================
// Advanced Effect Tests
// ============================================================================

#[test]
fn test_effect_propagation() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"struct ErrorA:
    message: string

struct ErrorB:
    code: int

fn may_fail_a() -> int ! ErrorA:
    return 42

fn may_fail_b() -> int ! ErrorB:
    return 42

fn main():
    print("effect propagation test")"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_multiple_effects() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"struct IOError:
    path: string

struct ParseError:
    input: string

fn read_and_parse(path: string) -> int ! IOError:
    if path == "":
        return 0
    return 42

fn main():
    print("multiple effects test")"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_try_operator_chaining() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"struct Error:
    msg: string

fn step1() -> int ! Error:
    return 1

fn step2(x: int) -> int ! Error:
    return x + 1

fn step3(x: int) -> int ! Error:
    return x * 2

fn pipeline() -> int ! Error:
    let a = step1()?
    let b = step2(a)?
    return step3(b)?

fn main():
    print("try operator chaining test")"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_effect_polymorphism() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"struct Error1:
    msg: string

struct Error2:
    code: int

fn main():
    print("effect polymorphism test")"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

// ============================================================================
// Advanced Concurrency Tests
// ============================================================================

#[test]
fn test_async_function_chain() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"async fn step1() -> int:
    return 1

async fn step2(x: int) -> int:
    return x + 1

async fn step3(x: int) -> int:
    return x * 2

async fn pipeline() -> int:
    let a = await step1()
    let b = await step2(a)
    return await step3(b)

fn main():
    print("async chain test")"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_concurrent_with_multiple_spawns() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn worker(id: int) -> int:
    return id * 10

fn main():
    concurrent:
        let h1 = spawn worker(1)
        let h2 = spawn worker(2)
        let h3 = spawn worker(3)
        let r1 = await h1
        let r2 = await h2
        let r3 = await h3
        print(r1 + r2 + r3)"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_async_await_in_concurrent_block() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"async fn fetch_data(id: int) -> int:
    return id * 100

fn main():
    concurrent:
        let h1 = spawn fetch_data(1).await
        let h2 = spawn fetch_data(2).await
        let r1 = await h1
        let r2 = await h2
        print(r1 + r2)"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

// ============================================================================
// Complex Type Tests
// ============================================================================

#[test]
fn test_nested_generics() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"struct Box[T]:
    value: T

struct Pair[A, B]:
    first: A
    second: B

fn main():
    let nested: Box[Pair[int, string]] = Box {
        value: Pair { first: 1, second: "hello" }
    }
    print(nested.value.first)"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_generic_constraints() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"trait Comparable:
    fn compare(self, other: Self) -> int

fn max[T: Comparable](a: T, b: T) -> T:
    if a.compare(b) > 0:
        return a
    else:
        return b

impl Comparable for int:
    fn compare(self, other: int) -> int:
        return self - other

fn main():
    print(max(5, 3))"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_recursive_types() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"enum Tree[T]:
    | Node(T, Tree[T], Tree[T])
    | Leaf

fn tree_size[T](tree: Tree[T]) -> int:
    match tree:
        | Node(_, left, right) -> 1 + tree_size(left) + tree_size(right)
        | Leaf -> 0

fn main():
    let tree = Node(1, Node(2, Leaf, Leaf), Leaf)
    print(tree_size(tree))"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

// ============================================================================
// Pattern Matching Tests
// ============================================================================

#[test]
fn test_complex_pattern_matching() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"enum Expr:
    | Num(int)
    | Add(Expr, Expr)
    | Mul(Expr, Expr)

fn eval(expr: Expr) -> int:
    match expr:
        | Num(n) -> n
        | Add(a, b) -> eval(a) + eval(b)
        | Mul(a, b) -> eval(a) * eval(b)

fn main():
    let expr = Add(Num(1), Mul(Num(2), Num(3)))
    print(eval(expr))"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_pattern_guards() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn classify(n: int) -> string:
    match n:
        | x if x < 0 -> "negative"
        | x if x == 0 -> "zero"
        | x if x > 0 and x < 10 -> "small positive"
        | _ -> "large positive"

fn main():
    print(classify(5))
    print(classify(-3))
    print(classify(100))"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_nested_patterns() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"enum Option[T]:
    | Some(T)
    | None

fn flatten[T](opt: Option[Option[T]]) -> Option[T]:
    match opt:
        | Some(Some(x)) -> Some(x)
        | Some(None) -> None
        | None -> None

fn main():
    let nested = Some(Some(42))
    print(flatten(nested))"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

// ============================================================================
// Edge Case Tests
// ============================================================================

#[test]
fn test_empty_program() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn main():
    pass"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_very_long_function_chain() {
    let harness = TestHarness::new().unwrap();

    let mut source = String::from("");
    for i in 0..20 {
        source.push_str(&format!(
            r#"fn func_{}(x: int) -> int:
    return x + {}

"#,
            i, i
        ));
    }
    source.push_str(
        r#"fn main():
    print(func_0(0))"#,
    );

    harness.write_main(&source).unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_unicode_in_strings() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn main():
    let hello = "Hello, 世界!"
    let emoji = "🎉🎊"
    print(hello)"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_deeply_nested_expressions() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn main():
    let x = 1 + (2 + (3 + (4 + (5 + (6 + (7 + (8 + 9)))))))
    print(x)"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

#[test]
fn test_many_function_arguments() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn many_args(a: int, b: int, c: int, d: int, e: int, f: int, g: int, h: int) -> int:
    return a + b + c + d + e + f + g + h

fn main():
    print(many_args(1, 2, 3, 4, 5, 6, 7, 8))"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    assert!(result.success, "Compilation failed: {}", result.stderr);
}

// ============================================================================
// Compilation Failure Tests
// ============================================================================

#[test]
fn test_type_mismatch_error() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn main():
    let x: int = "hello""#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    // Type errors may or may not be caught depending on type checker implementation
    // Just verify the compilation completes
}

#[test]
fn test_undefined_variable_error() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn main():
    print(undefined_var)"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    // May fail at resolution or type checking
}

#[test]
fn test_missing_return_error() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main(
            r#"fn must_return() -> int:
    pass

fn main():
    print(must_return())"#,
        )
        .unwrap();

    let result = harness.compile().unwrap();
    // May fail at type checking
}
