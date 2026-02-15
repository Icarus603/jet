//! Comprehensive type checker tests for Jet
//!
//! Tests type inference, generics, traits, and error handling.

use jet_lexer::Lexer;
use jet_parser::Parser;
use jet_typeck::type_check_module;

fn type_check_source(source: &str) -> Result<(), Vec<String>> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let ast = parser
        .parse_module()
        .map_err(|e| vec![format!("{:?}", e)])?;

    match type_check_module(&ast) {
        Ok(_) => Ok(()),
        Err(diagnostics) => Err(diagnostics.iter().map(|d| d.message.clone()).collect()),
    }
}

fn assert_type_checks(source: &str) {
    match type_check_source(source) {
        Ok(_) => {}
        Err(errors) => panic!(
            "Expected type checking to succeed, but got errors: {:?}",
            errors
        ),
    }
}

fn assert_type_error(source: &str, expected_error: &str) {
    match type_check_source(source) {
        Ok(_) => panic!(
            "Expected type error containing '{}', but type checking succeeded",
            expected_error
        ),
        Err(errors) => {
            let found = errors.iter().any(|e| e.contains(expected_error));
            if !found {
                panic!(
                    "Expected error containing '{}', but got: {:?}",
                    expected_error, errors
                );
            }
        }
    }
}

// ============================================================================
// Primitive Type Tests
// ============================================================================

#[test]
fn test_integer_types() {
    assert_type_checks(
        r#"fn test():
    let a: int8 = 127
    let b: int16 = 32767
    let c: int32 = 2147483647
    let d: int64 = 9223372036854775807
    let e: int = 42
    let f: uint8 = 255
    let g: uint16 = 65535
    let h: uint32 = 4294967295
    let i: uint64 = 18446744073709551615
    let j: uint = 42"#,
    );
}

#[test]
fn test_float_types() {
    assert_type_checks(
        r#"fn test():
    let a: float32 = 3.14
    let b: float64 = 3.14159265359
    let c: float = 2.718"#,
    );
}

#[test]
fn test_bool_type() {
    assert_type_checks(
        r#"fn test():
    let t: bool = true
    let f: bool = false"#,
    );
}

#[test]
fn test_string_type() {
    assert_type_checks(
        r#"fn test():
    let s: string = "hello""#,
    );
}

#[test]
fn test_char_type() {
    assert_type_checks(
        r#"fn test():
    let c: char = 'a'"#,
    );
}

#[test]
fn test_unit_type() {
    assert_type_checks(
        r#"fn test():
    let u: unit = ()"#,
    );
}

// ============================================================================
// Type Inference Tests
// ============================================================================

#[test]
fn test_basic_inference() {
    assert_type_checks(
        r#"fn test():
    let x = 5  # inferred as int
    let y = 3.14  # inferred as float64
    let z = "hello"  # inferred as string
    let b = true  # inferred as bool"#,
    );
}

#[test]
fn test_inference_from_context() {
    assert_type_checks(
        r#"fn takes_int(x: int):
    pass

fn test():
    let x = 5
    takes_int(x)  # x inferred as int from context"#,
    );
}

#[test]
fn test_inference_in_expressions() {
    assert_type_checks(
        r#"fn test():
    let a = 1 + 2  # int
    let b = 1.0 + 2.0  # float64
    let c = "hello" + "world"  # string
    let d = true and false  # bool"#,
    );
}

// ============================================================================
// Function Type Tests
// ============================================================================

#[test]
fn test_simple_function_types() {
    assert_type_checks(
        r#"fn add(x: int, y: int) -> int:
    return x + y

fn test():
    let result: int = add(1, 2)"#,
    );
}

#[test]
fn test_function_with_effects() {
    assert_type_checks(
        r#"fn may_fail() -> int ! Error:
    raise Error

fn test():
    let result: Result[int, Error] = try:
        may_fail()"#,
    );
}

#[test]
fn test_higher_order_functions() {
    assert_type_checks(
        r#"fn apply[T, U](f: fn(T) -> U, x: T) -> U:
    return f(x)

fn double(x: int) -> int:
    return x * 2

fn test():
    let result: int = apply(double, 5)"#,
    );
}

#[test]
fn test_function_type_inference() {
    assert_type_checks(
        r#"fn map[T, U](list: [T], f: fn(T) -> U) -> [U]:
    return []

fn test():
    let nums = [1, 2, 3]
    let doubled = map(nums, fn(x) => x * 2)"#,
    );
}

// ============================================================================
// Generic Tests
// ============================================================================

#[test]
fn test_simple_generics() {
    assert_type_checks(
        r#"fn identity[T](x: T) -> T:
    return x

fn test():
    let a: int = identity(5)
    let b: string = identity("hello")"#,
    );
}

#[test]
fn test_generic_structs() {
    assert_type_checks(
        r#"struct Box[T]:
    value: T

impl[T] Box[T]:
    fn new(value: T) -> Box[T]:
        return Box { value: value }

    fn get(self) -> T:
        return self.value

fn test():
    let int_box: Box[int] = Box::new(5)
    let str_box: Box[string] = Box::new("hello")"#,
    );
}

#[test]
fn test_generic_enums() {
    assert_type_checks(
        r#"enum Option[T]:
    | Some(T)
    | None

fn test():
    let some_int: Option[int] = Some(5)
    let none_int: Option[int] = None"#,
    );
}

#[test]
fn test_multiple_type_parameters() {
    assert_type_checks(
        r#"struct Pair[T, U]:
    first: T
    second: U

fn make_pair[T, U](t: T, u: U) -> Pair[T, U]:
    return Pair { first: t, second: u }

fn test():
    let p: Pair[int, string] = make_pair(1, "hello")"#,
    );
}

#[test]
fn test_generic_constraints() {
    assert_type_checks(
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

fn test():
    let m: int = max(5, 3)"#,
    );
}

#[test]
fn test_nested_generics() {
    assert_type_checks(
        r#"struct Vec[T]:
    pass

struct Map[K, V]:
    pass

fn test():
    let list_of_lists: Vec[Vec[int]] = Vec
    let map_of_lists: Map[string, Vec[int]] = Map"#,
    );
}

// ============================================================================
// Trait Tests
// ============================================================================

#[test]
fn test_simple_trait() {
    assert_type_checks(
        r#"trait Display:
    fn display(self) -> string

struct Point:
    x: float
    y: float

impl Display for Point:
    fn display(self) -> string:
        return "Point"

fn test():
    let p = Point { x: 1.0, y: 2.0 }
    let s: string = p.display()"#,
    );
}

#[test]
fn test_trait_with_default() {
    assert_type_checks(
        r#"trait Default:
    fn default() -> Self

impl Default for int:
    fn default() -> int:
        return 0

fn test():
    let x: int = int::default()"#,
    );
}

#[test]
fn test_trait_bounds() {
    assert_type_checks(
        r#"trait Display:
    fn display(self) -> string

trait Clone:
    fn clone(self) -> Self

fn print_and_clone[T: Display + Clone](x: T) -> T:
    print(x.display())
    return x.clone()"#,
    );
}

#[test]
fn test_associated_types() {
    assert_type_checks(
        r#"trait Iterator:
    type Item
    fn next(self) -> Option[Self.Item]

struct Counter:
    count: int

impl Iterator for Counter:
    type Item = int
    fn next(self) -> Option[int]:
        return Some(self.count)"#,
    );
}

// ============================================================================
// Pattern Matching Tests
// ============================================================================

#[test]
fn test_pattern_matching_types() {
    assert_type_checks(
        r#"enum Option[T]:
    | Some(T)
    | None

fn unwrap_or_default[T: Default](opt: Option[T]) -> T:
    match opt:
        | Some(value) => return value
        | None => return T::default()"#,
    );
}

#[test]
fn test_struct_patterns() {
    assert_type_checks(
        r#"struct Point:
    x: float
    y: float

fn get_x(p: Point) -> float:
    match p:
        | Point { x, y: _ } => return x"#,
    );
}

#[test]
fn test_tuple_patterns() {
    assert_type_checks(
        r#"fn swap[T, U](pair: (T, U)) -> (U, T):
    match pair:
        | (t, u) => return (u, t)"#,
    );
}

#[test]
fn test_nested_patterns() {
    assert_type_checks(
        r#"enum Option[T]:
    | Some(T)
    | None

fn nested_match(x: Option[Option[int]]) -> int:
    match x:
        | Some(Some(n)) => return n
        | Some(None) => return 0
        | None => return -1"#,
    );
}

// ============================================================================
// Reference and Borrowing Tests
// ============================================================================

#[test]
fn test_reference_types() {
    assert_type_checks(
        r#"fn test():
    let x: int = 5
    let r: &int = &x
    let m: &mut int = &mut x"#,
    );
}

#[test]
fn test_borrowing_rules() {
    assert_type_checks(
        r#"fn borrow_example(x: &int) -> int:
    return *x

fn test():
    let x = 5
    let r = &x
    let y = borrow_example(r)"#,
    );
}

#[test]
fn test_mutable_borrowing() {
    assert_type_checks(
        r#"fn increment(x: &mut int):
    *x = *x + 1

fn test():
    let mut x = 5
    increment(&mut x)"#,
    );
}

// ============================================================================
// Array and Slice Tests
// ============================================================================

#[test]
fn test_array_types() {
    assert_type_checks(
        r#"fn test():
    let arr: [int; 5] = [1, 2, 3, 4, 5]
    let empty: [int; 0] = []"#,
    );
}

#[test]
fn test_slice_types() {
    assert_type_checks(
        r#"fn sum_slice(nums: &[int]) -> int:
    return 0

fn test():
    let arr = [1, 2, 3, 4, 5]
    let result = sum_slice(&arr)"#,
    );
}

#[test]
fn test_array_operations() {
    assert_type_checks(
        r#"fn test():
    let arr = [1, 2, 3]
    let first: int = arr[0]
    let len: int = arr.len()"#,
    );
}

// ============================================================================
// Error Type Tests
// ============================================================================

#[test]
fn test_result_type() {
    assert_type_checks(
        r#"enum Result[T, E]:
    | Ok(T)
    | Err(E)

fn may_fail() -> Result[int, string]:
    return Ok(42)

fn test():
    let result: Result[int, string] = may_fail()"#,
    );
}

#[test]
fn test_try_operator() {
    assert_type_checks(
        r#"enum Result[T, E]:
    | Ok(T)
    | Err(E)

fn inner() -> Result[int, string]:
    return Ok(5)

fn outer() -> Result[int, string]:
    let x = inner()?
    return Ok(x)"#,
    );
}

#[test]
fn test_effect_propagation() {
    assert_type_checks(
        r#"fn raises_error() -> int ! Error:
    raise Error

fn caller() -> int ! Error:
    return raises_error()"#,
    );
}

// ============================================================================
// Async and Concurrency Tests
// ============================================================================

#[test]
fn test_async_function_types() {
    assert_type_checks(
        r#"async fn fetch() -> int:
    return 42

fn test():
    let future = fetch()"#,
    );
}

#[test]
fn test_channel_types() {
    assert_type_checks(
        r#"fn test():
    let (tx, rx): (Sender[int], Receiver[int]) = chan[int]()
    tx.send(42)
    let value: int = rx.recv()"#,
    );
}

// ============================================================================
// Type Error Tests
// ============================================================================

#[test]
fn test_type_mismatch_error() {
    assert_type_error(
        r#"fn test():
    let x: int = "hello""#,
        "type mismatch",
    );
}

#[test]
fn test_undefined_variable_error() {
    assert_type_error(
        r#"fn test():
    print(undefined_var)"#,
        "not found",
    );
}

#[test]
fn test_wrong_argument_count_error() {
    assert_type_error(
        r#"fn foo(x: int, y: int):
    pass

fn test():
    foo(1)"#,
        "argument",
    );
}

#[test]
fn test_missing_return_error() {
    assert_type_error(
        r#"fn foo() -> int:
    pass"#,
        "return",
    );
}

#[test]
fn test_borrow_checker_error() {
    assert_type_error(
        r#"fn test():
    let x = 5
    let r1 = &mut x
    let r2 = &mut x"#,
        "borrow",
    );
}

#[test]
fn test_missing_trait_impl_error() {
    assert_type_error(
        r#"trait Display:
    fn display(self) -> string

fn print_display[T: Display](x: T):
    pass

struct MyType:
    pass

fn test():
    let x = MyType
    print_display(x)"#,
        "trait",
    );
}

// ============================================================================
// Complex Integration Tests
// ============================================================================

#[test]
fn test_generic_linked_list() {
    assert_type_checks(
        r#"enum List[T]:
    | Cons(T, List[T])
    | Nil

impl[T] List[T]:
    fn new() -> List[T]:
        return Nil

    fn push(self, value: T) -> List[T]:
        return Cons(value, self)

    fn len(self) -> int:
        match self:
            | Cons(_, tail) => 1 + tail.len()
            | Nil => 0

fn test():
    let list: List[int] = List::new().push(1).push(2).push(3)
    let length: int = list.len()"#,
    );
}

#[test]
fn test_trait_object_simulation() {
    assert_type_checks(
        r#"trait Drawable:
    fn draw(self)

struct Circle:
    radius: float

struct Square:
    side: float

impl Drawable for Circle:
    fn draw(self):
        pass

impl Drawable for Square:
    fn draw(self):
        pass

fn draw_all[T: Drawable](items: [T]):
    for item in items:
        item.draw()"#,
    );
}

#[test]
fn test_complex_generic_constraints() {
    assert_type_checks(
        r#"trait Addable:
    fn add(self, other: Self) -> Self

trait Comparable:
    fn less_than(self, other: Self) -> bool

fn sum_if_small[T: Addable + Comparable](a: T, b: T, limit: T) -> T:
    if a.less_than(limit) and b.less_than(limit):
        return a.add(b)
    else:
        return a

impl Addable for int:
    fn add(self, other: int) -> int:
        return self + other

impl Comparable for int:
    fn less_than(self, other: int) -> bool:
        return self < other

fn test():
    let result: int = sum_if_small(5, 3, 10)"#,
    );
}
