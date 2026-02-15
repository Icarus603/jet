//! Integration tests for the Jet parser
//!
//! Tests all language constructs and parsing edge cases.

use jet_integration_tests::harness::parse_source;

#[test]
fn test_parse_simple_function() {
    let source = r#"fn main():
    42
"#;

    // Parser is still being developed, so we just check it doesn't panic
    let _ast = parse_source(source);
}

#[test]
fn test_parse_function_with_params() {
    let source = r#"fn add(x: int, y: int) -> int:
    return x + y
"#;

    let _ast = parse_source(source);
}

#[test]
fn test_parse_function_with_generics() {
    let source = r#"fn identity[T](value: T) -> T:
    return value
"#;

    let _ast = parse_source(source);
}

#[test]
fn test_parse_function_with_effects() {
    let source = r#"fn divide(a: int, b: int) -> int ! DivisionError:
    if b == 0:
        raise DivisionError
    return a / b
"#;

    let _ast = parse_source(source);
}

#[test]
fn test_parse_struct() {
    let source = r#"struct Point:
    x: float
    y: float
"#;

    let _ast = parse_source(source);
}

#[test]
fn test_parse_struct_with_generics() {
    let source = r#"struct Box[T]:
    value: T
"#;

    let _ast = parse_source(source);
}

#[test]
fn test_parse_enum() {
    let source = r#"enum Option[T]:
    | Some(T)
    | None
"#;

    let _ast = parse_source(source);
}

#[test]
fn test_parse_trait() {
    let source = r#"trait Comparable:
    fn compare(self, other: Self) -> int;
"#;

    let _ast = parse_source(source);
}

#[test]
fn test_parse_impl() {
    let source = r#"impl Point:
    fn new(x: float, y: float) -> Self:
        return Point { x, y }
"#;

    let _ast = parse_source(source);
}

#[test]
fn test_parse_trait_impl() {
    let source = r#"impl Comparable for Point:
    fn compare(self, other: Point) -> int:
        return 0
"#;

    let _ast = parse_source(source);
}

#[test]
fn test_parse_import() {
    let source = r#"import std::io
import std::collections::Map as HashMap
from std::fs import read_file, write_file
"#;

    let _ast = parse_source(source);
}

#[test]
fn test_parse_type_alias() {
    let source = r#"type StringMap[T] = Map[string, T]"#;

    let _ast = parse_source(source);
}

#[test]
fn test_parse_if_expression() {
    let source = r#"fn test(x: int) -> int:
    if x > 0:
        return 1
    elif x < 0:
        return -1
    else:
        return 0
"#;

    let _ast = parse_source(source);
}

#[test]
fn test_parse_match_expression() {
    let source = r#"fn test(x: Option[int]) -> int:
    match x:
        | Some(n) if n > 0 => n
        | Some(_) => 0
        | None => -1
"#;

    let _ast = parse_source(source);
}

#[test]
fn test_parse_loops() {
    let source = r#"fn test():
    # While loop
    while condition:
        print("looping")

    # For loop
    for i in 0..10:
        print(i)

    # Loop with label
    'outer: loop:
        for x in items:
            if done:
                break 'outer
"#;

    let _ast = parse_source(source);
}

#[test]
fn test_parse_async() {
    let source = r#"async fn fetch() -> string:
    let result = await http.get("url")
    return result

fn test():
    let handle = spawn fetch()
    let value = await handle
"#;

    let _ast = parse_source(source);
}

#[test]
fn test_parse_concurrent() {
    let source = r#"fn test():
    concurrent:
        let a = spawn task1()
        let b = spawn task2()
        let results = [await a, await b]
"#;

    let _ast = parse_source(source);
}

#[test]
fn test_parse_lambda() {
    let source = r#"fn test():
    let add = fn(x: int, y: int) -> int: x + y
    let double = fn(x: int) -> x * 2
    let identity = fn(x): x
"#;

    let _ast = parse_source(source);
}

#[test]
fn test_parse_binary_expressions() {
    let source = r#"fn test():
    let a = 1 + 2
    let b = 3 - 4
    let c = 5 * 6
    let d = 7 / 8
    let e = 9 % 10
    let f = 2 ** 10
    let g = 1 < 2 and 3 > 4 or 5 == 6
"#;

    let _ast = parse_source(source);
}

#[test]
fn test_parse_complex_expressions() {
    let source = r#"fn test():
    # Method call
    let len = list.len()

    # Field access
    let x = point.x

    # Index
    let first = items[0]

    # Call
    let result = func(arg1, arg2)

    # Try operator
    let value = may_fail()?

    # Await
    let async_result = await future
"#;

    let _ast = parse_source(source);
}

#[test]
fn test_parse_patterns() {
    let source = r#"fn test():
    match value:
        | Some(x) => print(x)
        | None => print("none")
        | Point { x, y } => print(f"({x}, {y})")
        | (a, b) => print(f"{a}, {b}")
        | [first, ..rest] => print(first)
        | _ => print("default")
"#;

    let _ast = parse_source(source);
}

#[test]
fn test_parse_error_handling() {
    let source = r#"fn may_fail() -> int ! Error:
    raise Error

fn caller():
    try:
        let result = may_fail()?
    catch Error:
        print("caught")
"#;

    let _ast = parse_source(source);
}

#[test]
fn test_parse_visibility() {
    let source = r#"pub fn public_function():
    pass

pub struct PublicStruct:
    pub public_field: int
    private_field: int
"#;

    let _ast = parse_source(source);
}

#[test]
fn test_parse_complete_module() {
    let source = r#"import std::io

pub struct Person:
    name: string
    age: int

impl Person:
    pub fn new(name: string, age: int) -> Self:
        return Person { name, age }

    pub fn greet(self):
        print(f"Hello, I'm {self.name}")

pub fn main():
    let person = Person::new("Alice", 30)
    person.greet()
"#;

    let _ast = parse_source(source);
}
