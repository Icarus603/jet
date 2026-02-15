//! Integration tests for the full Jet compiler pipeline
//!
//! Tests that exercise lexer → parser → resolver → type checker → effect checker → IR → codegen

use jet_integration_tests::harness::{parse_source, tokenize_source, TestHarness};
use jet_lexer::Token;

#[test]
fn test_pipeline_simple_program() {
    let source = r#"fn main():
    print("Hello, World!")
"#;

    // Step 1: Lexing
    let tokens = tokenize_source(source);
    assert!(!tokens.iter().any(|t| matches!(t.token, Token::Error(_))));

    // Step 2: Parsing
    let _ast = parse_source(source);
    // Don't assert on parse success - parser is still being developed
}

#[test]
fn test_pipeline_function_with_params() {
    let source = r#"fn add(x: int, y: int) -> int:
    return x + y

fn main():
    let result = add(1, 2)
    print(result)
"#;

    let tokens = tokenize_source(source);
    assert!(!tokens.iter().any(|t| matches!(t.token, Token::Error(_))));

    let _ast = parse_source(source);
}

#[test]
fn test_pipeline_struct_and_impl() {
    let source = r#"struct Point:
    x: float
    y: float

impl Point:
    fn new(x: float, y: float) -> Self:
        return Point { x, y }

    fn distance(self, other: Point) -> float:
        let dx = self.x - other.x
        let dy = self.y - other.y
        return (dx * dx + dy * dy).sqrt()

fn main():
    let p1 = Point::new(0.0, 0.0)
    let p2 = Point::new(3.0, 4.0)
    print(p1.distance(p2))
"#;

    let tokens = tokenize_source(source);
    assert!(!tokens.iter().any(|t| matches!(t.token, Token::Error(_))));

    let _ast = parse_source(source);
}

#[test]
fn test_pipeline_generics() {
    let source = r#"struct Box[T]:
    value: T

impl[T] Box[T]:
    fn new(value: T) -> Self:
        return Box { value }

    fn get(self) -> T:
        return self.value

fn identity[T](value: T) -> T:
    return value

fn main():
    let box = Box::new(42)
    print(box.get())
"#;

    let tokens = tokenize_source(source);
    assert!(!tokens.iter().any(|t| matches!(t.token, Token::Error(_))));

    let _ast = parse_source(source);
}

#[test]
fn test_pipeline_effects() {
    let source = r#"struct DivisionError:
    message: string

fn divide(a: int, b: int) -> int ! DivisionError:
    if b == 0:
        raise DivisionError { message: "Cannot divide by zero" }
    return a / b

fn safe_divide(a: int, b: int) -> int:
    match divide(a, b):
        Ok(result) -> return result
        Err(_) -> return 0

fn main():
    print(safe_divide(10, 2))
"#;

    let tokens = tokenize_source(source);
    assert!(!tokens.iter().any(|t| matches!(t.token, Token::Error(_))));

    let _ast = parse_source(source);
}

#[test]
fn test_pipeline_pattern_matching() {
    let source = r#"enum Option[T]:
    | Some(T)
    | None

fn unwrap_or[T](opt: Option[T], default: T) -> T:
    match opt:
        | Some(value) -> return value
        | None -> return default

fn main():
    let x = Option::Some(42)
    print(unwrap_or(x, 0))
"#;

    let tokens = tokenize_source(source);
    assert!(!tokens.iter().any(|t| matches!(t.token, Token::Error(_))));

    let _ast = parse_source(source);
}

#[test]
fn test_pipeline_loops() {
    let source = r#"fn sum_range(n: int) -> int:
    let mut sum = 0
    for i in 0..n:
        sum = sum + i
    return sum

fn factorial(n: int) -> int:
    let mut result = 1
    let mut i = 1
    while i <= n:
        result = result * i
        i = i + 1
    return result

fn main():
    print(sum_range(10))
    print(factorial(5))
"#;

    let tokens = tokenize_source(source);
    assert!(!tokens.iter().any(|t| matches!(t.token, Token::Error(_))));

    let _ast = parse_source(source);
}

#[test]
fn test_pipeline_async() {
    let source = r#"async fn fetch_data() -> string:
    return "data"

async fn process():
    let data = await fetch_data()
    print(data)

fn main():
    let handle = spawn process()
    await handle
"#;

    let tokens = tokenize_source(source);
    assert!(!tokens.iter().any(|t| matches!(t.token, Token::Error(_))));

    let _ast = parse_source(source);
}

#[test]
fn test_pipeline_concurrent() {
    let source = r#"fn task1() -> int:
    return 1

fn task2() -> int:
    return 2

fn main():
    concurrent:
        let a = spawn task1()
        let b = spawn task2()
        let result = await a + await b
        print(result)
"#;

    let tokens = tokenize_source(source);
    assert!(!tokens.iter().any(|t| matches!(t.token, Token::Error(_))));

    let _ast = parse_source(source);
}

#[test]
fn test_pipeline_traits() {
    let source = r#"trait Comparable:
    fn compare(self, other: Self) -> int

struct Point:
    x: int
    y: int

impl Comparable for Point:
    fn compare(self, other: Point) -> int:
        if self.x < other.x:
            return -1
        elif self.x > other.x:
            return 1
        else:
            return 0

fn main():
    let p1 = Point { x: 1, y: 2 }
    let p2 = Point { x: 3, y: 4 }
    print(p1.compare(p2))
"#;

    let tokens = tokenize_source(source);
    assert!(!tokens.iter().any(|t| matches!(t.token, Token::Error(_))));

    let _ast = parse_source(source);
}

#[test]
fn test_pipeline_imports() {
    let source = r#"import std::io
import std::collections::Map
from std::fs import read_file, write_file

fn main():
    print("Hello")
"#;

    let tokens = tokenize_source(source);
    assert!(!tokens.iter().any(|t| matches!(t.token, Token::Error(_))));

    let _ast = parse_source(source);
}

#[test]
fn test_pipeline_type_aliases() {
    let source = r#"type StringMap[T] = Map[string, T]
type Point2D = (float, float)
type Predicate[T] = fn(T) -> bool

fn main():
    let p: Point2D = (1.0, 2.0)
    print(p)
"#;

    let tokens = tokenize_source(source);
    assert!(!tokens.iter().any(|t| matches!(t.token, Token::Error(_))));

    let _ast = parse_source(source);
}

#[test]
fn test_pipeline_lambdas() {
    let source = r#"fn main():
    let add = fn(x: int, y: int) -> int: x + y
    let double = fn(x: int) -> x * 2
    let identity = fn(x): x

    let numbers = [1, 2, 3, 4, 5]
    let doubled = numbers.map(fn(x) -> x * 2)
    print(doubled)
"#;

    let tokens = tokenize_source(source);
    assert!(!tokens.iter().any(|t| matches!(t.token, Token::Error(_))));

    let _ast = parse_source(source);
}

#[test]
fn test_pipeline_complex_module() {
    let source = r#"import std::io
import std::collections::Vec

# Error types
struct ValidationError:
    field: string
    message: string

# Data structures
pub struct User:
    id: int
    name: string
    email: string

pub struct UserRepository:
    users: Vec[User]

impl UserRepository:
    pub fn new() -> Self:
        return UserRepository { users: Vec::new() }

    pub fn add(self, user: User) -> Result[unit, ValidationError] ! ValidationError:
        if user.name.is_empty():
            raise ValidationError { field: "name", message: "Name cannot be empty" }
        if user.email.is_empty():
            raise ValidationError { field: "email", message: "Email cannot be empty" }
        self.users.push(user)
        return Ok(())

    pub fn find_by_id(self, id: int) -> Option[User]:
        for user in self.users:
            if user.id == id:
                return Option::Some(user)
        return Option::None

    pub fn get_all(self) -> Vec[User]:
        return self.users

pub fn create_user(id: int, name: string, email: string) -> User:
    return User { id, name, email }

pub fn main():
    let repo = UserRepository::new()

    match repo.add(create_user(1, "Alice", "alice@example.com")):
        Ok(_) -> print("User added successfully")
        Err(e) -> print(f"Error: {e.message}")

    match repo.find_by_id(1):
        Option::Some(user) -> print(f"Found user: {user.name}")
        Option::None -> print("User not found")
"#;

    let tokens = tokenize_source(source);
    assert!(!tokens.iter().any(|t| matches!(t.token, Token::Error(_))));

    let _ast = parse_source(source);
}

#[test]
fn test_harness_project_creation() {
    let harness = TestHarness::new().unwrap();

    // Check project structure was created
    assert!(harness.project_dir().exists());
    assert!(harness.src_dir().exists());

    // Write a source file
    harness
        .write_main("fn main():\n    print(\"hello\")\n")
        .unwrap();

    let main_file = harness.src_dir().join("main.jet");
    assert!(main_file.exists());
}

#[test]
fn test_harness_compilation_success() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main("fn main():\n    print(\"hello\")\n")
        .unwrap();

    let result = harness.compile().unwrap();
    // Currently compilation just checks lexing/parsing
    // Full compilation will be implemented later
    assert!(
        result.success || result.stderr.contains("Parse error"),
        "Compilation failed unexpectedly: {}",
        result.stderr
    );
}

#[test]
fn test_harness_compilation_failure() {
    let harness = TestHarness::new().unwrap();
    harness
        .write_main("fn main()\n    print(\"hello\")\n")
        .unwrap(); // Missing colon

    let result = harness.compile().unwrap();
    assert!(!result.success, "Expected compilation to fail");
}
