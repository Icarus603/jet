//! Parser performance benchmarks
//!
//! Measures parsing speed for various AST constructs.

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use jet_lexer::Lexer;
use jet_parser::Parser;

/// Create a small AST source (~50 lines)
fn create_small_source() -> String {
    r#"
fn main():
    print("Hello, World!")

fn add(a: int, b: int) -> int:
    return a + b

fn factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)

struct Point:
    x: float
    y: float

impl Point:
    fn new(x: float, y: float) -> Point:
        return Point { x: x, y: y }

enum Result[T, E]:
    | Ok(T)
    | Err(E)

let PI = 3.14159
"#
    .to_string()
}

/// Create a large module source (~1000 functions)
fn create_large_module(n: usize) -> String {
    let mut result = String::new();

    for i in 0..n {
        result.push_str(&format!(
            r#"fn function_{i}(x: int) -> int:
    let a = x * {i}
    let b = a + {i}
    let c = b - {i}
    return c

"#
        ));
    }

    result
}

/// Create source with deep nested expressions
fn create_deep_nested_expression(depth: usize) -> String {
    let mut expr = "1".to_string();
    for _ in 0..depth {
        expr = format!("({} + {})", expr, expr);
    }
    format!("fn main():\n    let result = {}\n    return result\n", expr)
}

/// Create source with deep nested blocks
fn create_deep_nested_blocks(depth: usize) -> String {
    let mut result = String::from("fn main():\n");

    for i in 0..depth {
        let indent = "    ".repeat(i + 1);
        result.push_str(&format!("{}if true:\n", indent));
        result.push_str(&format!("{}    let x_{} = {}\n", indent, i, i));
    }

    // Close all blocks with returns
    for i in (0..depth).rev() {
        let indent = "    ".repeat(i + 1);
        result.push_str(&format!("{}    print(x_{})\n", indent, i));
    }

    result
}

/// Create source with many function definitions
fn create_many_functions(n: usize) -> String {
    let mut result = String::new();
    for i in 0..n {
        result.push_str(&format!(
            r#"fn func_{i}(x: int, y: float) -> Result[int, Error]:
    if x > 0:
        return Ok(x * 2)
    else:
        return Err(Error::Negative)

"#
        ));
    }
    result
}

/// Create source with many struct definitions
fn create_many_structs(n: usize) -> String {
    let mut result = String::new();
    for i in 0..n {
        result.push_str(&format!(
            r#"struct Point{i}:
    x: float
    y: float

impl Point{i}:
    fn new(x: float, y: float) -> Point{i}:
        return Point{i} {{ x: x, y: y }}

    fn distance(self, other: Point{i}) -> float:
        let dx = self.x - other.x
        let dy = self.y - other.y
        return sqrt(dx * dx + dy * dy)

"#
        ));
    }
    result
}

/// Create source with many enum definitions
fn create_many_enums(n: usize) -> String {
    let mut result = String::new();
    for i in 0..n {
        result.push_str(&format!(
            r#"enum Result{i}[T, E]:
    | Ok(T)
    | Err(E)

impl[T, E] Result{i}[T, E]:
    fn is_ok(self) -> bool:
        match self:
            | Ok(_) => true
            | Err(_) => false

    fn map[U](self, f: fn(T) -> U) -> Result{i}[U, E]:
        match self:
            | Ok(v) => Ok(f(v))
            | Err(e) => Err(e)

"#
        ));
    }
    result
}

/// Create source with complex expressions
fn create_complex_expressions(n: usize) -> String {
    let mut result = String::from("fn main():\n");
    for i in 0..n {
        result.push_str(&format!(
            "    let result_{i} = ((a + b) * (c - d)) / (e % f) + (g == h) and (i != j) or (k < l) and (m > n)\n"
        ));
    }
    result
}

fn parser_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser");

    // Benchmark small AST parsing
    let small_source = create_small_source();
    let small_tokens = Lexer::new(&small_source).tokenize();
    let token_count = small_tokens.len();
    group.throughput(Throughput::Elements(token_count as u64));
    group.bench_function("small_ast", |b| {
        b.iter(|| {
            let mut parser = Parser::new(black_box(small_tokens.clone()));
            let _result = parser.parse_module();
        });
    });

    // Benchmark large module parsing
    for size in [100, 500, 1000].iter() {
        let source = create_large_module(*size);
        let tokens = Lexer::new(&source).tokenize();
        let token_count = tokens.len();

        group.throughput(Throughput::Elements(token_count as u64));
        group.bench_with_input(
            BenchmarkId::new("large_module", size),
            &tokens,
            |b, tokens| {
                b.iter(|| {
                    let mut parser = Parser::new(black_box(tokens.clone()));
                    let _result = parser.parse_module();
                });
            },
        );
    }

    // Benchmark deep nested expressions
    for depth in [5, 10, 20, 50].iter() {
        let source = create_deep_nested_expression(*depth);
        let tokens = Lexer::new(&source).tokenize();
        let token_count = tokens.len();

        group.throughput(Throughput::Elements(token_count as u64));
        group.bench_with_input(
            BenchmarkId::new("deep_nested_expressions", depth),
            &tokens,
            |b, tokens| {
                b.iter(|| {
                    let mut parser = Parser::new(black_box(tokens.clone()));
                    let _result = parser.parse_module();
                });
            },
        );
    }

    // Benchmark deep nested blocks
    for depth in [5, 10, 20, 50].iter() {
        let source = create_deep_nested_blocks(*depth);
        let tokens = Lexer::new(&source).tokenize();
        let token_count = tokens.len();

        group.throughput(Throughput::Elements(token_count as u64));
        group.bench_with_input(
            BenchmarkId::new("deep_nested_blocks", depth),
            &tokens,
            |b, tokens| {
                b.iter(|| {
                    let mut parser = Parser::new(black_box(tokens.clone()));
                    let _result = parser.parse_module();
                });
            },
        );
    }

    // Benchmark function parsing
    for size in [10, 100, 500].iter() {
        let source = create_many_functions(*size);
        let tokens = Lexer::new(&source).tokenize();
        let token_count = tokens.len();

        group.throughput(Throughput::Elements(token_count as u64));
        group.bench_with_input(BenchmarkId::new("functions", size), &tokens, |b, tokens| {
            b.iter(|| {
                let mut parser = Parser::new(black_box(tokens.clone()));
                let _result = parser.parse_module();
            });
        });
    }

    // Benchmark struct parsing
    for size in [10, 100, 500].iter() {
        let source = create_many_structs(*size);
        let tokens = Lexer::new(&source).tokenize();
        let token_count = tokens.len();

        group.throughput(Throughput::Elements(token_count as u64));
        group.bench_with_input(BenchmarkId::new("structs", size), &tokens, |b, tokens| {
            b.iter(|| {
                let mut parser = Parser::new(black_box(tokens.clone()));
                let _result = parser.parse_module();
            });
        });
    }

    // Benchmark enum parsing
    for size in [10, 100, 500].iter() {
        let source = create_many_enums(*size);
        let tokens = Lexer::new(&source).tokenize();
        let token_count = tokens.len();

        group.throughput(Throughput::Elements(token_count as u64));
        group.bench_with_input(BenchmarkId::new("enums", size), &tokens, |b, tokens| {
            b.iter(|| {
                let mut parser = Parser::new(black_box(tokens.clone()));
                let _result = parser.parse_module();
            });
        });
    }

    // Benchmark complex expression parsing
    for size in [10, 100, 1000].iter() {
        let source = create_complex_expressions(*size);
        let tokens = Lexer::new(&source).tokenize();
        let token_count = tokens.len();

        group.throughput(Throughput::Elements(token_count as u64));
        group.bench_with_input(
            BenchmarkId::new("complex_expressions", size),
            &tokens,
            |b, tokens| {
                b.iter(|| {
                    let mut parser = Parser::new(black_box(tokens.clone()));
                    let _result = parser.parse_module();
                });
            },
        );
    }

    group.finish();
}

criterion_group!(benches, parser_benchmark);
criterion_main!(benches);
