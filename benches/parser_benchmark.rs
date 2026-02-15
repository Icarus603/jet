//! Parser performance benchmarks
//!
//! Measures parsing speed for various AST constructs.

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use jet_lexer::Lexer;
use jet_parser::Parser;

fn create_function_source(n: usize) -> String {
    let mut result = String::new();
    for i in 0..n {
        result.push_str(&format!(
            r#"fn func_{i}(x: int) -> int:
    if x > 0:
        return x * 2
    else:
        return x

"#
        ));
    }
    result
}

fn create_struct_source(n: usize) -> String {
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

fn create_enum_source(n: usize) -> String {
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

fn create_complex_expression_source(n: usize) -> String {
    let mut result = String::new();
    for i in 0..n {
        result.push_str(&format!(
            r#"let result_{i} = ((a + b) * (c - d)) / (e % f) + (g == h) and (i != j) or (k < l) and (m > n)
"
        ));
    }
    result
}

fn create_nested_block_source(depth: usize) -> String {
    let mut result = String::from("fn main():\n");
    for i in 0..depth {
        result.push_str(&"    ".repeat(i + 1));
        result.push_str("if true:\n");
    }
    result.push_str(&"    ".repeat(depth + 1));
    result.push_str("print(42)\n");
    result
}

fn parser_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser");

    // Benchmark function parsing
    for size in [10, 100, 1000].iter() {
        let source = create_function_source(*size);
        let tokens = Lexer::new(&source).tokenize();
        let token_count = tokens.len();\n        group.throughput(Throughput::Elements(token_count as u64));
        group.bench_with_input(
            BenchmarkId::new("functions", size),
            &tokens,
            |b, tokens| {
                b.iter(|| {
                    let mut parser = Parser::new(black_box(tokens.clone()));
                    let _result = parser.parse_module();
                });
            },
        );
    }

    // Benchmark struct parsing
    for size in [10, 100, 500].iter() {
        let source = create_struct_source(*size);
        let tokens = Lexer::new(&source).tokenize();
        let token_count = tokens.len();

        group.throughput(Throughput::Elements(token_count as u64));
        group.bench_with_input(
            BenchmarkId::new("structs", size),
            &tokens,
            |b, tokens| {
                b.iter(|| {
                    let mut parser = Parser::new(black_box(tokens.clone()));
                    let _result = parser.parse_module();
                });
            },
        );
    }

    // Benchmark enum parsing
    for size in [10, 100, 500].iter() {
        let source = create_enum_source(*size);
        let tokens = Lexer::new(&source).tokenize();
        let token_count = tokens.len();

        group.throughput(Throughput::Elements(token_count as u64));
        group.bench_with_input(
            BenchmarkId::new("enums", size),
            &tokens,
            |b, tokens| {
                b.iter(|| {
                    let mut parser = Parser::new(black_box(tokens.clone()));
                    let _result = parser.parse_module();
                });
            },
        );
    }

    // Benchmark complex expression parsing
    for size in [10, 100, 1000].iter() {
        let source = create_complex_expression_source(*size);
        let tokens = Lexer::new(&source).tokenize();
        let token_count = tokens.len();

        group.throughput(Throughput::Elements(token_count as u64));
        group.bench_with_input(
            BenchmarkId::new("expressions", size),
            &tokens,
            |b, tokens| {
                b.iter(|| {
                    let mut parser = Parser::new(black_box(tokens.clone()));
                    let _result = parser.parse_module();
                });
            },
        );
    }

    // Benchmark nested block parsing
    for depth in [5, 10, 20, 50].iter() {
        let source = create_nested_block_source(*depth);
        let tokens = Lexer::new(&source).tokenize();
        let token_count = tokens.len();

        group.throughput(Throughput::Elements(token_count as u64));
        group.bench_with_input(
            BenchmarkId::new("nested_blocks", depth),
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
