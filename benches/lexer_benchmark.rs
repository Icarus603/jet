//! Lexer performance benchmarks
//!
//! Measures tokenization speed for various input patterns.

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};

/// Create a small source file (~100 lines)
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

    fn distance(self, other: Point) -> float:
        let dx = self.x - other.x
        let dy = self.y - other.y
        return sqrt(dx * dx + dy * dy)

enum Result[T, E]:
    | Ok(T)
    | Err(E)

let PI = 3.14159
let MAX_SIZE = 1000

fn calculate(x: float) -> float:
    let result = x * PI + 2.0
    return result / 2.0

for i in 0..10:
    print(f"Iteration {i}")

while x > 0:
    x = x - 1

match value:
    | Some(v) => print(v)
    | None => print("nothing")

# This is a comment
## Another comment
let array = [1, 2, 3, 4, 5]
let string = "test string with escape \n\t"
"#
    .to_string()
}

/// Create a large source file (~10,000 lines)
fn create_large_source(n: usize) -> String {
    let mut result = String::new();

    for i in 0..n {
        result.push_str(&format!(
            r#"fn function_{i}(x: int, y: float) -> Result[int, Error]:
    if x > 0:
        let msg = "positive: {{x}}"
        print(msg)
        return Ok(x * 2)
    elif x < 0:
        return Err(Error::Negative)
    else:
        # Handle zero case
        match y:
            | 0.0 => return Ok(0)
            | _ => return Err(Error::NonZeroY)

"#
        ));
    }

    result
}

/// Create source with complex indentation patterns
fn create_indentation_source(depth: usize) -> String {
    let mut result = String::from("fn main():\n");

    for i in 0..depth {
        let indent = "    ".repeat(i + 1);
        result.push_str(&format!("{}if true:\n", indent));
        result.push_str(&format!("{}    let x_{} = {}\n", indent, i, i));
    }

    // Close all blocks
    for i in (0..depth).rev() {
        let indent = "    ".repeat(i + 1);
        result.push_str(&format!("{}print(x_{})\n", indent, i));
    }

    result
}

/// Create source with many string literals
fn create_string_heavy_source(n: usize) -> String {
    let mut result = String::new();
    for i in 0..n {
        result.push_str(&format!(
            r#"let s_{i} = "This is a test string with some content {i} and escape \n\t\r"
"#
        ));
    }
    result
}

/// Create source with many numeric literals
fn create_number_heavy_source(n: usize) -> String {
    let mut result = String::new();
    for i in 0..n {
        result.push_str(&format!(
            "let x_{i} = {i} + {i}.5 * 0x{i:X} + 0b{i:b} + 0o{i:o}\n"
        ));
    }
    result
}

/// Create source with many comments
fn create_comment_heavy_source(n: usize) -> String {
    let mut result = String::new();
    for i in 0..n {
        result.push_str(&format!(
            r#"# This is a comment explaining what line {i} does
# Another comment with more details
fn func_{i}():  # inline comment
    ###
    Multi-line comment block
    with lots of text explaining the function
    in great detail for documentation purposes
    ###
    return {i}

"#
        ));
    }
    result
}

fn lexer_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("lexer");

    // Benchmark small file tokenization (~100 lines)
    let small_source = create_small_source();
    group.throughput(Throughput::Bytes(small_source.len() as u64));
    group.bench_function("small_file_100_lines", |b| {
        b.iter(|| {
            let mut lexer = jet_lexer::Lexer::new(black_box(&small_source));
            let _tokens = lexer.tokenize();
        });
    });

    // Benchmark large file tokenization (~10,000 lines)
    let large_source = create_large_source(1000);
    group.throughput(Throughput::Bytes(large_source.len() as u64));
    group.bench_function("large_file_10000_lines", |b| {
        b.iter(|| {
            let mut lexer = jet_lexer::Lexer::new(black_box(&large_source));
            let _tokens = lexer.tokenize();
        });
    });

    // Benchmark complex indentation
    for depth in [10, 20, 50, 100].iter() {
        let source = create_indentation_source(*depth);
        group.throughput(Throughput::Bytes(source.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("complex_indentation", depth),
            &source,
            |b, source| {
                b.iter(|| {
                    let mut lexer = jet_lexer::Lexer::new(black_box(source));
                    let _tokens = lexer.tokenize();
                });
            },
        );
    }

    // Benchmark string-heavy tokenization
    for size in [100, 1000, 5000].iter() {
        let source = create_string_heavy_source(*size);
        group.throughput(Throughput::Bytes(source.len() as u64));
        group.bench_with_input(BenchmarkId::new("strings", size), &source, |b, source| {
            b.iter(|| {
                let mut lexer = jet_lexer::Lexer::new(black_box(source));
                let _tokens = lexer.tokenize();
            });
        });
    }

    // Benchmark number-heavy tokenization
    for size in [100, 1000, 5000].iter() {
        let source = create_number_heavy_source(*size);
        group.throughput(Throughput::Bytes(source.len() as u64));
        group.bench_with_input(BenchmarkId::new("numbers", size), &source, |b, source| {
            b.iter(|| {
                let mut lexer = jet_lexer::Lexer::new(black_box(source));
                let _tokens = lexer.tokenize();
            });
        });
    }

    // Benchmark comment-heavy tokenization
    for size in [10, 100, 1000].iter() {
        let source = create_comment_heavy_source(*size);
        group.throughput(Throughput::Bytes(source.len() as u64));
        group.bench_with_input(BenchmarkId::new("comments", size), &source, |b, source| {
            b.iter(|| {
                let mut lexer = jet_lexer::Lexer::new(black_box(source));
                let _tokens = lexer.tokenize();
            });
        });
    }

    group.finish();
}

fn lexer_individual_tokens(c: &mut Criterion) {
    let mut group = c.benchmark_group("lexer/tokens");

    // Benchmark individual token types
    let test_cases = [
        (
            "keyword",
            "fn let mut if else match return type struct enum impl",
        ),
        (
            "identifiers",
            "foo bar baz hello_world camelCase snake_case",
        ),
        ("integers", "0 42 123 999999 1000000 0xFF 0b1010 0o777"),
        ("floats", "0.0 3.14 1.5e10 2.5e-5 1e100"),
        (
            "strings",
            r#""hello" "world" "test string" "with \n escapes""#,
        ),
        (
            "operators",
            "+ - * / % == != < > <= >= = && || ! & | ^ << >>",
        ),
        ("punctuation", "() [] {} : , . -> => :: ; | _"),
    ];

    for (name, source) in &test_cases {
        let source = source.repeat(100);
        group.bench_function(*name, |b| {
            b.iter(|| {
                let mut lexer = jet_lexer::Lexer::new(black_box(&source));
                let _tokens = lexer.tokenize();
            });
        });
    }

    group.finish();
}

criterion_group!(benches, lexer_benchmark, lexer_individual_tokens);
criterion_main!(benches);
