//! Lexer performance benchmarks
//!
//! Measures tokenization speed for various input patterns.

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};

fn create_simple_source(n: usize) -> String {
    "fn main(): print(\"hello\")\n"
        .repeat(n)
        .collect::<String>()
}

fn create_complex_source(n: usize) -> String {
    let mut result = String::new();
    for i in 0..n {
        result.push_str(&format!(
            r#"fn function_{i}(x: int, y: float) -> Result[int, Error] ! Effect:
    if x > 0:
        let msg = \"positive: {{x}}\"
        return Ok(x * 2)
    elif x < 0:
        return Err(Error::Negative)
    else:
        # Handle zero case
        match y:
            | 0.0 => return Ok(0)
            | _ => return Err(Error::NonZeroY)

enum Result[T, E]:
    | Ok(T)
    | Err(E)

struct Point:
    x: float
    y: float

impl Point:
    fn new(x: float, y: float) -> Point:
        return Point {{ x: x, y: y }}
"#
        ));
    }
    result
}

fn create_string_heavy_source(n: usize) -> String {
    let mut result = String::new();
    for i in 0..n {
        result.push_str(&format!(
            r#"let s_{i} = \"This is a test string with some content {}\""#,
            i
        ));
        result.push('\n');
    }
    result
}

fn create_number_heavy_source(n: usize) -> String {
    let mut result = String::new();
    for i in 0..n {
        result.push_str(&format!(
            "let x_{i} = {i} + {i}.5 * 0x{i:X} + 0b{i:b} + 0o{i:o}\n"
        ));
    }
    result
}

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
    ###
    return {i}
"#
        ));
    }
    result
}

fn lexer_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("lexer");

    // Benchmark simple tokenization
    for size in [100, 1000, 10000].iter() {
        let source = create_simple_source(*size);
        group.throughput(Throughput::Bytes(source.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("simple", size),
            &source,
            |b, source| {
                b.iter(|| {
                    let lexer = jet_lexer::Lexer::new(black_box(source));
                    let _tokens = lexer.tokenize();
                });
            },
        );
    }

    // Benchmark complex tokenization
    for size in [1, 10, 100].iter() {
        let source = create_complex_source(*size);
        group.throughput(Throughput::Bytes(source.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("complex", size),
            &source,
            |b, source| {
                b.iter(|| {
                    let lexer = jet_lexer::Lexer::new(black_box(source));
                    let _tokens = lexer.tokenize();
                });
            },
        );
    }

    // Benchmark string-heavy tokenization
    for size in [100, 1000, 5000].iter() {
        let source = create_string_heavy_source(*size);
        group.throughput(Throughput::Bytes(source.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("strings", size),
            &source,
            |b, source| {
                b.iter(|| {
                    let lexer = jet_lexer::Lexer::new(black_box(source));
                    let _tokens = lexer.tokenize();
                });
            },
        );
    }

    // Benchmark number-heavy tokenization
    for size in [100, 1000, 5000].iter() {
        let source = create_number_heavy_source(*size);
        group.throughput(Throughput::Bytes(source.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("numbers", size),
            &source,
            |b, source| {
                b.iter(|| {
                    let lexer = jet_lexer::Lexer::new(black_box(source));
                    let _tokens = lexer.tokenize();
                });
            },
        );
    }

    // Benchmark comment-heavy tokenization
    for size in [10, 100, 1000].iter() {
        let source = create_comment_heavy_source(*size);
        group.throughput(Throughput::Bytes(source.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("comments", size),
            &source,
            |b, source| {
                b.iter(|| {
                    let lexer = jet_lexer::Lexer::new(black_box(source));
                    let _tokens = lexer.tokenize();
                });
            },
        );
    }

    group.finish();
}

fn lexer_individual_tokens(c: &mut Criterion) {
    let mut group = c.benchmark_group("lexer/tokens");

    // Benchmark individual token types
    let test_cases = [
        ("keyword", "fn let mut if else match return"),
        ("identifiers", "foo bar baz hello_world camelCase"),
        ("integers", "0 42 123 999999 1000000"),
        ("floats", "0.0 3.14 1.5e10 2.5e-5"),
        ("strings", r#""hello" "world" "test string""#),
        ("operators", "+ - * / % == != < > <= >= ="),
        ("punctuation", "() [] {} : , . -> =>"),
    ];

    for (name, source) in &test_cases {
        let source = source.repeat(100);
        group.bench_function(*name, |b| {
            b.iter(|| {
                let lexer = jet_lexer::Lexer::new(black_box(&source));
                let _tokens = lexer.tokenize();
            });
        });
    }

    group.finish();
}

criterion_group!(benches, lexer_benchmark, lexer_individual_tokens);
criterion_main!(benches);
