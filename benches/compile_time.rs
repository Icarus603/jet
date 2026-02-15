//! Compile time benchmarks
//!
//! Measures end-to-end compilation performance.

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use std::fs;
use std::path::Path;
use std::process::Command;
use tempfile::TempDir;

fn create_small_program() -> String {
    r#"
fn main():
    print("Hello, World!")
"
    .to_string()
}

fn create_medium_program() -> String {
    r#"
fn factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)

fn fibonacci(n: int) -> int:
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

fn main():
    let fact_5 = factorial(5)
    let fib_10 = fibonacci(10)
    print(f"factorial(5) = {fact_5}")
    print(f"fibonacci(10) = {fib_10}")

    for i in 0..10:
        print(f"i = {i}")

    let result = if fact_5 > fib_10:
        "factorial is larger"
    else:
        "fibonacci is larger"
    print(result)
"
    .to_string()
}

fn create_large_program() -> String {
    let mut result = String::new();

    // Generate many functions
    for i in 0..100 {
        result.push_str(&format!(
            r#"fn function_{i}(x: int) -> int:
    let a = x * {i}
    let b = a + {i}
    let c = b - {i}
    return c

"#
        ));
    }

    // Generate many structs
    for i in 0..50 {
        result.push_str(&format!(
            r#"struct Point{i}:
    x: float
    y: float

impl Point{i}:
    fn new(x: float, y: float) -> Point{i}:
        return Point{i} {{ x: x, y: y }}

    fn magnitude(self) -> float:
        return sqrt(self.x * self.x + self.y * self.y)

"#
        ));
    }

    // Generate many enums
    for i in 0..50 {
        result.push_str(&format!(
            r#"enum Status{i}:
    | Pending
    | Running
    | Completed
    | Failed

impl Status{i}:
    fn is_active(self) -> bool:
        match self:
            | Pending | Running => true
            | _ => false

"#
        ));
    }

    result.push_str(
        r#"fn main():
    print("Large program loaded")
"#,
    );

    result
}

fn compile_time_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("compile_time");

    // Find jet CLI binary
    let jet_binary = Path::new("target/release/jet");
    let jet_binary = if jet_binary.exists() {
        jet_binary.to_path_buf()
    } else {
        Path::new("target/debug/jet").to_path_buf()
    };

    if !jet_binary.exists() {
        eprintln!("Warning: jet binary not found, skipping compile_time benchmarks");
        return;
    }

    let programs = [
        ("small", create_small_program()),
        ("medium", create_medium_program()),
        ("large", create_large_program()),
    ];

    for (name, source) in &programs {
        let temp_dir = TempDir::new().unwrap();
        let source_file = temp_dir.path().join("test.jet");
        fs::write(&source_file, source).unwrap();

        let source_bytes = source.len() as u64;
        group.throughput(Throughput::Bytes(source_bytes));

        group.bench_with_input(
            BenchmarkId::new("full_compile", name),
            &source_file,
            |b, source_file| {
                b.iter(|| {
                    let output = Command::new(&jet_binary)
                        .arg("build")
                        .arg(black_box(source_file))
                        .arg("-o")
                        .arg(temp_dir.path().join("output"))
                        .output()
                        .expect("Failed to execute jet build");

                    // We don't care about success/failure for timing
                    let _ = output;
                });
            },
        );

        group.bench_with_input(
            BenchmarkId::new("parse_only", name),
            &source_file,
            |b, source_file| {
                b.iter(|| {
                    let output = Command::new(&jet_binary)
                        .arg("parse")
                        .arg(black_box(source_file))
                        .output()
                        .expect("Failed to execute jet parse");

                    let _ = output;
                });
            },
        );
    }

    group.finish();
}

criterion_group!(benches, compile_time_benchmark);
criterion_main!(benches);
