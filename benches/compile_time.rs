//! Compile time benchmarks
//!
//! Measures end-to-end compilation performance.

use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use std::fs;
use std::path::Path;
use std::process::Command;
use tempfile::TempDir;

/// Create a small program for compilation
fn create_small_program() -> String {
    r#"
fn main():
    print("Hello, World!")
"#
    .to_string()
}

/// Create a medium program for compilation
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
"#
    .to_string()
}

/// Create a large program for compilation
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

/// Create a program with complex types
fn create_complex_types_program() -> String {
    let mut result = String::new();

    // Generic structs
    for i in 0..20 {
        result.push_str(&format!(
            r#"struct Container{i}[T]:
    value: T
    count: int

impl[T] Container{i}[T]:
    fn new(value: T) -> Container{i}[T]:
        return Container{i} {{ value: value, count: 0 }}

    fn get(self) -> T:
        return self.value

    fn increment(mut self):
        self.count = self.count + 1

"#
        ));
    }

    // Nested generic types
    result.push_str(
        r#"
struct Pair[A, B]:
    first: A
    second: B

struct Triple[A, B, C]:
    first: A
    second: B
    third: C

enum Option[T]:
    | Some(T)
    | None

enum Result[T, E]:
    | Ok(T)
    | Err(E)

fn main():
    print("Complex types loaded")
"#,
    );

    result
}

/// Create a program with deep nesting
fn create_deep_nesting_program() -> String {
    let mut result = String::from("fn main():\n");

    // Deep if nesting
    for i in 0..20 {
        let indent = "    ".repeat(i + 1);
        result.push_str(&format!("{}if true:\n", indent));
        result.push_str(&format!("{}    let x_{} = {}\n", indent, i, i));
    }

    // Close all blocks
    for i in (0..20).rev() {
        let indent = "    ".repeat(i + 1);
        result.push_str(&format!("{}    print(x_{})\n", indent, i));
    }

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

    // Benchmark small program
    let small_source = create_small_program();
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("small.jet");
    fs::write(&source_file, &small_source).unwrap();

    group.throughput(Throughput::Bytes(small_source.len() as u64));
    group.bench_function("small_program", |b| {
        b.iter(|| {
            let output = Command::new(&jet_binary)
                .arg("build")
                .arg(black_box(&source_file))
                .arg("-o")
                .arg(temp_dir.path().join("small_output"))
                .output()
                .expect("Failed to execute jet build");

            let _ = black_box(output);
        });
    });

    // Benchmark medium program
    let medium_source = create_medium_program();
    let source_file = temp_dir.path().join("medium.jet");
    fs::write(&source_file, &medium_source).unwrap();

    group.throughput(Throughput::Bytes(medium_source.len() as u64));
    group.bench_function("medium_program", |b| {
        b.iter(|| {
            let output = Command::new(&jet_binary)
                .arg("build")
                .arg(black_box(&source_file))
                .arg("-o")
                .arg(temp_dir.path().join("medium_output"))
                .output()
                .expect("Failed to execute jet build");

            let _ = black_box(output);
        });
    });

    // Benchmark large program
    let large_source = create_large_program();
    let source_file = temp_dir.path().join("large.jet");
    fs::write(&source_file, &large_source).unwrap();

    group.throughput(Throughput::Bytes(large_source.len() as u64));
    group.bench_function("large_program", |b| {
        b.iter(|| {
            let output = Command::new(&jet_binary)
                .arg("build")
                .arg(black_box(&source_file))
                .arg("-o")
                .arg(temp_dir.path().join("large_output"))
                .output()
                .expect("Failed to execute jet build");

            let _ = black_box(output);
        });
    });

    // Benchmark complex types
    let complex_source = create_complex_types_program();
    let source_file = temp_dir.path().join("complex.jet");
    fs::write(&source_file, &complex_source).unwrap();

    group.throughput(Throughput::Bytes(complex_source.len() as u64));
    group.bench_function("complex_types", |b| {
        b.iter(|| {
            let output = Command::new(&jet_binary)
                .arg("build")
                .arg(black_box(&source_file))
                .arg("-o")
                .arg(temp_dir.path().join("complex_output"))
                .output()
                .expect("Failed to execute jet build");

            let _ = black_box(output);
        });
    });

    // Benchmark deep nesting
    let deep_source = create_deep_nesting_program();
    let source_file = temp_dir.path().join("deep.jet");
    fs::write(&source_file, &deep_source).unwrap();

    group.throughput(Throughput::Bytes(deep_source.len() as u64));
    group.bench_function("deep_nesting", |b| {
        b.iter(|| {
            let output = Command::new(&jet_binary)
                .arg("build")
                .arg(black_box(&source_file))
                .arg("-o")
                .arg(temp_dir.path().join("deep_output"))
                .output()
                .expect("Failed to execute jet build");

            let _ = black_box(output);
        });
    });

    // Benchmark parse-only for comparison
    group.bench_function("parse_only_small", |b| {
        let small_source = create_small_program();
        let source_file = temp_dir.path().join("parse_small.jet");
        fs::write(&source_file, &small_source).unwrap();

        b.iter(|| {
            let output = Command::new(&jet_binary)
                .arg("parse")
                .arg(black_box(&source_file))
                .output()
                .expect("Failed to execute jet parse");

            let _ = black_box(output);
        });
    });

    group.finish();
}

criterion_group!(benches, compile_time_benchmark);
criterion_main!(benches);
