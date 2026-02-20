//! Runtime performance benchmarks
//!
//! Measures performance of runtime components including:
//! - Function call overhead (when codegen is ready)
//! - Memory allocation performance
//! - Scheduler operations
//! - Synchronization primitives

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use jet_rt_gc::{ImmixHeap, TypeId};

/// Benchmark memory allocation patterns that would occur at runtime
fn memory_allocation_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("runtime/memory");

    // Benchmark allocation patterns similar to function calls
    for depth in [10, 100, 1000].iter() {
        group.bench_with_input(
            BenchmarkId::new("stack_like_allocation", depth),
            depth,
            |b, &depth| {
                b.iter(|| {
                    let mut heap = ImmixHeap::new();

                    // Simulate stack frames being allocated
                    for i in 0..depth {
                        let frame = heap.allocate_object(256, 8, TypeId::new(i as u32));
                        black_box(frame);
                    }

                    black_box(heap);
                });
            },
        );
    }

    // Benchmark heap allocation patterns
    for count in [100, 1000, 10000].iter() {
        group.throughput(Throughput::Elements(*count as u64));
        group.bench_with_input(
            BenchmarkId::new("heap_allocation", count),
            count,
            |b, &count| {
                b.iter(|| {
                    let mut heap = ImmixHeap::new();
                    let mut objects: Vec<*mut u8> = Vec::with_capacity(count);

                    for i in 0..count {
                        let ptr = heap.allocate_object(64, 8, TypeId::new(i as u32));
                        objects.push(ptr);
                    }

                    black_box(objects);
                    black_box(heap);
                });
            },
        );
    }

    // Benchmark object field access patterns
    group.bench_function("field_access_pattern", |b| {
        let mut heap = ImmixHeap::new();
        let ptr = heap.allocate_object(64, 8, TypeId::new(1));

        b.iter(|| {
            // Simulate reading/writing object fields
            black_box(ptr);
            for i in 0..100 {
                black_box(i);
            }
        });
    });

    group.finish();
}

/// Benchmark loop iteration patterns
fn loop_iteration_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("runtime/loops");

    // Simple counter loop
    for count in [100, 1000, 10000, 100000].iter() {
        group.throughput(Throughput::Elements(*count as u64));
        group.bench_with_input(
            BenchmarkId::new("counter_loop", count),
            count,
            |b, &count| {
                b.iter(|| {
                    let mut sum = 0i64;
                    for i in 0..count {
                        sum += i as i64;
                    }
                    black_box(sum);
                });
            },
        );
    }

    // Nested loop pattern
    for size in [10, 50, 100].iter() {
        let total = size * size;
        group.throughput(Throughput::Elements(total as u64));
        group.bench_with_input(BenchmarkId::new("nested_loop", size), size, |b, &size| {
            b.iter(|| {
                let mut sum = 0i64;
                for i in 0..size {
                    for j in 0..size {
                        sum += (i * j) as i64;
                    }
                }
                black_box(sum);
            });
        });
    }

    // While loop pattern
    for count in [100, 1000, 10000].iter() {
        group.throughput(Throughput::Elements(*count as u64));
        group.bench_with_input(BenchmarkId::new("while_loop", count), count, |b, &count| {
            b.iter(|| {
                let mut i = 0i64;
                let mut sum = 0i64;
                while i < count as i64 {
                    sum += i;
                    i += 1;
                }
                black_box(sum);
            });
        });
    }

    group.finish();
}

/// Benchmark recursive function patterns (simulated)
fn recursion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("runtime/recursion");

    // Fibonacci computation (recursive)
    fn fibonacci(n: i64) -> i64 {
        if n <= 1 {
            return n;
        }
        fibonacci(n - 1) + fibonacci(n - 2)
    }

    for n in [10, 15, 20, 25].iter() {
        let expected_calls = fibonacci_call_count(*n);
        group.throughput(Throughput::Elements(expected_calls));
        group.bench_with_input(BenchmarkId::new("fibonacci", n), n, |b, &n| {
            b.iter(|| {
                let result = fibonacci(black_box(n));
                black_box(result);
            });
        });
    }

    // Factorial computation (recursive)
    fn factorial(n: i64) -> i64 {
        if n <= 1 {
            return 1;
        }
        n * factorial(n - 1)
    }

    for n in [10, 100, 1000].iter() {
        group.throughput(Throughput::Elements(*n as u64));
        group.bench_with_input(BenchmarkId::new("factorial", n), n, |b, &n| {
            b.iter(|| {
                let result = factorial(black_box(n));
                black_box(result);
            });
        });
    }

    // Tail-recursive pattern (accumulator style)
    fn factorial_tail(n: i64, acc: i64) -> i64 {
        if n <= 1 {
            return acc;
        }
        factorial_tail(n - 1, n * acc)
    }

    for n in [10, 100, 1000, 10000].iter() {
        group.throughput(Throughput::Elements(*n as u64));
        group.bench_with_input(BenchmarkId::new("factorial_tail", n), n, |b, &n| {
            b.iter(|| {
                let result = factorial_tail(black_box(n), 1);
                black_box(result);
            });
        });
    }

    group.finish();
}

/// Helper function to estimate fibonacci call count
fn fibonacci_call_count(n: i64) -> u64 {
    if n <= 1 {
        return 1;
    }
    // Approximate: fib(n+2) - 1 calls
    let mut a = 1u64;
    let mut b = 1u64;
    for _ in 0..n as usize + 2 {
        let c = a + b;
        a = b;
        b = c;
    }
    b - 1
}

/// Benchmark function call overhead patterns
fn function_call_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("runtime/function_calls");

    // Direct function call
    fn add(a: i64, b: i64) -> i64 {
        a + b
    }

    group.bench_function("direct_call", |b| {
        b.iter(|| {
            let mut sum = 0i64;
            for i in 0..1000 {
                sum += add(black_box(i), black_box(i + 1));
            }
            black_box(sum);
        });
    });

    // Closure call
    group.bench_function("closure_call", |b| {
        let add_closure = |a: i64, b: i64| a + b;
        b.iter(|| {
            let mut sum = 0i64;
            for i in 0..1000 {
                sum += add_closure(black_box(i), black_box(i + 1));
            }
            black_box(sum);
        });
    });

    // Dynamic dispatch (trait object)
    trait BinaryOp {
        fn apply(&self, a: i64, b: i64) -> i64;
    }

    struct Add;
    impl BinaryOp for Add {
        fn apply(&self, a: i64, b: i64) -> i64 {
            a + b
        }
    }

    group.bench_function("dynamic_dispatch", |b| {
        let op: &dyn BinaryOp = &Add;
        b.iter(|| {
            let mut sum = 0i64;
            for i in 0..1000 {
                sum += op.apply(black_box(i), black_box(i + 1));
            }
            black_box(sum);
        });
    });

    // Function pointer call
    group.bench_function("function_pointer", |b| {
        let f: fn(i64, i64) -> i64 = add;
        b.iter(|| {
            let mut sum = 0i64;
            for i in 0..1000 {
                sum += f(black_box(i), black_box(i + 1));
            }
            black_box(sum);
        });
    });

    group.finish();
}

/// Benchmark arithmetic operations
fn arithmetic_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("runtime/arithmetic");

    // Integer operations
    group.bench_function("integer_add", |b| {
        b.iter(|| {
            let mut sum = 0i64;
            for i in 0..10000 {
                sum = sum.wrapping_add(black_box(i as i64));
            }
            black_box(sum);
        });
    });

    group.bench_function("integer_mul", |b| {
        b.iter(|| {
            let mut prod = 1i64;
            for i in 1..10000 {
                prod = prod.wrapping_mul(black_box(i as i64));
            }
            black_box(prod);
        });
    });

    group.bench_function("integer_div", |b| {
        b.iter(|| {
            let mut sum = 0i64;
            for i in 1..10000 {
                sum = sum.wrapping_add(black_box(1000000i64) / black_box(i as i64));
            }
            black_box(sum);
        });
    });

    // Floating point operations
    group.bench_function("float_add", |b| {
        b.iter(|| {
            let mut sum = 0.0f64;
            for i in 0..10000 {
                sum += black_box(i as f64);
            }
            black_box(sum);
        });
    });

    group.bench_function("float_mul", |b| {
        b.iter(|| {
            let mut prod = 1.0f64;
            for _i in 1..1000 {
                prod *= black_box(1.001f64);
            }
            black_box(prod);
        });
    });

    group.bench_function("float_div", |b| {
        b.iter(|| {
            let mut sum = 0.0f64;
            for i in 1..10000 {
                sum += black_box(1000000.0f64) / black_box(i as f64);
            }
            black_box(sum);
        });
    });

    group.finish();
}

/// Benchmark array/vector operations
fn array_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("runtime/arrays");

    // Array sum
    for size in [100, 1000, 10000, 100000].iter() {
        let data: Vec<i64> = (0..*size).map(|i| i as i64).collect();
        group.throughput(Throughput::Elements(*size as u64));
        group.bench_with_input(BenchmarkId::new("sum", size), &data, |b, data| {
            b.iter(|| {
                let sum: i64 = data.iter().map(|&x| black_box(x)).sum();
                black_box(sum);
            });
        });
    }

    // Array element access
    for size in [100, 1000, 10000].iter() {
        let data: Vec<i64> = (0..*size).map(|i| i as i64).collect();
        group.throughput(Throughput::Elements(*size as u64));
        group.bench_with_input(BenchmarkId::new("random_access", size), &data, |b, data| {
            b.iter(|| {
                let mut sum = 0i64;
                for i in 0..*size {
                    let idx = (i * 7) % size; // Pseudo-random access pattern
                    sum += black_box(data[idx]);
                }
                black_box(sum);
            });
        });
    }

    // Array allocation
    for size in [100, 1000, 10000].iter() {
        group.throughput(Throughput::Elements(*size as u64));
        group.bench_with_input(BenchmarkId::new("allocation", size), size, |b, &size| {
            b.iter(|| {
                let data: Vec<i64> = (0..size).map(|i| black_box(i as i64)).collect();
                black_box(data);
            });
        });
    }

    group.finish();
}

/// Benchmark string operations
fn string_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("runtime/strings");

    // String concatenation
    for count in [10, 100, 1000].iter() {
        group.throughput(Throughput::Elements(*count as u64));
        group.bench_with_input(
            BenchmarkId::new("concatenation", count),
            count,
            |b, &count| {
                b.iter(|| {
                    let mut s = String::new();
                    for i in 0..count {
                        s.push_str(&format!("item{}", black_box(i)));
                    }
                    black_box(s);
                });
            },
        );
    }

    // String length calculation
    for size in [100, 1000, 10000].iter() {
        let s = "a".repeat(*size);
        group.throughput(Throughput::Bytes(*size as u64));
        group.bench_with_input(BenchmarkId::new("length", size), &s, |b, s| {
            b.iter(|| {
                black_box(s.len());
            });
        });
    }

    // String comparison
    for size in [10, 100, 1000].iter() {
        let s1 = "a".repeat(*size);
        let s2 = "a".repeat(*size);
        group.throughput(Throughput::Bytes(*size as u64));
        group.bench_with_input(
            BenchmarkId::new("comparison", size),
            &(s1, s2),
            |b, (s1, s2)| {
                b.iter(|| {
                    black_box(s1 == s2);
                });
            },
        );
    }

    group.finish();
}

/// Placeholder for future codegen benchmarks
/// These will be populated once the code generator is complete
fn codegen_placeholder_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("runtime/codegen_placeholder");

    group.bench_function("fibonacci_20_placeholder", |b| {
        b.iter(|| {
            // Placeholder: will benchmark fibonacci(20) once codegen is ready
            let _n = black_box(20);
            // Result would be computed by compiled Jet code
        });
    });

    group.bench_function("factorial_100_placeholder", |b| {
        b.iter(|| {
            // Placeholder: will benchmark factorial(100) once codegen is ready
            let _n = black_box(100);
        });
    });

    group.bench_function("array_sum_1000_placeholder", |b| {
        b.iter(|| {
            // Placeholder: will benchmark sum of 1000 elements
            let _size = black_box(1000);
        });
    });

    group.bench_function("binary_tree_insert_1000_placeholder", |b| {
        b.iter(|| {
            // Placeholder: will benchmark tree operations
            let _count = black_box(1000);
        });
    });

    group.bench_function("string_concat_1000_placeholder", |b| {
        b.iter(|| {
            // Placeholder: will benchmark string operations
            let _iterations = black_box(1000);
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    memory_allocation_benchmark,
    loop_iteration_benchmark,
    recursion_benchmark,
    function_call_benchmark,
    arithmetic_benchmark,
    array_benchmark,
    string_benchmark,
    codegen_placeholder_benchmark
);
criterion_main!(benches);
