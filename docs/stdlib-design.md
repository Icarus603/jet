# Jet Standard Library Design

This document serves as the comprehensive blueprint for the Jet programming language's standard library.

## Table of Contents

1. [Design Principles](#design-principles)
2. [Core Module](#core-module)
3. [Option and Result Types](#option-and-result-types)
4. [Collections](#collections)
5. [IO Module](#io-module)
6. [Network Module](#network-module)
7. [Concurrency Module](#concurrency-module)
8. [Time Module](#time-module)
9. [Encoding and Serialization](#encoding-and-serialization)
10. [Error Types](#error-types)
11. [Traits (Type Classes)](#traits-type-classes)
12. [Platform Abstraction](#platform-abstraction)

---

## Design Principles

### Batteries Included
Jet provides a rich standard library out of the box. Common tasks should not require external dependencies.

### Zero-Cost Abstractions
High-level APIs compile down to efficient machine code. Abstractions should not impose runtime overhead.

### Explicit Error Handling via Effects
All operations that can fail declare their effects. The type system tracks what effects a function may perform.

### Thread-Safe by Design
All standard library types are safe to use across concurrent tasks unless explicitly marked otherwise.

### Effect Annotation Syntax
```jet
# Function with no effects
fn pure_function(x: Int) -> Int

# Function with IO effect
fn read_file(path: String) -> Result<String, IOError} with IO

# Function with multiple effects
fn fetch_data(url: String) -> Result<Bytes, NetworkError} with IO, Network

# Effect handler syntax
with handler {
    IOError => |e| log_error(e)
} {
    result := read_file("data.txt")
}
```

---

## Core Module

The `core` module provides fundamental operations and types available without import.

### Primitive Type Methods

```jet
# Int type
impl Int {
    fn abs(self) -> Int
    fn pow(self, exp: Int) -> Int
    fn to_string(self) -> String
    fn to_float(self) -> Float
    fn clamp(self, min: Int, max: Int) -> Int
    fn saturating_add(self, other: Int) -> Int
    fn saturating_sub(self, other: Int) -> Int
    fn wrapping_add(self, other: Int) -> Int
    fn wrapping_sub(self, other: Int) -> Int
}

# Float type
impl Float {
    fn abs(self) -> Float
    fn ceil(self) -> Float
    fn floor(self) -> Float
    fn round(self) -> Float
    fn sqrt(self) -> Float
    fn pow(self, exp: Float) -> Float
    fn to_string(self) -> String
    fn to_int(self) -> Int
    fn is_nan(self) -> Bool
    fn is_infinite(self) -> Bool
}

# Bool type
impl Bool {
    fn then<T>(self, f: () -> T) -> Option<T>
    fn then_some<T>(self, value: T) -> Option<T>
}

# Char type
impl Char {
    fn is_digit(self, radix: Int) -> Bool
    fn is_alphabetic(self) -> Bool
    fn is_whitespace(self) -> Bool
    fn is_uppercase(self) -> Bool
    fn is_lowercase(self) -> Bool
    fn to_uppercase(self) -> Char
    fn to_lowercase(self) -> Char
    fn to_digit(self, radix: Int) -> Option<Int>
}
```

### Panic and Recovery

```jet
# Panic stops execution immediately
fn panic(message: String) -> Never

# Assert conditions
fn assert(condition: Bool) with Panic
fn assert_eq<T: Eq>(left: T, right: T) with Panic
fn assert_ne<T: Eq>(left: T, right: T) with Panic

# Catch panics (for testing and recovery)
fn catch_panic<T>(f: () -> T) -> Result<T, PanicError>

# Unreachable code marker
fn unreachable() -> Never
fn todo() -> Never
fn unimplemented() -> Never
```

### Memory Allocation Hooks

```jet
# Configuration for custom allocators
type Allocator

# Global allocator access
fn set_global_allocator(alloc: Allocator)
fn get_global_allocator() -> Allocator

# Allocation tracking (debug builds only)
effect AllocationTrack {
    fn on_alloc(size: Int, align: Int)
    fn on_dealloc(ptr: Ptr, size: Int, align: Int)
}
```

---

## Option and Result Types

### Option<T>

Represents an optional value: either `Some(T)` or `None`.

```jet
enum Option<T> {
    Some(T),
    None,
}

impl<T> Option<T> {
    # Query methods
    fn is_some(self) -> Bool
    fn is_none(self) -> Bool
    fn is_some_and(self, f: (T) -> Bool) -> Bool

    # Extraction methods
    fn unwrap(self) -> T with Panic
    fn unwrap_or(self, default: T) -> T
    fn unwrap_or_else(self, f: () -> T) -> T
    fn expect(self, message: String) -> T with Panic

    # Transformation methods
    fn map<U>(self, f: (T) -> U) -> Option<U>
    fn map_or<U>(self, default: U, f: (T) -> U) -> U
    fn map_or_else<U>(self, default: () -> U, f: (T) -> U) -> U
    fn flat_map<U>(self, f: (T) -> Option<U>) -> Option<U>
    fn and<U>(self, other: Option<U>) -> Option<U>
    fn and_then<U>(self, f: (T) -> Option<U>) -> Option<U>
    fn or(self, other: Option<T>) -> Option<T>
    fn or_else(self, f: () -> Option<T>) -> Option<T>
    fn xor(self, other: Option<T>) -> Option<T>
    fn filter(self, f: (T) -> Bool) -> Option<T>

    # Conversion methods
    fn ok_or<E>(self, err: E) -> Result<T, E>
    fn ok_or_else<E>(self, f: () -> E) -> Result<T, E>
    fn to_result<E>(self, err: E) -> Result<T, E>
}

# Free functions
fn some<T>(value: T) -> Option<T>
fn none<T>() -> Option<T>
```

**Usage Examples:**

```jet
# Basic usage
fn find_user(id: Int) -> Option<User> {
    database.lookup(id)
}

# Chaining operations
fn get_user_name(id: Int) -> Option<String> {
    find_user(id).map(|u| u.name)
}

# With default
fn get_user_name_or_default(id: Int) -> String {
    find_user(id)
        .map(|u| u.name)
        .unwrap_or("Anonymous".to_string())
}

# Flat mapping
fn get_manager_name(id: Int) -> Option<String> {
    find_user(id)
        .flat_map(|u| find_user(u.manager_id))
        .map(|m| m.name)
}
```

### Result<T, E>

Represents either success (`Ok(T)`) or failure (`Err(E)`).

```jet
enum Result<T, E> {
    Ok(T),
    Err(E),
}

impl<T, E> Result<T, E> {
    # Query methods
    fn is_ok(self) -> Bool
    fn is_err(self) -> Bool
    fn is_ok_and(self, f: (T) -> Bool) -> Bool
    fn is_err_and(self, f: (E) -> Bool) -> Bool

    # Extraction methods
    fn unwrap(self) -> T with Panic
    fn unwrap_err(self) -> E with Panic
    fn unwrap_or(self, default: T) -> T
    fn unwrap_or_else(self, f: (E) -> T) -> T
    fn expect(self, message: String) -> T with Panic
    fn expect_err(self, message: String) -> E with Panic

    # Transformation methods
    fn map<U>(self, f: (T) -> U) -> Result<U, E>
    fn map_err<F>(self, f: (E) -> F) -> Result<T, F>
    fn map_or<U>(self, default: U, f: (T) -> U) -> U
    fn map_or_else<U>(self, default: (E) -> U, f: (T) -> U) -> U
    fn flat_map<U>(self, f: (T) -> Result<U, E>) -> Result<U, E>
    fn and<U>(self, other: Result<U, E>) -> Result<U, E>
    fn and_then<U>(self, f: (T) -> Result<U, E>) -> Result<U, E>
    fn or(self, other: Result<T, E>) -> Result<T, E>
    fn or_else(self, f: (E) -> Result<T, E>) -> Result<T, E>

    # Conversion methods
    fn ok(self) -> Option<T>
    fn err(self) -> Option<E>
    fn transpose(self) -> Option<Result<T, E>> where T: Option<U>
}

# Specialization for Result<T, Error>
impl<T> Result<T, Error> {
    fn with_context<F>(self, f: () -> String) -> Result<T, Error>
    fn context(self, msg: String) -> Result<T, Error>
}
```

**Usage Examples:**

```jet
# Reading a file with error handling
fn read_config() -> Result<Config, IOError> {
    content := std::io::read_file("config.json")?
    json::parse(content)?
}

# Chaining results
fn process_data(input: String) -> Result<Output, Error> {
    parsed := parse_input(input)?
    validated := validate(parsed)?
    transform(validated)
}

# Mapping errors
fn load_user(id: Int) -> Result<User, AppError> {
    db.find(id)
        .map_err(|e| AppError::Database(e))
}

# Combining results
fn parse_coordinates(x: String, y: String) -> Result<(Float, Float), ParseError> {
    x_parsed := x.parse::<Float>()?
    y_parsed := y.parse::<Float>()?
    Ok((x_parsed, y_parsed))
}
```

### Conversions Between Option and Result

```jet
# Option to Result
fn option_to_result<T, E>(opt: Option<T>, err: E) -> Result<T, E>

# Result to Option
fn result_to_option<T, E>(res: Result<T, E>) -> Option<T>
fn result_to_option_err<T, E>(res: Result<T, E>) -> Option<E>

# Collecting from iterators
fn collect_results<T, E, C>(iter: Iterator<Result<T, E>>) -> Result<C, E>
    where C: FromIterator<T>
```

---

## Collections

### Array<T, N>

Fixed-size array type.

```jet
type Array<T, const N: Int>

impl<T, const N: Int> Array<T, N> {
    fn new(value: T) -> Array<T, N> where T: Clone
    fn from_slice(slice: &[T]) -> Array<T, N> where T: Clone
    fn len(self) -> Int
    fn is_empty(self) -> Bool
    fn get(self, index: Int) -> Option<&T>
    fn get_mut(self, index: Int) -> Option<&mut T>
    fn first(self) -> Option<&T>
    fn last(self) -> Option<&T>
    fn iter(self) -> Iterator<&T>
    fn iter_mut(self) -> Iterator<&mut T>
    fn as_slice(self) -> &[T]
    fn map<U>(self, f: (T) -> U) -> Array<U, N>
    fn zip<U>(self, other: Array<U, N>) -> Array<(T, U), N>
}
```

### Vec<T>

Dynamic array (growable vector).

```jet
type Vec<T>

impl<T> Vec<T> {
    # Construction
    fn new() -> Vec<T>
    fn with_capacity(capacity: Int) -> Vec<T>
    fn from_array<const N: Int>(arr: Array<T, N>) -> Vec<T>
    fn from_slice(slice: &[T]) -> Vec<T> where T: Clone

    # Capacity
    fn len(self) -> Int
    fn is_empty(self) -> Bool
    fn capacity(self) -> Int
    fn reserve(self, additional: Int)
    fn reserve_exact(self, additional: Int)
    fn shrink_to_fit(self)
    fn shrink_to(self, min_capacity: Int)

    # Element access
    fn get(self, index: Int) -> Option<&T>
    fn get_mut(self, index: Int) -> Option<&mut T>
    fn first(self) -> Option<&T>
    fn first_mut(self) -> Option<&mut T>
    fn last(self) -> Option<&T>
    fn last_mut(self) -> Option<&mut T>
    fn swap(self, i: Int, j: Int)

    # Modification
    fn push(self, value: T)
    fn pop(self) -> Option<T>
    fn insert(self, index: Int, value: T)
    fn remove(self, index: Int) -> T
    fn swap_remove(self, index: Int) -> T
    fn clear(self)
    fn truncate(self, len: Int)
    fn resize(self, new_len: Int, value: T) where T: Clone
    fn resize_with(self, new_len: Int, f: () -> T)

    # Bulk operations
    fn extend(self, iter: Iterator<T>)
    fn append(self, other: Vec<T>)
    fn split_off(self, at: Int) -> Vec<T>
    fn drain(self, range: Range<Int>) -> Iterator<T>
    fn splice(self, range: Range<Int>, replace_with: Iterator<T>) -> Iterator<T>

    # Searching
    fn contains(self, value: T) -> Bool where T: Eq
    fn find(self, f: (T) -> Bool) -> Option<&T>
    fn position(self, f: (T) -> Bool) -> Option<Int>

    # Sorting
    fn sort(self) where T: Ord
    fn sort_by(self, f: (T, T) -> Ordering)
    fn sort_by_key<U: Ord>(self, f: (T) -> U)
    fn reverse(self)

    # Iteration
    fn iter(self) -> Iterator<&T>
    fn iter_mut(self) -> Iterator<&mut T>
    fn into_iter(self) -> Iterator<T>

    # Conversion
    fn into_array<const N: Int>(self) -> Option<Array<T, N>>
    fn as_slice(self) -> &[T]
    fn as_mut_slice(self) -> &mut [T]
}

# Macro for vec literals
macro vec![...]
```

**Usage Examples:**

```jet
# Creating vectors
let numbers = vec![1, 2, 3, 4, 5]
let empty: Vec<Int> = Vec::new()
let with_capacity = Vec::with_capacity(100)

# Adding elements
numbers.push(6)
numbers.extend([7, 8, 9])

# Accessing elements
if let Some(first) = numbers.first() {
    println("First: {}", first)
}

let third = numbers.get(2)  # Returns Option<&Int>

# Removing elements
if let Some(last) = numbers.pop() {
    println("Popped: {}", last)
}

# Iteration
for num in numbers.iter() {
    println("{}", num)
}

# Functional operations
let doubled: Vec<Int> = numbers.iter().map(|x| x * 2).collect()
let evens: Vec<Int> = numbers.iter().filter(|x| x % 2 == 0).collect()
let sum: Int = numbers.iter().fold(0, |acc, x| acc + x)

# Sorting
numbers.sort()
numbers.sort_by(|a, b| b.cmp(a))  # Reverse sort
```

### Map<K, V>

Hash map implementation.

```jet
type Map<K, V>

impl<K, V> Map<K, V> {
    # Construction
    fn new() -> Map<K, V>
    fn with_capacity(capacity: Int) -> Map<K, V>
    fn from_iter(iter: Iterator<(K, V)>) -> Map<K, V> where K: Eq + Hash

    # Capacity
    fn len(self) -> Int
    fn is_empty(self) -> Bool
    fn capacity(self) -> Int
    fn reserve(self, additional: Int)
    fn shrink_to_fit(self)

    # Access
    fn get(self, key: K) -> Option<&V>
    fn get_mut(self, key: K) -> Option<&mut V>
    fn contains_key(self, key: K) -> Bool
    fn get_key_value(self, key: K) -> Option<(&K, &V)>

    # Modification
    fn insert(self, key: K, value: V) -> Option<V>
    fn remove(self, key: K) -> Option<V>
    fn remove_entry(self, key: K) -> Option<(K, V)>
    fn clear(self)

    # Entry API
    fn entry(self, key: K) -> Entry<K, V>

    # Iteration
    fn iter(self) -> Iterator<(&K, &V)>
    fn iter_mut(self) -> Iterator<(&K, &mut V)>
    fn keys(self) -> Iterator<&K>
    fn values(self) -> Iterator<&V>
    fn values_mut(self) -> Iterator<&mut V>
    fn into_iter(self) -> Iterator<(K, V)>

    # Bulk operations
    fn extend(self, iter: Iterator<(K, V)>)
    fn retain(self, f: (K, V) -> Bool)
}

# Entry API
enum Entry<K, V> {
    Occupied(OccupiedEntry<K, V>),
    Vacant(VacantEntry<K, V>),
}

impl<K, V> Entry<K, V> {
    fn or_insert(self, default: V) -> &mut V
    fn or_insert_with(self, f: () -> V) -> &mut V
    fn or_default(self) -> &mut V where V: Default
    fn and_modify(self, f: (V) -> V) -> Entry<K, V>
}
```

**Usage Examples:**

```jet
# Creating maps
let mut scores = Map::new()
scores.insert("Alice", 100)
scores.insert("Bob", 85)

# Entry API
scores.entry("Alice").and_modify(|v| *v += 10).or_insert(0)
scores.entry("Charlie").or_insert(90)

# Accessing
if let Some(score) = scores.get("Alice") {
    println("Alice's score: {}", score)
}

# Iteration
for (name, score) in scores.iter() {
    println("{}: {}", name, score)
}

# Building from iterator
let word_counts = text.split_whitespace()
    .fold(Map::new(), |mut map, word| {
        *map.entry(word).or_insert(0) += 1
        map
    })
```

### Set<T>

Hash set implementation.

```jet
type Set<T>

impl<T> Set<T> {
    # Construction
    fn new() -> Set<T>
    fn with_capacity(capacity: Int) -> Set<T>
    fn from_iter(iter: Iterator<T>) -> Set<T> where T: Eq + Hash

    # Capacity
    fn len(self) -> Int
    fn is_empty(self) -> Bool
    fn capacity(self) -> Int

    # Access
    fn contains(self, value: T) -> Bool
    fn get(self, value: T) -> Option<&T>
    fn is_disjoint(self, other: Set<T>) -> Bool
    fn is_subset(self, other: Set<T>) -> Bool
    fn is_superset(self, other: Set<T>) -> Bool

    # Modification
    fn insert(self, value: T) -> Bool
    fn remove(self, value: T) -> Bool
    fn take(self, value: T) -> Option<T>
    fn clear(self)
    fn retain(self, f: (T) -> Bool)

    # Set operations
    fn union(self, other: Set<T>) -> Set<T>
    fn intersection(self, other: Set<T>) -> Set<T>
    fn difference(self, other: Set<T>) -> Set<T>
    fn symmetric_difference(self, other: Set<T>) -> Set<T>

    # Iteration
    fn iter(self) -> Iterator<&T>
    fn into_iter(self) -> Iterator<T>

    # Bulk operations
    fn extend(self, iter: Iterator<T>)
}
```

### String

UTF-8 encoded string type.

```jet
type String

impl String {
    # Construction
    fn new() -> String
    fn with_capacity(capacity: Int) -> String
    fn from_utf8(bytes: Vec<u8>) -> Result<String, FromUtf8Error>
    fn from_utf16(bytes: &[u16]) -> Result<String, FromUtf16Error>

    # Capacity
    fn len(self) -> Int
    fn is_empty(self) -> Bool
    fn capacity(self) -> Int
    fn reserve(self, additional: Int)
    fn shrink_to_fit(self)

    # Modification
    fn push(self, ch: Char)
    fn push_str(self, s: &str)
    fn pop(self) -> Option<Char>
    fn truncate(self, new_len: Int)
    fn clear(self)
    fn insert(self, idx: Int, ch: Char)
    fn insert_str(self, idx: Int, s: &str)
    fn remove(self, idx: Int) -> Char
    fn retain(self, f: (Char) -> Bool)

    # Concatenation
    fn append(self, other: String)

    # Splitting and slicing
    fn split(self, pattern: impl Pattern) -> Iterator<&str>
    fn split_whitespace(self) -> Iterator<&str>
    fn lines(self) -> Iterator<&str>
    fn split_at(self, mid: Int) -> (&str, &str)
    fn get(self, range: Range<Int>) -> Option<&str>

    # Searching
    fn contains(self, pattern: impl Pattern) -> Bool
    fn starts_with(self, pattern: impl Pattern) -> Bool
    fn ends_with(self, pattern: impl Pattern) -> Bool
    fn find(self, pattern: impl Pattern) -> Option<Int>
    fn rfind(self, pattern: impl Pattern) -> Option<Int>

    # Manipulation
    fn replace(self, from: impl Pattern, to: &str) -> String
    fn replacen(self, from: impl Pattern, to: &str, count: Int) -> String
    fn to_lowercase(self) -> String
    fn to_uppercase(self) -> String
    fn trim(self) -> &str
    fn trim_start(self) -> &str
    fn trim_end(self) -> &str
    fn repeat(self, n: Int) -> String

    # Conversion
    fn as_bytes(self) -> &[u8]
    fn into_bytes(self) -> Vec<u8>
    fn as_str(self) -> &str
}

# String slice methods
impl str {
    fn len(self) -> Int
    fn is_empty(self) -> Bool
    fn chars(self) -> Iterator<Char>
    fn char_indices(self) -> Iterator<(Int, Char)>
    fn bytes(self) -> Iterator<u8>
    fn parse<T: FromStr>(self) -> Result<T, T::Err>
    fn to_string(self) -> String
    # ... (similar to String methods)
}
```

**Usage Examples:**

```jet
# Creating strings
let s1 = "Hello, World!"
let s2 = String::from("Hello")
let s3 = format!("Hello, {}!", "World")

# Concatenation
let mut greeting = "Hello".to_string()
greeting.push_str(", World")
greeting.push('!')

# Searching and manipulation
if greeting.contains("World") {
    println("Found World!")
}

let replaced = greeting.replace("World", "Jet")
let upper = greeting.to_uppercase()
let trimmed = "  hello  ".trim()

# Splitting
for word in "hello world foo bar".split_whitespace() {
    println("{}", word)
}

for line in "line1\nline2\nline3".lines() {
    println("{}", line)
}

# Parsing
let num: Int = "42".parse()?
let float: Float = "3.14".parse()?
```

### Iterator<T> Trait

The iteration protocol.

```jet
trait Iterator<T> {
    fn next(self) -> Option<T>
    fn size_hint(self) -> (Int, Option<Int>)

    # Transformation
    fn map<U>(self, f: (T) -> U) -> Map<Self, U>
    fn filter(self, f: (T) -> Bool) -> Filter<Self>
    fn filter_map<U>(self, f: (T) -> Option<U>) -> FilterMap<Self, U>
    fn flat_map<U, I>(self, f: (T) -> I) -> FlatMap<Self, U>
        where I: Iterator<U>
    fn flatten(self) -> Flatten<Self> where T: Iterator<U>
    fn enumerate(self) -> Enumerate<Self>
    fn zip<U>(self, other: Iterator<U>) -> Zip<Self, U>
    fn chain(self, other: Iterator<T>) -> Chain<Self>
    fn inspect(self, f: (T) -> ()) -> Inspect<Self>
    fn skip(self, n: Int) -> Skip<Self>
    fn take(self, n: Int) -> Take<Self>
    fn skip_while(self, f: (T) -> Bool) -> SkipWhile<Self>
    fn take_while(self, f: (T) -> Bool) -> TakeWhile<Self>
    fn step_by(self, step: Int) -> StepBy<Self>

    # Consumption
    fn fold<U>(self, init: U, f: (U, T) -> U) -> U
    fn reduce(self, f: (T, T) -> T) -> Option<T>
    fn for_each(self, f: (T) -> ())
    fn collect<C>(self) -> C where C: FromIterator<T>
    fn count(self) -> Int
    fn sum<S>(self) -> S where S: Sum<T>
    fn product<P>(self) -> P where P: Product<T>

    # Searching
    fn find(self, f: (T) -> Bool) -> Option<T>
    fn position(self, f: (T) -> Bool) -> Option<Int>
    fn rposition(self, f: (T) -> Bool) -> Option<Int>
    fn any(self, f: (T) -> Bool) -> Bool
    fn all(self, f: (T) -> Bool) -> Bool
    fn max(self) -> Option<T> where T: Ord
    fn min(self) -> Option<T> where T: Ord
    fn max_by(self, f: (T, T) -> Ordering) -> Option<T>
    fn min_by(self, f: (T, T) -> Ordering) -> Option<T>
    fn max_by_key<U: Ord>(self, f: (T) -> U) -> Option<T>
    fn min_by_key<U: Ord>(self, f: (T) -> U) -> Option<T>

    # Comparison
    fn eq<I>(self, other: I) -> Bool where I: Iterator<T>, T: Eq
    fn lt<I>(self, other: I) -> Bool where I: Iterator<T>, T: Ord
    fn le<I>(self, other: I) -> Bool where I: Iterator<T>, T: Ord
    fn gt<I>(self, other: I) -> Bool where I: Iterator<T>, T: Ord
    fn ge<I>(self, other: I) -> Bool where I: Iterator<T>, T: Ord

    # Collecting to specific types
    fn to_vec(self) -> Vec<T>
    fn to_set(self) -> Set<T> where T: Eq + Hash
}

# Double-ended iterator
trait DoubleEndedIterator<T>: Iterator<T> {
    fn next_back(self) -> Option<T>
    fn rfind(self, f: (T) -> Bool) -> Option<T>
    fn rfold<U>(self, init: U, f: (U, T) -> U) -> U
}

# Exact size iterator
trait ExactSizeIterator<T>: Iterator<T> {
    fn len(self) -> Int
}

# FromIterator for collect
trait FromIterator<T> {
    fn from_iter(iter: Iterator<T>) -> Self
}
```

**Usage Examples:**

```jet
# Range iteration
for i in 0..10 {
    println("{}", i)
}

# Functional pipeline
let result = (0..100)
    .filter(|x| x % 2 == 0)
    .map(|x| x * x)
    .filter(|x| x > 100)
    .take(10)
    .collect::<Vec<Int>>()

# Folding
let sum = (1..=10).fold(0, |acc, x| acc + x)

# Finding
let first_even = numbers.iter().find(|x| x % 2 == 0)
let has_negative = numbers.iter().any(|x| x < 0)
let all_positive = numbers.iter().all(|x| x > 0)

# Zipping
let sums: Vec<Int> = vec_a.iter()
    .zip(vec_b.iter())
    .map(|(a, b)| a + b)
    .collect()

# Chaining
let combined: Vec<Int> = first_list.iter()
    .chain(second_list.iter())
    .copied()
    .collect()
```

---

## IO Module

The `std::io` module provides input/output operations.

### Effects

```jet
effect IOError {
    NotFound,
    PermissionDenied,
    AlreadyExists,
    InvalidInput,
    InvalidData,
    UnexpectedEof,
    WriteZero,
    Other(String),
}

effect FileError : IOError
```

### File Operations

```jet
# Reading files
fn read_to_string(path: impl AsPath) -> Result<String, FileError} with IO
fn read(path: impl AsPath) -> Result<Vec<u8>, FileError} with IO
fn read_lines(path: impl AsPath) -> Result<Iterator<String>, FileError} with IO

# Writing files
fn write(path: impl AsPath, contents: impl AsRef<[u8]>) -> Result<(), FileError} with IO
fn write_string(path: impl AsPath, contents: impl AsRef<str>) -> Result<(), FileError} with IO

# File type
struct File

impl File {
    fn open(path: impl AsPath) -> Result<File, FileError} with IO
    fn create(path: impl AsPath) -> Result<File, FileError} with IO
    fn create_new(path: impl AsPath) -> Result<File, FileError} with IO
    fn open_options(options: OpenOptions) -> Result<File, FileError} with IO

    fn metadata(self) -> Result<Metadata, FileError} with IO
    fn sync_all(self) -> Result<(), FileError} with IO
    fn sync_data(self) -> Result<(), FileError} with IO
    fn set_len(self, size: Int) -> Result<(), FileError} with IO

    fn try_clone(self) -> Result<File, FileError} with IO
}

# Open options
struct OpenOptions

impl OpenOptions {
    fn new() -> OpenOptions
    fn read(self, read: Bool) -> OpenOptions
    fn write(self, write: Bool) -> OpenOptions
    fn append(self, append: Bool) -> OpenOptions
    fn truncate(self, truncate: Bool) -> OpenOptions
    fn create(self, create: Bool) -> OpenOptions
    fn create_new(self, create_new: Bool) -> OpenOptions
    fn open(self, path: impl AsPath) -> Result<File, FileError} with IO
}

# File metadata
struct Metadata

impl Metadata {
    fn is_file(self) -> Bool
    fn is_dir(self) -> Bool
    fn is_symlink(self) -> Bool
    fn len(self) -> Int
    fn permissions(self) -> Permissions
    fn modified(self) -> Result<SystemTime, FileError} with IO
    fn accessed(self) -> Result<SystemTime, FileError} with IO
    fn created(self) -> Result<SystemTime, FileError} with IO
}
```

### Buffered IO

```jet
struct BufReader<R: Read> {
    fn new(inner: R) -> BufReader<R>
    fn with_capacity(capacity: Int, inner: R) -> BufReader<R>
    fn capacity(self) -> Int
    fn into_inner(self) -> R
}

struct BufWriter<W: Write> {
    fn new(inner: W) -> BufWriter<W>
    fn with_capacity(capacity: Int, inner: W) -> BufWriter<W>
    fn capacity(self) -> Int
    fn into_inner(self) -> Result<W, IntoInnerError<W>>
    fn flush(self) -> Result<(), IOError} with IO
}

struct LineWriter<W: Write> {
    fn new(inner: W) -> LineWriter<W>
}
```

### Standard Streams

```jet
# stdin
fn stdin() -> Stdin
struct Stdin

impl Stdin {
    fn read_line(self, buf: String) -> Result<Int, IOError} with IO
    fn lines(self) -> Iterator<Result<String, IOError>> with IO
    fn read_to_end(self, buf: Vec<u8>) -> Result<Int, IOError} with IO
    fn read_to_string(self, buf: String) -> Result<Int, IOError} with IO
}

# stdout
fn stdout() -> Stdout
struct Stdout

impl Stdout {
    fn write(self, buf: &[u8]) -> Result<Int, IOError} with IO
    fn write_all(self, buf: &[u8]) -> Result<(), IOError} with IO
    fn flush(self) -> Result<(), IOError} with IO
}

# stderr
fn stderr() -> Stderr
struct Stderr

impl Stderr {
    fn write(self, buf: &[u8]) -> Result<Int, IOError} with IO
    fn write_all(self, buf: &[u8]) -> Result<(), IOError} with IO
    fn flush(self) -> Result<(), IOError} with IO
}

# Print macros
macro print!(...)
macro println!(...)
macro eprint!(...)
macro eprintln!(...)
macro format!(...)
```

### Read and Write Traits

```jet
trait Read {
    fn read(self, buf: &mut [u8]) -> Result<Int, IOError} with IO
    fn read_exact(self, buf: &mut [u8]) -> Result<(), IOError} with IO
    fn read_to_end(self, buf: Vec<u8>) -> Result<Int, IOError} with IO
    fn read_to_string(self, buf: String) -> Result<Int, IOError} with IO
    fn bytes(self) -> Iterator<Result<u8, IOError>> with IO
    fn chain<R: Read>(self, next: R) -> Chain<Self, R>
    fn take(self, limit: Int) -> Take<Self>
}

trait Write {
    fn write(self, buf: &[u8]) -> Result<Int, IOError} with IO
    fn write_all(self, buf: &[u8]) -> Result<(), IOError} with IO
    fn write_vectored(self, bufs: &[IoSlice]) -> Result<Int, IOError} with IO
    fn flush(self) -> Result<(), IOError} with IO
    fn by_ref(self) -> &Self
}

trait Seek {
    fn seek(self, pos: SeekFrom) -> Result<Int, IOError} with IO
    fn rewind(self) -> Result<(), IOError} with IO
    fn stream_position(self) -> Result<Int, IOError} with IO
}

enum SeekFrom {
    Start(Int),
    End(Int),
    Current(Int),
}
```

**Usage Examples:**

```jet
# Reading a file
let contents = std::io::read_to_string("data.txt")?

# Writing a file
std::io::write("output.txt", "Hello, World!")?

# Buffered reading
let file = File::open("large_file.txt")?
let reader = BufReader::new(file)

for line in reader.lines() {
    let line = line?
    process_line(line)
}

# Buffered writing
let file = File::create("output.txt")?
let mut writer = BufWriter::new(file)

for item in data {
    writer.write_all(format!("{}\n", item).as_bytes())?
}
writer.flush()?

# Reading from stdin
let mut input = String::new()
stdin().read_line(input)?

# Copying data
std::io::copy(&mut source, &mut dest)?
```

---

## Network Module

The `std::net` module provides networking capabilities.

### Effects

```jet
effect NetworkError {
    ConnectionRefused,
    ConnectionReset,
    ConnectionAborted,
    NotConnected,
    AddrInUse,
    AddrNotAvailable,
    BrokenPipe,
    AlreadyExists,
    InvalidInput,
    TimedOut,
    Interrupted,
    Other(String),
}

effect Timeout
```

### TCP

```jet
# TCP listener
struct TcpListener

impl TcpListener {
    fn bind(addr: impl ToSocketAddrs) -> Result<TcpListener, NetworkError} with IO, Network
    fn accept(self) -> Result<(TcpStream, SocketAddr), NetworkError} with IO, Network
    fn local_addr(self) -> Result<SocketAddr, NetworkError}
    fn set_nonblocking(self, nonblocking: Bool) -> Result<(), NetworkError}
    fn take_error(self) -> Result<Option<NetworkError>, NetworkError}
}

# TCP stream
struct TcpStream

impl TcpStream {
    fn connect(addr: impl ToSocketAddrs) -> Result<TcpStream, NetworkError} with IO, Network
    fn peer_addr(self) -> Result<SocketAddr, NetworkError}
    fn local_addr(self) -> Result<SocketAddr, NetworkError}
    fn shutdown(self, how: Shutdown) -> Result<(), NetworkError} with IO, Network
    fn set_nodelay(self, nodelay: Bool) -> Result<(), NetworkError}
    fn nodelay(self) -> Result<Bool, NetworkError}
    fn set_ttl(self, ttl: Int) -> Result<(), NetworkError}
    fn ttl(self) -> Result<Int, NetworkError}
    fn set_nonblocking(self, nonblocking: Bool) -> Result<(), NetworkError}
    fn take_error(self) -> Result<Option<NetworkError>, NetworkError}
    fn try_clone(self) -> Result<TcpStream, NetworkError}
}

impl Read for TcpStream
impl Write for TcpStream

enum Shutdown {
    Read,
    Write,
    Both,
}
```

### UDP

```jet
struct UdpSocket

impl UdpSocket {
    fn bind(addr: impl ToSocketAddrs) -> Result<UdpSocket, NetworkError} with IO, Network
    fn connect(self, addr: impl ToSocketAddrs) -> Result<(), NetworkError} with IO, Network
    fn local_addr(self) -> Result<SocketAddr, NetworkError}
    fn peer_addr(self) -> Result<SocketAddr, NetworkError}
    fn send(self, buf: &[u8]) -> Result<Int, NetworkError} with IO, Network
    fn recv(self, buf: &mut [u8]) -> Result<Int, NetworkError} with IO, Network
    fn send_to(self, buf: &[u8], addr: impl ToSocketAddrs) -> Result<Int, NetworkError} with IO, Network
    fn recv_from(self, buf: &mut [u8]) -> Result<(Int, SocketAddr), NetworkError} with IO, Network
    fn set_broadcast(self, broadcast: Bool) -> Result<(), NetworkError}
    fn broadcast(self) -> Result<Bool, NetworkError}
    fn set_nonblocking(self, nonblocking: Bool) -> Result<(), NetworkError}
    fn take_error(self) -> Result<Option<NetworkError>, NetworkError}
    fn try_clone(self) -> Result<UdpSocket, NetworkError}
}
```

### Socket Addresses

```jet
struct SocketAddr {
    ip: IpAddr,
    port: Int,
}

enum IpAddr {
    V4(Ipv4Addr),
    V6(Ipv6Addr),
}

struct Ipv4Addr
struct Ipv6Addr

impl SocketAddr {
    fn new(ip: IpAddr, port: Int) -> SocketAddr
    fn ip(self) -> IpAddr
    fn port(self) -> Int
    fn is_ipv4(self) -> Bool
    fn is_ipv6(self) -> Bool
}

trait ToSocketAddrs {
    fn to_socket_addrs(self) -> Iterator<SocketAddr> with IO, Network
}
```

### HTTP Client

```jet
module std::net::http

struct Client

impl Client {
    fn new() -> Client
    fn builder() -> ClientBuilder

    fn get(self, url: impl AsRef<str>) -> RequestBuilder
    fn post(self, url: impl AsRef<str>) -> RequestBuilder
    fn put(self, url: impl AsRef<str>) -> RequestBuilder
    fn delete(self, url: impl AsRef<str>) -> RequestBuilder
    fn head(self, url: impl AsRef<str>) -> RequestBuilder
    fn patch(self, url: impl AsRef<str>) -> RequestBuilder
    fn request(self, method: Method, url: impl AsRef<str>) -> RequestBuilder

    fn execute(self, request: Request) -> Result<Response, NetworkError} with IO, Network, Timeout
}

struct ClientBuilder

impl ClientBuilder {
    fn timeout(self, timeout: Duration) -> ClientBuilder
    fn connect_timeout(self, timeout: Duration) -> ClientBuilder
    fn pool_max_idle_per_host(self, max: Int) -> ClientBuilder
    fn user_agent(self, user_agent: impl AsRef<str>) -> ClientBuilder
    fn default_headers(self, headers: HeaderMap) -> ClientBuilder
    fn build(self) -> Result<Client, NetworkError}
}

struct RequestBuilder

impl RequestBuilder {
    fn header(self, key: impl AsRef<str>, value: impl AsRef<str>) -> RequestBuilder
    fn headers(self, headers: HeaderMap) -> RequestBuilder
    fn body(self, body: impl AsRef<[u8]>) -> RequestBuilder
    fn json(self, json: impl Serialize) -> RequestBuilder
    fn form(self, form: impl Serialize) -> RequestBuilder
    fn query(self, query: impl Serialize) -> RequestBuilder
    fn timeout(self, timeout: Duration) -> RequestBuilder
    fn build(self) -> Result<Request, NetworkError}
    fn send(self) -> Result<Response, NetworkError} with IO, Network, Timeout
}

struct Request {
    method: Method,
    url: Url,
    headers: HeaderMap,
    body: Option<Body>,
}

struct Response {
    status: StatusCode,
    headers: HeaderMap,
    url: Url,
    body: Body,
}

impl Response {
    fn status(self) -> StatusCode
    fn headers(self) -> HeaderMap
    fn url(self) -> Url
    fn content_length(self) -> Option<Int>
    fn text(self) -> Result<String, NetworkError} with IO
    fn json<T: Deserialize>(self) -> Result<T, NetworkError} with IO
    fn bytes(self) -> Result<Vec<u8>, NetworkError} with IO
}

enum Method {
    GET,
    POST,
    PUT,
    DELETE,
    HEAD,
    OPTIONS,
    TRACE,
    CONNECT,
    PATCH,
}

struct StatusCode

impl StatusCode {
    fn as_u16(self) -> Int
    fn is_informational(self) -> Bool
    fn is_success(self) -> Bool
    fn is_redirection(self) -> Bool
    fn is_client_error(self) -> Bool
    fn is_server_error(self) -> Bool
}
```

**Usage Examples:**

```jet
# TCP server
fn run_server() -> Result<(), NetworkError} with IO, Network {
    let listener = TcpListener::bind("127.0.0.1:8080")?
    println!("Server listening on {}", listener.local_addr()?)

    for conn in listener.incoming() {
        let (stream, addr) = conn?
        spawn handle_connection(stream, addr)
    }
}

# TCP client
fn connect_to_server() -> Result<(), NetworkError} with IO, Network {
    let mut stream = TcpStream::connect("127.0.0.1:8080")?
    stream.write_all(b"Hello, Server!")?

    let mut buffer = [0u8; 1024]
    let n = stream.read(buffer)?
    println!("Received: {}", String::from_utf8_lossy(buffer[..n]))
}

# UDP
fn udp_example() -> Result<(), NetworkError} with IO, Network {
    let socket = UdpSocket::bind("0.0.0.0:0")?
    socket.send_to(b"Hello", "127.0.0.1:8080")?

    let mut buf = [0u8; 1024]
    let (n, addr) = socket.recv_from(buf)?
    println!("Received {} bytes from {}", n, addr)
}

# HTTP client
fn http_get() -> Result<(), NetworkError} with IO, Network, Timeout {
    let client = http::Client::new()
    let response = client.get("https://api.example.com/data").send()?

    if response.status().is_success() {
        let data: JsonValue = response.json()?
        println!("{}", data)
    }
}

# HTTP with body
fn http_post() -> Result<(), NetworkError} with IO, Network, Timeout {
    let client = http::Client::new()
    let response = client
        .post("https://api.example.com/users")
        .header("Content-Type", "application/json")
        .json(Map::from([
            ("name", "Alice"),
            ("email", "alice@example.com"),
        ]))
        .send()?

    println!("Status: {}", response.status().as_u16())
}
```

---

## Concurrency Module

The `std::concurrency` module provides structured concurrency primitives.

### Effects

```jet
effect Panic {
    # Propagated from spawned tasks
}

effect Cancel {
    # Task cancellation signal
}
```

### Task

```jet
struct Task<T>

impl<T> Task<T> {
    # Spawn a new task
    fn spawn<F>(f: F) -> Task<T> with Concurrency
        where F: FnOnce() -> T

    fn spawn_blocking<F>(f: F) -> Task<T> with Concurrency
        where F: FnOnce() -> T

    # Await completion
    fn await(self) -> Result<T, Panic} with Concurrency

    # Cancel the task
    fn cancel(self)

    # Check if finished
    fn is_finished(self) -> Bool
}

# Scoped spawning (guaranteed to complete before scope exits)
fn scope<F, R>(f: F) -> R
    where F: FnOnce(Scope) -> R

struct Scope

impl Scope {
    fn spawn<F, T>(self, f: F) -> Task<T>
        where F: FnOnce() -> T
}
```

### Channel<T>

```jet
enum Channel<T> {
    # Bounded channel with capacity
    Bounded(Int),
    # Unbounded channel
    Unbounded,
}

fn channel<T>() -> (Sender<T>, Receiver<T>)
fn bounded_channel<T>(capacity: Int) -> (Sender<T>, Receiver<T>)

struct Sender<T>

impl<T> Sender<T> {
    fn send(self, value: T) -> Result<(), SendError<T>> with Concurrency
    fn try_send(self, value: T) -> Result<(), TrySendError<T>>
    fn send_timeout(self, value: T, timeout: Duration) -> Result<(), SendTimeoutError<T>> with Concurrency, Timeout
    fn is_closed(self) -> Bool
    fn close(self)
    fn clone(self) -> Sender<T> where T: Clone
}

struct Receiver<T>

impl<T> Receiver<T> {
    fn recv(self) -> Result<T, RecvError> with Concurrency
    fn try_recv(self) -> Result<T, TryRecvError>
    fn recv_timeout(self, timeout: Duration) -> Result<T, RecvTimeoutError> with Concurrency, Timeout
    fn close(self)
    fn is_closed(self) -> Bool
    fn is_empty(self) -> Bool
    fn len(self) -> Int

    # Iterator support
    fn iter(self) -> Iterator<T> with Concurrency
    fn try_iter(self) -> Iterator<T>
}

# Error types
struct SendError<T> { value: T }
struct RecvError
enum TrySendError<T> { Full(T), Closed(T) }
enum TryRecvError { Empty, Closed }
enum SendTimeoutError<T> { Timeout(T), Closed(T) }
enum RecvTimeoutError { Timeout, Closed }
```

### Synchronization Primitives

```jet
# Mutex
struct Mutex<T>

impl<T> Mutex<T> {
    fn new(value: T) -> Mutex<T>
    fn lock(self) -> MutexGuard<T> with Concurrency
    fn try_lock(self) -> Option<MutexGuard<T>>
    fn is_poisoned(self) -> Bool
    fn into_inner(self) -> T
}

struct MutexGuard<T>: Deref<T>, DerefMut<T> {}

# RwLock
struct RwLock<T>

impl<T> RwLock<T> {
    fn new(value: T) -> RwLock<T>
    fn read(self) -> RwLockReadGuard<T> with Concurrency
    fn try_read(self) -> Option<RwLockReadGuard<T>>
    fn write(self) -> RwLockWriteGuard<T> with Concurrency
    fn try_write(self) -> Option<RwLockWriteGuard<T>>
    fn is_poisoned(self) -> Bool
    fn into_inner(self) -> T
}

struct RwLockReadGuard<T>: Deref<T> {}
struct RwLockWriteGuard<T>: Deref<T>, DerefMut<T> {}

# Atomic types
struct AtomicBool
struct AtomicInt
struct AtomicInt64
struct AtomicPtr<T>

impl AtomicBool {
    fn new(value: Bool) -> AtomicBool
    fn load(self, ordering: Ordering) -> Bool
    fn store(self, value: Bool, ordering: Ordering)
    fn swap(self, value: Bool, ordering: Ordering) -> Bool
    fn compare_exchange(self, current: Bool, new: Bool, success: Ordering, failure: Ordering) -> Result<Bool, Bool>
    fn fetch_and(self, val: Bool, ordering: Ordering) -> Bool
    fn fetch_or(self, val: Bool, ordering: Ordering) -> Bool
    fn fetch_xor(self, val: Bool, ordering: Ordering) -> Bool
}

enum Ordering {
    Relaxed,
    Acquire,
    Release,
    AcqRel,
    SeqCst,
}

# WaitGroup
struct WaitGroup

impl WaitGroup {
    fn new() -> WaitGroup
    fn add(self, delta: Int)
    fn done(self)
    fn wait(self) with Concurrency
}

# Semaphore
struct Semaphore

impl Semaphore {
    fn new(permits: Int) -> Semaphore
    fn acquire(self) -> SemaphorePermit with Concurrency
    fn try_acquire(self) -> Option<SemaphorePermit>
    fn acquire_many(self, n: Int) -> SemaphorePermit with Concurrency
    fn available_permits(self) -> Int
}

struct SemaphorePermit

impl SemaphorePermit {
    fn forget(self)
}

# Barrier
struct Barrier

impl Barrier {
    fn new(n: Int) -> Barrier
    fn wait(self) -> BarrierWaitResult with Concurrency
}

struct BarrierWaitResult {
    fn is_leader(self) -> Bool
}

# One-shot channel
struct OneShot<T>

impl<T> OneShot<T> {
    fn new() -> (Sender<T>, Receiver<T>)
}
```

**Usage Examples:**

```jet
# Spawning tasks
fn concurrent_computation() -> Result<Int, Panic} with Concurrency {
    let task1 = Task::spawn(|| heavy_computation_1())
    let task2 = Task::spawn(|| heavy_computation_2())

    let result1 = task1.await()?
    let result2 = task2.await()?

    result1 + result2
}

# Scoped tasks (guaranteed to complete)
fn scoped_example(data: &[Int]) -> Vec<Int> {
    concurrency::scope(|s| {
        let mut results = Vec::with_capacity(data.len())

        for item in data {
            let task = s.spawn(|| process(*item))
            results.push(task)
        }

        # All tasks complete here before scope exits
        results.into_iter().map(|t| t.await().unwrap()).collect()
    })
}

# Channels
fn producer_consumer() {
    let (tx, rx) = channel::<Int>()

    let producer = Task::spawn(move || {
        for i in 0..10 {
            tx.send(i).unwrap()
        }
    })

    let consumer = Task::spawn(move || {
        while let Ok(value) = rx.recv() {
            println!("Received: {}", value)
        }
    })

    producer.await()
    consumer.await()
}

# Mutex
fn shared_state() {
    let counter = Mutex::new(0)
    let mut tasks = Vec::new()

    for _ in 0..10 {
        let counter = counter.clone()
        tasks.push(Task::spawn(move || {
            let mut guard = counter.lock()
            *guard += 1
        }))
    }

    for task in tasks {
        task.await()
    }

    println!("Final count: {}", counter.into_inner())
}

# RwLock
fn rwlock_example() {
    let data = RwLock::new(Vec::new())

    # Multiple readers
    let r1 = data.read()
    let r2 = data.read()

    # Exclusive writer
    let mut w = data.write()
    w.push(42)
}

# WaitGroup
fn waitgroup_example() {
    let wg = WaitGroup::new()

    for i in 0..5 {
        wg.add(1)
        let wg = wg.clone()
        Task::spawn(move || {
            println!("Task {} done", i)
            wg.done()
        })
    }

    wg.wait()
    println!("All tasks completed")
}

# Semaphore
fn semaphore_example() {
    let sem = Semaphore::new(3)  # Limit to 3 concurrent operations

    for i in 0..10 {
        let permit = sem.acquire()
        Task::spawn(move || {
            println!("Task {} acquired permit", i)
            sleep(Duration::from_secs(1))
            drop(permit)
        })
    }
}
```

---

## Time Module

The `std::time` module provides time-related functionality.

### Duration

```jet
struct Duration {
    secs: Int,
    nanos: Int,
}

impl Duration {
    # Construction
    fn new(secs: Int, nanos: Int) -> Duration
    fn from_secs(secs: Int) -> Duration
    fn from_millis(millis: Int) -> Duration
    fn from_micros(micros: Int) -> Duration
    fn from_nanos(nanos: Int) -> Duration

    # Accessors
    fn as_secs(self) -> Int
    fn as_millis(self) -> Int
    fn as_micros(self) -> Int
    fn as_nanos(self) -> Int
    fn subsec_millis(self) -> Int
    fn subsec_micros(self) -> Int
    fn subsec_nanos(self) -> Int

    # Operations
    fn checked_add(self, other: Duration) -> Option<Duration>
    fn checked_sub(self, other: Duration) -> Option<Duration>
    fn checked_mul(self, n: Int) -> Option<Duration>
    fn checked_div(self, n: Int) -> Option<Duration>
    fn saturating_add(self, other: Duration) -> Duration
    fn saturating_sub(self, other: Duration) -> Duration
    fn mul_f64(self, n: Float) -> Duration
    fn div_f64(self, n: Float) -> Duration
}

impl Add for Duration
impl Sub for Duration
impl Mul<Int> for Duration
impl Div<Int> for Duration
impl PartialOrd for Duration
impl Ord for Duration
```

### Instant

```jet
struct Instant

impl Instant {
    fn now() -> Instant with IO
    fn duration_since(self, earlier: Instant) -> Duration
    fn checked_duration_since(self, earlier: Instant) -> Option<Duration>
    fn saturating_duration_since(self, earlier: Instant) -> Duration
    fn elapsed(self) -> Duration with IO
}

impl Add<Duration> for Instant -> Instant
impl Sub<Duration> for Instant -> Instant
impl Sub<Instant> for Instant -> Duration
impl PartialOrd for Instant
impl Ord for Instant
```

### SystemTime

```jet
struct SystemTime

impl SystemTime {
    fn now() -> SystemTime with IO
    fn duration_since(self, earlier: SystemTime) -> Result<Duration, SystemTimeError}
    fn checked_duration_since(self, earlier: SystemTime) -> Option<Duration>
    fn elapsed(self) -> Result<Duration, SystemTimeError} with IO
    fn checked_add(self, duration: Duration) -> Option<SystemTime>
    fn checked_sub(self, duration: Duration) -> Option<SystemTime>
}

struct SystemTimeError

impl SystemTimeError {
    fn duration(self) -> Duration
}

# Constants
const UNIX_EPOCH: SystemTime
```

### Sleeping and Timeouts

```jet
# Sleep for a duration
fn sleep(duration: Duration) with IO

# Sleep until an instant
fn sleep_until(deadline: Instant) with IO

# Timeout wrapper
fn timeout<T>(duration: Duration, f: () -> T) -> Result<T, TimeoutError} with IO, Timeout

# Interval for periodic tasks
struct Interval

impl Interval {
    fn new(period: Duration) -> Interval with IO
    fn tick(self) -> Instant with IO
    fn reset(self) with IO
    fn reset_immediately(self) with IO
    fn reset_after(self, duration: Duration) with IO
}

# Timeout error
struct TimeoutError
```

### Date and DateTime

```jet
# Date (calendar date without time)
struct Date {
    year: Int,
    month: Int,   # 1-12
    day: Int,     # 1-31
}

impl Date {
    fn new(year: Int, month: Int, day: Int) -> Option<Date>
    fn today() -> Date with IO
    fn from_system_time(time: SystemTime) -> Date
    fn weekday(self) -> Weekday
    fn ordinal(self) -> Int  # Day of year (1-366)
    fn is_leap_year(self) -> Bool
    fn days_in_month(self) -> Int
    fn checked_add_days(self, days: Int) -> Option<Date>
    fn checked_sub_days(self, days: Int) -> Option<Date>
    fn format(self, fmt: &str) -> String
}

# Time of day
struct Time {
    hour: Int,        # 0-23
    minute: Int,      # 0-59
    second: Int,      # 0-59
    nanosecond: Int,  # 0-999,999,999
}

impl Time {
    fn new(hour: Int, minute: Int, second: Int) -> Option<Time>
    fn with_nanos(hour: Int, minute: Int, second: Int, nanos: Int) -> Option<Time>
    fn midnight() -> Time
}

# DateTime (date + time)
struct DateTime {
    date: Date,
    time: Time,
    offset: UtcOffset,  # Defaults to UTC
}

impl DateTime {
    fn new(date: Date, time: Time) -> DateTime
    fn now() -> DateTime with IO
    fn now_utc() -> DateTime with IO
    fn from_system_time(time: SystemTime) -> DateTime
    fn to_system_time(self) -> SystemTime
    fn timestamp(self) -> Int  # Seconds since Unix epoch
    fn timestamp_millis(self) -> Int
    fn format(self, fmt: &str) -> String
    fn parse(s: &str, fmt: &str) -> Result<DateTime, ParseError}

    # Component access
    fn year(self) -> Int
    fn month(self) -> Int
    fn day(self) -> Int
    fn hour(self) -> Int
    fn minute(self) -> Int
    fn second(self) -> Int
    fn nanosecond(self) -> Int

    # Arithmetic
    fn checked_add_duration(self, duration: Duration) -> Option<DateTime>
    fn checked_sub_duration(self, duration: Duration) -> Option<DateTime>
    fn duration_since(self, other: DateTime) -> Duration
}

enum Weekday {
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
    Saturday,
    Sunday,
}

impl Weekday {
    fn number_from_monday(self) -> Int  # 1-7
    fn number_from_sunday(self) -> Int  # 1-7
    fn succ(self) -> Weekday
    fn pred(self) -> Weekday
}

struct UtcOffset

impl UtcOffset {
    fn utc() -> UtcOffset
    fn from_seconds(seconds: Int) -> Option<UtcOffset>
    fn from_hms(hours: Int, minutes: Int, seconds: Int) -> Option<UtcOffset>
    fn local() -> UtcOffset with IO
}

# Timezone support
struct TimeZone

impl TimeZone {
    fn utc() -> TimeZone
    fn local() -> TimeZone with IO
    fn offset_at(self, datetime: DateTime) -> UtcOffset
}
```

**Usage Examples:**

```jet
# Duration
let timeout = Duration::from_secs(30)
let short_delay = Duration::from_millis(100)

# Timing operations
let start = Instant::now()
perform_work()
let elapsed = start.elapsed()
println!("Operation took {:?}", elapsed)

# Sleeping
sleep(Duration::from_secs(1))
sleep_until(Instant::now() + Duration::from_secs(5))

# Periodic tasks
let mut interval = Interval::new(Duration::from_secs(60))
loop {
    interval.tick()
    perform_periodic_task()
}

# Timeout
let result = timeout(Duration::from_secs(5), || {
    long_running_operation()
})

# DateTime
let now = DateTime::now()
let formatted = now.format("%Y-%m-%d %H:%M:%S")

let specific_date = Date::new(2024, 12, 25).unwrap()
let christmas = DateTime::new(specific_date, Time::midnight())

# Parsing
let dt = DateTime::parse("2024-01-15", "%Y-%m-%d")?
```

---

## Encoding and Serialization

### JSON

```jet
module std::json

# Value type
enum Value {
    Null,
    Bool(Bool),
    Number(Number),
    String(String),
    Array(Vec<Value>),
    Object(Map<String, Value>),
}

# Number representation
enum Number {
    Int(Int),
    Float(Float),
}

# Serialization
trait Serialize {
    fn serialize(self, serializer: Serializer) -> Result<(), SerializeError}
}

# Deserialization
trait Deserialize {
    fn deserialize(deserializer: Deserializer) -> Result<Self, DeserializeError}
}

# Functions
fn to_string<T: Serialize>(value: T) -> Result<String, SerializeError>
fn to_string_pretty<T: Serialize>(value: T) -> Result<String, SerializeError>
fn to_vec<T: Serialize>(value: T) -> Result<Vec<u8>, SerializeError>
fn from_str<T: Deserialize>(s: &str) -> Result<T, DeserializeError>
fn from_slice<T: Deserialize>(v: &[u8]) -> Result<T, DeserializeError>

# Value manipulation
impl Value {
    fn get(self, key: &str) -> Option<Value>
    fn get_index(self, index: Int) -> Option<Value>
    fn as_bool(self) -> Option<Bool>
    fn as_int(self) -> Option<Int>
    fn as_float(self) -> Option<Float>
    fn as_string(self) -> Option<String>
    fn as_array(self) -> Option<Vec<Value>>
    fn as_object(self) -> Option<Map<String, Value>>
    fn is_null(self) -> Bool
}

# Error types
struct SerializeError
struct DeserializeError
```

**Usage Examples:**

```jet
# Serialize to JSON
let user = User {
    name: "Alice",
    age: 30,
    email: "alice@example.com",
}
let json_str = json::to_string(user)?

# Deserialize from JSON
let user: User = json::from_str(json_str)?

# Working with JSON values
let data = json::from_str(r#"{"name": "Bob", "scores": [85, 92, 78]}"#)?
let name = data.get("name").and_then(|v| v.as_string())
let scores = data.get("scores").and_then(|v| v.as_array())

# Pretty printing
let pretty = json::to_string_pretty(data)?
```

### Base64

```jet
module std::base64

enum Variant {
    Standard,       # Standard Base64 (RFC 4648)
    UrlSafe,        # URL-safe Base64 (RFC 4648)
    StandardNoPad,  # Standard without padding
    UrlSafeNoPad,   # URL-safe without padding
}

fn encode(input: impl AsRef<[u8]>) -> String
fn encode_config(input: impl AsRef<[u8]>, variant: Variant) -> String
fn decode(input: impl AsRef<str>) -> Result<Vec<u8>, DecodeError}
fn decode_config(input: impl AsRef<str>, variant: Variant) -> Result<Vec<u8>, DecodeError}

struct DecodeError
```

**Usage Examples:**

```jet
let data = b"Hello, World!"
let encoded = base64::encode(data)
let decoded = base64::decode(encoded)?

# URL-safe variant
let url_safe = base64::encode_config(data, base64::UrlSafe)
```

### URL Encoding

```jet
module std::url

fn encode(input: impl AsRef<[u8]>) -> String
fn encode_component(input: impl AsRef<str>) -> String
fn decode(input: impl AsRef<str>) -> Result<String, DecodeError}
fn decode_component(input: impl AsRef<str>) -> Result<String, DecodeError}

# URL type
struct Url

impl Url {
    fn parse(input: impl AsRef<str>) -> Result<Url, ParseError}
    fn join(self, input: impl AsRef<str>) -> Result<Url, ParseError>

    # Component access
    fn scheme(self) -> &str
    fn host(self) -> Option<&str>
    fn port(self) -> Option<Int>
    fn path(self) -> &str
    fn query(self) -> Option<&str>
    fn fragment(self) -> Option<&str>

    # Modification
    fn set_scheme(self, scheme: &str) -> Result<(), ()>
    fn set_host(self, host: Option<&str>)
    fn set_port(self, port: Option<Int>)
    fn set_path(self, path: &str)
    fn set_query(self, query: Option<&str>)
    fn set_fragment(self, fragment: Option<&str>)

    # Query parameters
    fn query_pairs(self) -> Iterator<(String, String)>
    fn with_query_pairs(self, pairs: Iterator<(&str, &str)>) -> Url
}

struct ParseError
```

**Usage Examples:**

```jet
# URL encoding
let query = "hello world & more"
let encoded = url::encode_component(query)  # "hello%20world%20%26%20more"

# URL parsing
let url = Url::parse("https://example.com/path?key=value#section")?
println!("Host: {}", url.host().unwrap())
println!("Path: {}", url.path())

# Building URLs
let mut url = Url::parse("https://api.example.com")?
url.set_path("/v1/users")
url.set_query(Some("page=1&limit=10"))
```

---

## Error Types

### Standard Error Hierarchy

```jet
# Base error trait
trait Error: Display + Debug {
    fn source(self) -> Option<&Error>
    fn description(self) -> &str
    fn cause(self) -> Option<&Error>
}

# Standard error types
struct Error

impl Error {
    fn new(message: impl AsRef<str>) -> Error
    fn with_source(message: impl AsRef<str>, source: impl Error) -> Error
}

# Common error categories
enum IOError {
    NotFound { path: String },
    PermissionDenied { path: String },
    AlreadyExists { path: String },
    InvalidInput { message: String },
    InvalidData { message: String },
    UnexpectedEof,
    WriteZero,
    Interrupted,
    Other { message: String },
}

enum NetworkError {
    ConnectionRefused,
    ConnectionReset,
    ConnectionAborted,
    NotConnected,
    AddrInUse,
    AddrNotAvailable,
    BrokenPipe,
    TimedOut,
    Interrupted,
    HostNotFound,
    Other { message: String },
}

enum ParseError {
    InvalidSyntax { message: String, position: Int },
    UnexpectedToken { expected: String, found: String },
    Incomplete,
}

enum ConversionError {
    OutOfRange,
    InvalidFormat,
    IncompatibleTypes { from: TypeId, to: TypeId },
}
```

### Display and Debug Traits

```jet
# Display for user-facing output
trait Display {
    fn fmt(self, f: Formatter) -> Result<(), FormatError}
}

# Debug for developer output
trait Debug {
    fn fmt(self, f: Formatter) -> Result<(), FormatError}
}

# Formatter
struct Formatter

impl Formatter {
    fn write_str(self, s: &str) -> Result<(), FormatError>
    fn write_char(self, c: Char) -> Result<(), FormatError>
    fn write_debug(self, value: impl Debug) -> Result<(), FormatError>
    fn write_display(self, value: impl Display) -> Result<(), FormatError>
}

# Derive macros
macro derive_Display!(...)
macro derive_Debug!(...)
```

**Usage Examples:**

```jet
# Custom error type
struct ValidationError {
    field: String,
    message: String,
}

impl Display for ValidationError {
    fn fmt(self, f: Formatter) -> Result<(), FormatError> {
        f.write_str(format!("Validation failed for '{}': {}", self.field, self.message))
    }
}

impl Debug for ValidationError {
    fn fmt(self, f: Formatter) -> Result<(), FormatError> {
        f.write_str(format!("ValidationError {{ field: {:?}, message: {:?} }}",
            self.field, self.message))
    }
}

# Using errors
fn validate_user(user: User) -> Result<(), ValidationError> {
    if user.name.is_empty() {
        return Err(ValidationError {
            field: "name".to_string(),
            message: "Name cannot be empty".to_string(),
        })
    }
    Ok(())
}
```

---

## Traits (Type Classes)

### Comparison Traits

```jet
# Equality
trait Eq {
    fn eq(self, other: Self) -> Bool
    fn ne(self, other: Self) -> Bool { !self.eq(other) }
}

# Ordering
enum Ordering {
    Less,
    Equal,
    Greater,
}

trait Ord: Eq {
    fn cmp(self, other: Self) -> Ordering
    fn max(self, other: Self) -> Self
    fn min(self, other: Self) -> Self
    fn clamp(self, min: Self, max: Self) -> Self
}

# Partial equality (for floats, etc.)
trait PartialEq {
    fn eq(self, other: Self) -> Bool
}

# Partial ordering
trait PartialOrd: PartialEq {
    fn partial_cmp(self, other: Self) -> Option<Ordering>
    fn lt(self, other: Self) -> Bool
    fn le(self, other: Self) -> Bool
    fn gt(self, other: Self) -> Bool
    fn ge(self, other: Self) -> Bool
}
```

### Display Traits

```jet
trait Show: Display  # Alias for Display

trait Debug {
    fn debug_fmt(self, f: Formatter) -> Result<(), FormatError}
}

# Pretty printing
trait PrettyDebug: Debug {
    fn pretty_debug_fmt(self, f: Formatter) -> Result<(), FormatError}
}
```

### Hash Trait

```jet
trait Hash {
    fn hash<H: Hasher>(self, state: H)
    fn hash_slice<H: Hasher>(data: &[Self], state: H) where Self: Sized
}

trait Hasher {
    fn finish(self) -> Int
    fn write(self, bytes: &[u8])
    fn write_u8(self, i: u8)
    fn write_u16(self, i: u16)
    fn write_u32(self, i: u32)
    fn write_u64(self, i: u64)
    fn write_usize(self, i: Int)
    fn write_i8(self, i: i8)
    fn write_i16(self, i: i16)
    fn write_i32(self, i: i32)
    fn write_i64(self, i: i64)
    fn write_isize(self, i: Int)
}

# BuildHasher for Map/Set
trait BuildHasher {
    type Hasher: Hasher
    fn build_hasher(self) -> Self::Hasher
}

# Default hasher
struct DefaultHasher
impl Hasher for DefaultHasher
impl BuildHasher for DefaultHasher
```

### Clone and Copy

```jet
# Clone (explicit copy)
trait Clone {
    fn clone(self) -> Self
    fn clone_from(self, source: Self)
}

# Copy (implicit, bitwise copy)
trait Copy: Clone {}

# Auto-implemented for:
# - Primitive types (Int, Float, Bool, Char)
# - Arrays of Copy types
# - Tuples of Copy types
```

### Drop (Destructors)

```jet
trait Drop {
    fn drop(self)
}

# Auto-called when value goes out of scope
```

### Concurrency Traits

```jet
# Send: safe to send to another thread
trait Send {}

# Auto-implemented when:
# - All fields implement Send
# - No raw pointers (unless marked unsafe)

# Sync: safe to share between threads
trait Sync {}

# Auto-implemented when:
# - &T implements Send
# - All fields implement Sync

# Marker traits for unsafe impls
unsafe impl Send for MyType {}
unsafe impl Sync for MyType {}
```

### Conversion Traits

```jet
# From: infallible conversion
trait From<T> {
    fn from(value: T) -> Self
}

# Into: fallible conversion (auto-implemented if From exists)
trait Into<T> {
    fn into(self) -> T
}

# TryFrom: fallible conversion
trait TryFrom<T> {
    type Error
    fn try_from(value: T) -> Result<Self, Self::Error>
}

# TryInto: fallible conversion (auto-implemented if TryFrom exists)
trait TryInto<T> {
    type Error
    fn try_into(self) -> Result<T, Self::Error>
}

# AsRef: reference conversion
trait AsRef<T> {
    fn as_ref(self) -> &T
}

# AsMut: mutable reference conversion
trait AsMut<T> {
    fn as_mut(self) -> &mut T
}

# Borrow: for hash map keys
trait Borrow<T> {
    fn borrow(self) -> &T
}

# ToOwned: create owned version
trait ToOwned {
    type Owned
    fn to_owned(self) -> Self::Owned
}
```

### Default Trait

```jet
trait Default {
    fn default() -> Self
}

# Derive macro
macro derive_Default!(...)
```

### Iterator Traits

```jet
trait Iterator<T> {
    type Item = T
    fn next(self) -> Option<T>
    ...  # See Collections section
}

trait IntoIterator {
    type Item
    type IntoIter: Iterator<Self::Item>
    fn into_iter(self) -> Self::IntoIter
}

trait FromIterator<T> {
    fn from_iter(iter: Iterator<T>) -> Self
}

trait Extend<T> {
    fn extend(self, iter: Iterator<T>)
}
```

### Deref Traits

```jet
trait Deref<T> {
    fn deref(self) -> &T
}

trait DerefMut<T>: Deref<T> {
    fn deref_mut(self) -> &mut T
}
```

---

## Platform Abstraction

### OS-Specific APIs

```jet
module std::os

# Unix-specific
#[cfg(unix)]
module std::os::unix {
    trait OsExt {
        fn as_raw_fd(self) -> RawFd
        fn into_raw_fd(self) -> RawFd
    }

    impl OsExt for File
    impl OsExt for TcpStream
    impl OsExt for UdpSocket

    fn symlink(original: impl AsPath, link: impl AsPath) -> Result<(), IOError} with IO
    fn symlink_metadata(path: impl AsPath) -> Result<Metadata, IOError} with IO
    fn hard_link(original: impl AsPath, link: impl AsPath) -> Result<(), IOError} with IO
}

# Windows-specific
#[cfg(windows)]
module std::os::windows {
    trait OsExt {
        fn as_raw_handle(self) -> RawHandle
        fn into_raw_handle(self) -> RawHandle
    }

    impl OsExt for File
    impl OsExt for TcpStream

    fn symlink_file(original: impl AsPath, link: impl AsPath) -> Result<(), IOError} with IO
    fn symlink_dir(original: impl AsPath, link: impl AsPath) -> Result<(), IOError} with IO
}

# macOS-specific
#[cfg(macos)]
module std::os::macos {
    # Platform-specific extensions
}

# Linux-specific
#[cfg(linux)]
module std::os::linux {
    # Platform-specific extensions
}
```

### FFI to C

```jet
module std::ffi

# C string types
struct CString  # Owned, null-terminated
struct CStr     # Borrowed, null-terminated

impl CString {
    fn new<T: Into<Vec<u8>>>(t: T) -> Result<CString, NulError>
    fn from_vec_unchecked(v: Vec<u8>) -> CString
    fn as_bytes(self) -> &[u8]
    fn as_bytes_with_nul(self) -> &[u8]
    fn as_c_str(self) -> &CStr
    fn into_raw(self) -> *mut c_char
    unsafe fn from_raw(ptr: *mut c_char) -> CString
}

impl CStr {
    unsafe fn from_ptr<'a>(ptr: *const c_char) -> &'a CStr
    fn to_bytes(self) -> &[u8]
    fn to_bytes_with_nul(self) -> &[u8]
    fn to_str(self) -> Result<&str, Utf8Error>
    fn to_string_lossy(self) -> Cow<str>
    fn as_ptr(self) -> *const c_char
}

# C types
module std::ffi::ctypes {
    type c_void
    type c_char
    type c_schar
    type c_uchar
    type c_short
    type c_ushort
    type c_int
    type c_uint
    type c_long
    type c_ulong
    type c_longlong
    type c_ulonglong
    type c_float
    type c_double
}

# Platform-specific integer types
module std::ffi::ctypes {
    type c_size_t
    type c_ssize_t
    type c_ptrdiff_t
}

# Null pointer constant
const NULL: *mut c_void

# FFI functions
unsafe fn transmute<T, U>(value: T) -> U

# Pointer operations
fn raw_ptr<T>(value: &T) -> *const T
fn raw_ptr_mut<T>(value: &mut T) -> *mut T
fn dereference<T>(ptr: *const T) -> Option<&T>
unsafe fn dereference_unchecked<T>(ptr: *const T) -> &T

# External function declaration
extern "C" {
    fn c_function(arg: c_int) -> c_int
}

# Calling convention specifiers
extern "C"        # C calling convention
extern "system"   # Platform default
extern "stdcall"  # Windows stdcall
extern "fastcall" # Fast calling convention
extern "C" fn callback()  # Function with C calling convention
```

### Process Management

```jet
module std::process

struct Command

impl Command {
    fn new(program: impl AsRef<str>) -> Command
    fn arg(self, arg: impl AsRef<str>) -> Command
    fn args(self, args: Iterator<impl AsRef<str>>) -> Command
    fn env(self, key: impl AsRef<str>, val: impl AsRef<str>) -> Command
    fn env_remove(self, key: impl AsRef<str>) -> Command
    fn env_clear(self) -> Command
    fn current_dir(self, dir: impl AsRef<Path>) -> Command
    fn stdin(self, cfg: Stdio) -> Command
    fn stdout(self, cfg: Stdio) -> Command
    fn stderr(self, cfg: Stdio) -> Command

    fn spawn(self) -> Result<Child, IOError} with IO
    fn output(self) -> Result<Output, IOError} with IO
    fn status(self) -> Result<ExitStatus, IOError} with IO
}

struct Child {
    stdin: Option<ChildStdin>,
    stdout: Option<ChildStdout>,
    stderr: Option<ChildStderr>,
}

impl Child {
    fn kill(self) -> Result<(), IOError} with IO
    fn wait(self) -> Result<ExitStatus, IOError} with IO
    fn try_wait(self) -> Result<Option<ExitStatus>, IOError}
    fn wait_with_output(self) -> Result<Output, IOError} with IO
}

struct ExitStatus

impl ExitStatus {
    fn success(self) -> Bool
    fn code(self) -> Option<Int>
    fn signal(self) -> Option<Int>  # Unix only
}

struct Output {
    status: ExitStatus,
    stdout: Vec<u8>,
    stderr: Vec<u8>,
}

enum Stdio {
    Inherit,
    Piped,
    Null,
    From(File),
}

# Current process
fn exit(code: Int) -> Never
fn abort() -> Never
fn current_exe() -> Result<PathBuf, IOError} with IO
fn current_dir() -> Result<PathBuf, IOError} with IO
fn set_current_dir(path: impl AsRef<Path>) -> Result<(), IOError} with IO
fn args() -> Iterator<String>
fn vars() -> Iterator<(String, String)>
fn var(key: impl AsRef<str>) -> Result<String, VarError}
```

### Environment

```jet
module std::env

fn current_dir() -> Result<PathBuf, IOError} with IO
fn set_current_dir(path: impl AsRef<Path>) -> Result<(), IOError} with IO

fn home_dir() -> Option<PathBuf>
fn temp_dir() -> PathBuf
fn current_exe() -> Result<PathBuf, IOError} with IO

fn args() -> Iterator<String>
fn args_os() -> Iterator<OsString>

fn vars() -> Iterator<(String, String)>
fn vars_os() -> Iterator<(OsString, OsString)>
fn var(key: impl AsRef<str>) -> Result<String, VarError}
fn var_os(key: impl AsRef<str>) -> Option<OsString>
fn set_var(key: impl AsRef<str>, value: impl AsRef<str>)
fn remove_var(key: impl AsRef<str>)

fn split_paths<T: AsRef<OsStr>>(paths: T) -> Iterator<PathBuf>
fn join_paths(paths: Iterator<PathBuf>) -> Result<OsString, JoinPathsError>

const ARCH: &str
const FAMILY: &str  # "unix" or "windows"
const OS: &str
const DLL_EXTENSION: &str
const DLL_PREFIX: &str
const DLL_SUFFIX: &str
const EXE_EXTENSION: &str
const EXE_SUFFIX: &str
```

### Path Handling

```jet
module std::path

struct PathBuf  # Owned path
struct Path     # Path slice

impl PathBuf {
    fn new() -> PathBuf
    fn from(s: impl AsRef<str>) -> PathBuf
    fn push(self, path: impl AsRef<Path>)
    fn pop(self) -> Bool
    fn set_file_name(self, file_name: impl AsRef<str>)
    fn set_extension(self, extension: impl AsRef<str>) -> Bool
    fn into_os_string(self) -> OsString
    fn as_path(self) -> &Path
}

impl Path {
    fn new(s: &str) -> &Path
    fn to_path_buf(self) -> PathBuf
    fn to_str(self) -> Option<&str>
    fn to_string_lossy(self) -> Cow<str>
    fn to_os_str(self) -> &OsStr

    # Component queries
    fn is_absolute(self) -> Bool
    fn is_relative(self) -> Bool
    fn has_root(self) -> Bool
    fn parent(self) -> Option<&Path>
    fn ancestors(self) -> Iterator<&Path>
    fn file_name(self) -> Option<&OsStr>
    fn file_stem(self) -> Option<&OsStr>
    fn extension(self) -> Option<&OsStr>
    fn components(self) -> Iterator<Component>

    # Path operations
    fn join(self, path: impl AsRef<Path>) -> PathBuf
    fn with_file_name(self, file_name: impl AsRef<str>) -> PathBuf
    fn with_extension(self, extension: impl AsRef<str>) -> PathBuf
    fn canonicalize(self) -> Result<PathBuf, IOError} with IO
    fn read_link(self) -> Result<PathBuf, IOError} with IO
    fn metadata(self) -> Result<Metadata, IOError} with IO
    fn symlink_metadata(self) -> Result<Metadata, IOError} with IO
    fn exists(self) -> Bool with IO
    fn is_file(self) -> Bool with IO
    fn is_dir(self) -> Bool with IO
    fn is_symlink(self) -> Bool with IO
    fn starts_with(self, base: impl AsRef<Path>) -> Bool
    fn ends_with(self, child: impl AsRef<Path>) -> Bool
    fn strip_prefix(self, base: impl AsRef<Path>) -> Result<&Path, StripPrefixError>
}

enum Component {
    Prefix(PrefixComponent),
    RootDir,
    CurDir,
    ParentDir,
    Normal(OsStr),
}
```

**Usage Examples:**

```jet
# Path manipulation
let path = PathBuf::from("/usr/local/bin")
let joined = path.join("myapp")
println!("{}", joined.display())  # /usr/local/bin/myapp

# Path components
if let Some(filename) = path.file_name() {
    println!("File: {}", filename.to_string_lossy())
}

if let Some(parent) = path.parent() {
    println!("Parent: {}", parent.display())
}

# Checking existence
if path.exists() {
    println!("Path exists")
}

# FFI example
extern "C" {
    fn strlen(s: *const c_char) -> c_size_t
}

fn get_c_string_length(s: &CStr) -> Int {
    unsafe { strlen(s.as_ptr()) as Int }
}

# Process spawning
let output = Command::new("ls")
    .arg("-la")
    .current_dir("/tmp")
    .output()?

if output.status.success() {
    println!("{}", String::from_utf8_lossy(output.stdout))
}

# Environment
if let Ok(home) = env::var("HOME") {
    println!("Home: {}", home)
}

for (key, value) in env::vars() {
    println!("{}={}", key, value)
}
```

---

## Summary

This standard library design provides:

1. **Rich Core Types**: Option, Result, and primitive type methods with comprehensive APIs
2. **Powerful Collections**: Vec, Map, Set with efficient implementations
3. **Robust IO**: File operations, buffered IO, and standard streams
4. **Modern Networking**: TCP/UDP sockets and high-level HTTP client
5. **Structured Concurrency**: Tasks, channels, and synchronization primitives
6. **Time Handling**: Duration, Instant, DateTime with timezone support
7. **Encoding Support**: JSON, Base64, URL encoding built-in
8. **Clear Error Handling**: Hierarchical error types with Display/Debug
9. **Comprehensive Traits**: Eq, Ord, Hash, Clone, Send, Sync, and more
10. **Platform Abstraction**: OS-specific APIs and FFI support

The design emphasizes:
- **Safety**: Thread-safe by default, explicit error handling
- **Performance**: Zero-cost abstractions, efficient implementations
- **Ergonomics**: Python-like syntax, rich APIs
- **Composability**: Iterator chains, effect handlers, trait-based design
