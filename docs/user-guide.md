# Jet User Guide

Welcome to Jet! This guide will help you learn the Jet programming language from the basics to advanced topics.

## Table of Contents

1. [Installation](#installation)
2. [Hello World](#hello-world)
3. [Language Fundamentals](#language-fundamentals)
4. [Control Flow](#control-flow)
5. [Structs and Enums](#structs-and-enums)
6. [Error Handling](#error-handling)
7. [Traits](#traits)
8. [Generics](#generics)
9. [Async and Concurrency](#async-and-concurrency)
10. [Standard Library](#standard-library)
11. [Common Patterns](#common-patterns)

---

## 1.0 Support Matrix

| Area | Status | Notes |
|------|--------|-------|
| Core language (lexer/parser/typecheck/codegen) | Supported | Production target for 1.0 |
| `jet build`, `jet run`, `jet check`, `jet fmt` | Supported | Primary workflows |
| Workspace builds | Supported | All members are compiled |
| `jet test` | Supported | Discovers and executes Jet test functions via compile/run |
| Registry publish/install/search/show | Experimental | Remote registry operations are not fully enabled in 1.0 |
| Advanced LSP features (inlay hints/signature help/selection ranges/workspace symbols) | Experimental | Planned for post-1.0 iterations |

---

## Installation

### Quick Install

macOS (Homebrew):

```bash
brew tap Icarus603/jet
brew install jet
```

Linux (installer script):

```bash
curl -fsSL https://raw.githubusercontent.com/Icarus603/jet/main/scripts/install.sh | bash
```

### Prerequisites

- Rust toolchain (1.70+ recommended)
- LLVM 21.1.x (for code generation)
- Supported host OS: macOS and Linux only

### Build From Source (macOS)

```bash
# Install LLVM 21
brew install llvm@21

# Set environment variable
export LLVM_SYS_211_PREFIX=/opt/homebrew/opt/llvm@21

# Clone and build
git clone https://github.com/Icarus603/jet
cd jet
cargo build --release

# Add to PATH
export PATH="$PWD/target/release:$PATH"
```

### Build From Source (Linux)

```bash
# Install LLVM (Ubuntu/Debian)
sudo apt-get install llvm-21-dev libclang-21-dev

# Set environment variable
export LLVM_SYS_211_PREFIX=/usr/lib/llvm-21

# Clone and build
git clone https://github.com/Icarus603/jet
cd jet
cargo build --release
```

Windows is intentionally unsupported in Jet 1.0.

---

## Toolchain Support Policy

- Jet currently supports LLVM **21.1.x** only.
- Compatibility with LLVM 17 or LLVM 21.2+ is not guaranteed.
- LLVM patch upgrades are driven by automated reminders and merged only after full validation passes.

---

## Hello World

Create your first Jet program:

```jet
# hello.jet
fn main():
    print("Hello, World!")
```

Run it:

```bash
jet run hello.jet
```

Or compile and run:

```bash
jet build hello.jet -o hello
./hello
```

---

## Language Fundamentals

### Variables

Jet uses `let` for variable declarations:

```jet
# Immutable by default
let name = "Jet"

# Mutable variables
let mut counter = 0
counter = counter + 1

# Type annotations (optional)
let age: int = 5
let pi: float = 3.14159
let is_cool: bool = true

# Constants
const MAX_SIZE = 100
```

### Primitive Types

| Type | Description | Examples |
|------|-------------|----------|
| `int` | Signed integer | `42`, `-17` |
| `float` | 64-bit floating point | `3.14`, `-0.5` |
| `bool` | Boolean | `true`, `false` |
| `char` | Single Unicode character | `'a'`, `'☺'` |
| `string` | UTF-8 string | `"hello"` |
| `unit` | Empty type | `()` |

### Comments

```jet
# Single-line comment

###
Multi-line comment
Useful for longer explanations
###
```

### Functions

```jet
# Basic function
fn greet(name: string):
    print(f"Hello, {name}!")

# Function with return type
fn add(a: int, b: int) -> int:
    return a + b

# Multiple parameters
fn describe(name: string, age: int):
    print(f"{name} is {age} years old")

# Early return
fn max(a: int, b: int) -> int:
    if a > b:
        return a
    return b
```

---

## Control Flow

### If/Else

```jet
let score = 85

if score >= 90:
    print("A")
elif score >= 80:
    print("B")
elif score >= 70:
    print("C")
else:
    print("F")

# If as expression
let category = if age < 13: "child" else: "adult"
```

### Loops

```jet
# While loop
let mut i = 0
while i < 10:
    print(i)
    i = i + 1

# For loop with range
for i in 0..10:       # 0 to 9
    print(i)

for i in 0..=10:      # 0 to 10 (inclusive)
    print(i)

# Iterate over collection
let colors = ["red", "green", "blue"]
for color in colors:
    print(color)

# With index
for i, color in colors.enumerate():
    print(f"{i}: {color}")
```

### Match

```jet
match value:
    | 1 => print("one")
    | 2 => print("two")
    | 3 | 4 | 5 => print("few")
    | n if n > 100 => print("many")
    | _ => print("some")
```

---

## Structs and Enums

### Structs

```jet
struct Point:
    x: float
    y: float

struct Person:
    name: string
    age: int

# Create instances
let p = Point { x: 1.0, y: 2.0 }
let person = Person { name: "Alice", age: 30 }

# Access fields
print(p.x)
print(person.name)
```

### Impl Blocks

```jet
impl Point:
    # Constructor
    fn new(x: float, y: float) -> Point:
        return Point { x: x, y: y }

    # Instance method
    fn distance(self, other: Point) -> float:
        let dx = self.x - other.x
        let dy = self.y - other.y
        return sqrt(dx * dx + dy * dy)

    # Method without self
    fn origin() -> Point:
        return Point { x: 0.0, y: 0.0 }
```

### Enums

```jet
# Simple enum
enum Direction:
    | North
    | South
    | East
    | West

# Enum with data
enum Message:
    | Quit
    | Move(x: int, y: int)
    | Write(text: string)
    | ChangeColor(r: int, g: int, b: int)

# Use with match
let msg = Move(10, 20)

match msg:
    | Quit => print("Quitting")
    | Move(x, y) => print(f"Moving to ({x}, {y})")
    | Write(text) => print(text)
    | ChangeColor(r, g, b) => print(f"RGB({r}, {g}, {b})")
```

---

## Error Handling

Jet uses an explicit effects system for error handling.

### Defining Errors

```jet
enum ParseError:
    | InvalidFormat
    | OutOfRange
    | EmptyInput
```

### Functions That Can Fail

```jet
# Function declares it can raise ParseError
fn parse_number(input: string) -> int ! ParseError:
    if input.len() == 0:
        raise EmptyInput
    # ... parse logic
    return result
```

### Handling Errors

```jet
# Using match
match parse_number("42"):
    | Ok(n) => print(f"Parsed: {n}")
    | Err(EmptyInput) => print("Empty input")
    | Err(InvalidFormat) => print("Invalid format")
    | Err(OutOfRange) => print("Out of range")

# Using try operator (propagates errors)
fn calculate(input: string) -> int ! ParseError:
    let n = parse_number(input)?  # If parse fails, returns early
    return n * 2
```

### Option Type

```jet
enum Option[T]:
    | Some(T)
    | None

fn find(arr: [int], target: int) -> Option[int]:
    for i, v in arr.enumerate():
        if v == target:
            return Some(i)
    return None

# Using Option
match find(numbers, 42):
    | Some(index) => print(f"Found at {index}")
    | None => print("Not found")

# With default
let value = maybe_value.unwrap_or(default)
```

---

## Traits

Traits define shared behavior.

### Defining Traits

```jet
trait Drawable:
    fn draw(self)
    fn area(self) -> float

trait Comparable:
    fn compare(self, other: Self) -> int
```

### Implementing Traits

```jet
struct Circle:
    radius: float

impl Drawable for Circle:
    fn draw(self):
        print(f"Drawing circle with radius {self.radius}")

    fn area(self) -> float:
        return 3.14159 * self.radius * self.radius

impl Comparable for Circle:
    fn compare(self, other: Circle) -> int:
        # Compare by area
        let self_area = self.area()
        let other_area = other.area()
        if self_area < other_area: return -1
        if self_area > other_area: return 1
        return 0
```

### Generic Functions with Traits

```jet
fn max[T: Comparable](a: T, b: T) -> T:
    if a.compare(b) > 0:
        return a
    return b

fn render[T: Drawable](shape: T):
    shape.draw()
    print(f"Area: {shape.area()}")
```

---

## Generics

### Generic Functions

```jet
fn identity[T](value: T) -> T:
    return value

fn pair[T, U](first: T, second: U) -> (T, U):
    return (first, second)
```

### Generic Structs

```jet
struct Container[T]:
    value: T

impl[T] Container[T]:
    fn new(value: T) -> Container[T]:
        return Container { value: value }

    fn get(self) -> T:
        return self.value

# Usage
let int_box = Container::new(42)
let str_box = Container::new("hello")
```

### Generic Constraints

```jet
fn find_max[T: Comparable](items: [T]) -> Option[T]:
    if items.len() == 0:
        return None

    let mut max = items[0]
    for item in items:
        if item.compare(max) > 0:
            max = item

    return Some(max)
```

---

## Async and Concurrency

### Async Functions

```jet
async fn fetch_data(url: string) -> string ! NetworkError:
    let response = await http_get(url)
    return response.body

async fn process_items(items: [string]) -> [int]:
    let mut results = []
    for item in items:
        let processed = await process(item)
        results.push(processed)
    return results
```

### Concurrent Execution

```jet
async fn fetch_multiple(urls: [string]) -> [string]:
    concurrent:
        # Spawn tasks concurrently
        let t1 = spawn fetch_data(urls[0])
        let t2 = spawn fetch_data(urls[1])
        let t3 = spawn fetch_data(urls[2])

        # Await all results
        return [await t1, await t2, await t3]
```

### Channels

```jet
async fn producer_consumer():
    let (tx, rx) = channel[int]()

    concurrent:
        # Producer
        spawn:
            for i in 0..10:
                await tx.send(i)

        # Consumer
        spawn:
            while let Some(value) = await rx.recv():
                print(f"Received: {value}")
```

### Select

```jet
async fn select_example():
    let (tx1, rx1) = channel[string]()
    let (tx2, rx2) = channel[string]()

    concurrent:
        spawn slow_sender(tx1, "first", 200)
        spawn slow_sender(tx2, "second", 100)

        select:
            | msg = rx1.recv() => print(f"From first: {msg}")
            | msg = rx2.recv() => print(f"From second: {msg}")
```

---

## Standard Library

### Collections

```jet
# Arrays
let arr = [1, 2, 3, 4, 5]
arr.push(6)
let last = arr.pop()

# Maps
let mut scores = {"Alice": 95, "Bob": 87}
scores["Charlie"] = 92

for name, score in scores:
    print(f"{name}: {score}")
```

### Common Operations

```jet
# Iteration methods
let doubled = numbers.map(fn(x) -> x * 2)
let evens = numbers.filter(fn(x) -> x % 2 == 0)
let sum = numbers.fold(0, fn(acc, x) -> acc + x)

# Find
match items.find(fn(x) -> x > 10):
    | Some(x) => print(f"Found: {x}")
    | None => print("Not found")

# Sorting
let sorted = items.sort()
let sorted_by = items.sort_by(fn(x) -> x.value)
```

### I/O

```jet
# File reading
let content = read_file("data.txt")?

# File writing
write_file("output.txt", "Hello")?

# Buffered I/O
let file = open("large.txt")?
for line in file.lines():
    process(line)
```

---

## Common Patterns

### Builder Pattern

```jet
struct RequestBuilder:
    url: string
    method: string
    headers: Map[string, string]
    body: Option[string]

impl RequestBuilder:
    fn new(url: string) -> RequestBuilder:
        return RequestBuilder {
            url: url,
            method: "GET",
            headers: {},
            body: None
        }

    fn with_method(self, method: string) -> RequestBuilder:
        return RequestBuilder {
            ..self,
            method: method
        }

    fn with_header(self, key: string, value: string) -> RequestBuilder:
        let mut new_headers = self.headers
        new_headers[key] = value
        return RequestBuilder {
            ..self,
            headers: new_headers
        }

    fn build(self) -> Request:
        return Request {
            url: self.url,
            method: self.method,
            headers: self.headers,
            body: self.body
        }

# Usage
let request = RequestBuilder::new("https://api.example.com")
    .with_method("POST")
    .with_header("Content-Type", "application/json")
    .build()
```

### RAII Pattern

```jet
struct File:
    path: string
    handle: FileHandle

impl File:
    fn open(path: string) -> Result[File, FileError]:
        let handle = os_open(path)?
        return Ok(File { path: path, handle: handle })

    # Destructor called when File goes out of scope
    fn drop(self):
        os_close(self.handle)

# File is automatically closed when it goes out of scope
{
    let file = File::open("data.txt")?
    # ... use file
}  # File::drop called here
```

### State Machine with Enums

```jet
enum Connection:
    | Disconnected
    | Connecting
    | Connected(socket: Socket)
    | Error(message: string)

impl Connection:
    fn connect(self, address: string) -> Connection:
        match self:
            | Disconnected =>
                match create_socket(address):
                    | Ok(socket) => Connected(socket)
                    | Err(e) => Error(e.message)
            | _ => self

    fn disconnect(self) -> Connection:
        match self:
            | Connected(socket) =>
                socket.close()
                Disconnected
            | _ => self
```

### Type State Pattern

```jet
# Using phantom types to encode state at type level
struct Unauthenticated
struct Authenticated

struct Session[State]:
    token: string
    user_id: Option[int]

impl Session[Unauthenticated]:
    fn new() -> Session[Unauthenticated]:
        return Session { token: "", user_id: None }

    fn authenticate(self, credentials: Credentials)
            -> Result[Session[Authenticated], AuthError]:
        let token = verify_credentials(credentials)?
        return Ok(Session {
            token: token,
            user_id: Some(credentials.user_id)
        })

impl Session[Authenticated]:
    fn get_user_data(self) -> UserData:
        # Only callable on authenticated sessions
        return fetch_user_data(self.user_id.unwrap())
```

---

## Next Steps

- Explore the [examples/](../examples/) directory for complete programs
- Read the [language specification](language-spec.md) for detailed syntax
- Check out the [standard library documentation](stdlib-design.md)
- Join our community [Discord/Forum]

## Getting Help

- `jet --help` - CLI help
- `jet doc` - Open local documentation
- [GitHub Issues](https://github.com/Icarus603/jet/issues)
- [Documentation](https://jet-lang.org/docs)
