# Jet

<p align="center">
  <img alt="Rust" src="https://img.shields.io/badge/Rust-1.70+-f74c00?logo=rust&logoColor=white">
  <img alt="LLVM" src="https://img.shields.io/badge/LLVM-21.1.x-5c4094?logo=llvm&logoColor=white">
  <img alt="License" src="https://img.shields.io/badge/License-MIT-3da639?logo=openstreetmap&logoColor=white">
  <img alt="Platform" src="https://img.shields.io/badge/Platform-macOS%20%7C%20Linux-0078d4?logo=linux&logoColor=white">
</p>

<p align="center">
  <b>The systems language that reads like poetry</b>
</p>

<p align="center">
  <a href="#-why-jet">Why Jet?</a> •
  <a href="#-quick-example">Quick Example</a> •
  <a href="#-features">Features</a> •
  <a href="#-installation">Installation</a> •
  <a href="#-documentation">Documentation</a>
</p>

---

## Why Jet?

Jet is a systems programming language designed at the intersection of **readability**, **safety**, and **expressiveness**.

```jet
fn main():
    let numbers = [1, 2, 3, 4, 5]
    let sum = numbers.filter(fn(n) -> n % 2 == 0).sum()
    print(f"Sum of evens: {sum}")
```

**Beautiful by design.** Python-inspired syntax with significant indentation—no semicolons, minimal punctuation, maximum clarity.

**Safe by default.** Rust-level memory safety and type guarantees without the complexity. No null pointers, no data races, no undefined behavior.

**Fast when it matters.** Compiles to native code via LLVM. Zero-cost abstractions. Production-ready performance.

---

## Quick Example

```jet
import std::sync::chan

### A concurrent pipeline that squares numbers
fn main():
    let (tx, rx) = chan::new(10)
    
    concurrent:
        spawn producer(tx)
        spawn consumer(rx)

fn producer(tx: chan[int]) -> int ! chan::SendError:
    for i in 1..=5:
        tx.send(i * i)?
    return 5

fn consumer(rx: chan[int]) -> int ! chan::RecvError:
    let mut total = 0
    loop:
        match rx.recv():
            | Ok(n) -> total += n
            | Err(chan::RecvError::Closed) -> break
    return total
```

---

## Features

### 🎨 Elegant Syntax

```jet
# Type inference—let the compiler do the work
let name = "Jet"
let version = 1.0
let features = ["safe", "fast", "readable"]

# Pattern matching with guards
match score:
    | n if n >= 90 => "A"
    | n if n >= 80 => "B"
    | _            => "C"

# If as an expression
let category = if age < 13: "child" else: "adult"
```

### 🛡️ Zero-Cost Safety

```jet
# No null—use Option
fn find_user(id: int) -> Option[User]:
    # Returns Some(user) or None

# Explicit effects—errors are tracked
fn parse_config(path: string) -> Config ! ParseError | IoError:
    # The ! declares what can fail

# Ownership without the headache
fn process(data: [int]) -> [int]:
    # Immutable by default, borrow-checked at compile time
```

### 🚀 Fearless Concurrency

```jet
# Structured concurrency—tasks complete together
concurrent:
    let a = spawn fetch_user(id)
    let b = spawn fetch_orders(user_id)
    process(a.await()?, b.await()?)

# Channels for communication
let (tx, rx) = chan::new(100)
spawn producer(tx)
spawn consumer(rx)

# Select for multiple operations
select:
    | msg = ch1.recv() => handle(msg)
    | msg = ch2.recv() => handle(msg)
    | 5s               => log("timeout")
```

### 🔧 Powerful Abstractions

```jet
# Traits for shared behavior
trait Drawable:
    fn draw(self)
    fn area(self) -> float

# Generic with constraints
fn max[T: Comparable](a: T, b: T) -> T:
    if a.compare(b) > 0: a else: b

# Enum variants with data
enum HttpResponse:
    | Ok(body: string, status: int)
    | Err(error: string, code: int)
```

---

## Installation

### Quick Install

```bash
# macOS
brew tap Icarus603/jet
brew install jet-lang
```

```bash
# Linux
curl -fsSL https://raw.githubusercontent.com/Icarus603/jet/main/scripts/install.sh | bash
```

### Prerequisites

- Rust toolchain (1.70+)
- LLVM 21.1.x
- Supported host OS: macOS and Linux only

### Build From Source (macOS)

```bash
brew install llvm@21
export LLVM_SYS_211_PREFIX=/opt/homebrew/opt/llvm@21

git clone https://github.com/Icarus603/jet
cd jet && cargo build --release
```

### Build From Source (Linux)

```bash
# Ubuntu/Debian
sudo apt-get install llvm-21-dev libclang-21-dev
export LLVM_SYS_211_PREFIX=/usr/lib/llvm-21

git clone https://github.com/Icarus603/jet
cd jet && cargo build --release
```

Windows is intentionally unsupported in Jet 1.0.

---

## Documentation

| Resource | Description |
|----------|-------------|
| [User Guide](docs/user-guide.md) | Learn Jet from the ground up |
| [Language Spec](docs/language-spec.md) | Complete syntax and semantics |
| [Standard Library](docs/stdlib-design.md) | APIs and best practices |
| [Architecture](docs/compiler-architecture.md) | How the compiler works |
| [Contributing](CONTRIBUTING.md) | Get involved |

---

## Philosophy

> **Code is read more often than it is written.**

Jet prioritizes:

1. **Clarity over cleverness** — If it's hard to read, it's wrong
2. **Safety without ceremony** — The compiler works for you, not against you
3. **Fearless refactoring** — Strong types catch bugs before they ship
4. **Concurrent by design** — Parallelism should be easy and safe

---

## Status

Jet is under active development. See [PRODUCTION_READINESS.md](PRODUCTION_READINESS.md) for current status and roadmap.

---

<p align="center">
  Built with care by the Jet team and contributors
</p>

<p align="center">
  <a href="LICENSE">MIT License</a>
</p>
