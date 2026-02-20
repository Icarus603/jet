<img src="https://raw.githubusercontent.com/Icarus603/jet/main/assets/logo.svg" width="120" align="right" alt="Jet Programming Language">

# Jet

**The systems language that reads like poetry**

[![Rust](https://img.shields.io/badge/Rust-1.70+-f74c00?logo=rust&logoColor=white)](https://www.rust-lang.org)
[![LLVM](https://img.shields.io/badge/LLVM-21.1.x-5c4094?logo=llvm&logoColor=white)](https://llvm.org)
[![License](https://img.shields.io/badge/License-MIT-3da639?logo=openstreetmap&logoColor=white)](LICENSE)
[![Platform](https://img.shields.io/badge/Platform-macOS%20|%20Linux-0078d4?logo=linux&logoColor=white)](#installation)
[![CI](https://github.com/Icarus603/jet/workflows/CI/badge.svg)](https://github.com/Icarus603/jet/actions)
[![Release](https://img.shields.io/github/v/release/Icarus603/jet?include_prereleases)](https://github.com/Icarus603/jet/releases)

[Why Jet?](#-why-jet) • [Quick Start](#-quick-start) • [Features](#-features) • [Installation](#-installation) • [Docs](#-documentation)

---

## ✨ Why Jet?

Jet is a systems programming language designed at the intersection of **readability**, **safety**, and **expressiveness**.

<table>
<tr>
<td width="33%" valign="top">

### 🎨 Beautiful
Python-inspired syntax with significant indentation—no semicolons, minimal punctuation, maximum clarity.

```jet
fn main():
    let numbers = [1, 2, 3, 4, 5]
    let sum = numbers
        .filter(fn(n) -> n % 2 == 0)
        .sum()
    print(f"Sum of evens: {sum}")
```

</td>
<td width="33%" valign="top">

### 🛡️ Safe
Rust-level memory safety without the complexity. No null pointers, no data races, no undefined behavior.

```jet
fn find_user(id: int) -> Option[User]:
    # Returns Some(user) or None

fn parse_config(path: string)
    -> Config ! ParseError | IoError:
    # The ! declares what can fail
```

</td>
<td width="33%" valign="top">

### 🚀 Fast
Compiles to native code via LLVM. Zero-cost abstractions. Production-ready performance.

```jet
concurrent:
    let a = spawn fetch_user(id)
    let b = spawn fetch_orders(user_id)
    process(a.await()?, b.await()?)
```

</td>
</tr>
</table>

---

## 🚀 Quick Start

### One-line install

**macOS** (Apple Silicon):
```bash
brew tap Icarus603/jet && brew install jet-lang
```

**Linux** (x64):
```bash
curl -fsSL https://raw.githubusercontent.com/Icarus603/jet/main/scripts/install.sh | bash
```

### Hello, Jet!

```bash
$ jet new hello && cd hello
$ echo 'fn main(): print("Hello, World!")' > src/main.jet
$ jet run
Hello, World!
```

### Concurrent Pipeline

```jet
import std::sync::chan

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

## 📦 Features

### Elegant Syntax

<table><tr><td>

```jet
# Type inference
let name = "Jet"
let features = ["safe", "fast", "readable"]

# Pattern matching
match score:
    | n if n >= 90 => "A"
    | n if n >= 80 => "B"
    | _            => "C"

# If as expression
let category = if age < 13: "child" else: "adult"
```

</td></tr></table>

### Zero-Cost Safety

<table><tr><td>

```jet
# No null—use Option
fn find_user(id: int) -> Option[User]:
    # Some(user) or None

# Explicit effects
fn parse_config(path: string)
    -> Config ! ParseError | IoError:
    # ! declares what can fail

# Ownership without headache
fn process(data: [int]) -> [int]:
    # Immutable by default
```

</td></tr></table>

### Fearless Concurrency

<table><tr><td>

```jet
# Structured concurrency
concurrent:
    let a = spawn fetch_user(id)
    let b = spawn fetch_orders(user_id)
    process(a.await()?, b.await()?)

# Channels
let (tx, rx) = chan::new(100)
spawn producer(tx)
spawn consumer(rx)

# Select
select:
    | msg = ch1.recv() => handle(msg)
    | msg = ch2.recv() => handle(msg)
    | 5s               => log("timeout")
```

</td></tr></table>

### Powerful Abstractions

<table><tr><td>

```jet
# Traits
trait Drawable:
    fn draw(self)
    fn area(self) -> float

# Generics
fn max[T: Comparable](a: T, b: T) -> T:
    if a.compare(b) > 0: a else: b

# Enums with data
enum HttpResponse:
    | Ok(body: string, status: int)
    | Err(error: string, code: int)
```

</td></tr></table>

---

## 📥 Installation

### Quick Install

| Platform | Command |
|----------|---------|
| **macOS** (Apple Silicon) | `brew tap Icarus603/jet && brew install jet-lang` |
| **Linux** (x64) | `curl -fsSL https://raw.githubusercontent.com/Icarus603/jet/main/scripts/install.sh \| bash` |

### Prerequisites

- Rust toolchain (1.70+)
- LLVM 21.1.x
- Supported: macOS, Linux (x64)

### Build From Source

<details>
<summary><b>macOS</b></summary>

```bash
brew install llvm@21
export LLVM_SYS_211_PREFIX=/opt/homebrew/opt/llvm@21

git clone https://github.com/Icarus603/jet
cd jet && cargo build --release
```
</details>

<details>
<summary><b>Linux (Ubuntu/Debian)</b></summary>

```bash
sudo apt-get install llvm-21-dev libclang-21-dev libpolly-21-dev
export LLVM_SYS_211_PREFIX=/usr/lib/llvm-21

git clone https://github.com/Icarus603/jet
cd jet && cargo build --release
```
</details>

> **Note:** Windows is intentionally unsupported in Jet 1.0.

---

## 📚 Documentation

| Resource | Description |
|----------|-------------|
| 📖 [User Guide](docs/user-guide.md) | Learn Jet from the ground up |
| 📘 [Language Spec](docs/language-spec.md) | Complete syntax and semantics |
| 🛠️ [Standard Library](docs/stdlib-design.md) | APIs and best practices |
| 🏗️ [Architecture](docs/compiler-architecture.md) | How the compiler works |
| 🤝 [Contributing](CONTRIBUTING.md) | Get involved |
| 📋 [Changelog](CHANGELOG.md) | Release history |

---

## 🎯 Philosophy

> **Code is read more often than it is written.**

Jet prioritizes:

1. **Clarity over cleverness** — If it's hard to read, it's wrong
2. **Safety without ceremony** — The compiler works for you, not against you
3. **Fearless refactoring** — Strong types catch bugs before they ship
4. **Concurrent by design** — Parallelism should be easy and safe

---

## 🗺️ Roadmap

- [x] **1.0.0** (Current) — Production release with full stdlib
- [ ] **1.1.0** — Package registry improvements, LSP enhancements
- [ ] **1.2.0** — Incremental compilation, IDE plugins
- [ ] **2.0.0** — WASM target, cross-compilation

---

## 📊 Stats

![GitHub stars](https://img.shields.io/github/stars/Icarus603/jet?style=social)
![GitHub forks](https://img.shields.io/github/forks/Icarus603/jet?style=social)

---

<div align="center">

Built with ❤️ by the Jet team and contributors

[MIT License](LICENSE) • [Report Issues](https://github.com/Icarus603/jet/issues)

</div>
