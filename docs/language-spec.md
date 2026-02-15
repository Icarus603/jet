# Jet Language Specification

**Version:** 1.0.0-draft
**Status:** Work in Progress

---

## Table of Contents

1. [Introduction](#1-introduction)
2. [Lexical Structure](#2-lexical-structure)
3. [Grammar](#3-grammar)
4. [Type System](#4-type-system)
5. [Effects and Error Handling](#5-effects-and-error-handling)
6. [Concurrency](#6-concurrency)
7. [Memory Model](#7-memory-model)
8. [Module System](#8-module-system)

---

## 1. Introduction

Jet is a statically-typed systems programming language with the following design goals:

- **Readable syntax**: Python-inspired significant indentation and minimal punctuation
- **Type safety**: Rust-level guarantees (no null, no unchecked exceptions, memory safety)
- **Aggressive type inference**: Write `let x = 5`, not `let x: int`
- **Effects tracking**: Functions declare what can fail via an effects system
- **Structured concurrency**: Go-style goroutines and channels with compile-time safety

---

## 2. Lexical Structure

### 2.1 Source Encoding

Source files are UTF-8 encoded. The byte order mark (U+FEFF) is optional and ignored if present.

### 2.2 Whitespace and Indentation

Jet uses Python-style significant indentation for block delimitation.

**Indentation Rules:**
- Indentation must be consistent within a file (all spaces or all tabs, not mixed)
- Standard indentation is 4 spaces
- A physical line with only whitespace is ignored for indentation purposes
- The first non-empty, non-comment line establishes the base indentation level (zero)

**Line Continuation:**
Expressions may span multiple lines using implicit continuation inside parentheses, brackets, or braces:

```jet
let result = some_long_function_name(
    first_argument,
    second_argument,
    third_argument
)
```

Explicit continuation with backslash is not supported.

### 2.3 Comments

**Line Comments:**
```jet
# This is a comment
let x = 5  # trailing comment
```

**Doc Comments:**
Triple-hash comments are doc comments, attached to the following declaration:
```jet
### Computes the factorial of n.
### Returns an error if n is negative.
fn factorial(n: int) -> int ! Overflow
```

### 2.4 Keywords

The following are reserved keywords and cannot be used as identifiers:

```
and         async       await       break       chan
concurrent  continue    else        enum        false
fn          for         if          import      in
let         loop        match       mod         mut
not         or          pub         return      self
spawn       struct      trait       true        type
unit        use         where       while
```

### 2.5 Identifiers

Identifiers follow Unicode Standard Annex #31 (Unicode Identifier and Pattern Syntax):

- Start with a letter (Unicode category L) or underscore `_`
- Continue with letters, decimal digits (Unicode category Nd), or underscores
- Case-sensitive

```
identifier ::= (letter | "_") (letter | digit | "_")*
letter     ::= Unicode category L (letters)
digit      ::= Unicode category Nd (decimal digits)
```

**Naming Conventions (non-enforced):**
- Types, traits, enums: PascalCase (`MyStruct`, `HttpError`)
- Functions, variables, modules: snake_case (`compute_value`, `http_client`)
- Constants: SCREAMING_SNAKE_CASE (`MAX_SIZE`)

### 2.6 Literals

#### 2.6.1 Integer Literals

```
int_literal ::= decimal_literal | hex_literal | oct_literal | bin_literal
decimal_literal ::= digit+ ("_" digit+)*
hex_literal ::= "0x" hex_digit+ ("_" hex_digit+)*
oct_literal ::= "0o" oct_digit+ ("_" oct_digit+)*
bin_literal ::= "0b" bin_digit+ ("_" bin_digit+)*
hex_digit ::= [0-9a-fA-F]
oct_digit ::= [0-7]
bin_digit ::= [01]
```

Examples: `42`, `1_000_000`, `0xFF`, `0o755`, `0b1010`

Type inference assigns the smallest signed type that can hold the value (`int8` to `int64`).
Suffixes for explicit types: `42i8`, `100u32`, `0xFFu64`

#### 2.6.2 Float Literals

```
float_literal ::= decimal_literal "." decimal_literal [exponent]
                | decimal_literal exponent
exponent ::= ("e" | "E") ["+" | "-"] decimal_literal
```

Examples: `3.14`, `1.0e10`, `2.5e-3`

Default type is `float64`. Suffix `f32` for explicit `float32`.

#### 2.6.3 Boolean Literals

```
bool_literal ::= "true" | "false"
```

#### 2.6.4 String Literals

**Double-quoted strings (interpreted):**
```
string_literal ::= '"' string_char* '"'
string_char ::= unicode_char - ['"' '\' '\n']
              | escape_sequence
escape_sequence ::= '\\' ('"' | '\\' | 'n' | 't' | 'r' | '0' | 'x' hex_digit{2} | 'u{' hex_digit+ '}')
```

**Raw strings (no escape processing):**
```
raw_string_literal ::= 'r' raw_string
raw_string ::= '"' any_char_except_quote* '"'
             | '#' raw_string '#'
             | '##' raw_string '##'
             | ... (up to 6 # pairs)
```

Examples:
```jet
let s1 = "Hello, world!"
let s2 = "Line 1\nLine 2"
let s3 = r"C:\Users\name"          # raw string
let s4 = r#"Contains "quotes""#   # can contain unescaped quotes
```

#### 2.6.5 Character Literals

```
char_literal ::= "'" (unicode_char - ["'" "\\"] | escape_sequence) "'"
```

Example: `'a'`, `'\n'`, `'\u{1F600}'`

### 2.7 Operators and Delimiters

**Arithmetic:** `+` `-` `*` `/` `%` `**`
**Bitwise:** `&` `|` `^` `<<` `>>` `~`
**Comparison:** `==` `!=` `<` `>` `<=` `>=`
**Logical:** `and` `or` `not`
**Assignment:** `=` `+=` `-=` `*=` `/=` `%=` `&=` `|=` `^=` `<<=` `>>=`
**Effects:** `!` (effect separator)
**Other:** `?` `.` `..` `...` `->` `=>` `@` `#`

**Delimiters:**
- Parentheses: `(` `)` - grouping, tuples, function calls
- Brackets: `[` `]` - arrays, indexing
- Braces: `{` `}` - blocks (not for indentation-based blocks)
- Angle brackets: `<` `>` - generics (when unambiguous)
- Colon: `:` - type annotations
- Comma: `,` - separators
- Semicolon: `;` - statement separator (rarely needed)

---

## 3. Grammar

### 3.1 Program Structure

```ebnf
program ::= module_item*

module_item ::= import_stmt
              | function_decl
              | struct_decl
              | enum_decl
              | trait_decl
              | impl_decl
              | type_alias
              | constant_decl
```

### 3.2 Statements

```ebnf
statement ::= let_stmt
            | assign_stmt
            | expr_stmt
            | return_stmt
            | break_stmt
            | continue_stmt
            | for_stmt
            | while_stmt
            | if_stmt
            | match_stmt
            | concurrent_stmt

let_stmt ::= "let" ["mut"] pattern [":" type] "=" expr
           | "let" ["mut"] pattern [":" type] "=" expr ";"  # explicit semicolon allowed

assign_stmt ::= expr "=" expr
              | expr assign_op expr

assign_op ::= "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>="

expr_stmt ::= expr

return_stmt ::= "return" [expr]

break_stmt ::= "break" [label]

continue_stmt ::= "continue" [label]
```

### 3.3 Control Flow

```ebnf
if_stmt ::= "if" expr ":" block
            ("elif" expr ":" block)*
            ["else" ":" block]

while_stmt ::= [label ":"] "while" expr ":" block

for_stmt ::= [label ":"] "for" pattern "in" expr ":" block

match_stmt ::= "match" expr ":" match_arm*

match_arm ::= "|" pattern ["if" expr] "=>" (expr | block)

block ::= NEWLINE INDENT statement+ DEDENT
        | "{" statement* [expr] "}"  # explicit block (rare)

label ::= "'" identifier
```

### 3.4 Expressions

```ebnf
expr ::= primary_expr
       | expr "." identifier                    # field access
       | expr "(" [arg_list] ")"                # function call
       | expr "[" expr "]"                      # index
       | "-" expr                               # unary minus
       | "not" expr                             # logical not
       | "~" expr                               # bitwise not
       | "*" expr                               # dereference
       | "&" ["mut"] expr                       # borrow
       | expr "?"                               # try operator
       | expr "**" expr                         # exponentiation
       | expr ("*" | "/" | "%") expr            # multiplicative
       | expr ("+" | "-") expr                  # additive
       | expr ("<<" | ">>") expr                # shift
       | expr ("&" | "|" | "^") expr            # bitwise
       | expr ("==" | "!=" | "<" | ">" | "<=" | ">=") expr  # comparison
       | expr "and" expr                        # logical and
       | expr "or" expr                         # logical or
       | expr ".." expr                         # range (exclusive)
       | expr "..." expr                        # range (inclusive)
       | "async" expr                           # async block
       | "spawn" expr                           # spawn task
       | expr "=" expr                          # assignment (statement context)
       | lambda_expr
       | "if" expr "then" expr "else" expr      # ternary/if-expr
       | "match" expr "{" match_arm* "}"        # match expression

primary_expr ::= literal
               | identifier
               | "self"
               | "(" [expr_list] ")"            # tuple or grouping
               | "[" [expr_list] "]"            # array literal
               | "{" [field_init_list] "}"      # struct literal
               | block_expr

block_expr ::= "{" statement* expr "}"

arg_list ::= expr ("," expr)* [","]
expr_list ::= expr ("," expr)* [","]

field_init_list ::= field_init ("," field_init)* [","]
field_init ::= identifier [":" expr]           # shorthand: `x` means `x: x`

lambda_expr ::= "fn" ["(" param_list ")"] ["->" type] [effect_clause] "=>" expr
              | "fn" ["(" param_list ")"] ["->" type] [effect_clause] block
```

### 3.5 Patterns

```ebnf
pattern ::= identifier_pattern
          | literal_pattern
          | wildcard_pattern
          | tuple_pattern
          | struct_pattern
          | enum_pattern
          | array_pattern
          | rest_pattern
          | "mut" pattern
          | "ref" pattern
          | "ref" "mut" pattern
          | pattern "|" pattern                  # or-pattern
          | pattern "@" pattern                  # bind and match

identifier_pattern ::= ["mut"] identifier
wildcard_pattern ::= "_"
literal_pattern ::= literal
tuple_pattern ::= "(" [pattern ("," pattern)* [","]] ")"
struct_pattern ::= path "{" [field_pattern ("," field_pattern)* [","]] "}"
field_pattern ::= identifier [":" pattern]
                | ".."
enum_pattern ::= path "::" identifier ["(" pattern ")"]
array_pattern ::= "[" [pattern ("," pattern)* [","]] "]"
rest_pattern ::= ".." [identifier]             # .. or ..rest
```

### 3.6 Type Expressions

```ebnf
type ::= path                                    # named type
       | type "[" type "]"                     # generic instantiation
       | "(" [type ("," type)*] ")"            # tuple type
       | "[" type ";" expr "]"                 # array type
       | "fn" "(" [param_type_list] ")" "->" type [effect_clause]  # function type
       | "&" ["mut"] type                      # reference type
       | "chan" "[" type "]"                   # channel type
       | "async" type                          # future type
       | "_"                                   # inferred type (rarely written)

param_type_list ::= param_type ("," param_type)*
param_type ::= ["mut"] type

effect_clause ::= "!" effect ("|" effect)*
effect ::= path                                 # error/exception type
         | "async"                              # async effect
         | "io"                                 # IO effect
         | "unsafe"                             # unsafe effect
```

### 3.7 Function Declarations

```ebnf
function_decl ::= ["pub"] "fn" identifier [generic_params]
                  "(" [param_list] ")"
                  ["->" type]
                  [effect_clause]
                  [where_clause]
                  block

param_list ::= param ("," param)* [","]
param ::= ["mut"] identifier ":" type
        | "self"                               # by value
        | "&" "self"                           # immutable borrow
        | "&" "mut" "self"                     # mutable borrow

generic_params ::= "[" generic_param ("," generic_param)* [","] "]"
generic_param ::= identifier [":" bound]
bound ::= trait_bound ("+" trait_bound)*
trait_bound ::= path ["[" type ("," type)* "]"]

where_clause ::= "where" where_bound ("," where_bound)*
where_bound ::= type ":" bound
```

### 3.8 Type Declarations

```ebnf
struct_decl ::= ["pub"] "struct" identifier [generic_params] struct_body
struct_body ::= "{" [field_decl ("," field_decl)* [","]] "}"
              | "(" [type ("," type)* [","]] ")"          # tuple struct
              | ";"                                            # unit struct

field_decl ::= ["pub"] identifier ":" type

enum_decl ::= ["pub"] "enum" identifier [generic_params] "{" enum_variant* "}"
enum_variant ::= "|" identifier [variant_body]
variant_body ::= "{" [field_decl ("," field_decl)* [","]] "}"
               | "(" [type ("," type)* [","]] ")"
               | "=" expr                                     # discriminant

type_alias ::= ["pub"] "type" identifier [generic_params] "=" type

trait_decl ::= ["pub"] "trait" identifier [generic_params] [":" bound] "{" trait_item* "}"
trait_item ::= function_decl (without body, ends with ";")
             | type_alias (without "=", ends with ";")
             | constant_decl (without "=", ends with ";")

impl_decl ::= "impl" [generic_params] [trait_path "for"] type [where_clause] "{" impl_item* "}"
impl_item ::= function_decl
            | constant_decl
```

### 3.9 Import Statements

```ebnf
import_stmt ::= "import" import_path ["as" identifier]
              | "from" import_path "import" import_item ("," import_item)*

import_path ::= identifier ("::" identifier)* ["::"]
import_item ::= identifier ["as" identifier]
              | "{" identifier ["as" identifier] ("," identifier ["as" identifier])* [","] "}"
```

---

## 4. Type System

### 4.1 Primitive Types

| Type | Description | Size |
|------|-------------|------|
| `bool` | Boolean | 1 byte |
| `int8` | Signed 8-bit integer | 1 byte |
| `int16` | Signed 16-bit integer | 2 bytes |
| `int32` | Signed 32-bit integer | 4 bytes |
| `int64` | Signed 64-bit integer | 8 bytes |
| `int` | Platform-sized signed integer (int64) | 8 bytes |
| `uint8` | Unsigned 8-bit integer | 1 byte |
| `uint16` | Unsigned 16-bit integer | 2 bytes |
| `uint32` | Unsigned 32-bit integer | 4 bytes |
| `uint64` | Unsigned 64-bit integer | 8 bytes |
| `uint` | Platform-sized unsigned integer (uint64) | 8 bytes |
| `float32` | IEEE 754 single-precision float | 4 bytes |
| `float64` | IEEE 754 double-precision float | 8 bytes |
| `float` | Alias for `float64` | 8 bytes |
| `char` | Unicode scalar value | 4 bytes |
| `string` | UTF-8 string (immutable, shared) | pointer + length |
| `unit` | The empty type `()` | 0 bytes |

### 4.2 Composite Types

#### 4.2.1 Tuples

Fixed-size heterogeneous collections:
```jet
let point: (int, int) = (10, 20)
let name_and_age: (string, int) = ("Alice", 30)
let unit: unit = ()
```

Access via pattern matching or index (0-based):
```jet
let (x, y) = point
let first = point.0
```

#### 4.2.2 Arrays

Fixed-size homogeneous collections:
```jet
let nums: [int; 5] = [1, 2, 3, 4, 5]
let zeros = [0; 10]  # ten zeros
```

Arrays are stack-allocated when small, heap-allocated when large.

#### 4.2.3 Slices

Dynamically-sized views into arrays:
```jet
let arr = [1, 2, 3, 4, 5]
let slice: &[int] = &arr[1..4]  # [2, 3, 4]
```

#### 4.2.4 Structs

Named product types:
```jet
struct Point:
    x: float
    y: float

struct Person:
    name: string
    age: int

# Usage
let p = Point { x: 1.0, y: 2.0 }
let person = Person { name: "Alice", age: 30 }
```

Tuple structs (named tuples):
```jet
struct Meters(float)
let distance = Meters(100.0)
```

Unit structs (no fields):
```jet
struct Empty
```

#### 4.2.5 Enums

Sum types with variants:
```jet
enum Option[T]:
    | Some(T)
    | None

enum Result[T, E]:
    | Ok(T)
    | Err(E)

enum HttpStatus:
    | Ok
    | NotFound
    | Error { code: int, message: string }
```

#### 4.2.6 Maps

Hash maps (built-in, not special syntax):
```jet
let scores: Map[string, int] = {"Alice": 100, "Bob": 95}
let alice_score = scores["Alice"]
```

### 4.3 Function Types

Functions are first-class with effect tracking:
```jet
# Simple function type
let add: fn(int, int) -> int = fn(a, b) => a + b

# Function with effects
let read_file: fn(string) -> string ! IoError = ...

# Higher-order function
fn map[T, U](list: [T], f: fn(T) -> U) -> [U]:
    ...
```

### 4.4 Generics

Type parameters in square brackets:
```jet
struct Box[T]:
    value: T

fn identity[T](x: T) -> T:
    return x

fn max[T: Comparable](a: T, b: T) -> T:
    if a > b:
        return a
    else:
        return b
```

#### 4.4.1 Constraints

Bounds on type parameters:
```jet
# Single trait bound
fn print[T: Display](value: T):
    ...

# Multiple trait bounds
fn process[T: Display + Clone](value: T):
    ...

# Where clauses for complex constraints
fn complex[T, U](t: T, u: U) -> Result[T, U]
    where T: Display + Clone,
          U: Comparable:
    ...
```

### 4.5 Type Inference

Jet uses Hindley-Milner style inference with extensions:

**Local variable inference:**
```jet
let x = 5           # x: int
let y = 3.14        # y: float64
let s = "hello"     # s: string
let b = true        # b: bool
```

**Function return type inference:**
```jet
fn double(x: int):   # return type inferred as int
    return x * 2
```

**Generic instantiation inference:**
```jet
let opt = Some(5)   # opt: Option[int]
let result = Ok("x") # result: Result[string, _]
```

**Limitations:**
- Recursive functions need explicit return types
- Complex control flow may need annotations
- Generic function parameters often need explicit types

---

## 5. Effects and Error Handling

### 5.1 The Effects System

Functions declare their effects after `!` in the signature:
```jet
# Pure function - no effects
fn pure_add(a: int, b: int) -> int:
    return a + b

# Function that can fail
fn divide(a: float, b: float) -> float ! DivisionByZero:
    if b == 0.0:
        raise DivisionByZero()
    return a / b

# Multiple effects
fn fetch_data(url: string) -> Data ! NetworkError | ParseError:
    ...

# Async effect
fn async_fetch(url: string) -> Data ! async | NetworkError:
    ...
```

### 5.2 Defining Error Types

Errors are enums that implement the `Error` trait:
```jet
enum NetworkError:
    | Timeout
    | ConnectionFailed { reason: string }
    | InvalidResponse { status: int }

impl Error for NetworkError:
    fn message(self) -> string:
        match self:
            | Timeout => "Request timed out"
            | ConnectionFailed { reason } => f"Connection failed: {reason}"
            | InvalidResponse { status } => f"Invalid response: {status}"
```

### 5.3 Raising Errors

Use `raise` to throw an error:
```jet
fn parse_int(s: string) -> int ! ParseError:
    if s.is_empty():
        raise ParseError("Empty string")
    ...
```

### 5.4 The Try Operator `?`

Propagate errors automatically:
```jet
fn process_file(path: string) -> Data ! IoError | ParseError:
    let content = read_file(path)?       # if Err, return immediately
    let parsed = parse_json(content)?    # if Err, return immediately
    return transform(parsed)
```

The `?` operator works on any type implementing `Try` (Result, Option, etc.).

### 5.5 Handling Errors

**Pattern matching:**
```jet
match divide(10.0, 0.0):
    | Ok(result) => print(f"Result: {result}")
    | Err(DivisionByZero) => print("Cannot divide by zero")
```

**If-let:**
```jet
if let Ok(result) = divide(10.0, 2.0):
    print(f"Success: {result}")
```

**While-let:**
```jet
while let Some(item) = iterator.next():
    process(item)
```

**Try-catch style (explicit):**
```jet
let result = try:
    risky_operation()?
    another_risky()?
catch NetworkError as e:
    handle_network(e)
catch ParseError as e:
    handle_parse(e)
```

### 5.6 Effect Polymorphism

Higher-order functions can be polymorphic over effects:
```jet
# `f` can have any effects, which are propagated
fn map_result[T, E, U](r: Result[T, E], f: fn(T) -> U) -> Result[U, E]:
    match r:
        | Ok(v) => Ok(f(v))
        | Err(e) => Err(e)

# Explicit effect polymorphism with effect variables
fn compose[A, B, C, E](f: fn(A) -> B ! E, g: fn(B) -> C ! E) -> fn(A) -> C ! E:
    return fn(x) => g(f(x))
```

### 5.7 The Result and Option Types

Built-in enums for error handling:
```jet
enum Option[T]:
    | Some(T)
    | None

enum Result[T, E]:
    | Ok(T)
    | Err(E)
```

Convenience methods:
```jet
let x: Option[int] = Some(5)
let y = x.unwrap_or(0)        # 5 or default
let z = x.map(|v| v * 2)      # Some(10)

let r: Result[int, Error] = Ok(5)
let v = r.expect("Must succeed")  # panics on Err
```

---

## 6. Concurrency

### 6.1 Overview

Jet uses structured concurrency based on goroutines and channels:
- Tasks are lightweight threads managed by the runtime
- Channels provide type-safe communication
- No uncontrolled async/await spaghetti
- Compile-time tracking of concurrent effects

### 6.2 Async Functions

Functions that can suspend are marked `async`:
```jet
async fn fetch_url(url: string) -> Response ! NetworkError:
    let conn = connect(url).await?       # suspend here
    let response = conn.send_request().await?
    return response
```

### 6.3 The Await Operator

`.await` suspends the current task until the future completes:
```jet
let data = fetch_url("https://example.com").await
```

### 6.4 Concurrent Blocks

`concurrent` blocks run tasks concurrently with structured lifetime:
```jet
fn fetch_all(urls: [string]) -> [Response] ! NetworkError:
    concurrent:                          # all tasks complete before block exits
        let tasks = urls.map(|url| spawn fetch_url(url))
        let results = tasks.map(|t| t.await)
        return results
```

### 6.5 Spawning Tasks

`spawn` creates a new concurrent task:
```jet
let handle = spawn long_running_computation()
# ... do other work ...
let result = handle.await
```

Task handles implement `Future` and can be awaited.

### 6.6 Channels

Type-safe message passing:
```jet
# Create a channel
let (tx, rx) = chan[int]()

# Send
spawn:
    for i in 0..10:
        tx.send(i)
    tx.close()

# Receive
concurrent:
    while let Some(value) = rx.recv().await:
        process(value)
```

Channel types:
```jet
let (tx, rx): (Sender[int], Receiver[int]) = chan[int]()
```

Buffered channels:
```jet
let (tx, rx) = chan[int](buffer: 100)  # capacity of 100
```

### 6.7 Select

Wait on multiple channel operations:
```jet
concurrent:
    loop:
        select:
            | msg = rx1.recv() => handle_msg1(msg)
            | msg = rx2.recv() => handle_msg2(msg)
            | tx.send(value) => print("Sent!")
            | default => print("No activity")
```

### 6.8 Synchronization Primitives

**Mutex:**
```jet
let data = Mutex::new(0)

concurrent:
    let mut guard = data.lock().await
    *guard += 1
    # lock released when guard drops
```

**RwLock:**
```jet
let cache = RwLock::new(Map::new())

# Many readers
let read_guard = cache.read().await

# Exclusive writer
let mut write_guard = cache.write().await
```

### 6.9 Cancellation

Tasks are automatically cancelled when their parent scope exits:
```jet
concurrent:
    let task = spawn infinite_loop()
    sleep(1.0).await
    # task is cancelled here when block exits
```

Explicit cancellation:
```jet
let handle = spawn worker()
handle.cancel().await  # wait for graceful shutdown
```

### 6.10 Effect Tracking for Concurrency

Async operations are tracked as effects:
```jet
# This function has the async effect
fn async_op() -> int ! async:
    sleep(1.0).await
    return 42

# Can only be called from async contexts or concurrent blocks
```

---

## 7. Memory Model

### 7.1 Overview

Jet combines:
- **Ownership**: Linear types prevent use-after-free and data races
- **Borrowing**: Immutable and mutable references with lifetime tracking
- **Garbage Collection**: For cycles and shared immutable data
- **Stack allocation**: Default for fixed-size data
- **Heap allocation**: Explicit for dynamic data

### 7.2 Ownership Rules

1. Each value has exactly one owner at a time
2. When the owner goes out of scope, the value is dropped
3. Ownership can be transferred (moved) or borrowed

```jet
fn main():
    let s = "hello".to_string()   # s owns the string
    takes_ownership(s)            # s is moved, no longer valid here
    # print(s)                    # ERROR: s was moved

fn takes_ownership(s: string):
    print(s)                      # s is valid here
                                  # s is dropped when function returns
```

### 7.3 Moves and Copies

**Copy types** (implicit copy, not move):
- All primitives (`int`, `float`, `bool`, `char`)
- Tuples of copy types
- Arrays of copy types

```jet
let x = 5
let y = x       # x is copied, still valid
print(x)        # OK
```

**Move types** (ownership transfer):
- `string`, `Vec`, `Box`, most user-defined types

```jet
let s1 = "hello".to_string()
let s2 = s1     # s1 is moved
# print(s1)     # ERROR: s1 was moved
```

### 7.4 Borrowing

**Immutable references** (`&T`):
```jet
fn main():
    let s = "hello".to_string()
    let len = calculate_length(&s)  # borrow s immutably
    print(s)                        # OK: s still valid

fn calculate_length(s: &string) -> int:
    return s.len()
```

**Mutable references** (`&mut T`):
```jet
fn main():
    let mut s = "hello".to_string()
    change(&mut s)
    print(s)                        # "hello, world"

fn change(s: &mut string):
    s.push_str(", world")
```

**Borrowing Rules:**
1. Any number of immutable references OR exactly one mutable reference
2. References must not outlive the data they reference
3. No mutable reference while immutable references exist

### 7.5 Lifetimes

Lifetimes are mostly inferred. Explicit when needed:
```jet
# Explicit lifetime annotation
fn longest['a](x: &'a string, y: &'a string) -> &'a string:
    if x.len() > y.len():
        return x
    else:
        return y
```

Lifetime elision rules handle common cases automatically.

### 7.6 Smart Pointers

**Box** (heap allocation):
```jet
let b = Box::new(5)     # heap-allocated integer
```

**Rc** (reference counted, immutable sharing):
```jet
let data = Rc::new(vec![1, 2, 3])
let data2 = Rc.clone(&data)   # shared ownership
```

**Arc** (atomic reference counted, thread-safe):
```jet
let data = Arc::new(vec![1, 2, 3])
```

### 7.7 Garbage Collection

GC is used for:
- Reference cycles (broken by cycle collector)
- Large immutable shared data
- When explicitly requested

```jet
let gc_data = gc::new(large_data)   # GC-managed
# Access via Gc smart pointer
```

### 7.8 Interior Mutability

**Cell** (for copy types):
```jet
let c = Cell::new(5)
c.set(10)
```

**RefCell** (runtime borrow checking):
```jet
let r = RefCell::new(vec![1, 2, 3])
let mut borrow = r.borrow_mut()
borrow.push(4)
```

### 7.9 Unsafe Code

Unsafe blocks allow raw pointer operations:
```jet
unsafe fn dangerous():
    # raw pointer dereference, etc.

fn safe_wrapper():
    unsafe:
        dangerous()
```

Unsafe code must be marked with the `unsafe` effect:
```jet
fn raw_access() -> T ! unsafe:
    unsafe:
        # ...
```

---

## 8. Module System

### 8.1 File Organization

A Jet project has the following structure:
```
my_project/
├── jet.toml          # Project manifest
├── src/
│   ├── main.jet      # Entry point (executable)
│   └── lib.jet       # Library root (optional)
│   └── foo/
│       ├── mod.jet   # Module file
│       └── bar.jet   # Sub-module
```

### 8.2 Module Declaration

Files automatically define modules based on path:
- `src/lib.jet` = crate root
- `src/foo/mod.jet` = module `foo`
- `src/foo/bar.jet` = submodule `foo::bar`

Explicit module declaration (rare):
```jet
mod foo   # looks for foo.jet or foo/mod.jet
```

### 8.3 Visibility

Default is private. Use `pub` to export:
```jet
# Private by default
fn internal_helper():
    ...

# Public function
pub fn public_api():
    ...

# Public struct with private field
pub struct Counter:
    count: int          # private

# Public struct with public field
pub struct Point:
    pub x: float
    pub y: float

# Public enum (variants are public)
pub enum Status:
    | Active
    | Inactive
```

Visibility modifiers:
- `pub` - visible everywhere
- `pub(crate)` - visible within current crate
- `pub(super)` - visible to parent module
- `pub(self)` - private (default)
- `pub(in path)` - visible within specific module path

### 8.4 Imports

**Absolute imports:**
```jet
import std::collections::Map
import std::io::File
```

**Relative imports:**
```jet
import .::sibling_module       # sibling
import ..::parent_module       # parent
import self::submodule         # current module's child
```

**Import with alias:**
```jet
import std::collections::HashMap as Map
```

**Selective imports:**
```jet
from std::io import File, BufReader, BufWriter
from std::collections import {HashMap, HashSet, VecDeque}
```

**Glob imports (discouraged):**
```jet
from std::prelude import *    # automatically imported
```

### 8.5 The Prelude

The following are automatically imported in every module:
```jet
from std::prelude import *
# Includes: Option, Result, Vec, Map, string, print, panic, etc.
```

### 8.6 Re-exports

Expose items from other modules:
```jet
pub import std::collections::HashMap

pub use self::inner::private_impl as public_api
```

### 8.7 Crate Structure

**Library crate:**
```jet
# src/lib.jet
pub mod parser
pub mod codegen

pub fn compile(source: string) -> Result[Bytecode, Error]:
    ...
```

**Binary crate:**
```jet
# src/main.jet
import my_library::compile

fn main():
    let args = std::env::args()
    let result = compile(read_file(args[1]))
    ...
```

### 8.8 External Dependencies

Declared in `jet.toml`:
```toml
[dependencies]
serde = "1.0"
tokio = { version = "1.0", features = ["full"] }
local_crate = { path = "../local_crate" }
```

Imported by name:
```jet
import serde::json
import tokio::runtime
```

---

## Appendix A: Operator Precedence

| Precedence | Operator | Description | Associativity |
|------------|----------|-------------|---------------|
| 1 (highest) | `::` | Path/namespace | Left |
| 2 | `.` `[]` `()` `?` | Field access, indexing, call, try | Left |
| 3 | `-` (unary) `not` `~` `*` `&` `&mut` | Unary operators | Right |
| 4 | `**` | Exponentiation | Right |
| 5 | `*` `/` `%` | Multiplicative | Left |
| 6 | `+` `-` | Additive | Left |
| 7 | `<<` `>>` | Bit shift | Left |
| 8 | `&` | Bitwise AND | Left |
| 9 | `^` | Bitwise XOR | Left |
| 10 | `\|` | Bitwise OR | Left |
| 11 | `==` `!=` `<` `>` `<=` `>=` | Comparison | None |
| 12 | `and` | Logical AND | Left |
| 13 | `or` | Logical OR | Left |
| 14 | `..` `...` | Range | None |
| 15 | `=` `+=` `-=` etc. | Assignment | Right |
| 16 (lowest) | `return` `break` `continue` | Control flow | Right |

---

## Appendix B: Reserved for Future Use

The following features are reserved for future versions:

- `macro` keyword for hygienic macros
- `const` generics
- `impl Trait` in argument position
- `async` closures
- `yield` for generators
- `unsafe` trait markers
- `dyn` for dynamic dispatch

---

## Appendix C: Grammar Summary

```ebnf
# Top-level
program ::= module_item*
module_item ::= import_stmt | decl

# Declarations
decl ::= function_decl | struct_decl | enum_decl | trait_decl
       | impl_decl | type_alias | constant_decl

# Statements
statement ::= let_stmt | assign_stmt | expr_stmt | control_stmt
let_stmt ::= "let" ["mut"] pattern [":" type] "=" expr

# Expressions
expr ::= primary | postfix | unary | binary | special
primary ::= literal | identifier | "(" expr ")" | "[" [expr_list] "]"
postfix ::= expr ("." id | "(" [args] ")" | "[" expr "]" | "?")*
unary ::= ("-" | "not" | "~" | "*" | "&" ["mut"]) expr
binary ::= expr (bin_op expr)*
bin_op ::= "**" | "*" | "/" | "%" | "+" | "-" | "<<" | ">>"
         | "&" | "^" | "|" | cmp_op | "and" | "or"
cmp_op ::= "==" | "!=" | "<" | ">" | "<=" | ">="
special ::= "if" expr "then" expr "else" expr
          | "match" expr ":" match_arm*
          | "async" expr | "spawn" expr
          | lambda_expr

# Types
type ::= path ["[" type_list "]"] | "(" [type_list] ")"
       | "[" type ";" expr "]" | "fn" "(" [params] ")" "->" type [effects]
       | "&" ["mut"] type | "chan" "[" type "]"

effects ::= "!" effect ("|" effect)*

# Patterns
pattern ::= id_pattern | literal | "_" | "(" [patterns] ")"
          | struct_pattern | enum_pattern | ".." [id]
```

---

*End of Specification*
