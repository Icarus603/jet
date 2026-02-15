# Jet AI Integration Design

## Overview

Jet is designed for the AI era where code is increasingly:
- **Written by AI** - Large language models generate implementations
- **Verified by humans** - Engineers review, validate, and refine
- **Executed safely** - Runtime prevents errors AI might introduce

This document describes language features that make Jet the best language for AI-human collaboration.

---

## 1. Explicit Intent Markers

### Confidence Annotations

AI can annotate code with confidence levels that the compiler can check:

```jet
@confidence(high)  # AI is confident this is correct
fn calculate_tax(income: float) -> float {
    return income * 0.25
}

@confidence(low)   # AI is uncertain, needs human review
fn complex_derivative(f: Function, x: float) -> float {
    # implementation
}
```

The compiler can:
- Warn when calling `@confidence(low)` functions from `@confidence(high)` ones
- Generate reports of uncertain code for human review
- Require explicit acknowledgment before building with low-confidence code

### Generation Metadata

Every file can contain generation provenance:

```jet
@generated_by("claude-4", confidence=0.92, reviewed=false)
@prompt("Create a function to parse HTTP headers")
@human_edit_count(3)
```

---

## 2. Formal Contracts

### Pre and Post Conditions

Functions declare contracts that the compiler can verify:

```jet
fn divide(a: float, b: float) -> float
    requires b != 0.0
    ensures result * b == a
{
    return a / b
}
```

At call sites:
```jet
# Compiler verifies: is y != 0.0 here?
let result = divide(x, y)
```

### Loop Invariants

```jet
fn binary_search(arr: [int], target: int) -> Option<int>
    requires is_sorted(arr)
{
    let mut left = 0
    let mut right = arr.len()

    while left < right
        invariant 0 <= left && left <= right && right <= arr.len()
        invariant if_exists(target, arr) then target in arr[left..right]
    {
        let mid = (left + right) / 2
        if arr[mid] < target {
            left = mid + 1
        } else {
            right = mid
        }
    }

    if left < arr.len() && arr[left] == target {
        return Some(left)
    }
    return None
}
```

### Ghost Types for Specifications

Types that exist only for verification:

```jet
ghost type SortedList<T> = List<T> where is_sorted(List)

fn merge(a: SortedList<int>, b: SortedList<int>) -> SortedList<int> {
    # Compiler verifies output is sorted
}
```

---

## 3. Type-Guided Generation

### Type-Driven Synthesis

The type system guides AI to correct implementations:

```jet
# AI sees: must return Result<User, DbError | NotFound>
# This constrains the possible implementations dramatically
fn fetch_user(id: UUID) -> Result<User, DbError | NotFound> {
    # AI knows it must handle:
    # - Database connection (DbError possible)
    # - Missing user (NotFound possible)
    # - Must return a User on success
}
```

### Hole-Driven Development

Use `_` as placeholders for AI to fill:

```jet
fn process_data(input: Data) -> Result<Output, Error> {
    let validated = _  # AI suggests: validate(input)?
    let transformed = _  # AI suggests: transform(validated)?
    return Ok(transformed)
}
```

The compiler reports the expected type at each hole.

### Effect Inference

AI doesn't need to remember to add error annotations:

```jet
fn foo() {  # AI writes without effects
    file.read()?   # Compiler infers: -> T ! IOError
    network.get()? # Compiler adds: ! NetworkError
}

# Compiler final signature:
# fn foo() -> T ! IOError | NetworkError
```

---

## 4. Verification-Friendly Structure

### Local Reasoning

Code is structured so humans can verify it locally:

```jet
# Good: All effects are visible in the signature
fn process_order(order: Order) -> Receipt ! ValidationError | PaymentError {
    let validated = validate(order)?      # Can fail with ValidationError
    let payment = charge(validated)?      # Can fail with PaymentError
    return create_receipt(payment)        # Never fails
}

# Bad: Hidden effects, global state
fn process_order_bad(order: Order) -> Receipt {
    # May throw, may access database, may call external service
    # Hard to verify without reading entire implementation
}
```

### Exhaustive Checking

The compiler forces AI to handle all cases:

```jet
match result {
    Ok(user) -> display(user),
    Err(e) -> log(e),  # Compiler verifies: did you handle all error types?
}
```

### Immutable by Default

```jet
let x = 5       # Immutable - AI can't accidentally modify
var y = 10      # Mutable - explicit opt-in

# Data structures are immutable by default
type Point = {
    x: float,    # Immutable field
    y: float,
}
```

This makes AI-generated code easier to reason about.

---

## 5. Literate Programming

### Executable Specifications

Mix natural language specs with code:

```jet
## HTTP Client Module

This module provides a simple HTTP client with the following guarantees:
- All requests timeout after 30 seconds
- Connections are pooled and reused
- Redirects are followed automatically (up to 10)

### Making Requests

The `get` function fetches a URL and returns the response body.
It can fail if:
- The URL is malformed (UrlError)
- The network is unreachable (NetworkError)
- The server returns an error status (HttpError)

@spec "GET requests return 200 OK responses"
@example
fn get(url: string) -> Result<string, NetworkError | HttpError> {
    let request = build_get_request(url)?
    let response = send_request(request).await?
    if response.status == 200 {
        return Ok(response.body)
    }
    return Err(HttpError(response.status))
}
```

### Test Generation from Specs

```jet
@spec "divide should return quotient for valid inputs"
@test_for(divide)
fn test_divide_valid() {
    assert divide(10, 2) == 5
    assert divide(7, 2) == 3.5
}

@spec "divide should require non-zero divisor"
@test_for(divide)
fn test_divide_by_zero() {
    expect_panic(divide(10, 0))
}
```

---

## 6. Automatic Test Generation

### Property-Based Testing from Types

```jet
# AI generates: fn reverse_twice_is_identity(list: [int]) -> bool
@test_auto
fn reverse_properties(list: [int]) {
    assert list == reverse(reverse(list))
}

# Compiler generates random test cases
```

### Effect Coverage

```jet
fn fetch_and_save(url: string) -> Result<(), Error> ! NetworkError | IOError {
    let data = http.get(url)?    # Can fail with NetworkError
    file.write(data)?            # Can fail with IOError
    return Ok(())
}

# Compiler generates tests:
# 1. Test with valid URL and writable file (success path)
# 2. Test with invalid URL (NetworkError)
# 3. Test with unwritable file (IOError)
```

### Mutation Testing

```jet
@mutation_test  # AI verifies tests catch bugs
fn my_function(x: int) -> int {
    return x * 2
}
```

The compiler introduces mutations (change `*` to `+`, etc.) and verifies tests fail.

---

## 7. AI-Safe Refactoring

### Semantic Preserving Transformations

Refactorings that preserve meaning:

```jet
# Before
fn process(items: [Item]) -> Result<(), Error> {
    for item in items {
        handle(item)?
    }
    return Ok(())
}

# AI refactors to (equivalent):
fn process(items: [Item]) -> Result<(), Error> {
    items.try_for_each(handle)?
}
```

Compiler verifies:
- Same effects
- Same return type
- Same behavior for all inputs

### Automated Renames

```jet
@rename(from="old_name", to="new_name", applied_by="ai")
fn new_name() { }
```

The compiler tracks renames across the codebase.

---

## 8. Documentation as Code

### Executable Examples

```jet
### Adds two numbers together.
###
### Example:
###   >>> add(2, 3)
###   5
###
###   >>> add(-1, 1)
###   0
fn add(a: int, b: int) -> int {
    return a + b
}
```

Examples are run as tests.

### Type-Driven Documentation

```jet
fn map<T, U>(option: Option<T>, f: fn(T) -> U) -> Option<U>
    where T: Clone, U: Clone
```

AI generates documentation from type signature:

```
Transforms an Option<T> into an Option<U> by applying a function.

## Type Parameters
- `T`: The input type (must implement Clone)
- `U`: The output type (must implement Clone)

## Arguments
- `option`: The optional value to transform
- `f`: Function to apply if Some

## Returns
- Some(f(value)) if option is Some(value)
- None if option is None
```

---

## 9. Interaction Model

### AI-Assisted Editing

```
Human: "Add a function to validate email addresses"

AI generates:
```jet
@confidence(medium)
@prompt("Add email validation function")
fn validate_email(email: string) -> Result<Email, ValidationError> {
    # Implementation
}
```

Human reviews and either:
- Accepts: removes @confidence annotation
- Modifies: increments @human_edit_count
- Rejects: provides feedback for regeneration
```

### Code Review Integration

```jet
@ai_suggestion("Consider using pattern matching instead of if-else chain")
fn process(status: Status) -> Response {
    # AI suggests alternative implementation
}
```

### Continuous Verification

```
CI Pipeline:
1. Check all @confidence(low) code has been reviewed
2. Verify all @spec tests pass
3. Run mutation tests
4. Generate coverage report
```

---

## Summary

Jet's AI integration features:

1. **Confidence markers** - Explicit uncertainty for human review
2. **Formal contracts** - Machine-verified specifications
3. **Type guidance** - Compiler constrains AI to valid code
4. **Local reasoning** - Code is verifiable in isolation
5. **Literate specs** - Natural language mixed with code
6. **Auto tests** - Generated from types and specs
7. **Safe refactoring** - Semantic-preserving transformations
8. **Executable docs** - Examples that run as tests

These features make Jet the optimal language for AI-human collaboration: AI generates code within guardrails, humans verify with confidence, and the runtime prevents errors.
