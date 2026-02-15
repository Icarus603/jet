//! Result - Result type for Jet
//!
//! This module provides the Result type representing either Ok value or Err error.

/// Represents either success (`Ok(T)`) or failure (`Err(E)`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Result<T, E> {
    /// Success value of type T
    Ok(T),
    /// Error value of type E
    Err(E),
}

impl<T, E> Result<T, E> {
    /// Returns true if the result is Ok.
    pub fn is_ok(&self) -> bool {
        matches!(self, Result::Ok(_))
    }

    /// Returns true if the result is Err.
    pub fn is_err(&self) -> bool {
        !self.is_ok()
    }

    /// Returns true if the result is Ok and the value satisfies the predicate.
    pub fn is_ok_and<F>(&self, f: F) -> bool
    where
        F: FnOnce(&T) -> bool,
    {
        match self {
            Result::Ok(v) => f(v),
            Result::Err(_) => false,
        }
    }

    /// Returns true if the result is Err and the error satisfies the predicate.
    pub fn is_err_and<F>(&self, f: F) -> bool
    where
        F: FnOnce(&E) -> bool,
    {
        match self {
            Result::Ok(_) => false,
            Result::Err(e) => f(e),
        }
    }

    /// Unwraps the result, returning the value.
    ///
    /// Panics if the result is Err.
    pub fn unwrap(self) -> T {
        match self {
            Result::Ok(v) => v,
            Result::Err(_) => panic!("called `Result::unwrap()` on an `Err` value"),
        }
    }

    /// Unwraps the result, returning the error.
    ///
    /// Panics if the result is Ok.
    pub fn unwrap_err(self) -> E {
        match self {
            Result::Ok(_) => panic!("called `Result::unwrap_err()` on an `Ok` value"),
            Result::Err(e) => e,
        }
    }

    /// Returns the contained value or a default.
    pub fn unwrap_or(self, default: T) -> T {
        match self {
            Result::Ok(v) => v,
            Result::Err(_) => default,
        }
    }

    /// Returns the contained value or computes a default.
    pub fn unwrap_or_else<F>(self, f: F) -> T
    where
        F: FnOnce(&E) -> T,
    {
        match self {
            Result::Ok(v) => v,
            Result::Err(ref e) => f(e),
        }
    }

    /// Returns the contained error or a default.
    pub fn unwrap_err_or(self, default: E) -> E {
        match self {
            Result::Ok(_) => default,
            Result::Err(e) => e,
        }
    }

    /// Returns the contained error or computes a default.
    pub fn unwrap_err_or_else<F>(self, f: F) -> E
    where
        F: FnOnce(&T) -> E,
    {
        match self {
            Result::Ok(ref v) => f(v),
            Result::Err(e) => e,
        }
    }

    /// Returns the contained value or panics with a message.
    pub fn expect(self, msg: &str) -> T {
        match self {
            Result::Ok(v) => v,
            Result::Err(_) => panic!("{}", msg),
        }
    }

    /// Returns the contained error or panics with a message.
    pub fn expect_err(self, msg: &str) -> E {
        match self {
            Result::Ok(_) => panic!("{}", msg),
            Result::Err(e) => e,
        }
    }

    /// Maps Result<T, E> to Result<U, E>.
    pub fn map<U, F>(self, f: F) -> Result<U, E>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Result::Ok(v) => Result::Ok(f(v)),
            Result::Err(e) => Result::Err(e),
        }
    }

    /// Maps the error type.
    pub fn map_err<F, O>(self, f: F) -> Result<T, O>
    where
        F: FnOnce(E) -> O,
    {
        match self {
            Result::Ok(v) => Result::Ok(v),
            Result::Err(e) => Result::Err(f(e)),
        }
    }

    /// Chains operations that may fail.
    pub fn flat_map<U, F>(self, f: F) -> Result<U, E>
    where
        F: FnOnce(T) -> Result<U, E>,
    {
        match self {
            Result::Ok(v) => f(v),
            Result::Err(e) => Result::Err(e),
        }
    }

    /// Chains operations that may fail with a different error type.
    pub fn and_then<U, F, O>(self, f: F) -> Result<U, O>
    where
        F: FnOnce(T) -> Result<U, O>,
        E: Into<O>,
    {
        match self {
            Result::Ok(v) => f(v),
            Result::Err(e) => Result::Err(e.into()),
        }
    }

    /// Returns other if the result is Ok, otherwise returns self.
    pub fn and<U>(self, other: Result<U, E>) -> Result<U, E> {
        match self {
            Result::Ok(_) => other,
            Result::Err(e) => Result::Err(e),
        }
    }

    /// Returns other if the result is Err, otherwise returns self.
    pub fn or<F>(self, other: Result<T, F>) -> Result<T, F> {
        match self {
            Result::Ok(v) => Result::Ok(v),
            Result::Err(_) => other,
        }
    }

    /// Returns the result of the function if the result is Err, otherwise returns self.
    pub fn or_else<F, O>(self, f: F) -> Result<T, O>
    where
        F: FnOnce(E) -> Result<T, O>,
    {
        match self {
            Result::Ok(v) => Result::Ok(v),
            Result::Err(e) => f(e),
        }
    }

    /// Returns an iterator over the possibly contained value.
    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            inner: self.as_ref(),
        }
    }

    /// Returns an iterator over the possibly contained error.
    pub fn iter_err(&self) -> IterErr<'_, E> {
        IterErr {
            inner: self.as_ref_err(),
        }
    }

    /// Returns an Option with a reference to the value.
    pub fn as_ref(&self) -> Option<&T> {
        match self {
            Result::Ok(v) => Some(v),
            Result::Err(_) => None,
        }
    }

    /// Returns an Option with a reference to the error.
    pub fn as_ref_err(&self) -> Option<&E> {
        match self {
            Result::Ok(_) => None,
            Result::Err(e) => Some(e),
        }
    }

    /// Returns an Option with a mutable reference to the value.
    pub fn as_mut(&mut self) -> Option<&mut T> {
        match self {
            Result::Ok(v) => Some(v),
            Result::Err(_) => None,
        }
    }

    /// Returns an Option with a mutable reference to the error.
    pub fn as_mut_err(&mut self) -> Option<&mut E> {
        match self {
            Result::Ok(_) => None,
            Result::Err(e) => Some(e),
        }
    }

    /// Transposes a Result of an Option into an Option of a Result.
    pub fn transpose<U>(self) -> Option<Result<U, E>>
    where
        T: Into<Option<U>>,
    {
        match self {
            Result::Ok(v) => v.into().map(Result::Ok),
            Result::Err(e) => Some(Result::Err(e)),
        }
    }

    /// Flattens a Result of a Result.
    pub fn flatten<U>(self) -> Result<U, E>
    where
        T: Into<Result<U, E>>,
    {
        self.flat_map(|v| v.into())
    }

    /// Maps Result<T, E> to U by applying a function to the contained Ok value,
    /// or returns a default.
    pub fn map_or<U, F>(self, default: U, f: F) -> U
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Result::Ok(v) => f(v),
            Result::Err(_) => default,
        }
    }

    /// Maps Result<T, E> to U by applying a function to the contained Ok value,
    /// or a default function to the contained Err value.
    pub fn map_or_else<U, D, F>(self, default: D, f: F) -> U
    where
        D: FnOnce(&E) -> U,
        F: FnOnce(T) -> U,
    {
        match self {
            Result::Ok(v) => f(v),
            Result::Err(ref e) => default(e),
        }
    }

    /// Inserts a value into the result if it is Err.
    pub fn get_or_insert(self, value: T) -> T {
        match self {
            Result::Ok(v) => v,
            Result::Err(_) => value,
        }
    }

    /// Inserts a computed value into the result if it is Err.
    pub fn get_or_insert_with<F>(self, f: F) -> T
    where
        F: FnOnce(&E) -> T,
    {
        match self {
            Result::Ok(v) => v,
            Result::Err(ref e) => f(e),
        }
    }

    /// Maps Result<T, E> to Result<T, F> by applying a function that may fail.
    pub fn try_map<U, F, O>(self, f: F) -> Result<U, O>
    where
        F: FnOnce(T) -> Result<U, O>,
        E: Into<O>,
    {
        match self {
            Result::Ok(v) => f(v),
            Result::Err(e) => Result::Err(e.into()),
        }
    }

    /// Applies a function to the contained value if Ok, otherwise returns self.
    pub fn inspect<F>(self, f: F) -> Self
    where
        F: FnOnce(&T),
    {
        match &self {
            Result::Ok(v) => f(v),
            Result::Err(_) => (),
        }
        self
    }

    /// Applies a function to the contained error if Err, otherwise returns self.
    pub fn inspect_err<F>(self, f: F) -> Self
    where
        F: FnOnce(&E),
    {
        match &self {
            Result::Ok(_) => (),
            Result::Err(e) => f(e),
        }
        self
    }

    /// Converts from `Result<T, E>` to `Option<T>`.
    pub fn ok(self) -> Option<T> {
        match self {
            Result::Ok(v) => Some(v),
            Result::Err(_) => None,
        }
    }

    /// Converts from `Result<T, E>` to `Option<E>`.
    pub fn err(self) -> Option<E> {
        match self {
            Result::Ok(_) => None,
            Result::Err(e) => Some(e),
        }
    }
}

impl<T, E> Default for Result<T, E>
where
    T: Default,
{
    fn default() -> Self {
        Result::Ok(T::default())
    }
}

impl<T, E> From<T> for Result<T, E> {
    fn from(value: T) -> Self {
        Result::Ok(value)
    }
}

/// Iterator over a Result's Ok value
pub struct Iter<'a, T> {
    inner: Option<&'a T>,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.take()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = if self.inner.is_some() { 1 } else { 0 };
        (len, Some(len))
    }
}

/// Iterator over a Result's Err value
pub struct IterErr<'a, E> {
    inner: Option<&'a E>,
}

impl<'a, E> Iterator for IterErr<'a, E> {
    type Item = &'a E;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.take()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = if self.inner.is_some() { 1 } else { 0 };
        (len, Some(len))
    }
}

// C ABI exports

/// Creates an Ok variant
#[no_mangle]
pub extern "C" fn jet_result_ok(value: i64) -> *mut Result<i64, i64> {
    Box::into_raw(Box::new(Result::Ok(value)))
}

/// Creates an Err variant
#[no_mangle]
pub extern "C" fn jet_result_err(error: i64) -> *mut Result<i64, i64> {
    Box::into_raw(Box::new(Result::Err(error)))
}

/// Frees a result
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_result_free`'s FFI contract.
pub unsafe extern "C" fn jet_result_free(result: *mut Result<i64, i64>) {
    if !result.is_null() {
        unsafe { drop(Box::from_raw(result)) };
    }
}

/// Returns true if the result is Ok
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_result_is_ok`'s FFI contract.
pub unsafe extern "C" fn jet_result_is_ok(result: *const Result<i64, i64>) -> bool {
    unsafe { (*result).is_ok() }
}

/// Returns true if the result is Err
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_result_is_err`'s FFI contract.
pub unsafe extern "C" fn jet_result_is_err(result: *const Result<i64, i64>) -> bool {
    unsafe { (*result).is_err() }
}

/// Unwraps a result (returns 0 if Err)
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_result_unwrap`'s FFI contract.
pub unsafe extern "C" fn jet_result_unwrap(result: *const Result<i64, i64>) -> i64 {
    unsafe {
        match &*result {
            Result::Ok(v) => *v,
            Result::Err(_) => 0,
        }
    }
}

/// Unwraps the error (returns 0 if Ok)
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_result_unwrap_err`'s FFI contract.
pub unsafe extern "C" fn jet_result_unwrap_err(result: *const Result<i64, i64>) -> i64 {
    unsafe {
        match &*result {
            Result::Ok(_) => 0,
            Result::Err(e) => *e,
        }
    }
}

/// Creates an Ok variant for generic types
pub fn ok<T, E>(value: T) -> Result<T, E> {
    Result::Ok(value)
}

/// Creates an Err variant for generic types
pub fn err<T, E>(error: E) -> Result<T, E> {
    Result::Err(error)
}
