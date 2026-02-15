//! Option - Optional value type for Jet
//!
//! This module provides the Option type representing either Some value or None.

/// Represents an optional value: either `Some(T)` or `None`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum Option<T> {
    /// Some value of type T
    Some(T),
    /// No value
    #[default]
    None,
}

impl<T> Option<T> {
    /// Returns true if the option is Some.
    pub fn is_some(&self) -> bool {
        matches!(self, Option::Some(_))
    }

    /// Returns true if the option is None.
    pub fn is_none(&self) -> bool {
        !self.is_some()
    }

    /// Returns true if the option is Some and the value satisfies the predicate.
    pub fn is_some_and<F>(&self, f: F) -> bool
    where
        F: FnOnce(&T) -> bool,
    {
        match self {
            Option::Some(v) => f(v),
            Option::None => false,
        }
    }

    /// Unwraps the option, returning the value.
    ///
    /// Panics if the option is None.
    pub fn unwrap(self) -> T {
        match self {
            Option::Some(v) => v,
            Option::None => panic!("called `Option::unwrap()` on a `None` value"),
        }
    }

    /// Returns the contained value or a default.
    pub fn unwrap_or(self, default: T) -> T {
        match self {
            Option::Some(v) => v,
            Option::None => default,
        }
    }

    /// Returns the contained value or computes a default.
    pub fn unwrap_or_else<F>(self, f: F) -> T
    where
        F: FnOnce() -> T,
    {
        match self {
            Option::Some(v) => v,
            Option::None => f(),
        }
    }

    /// Returns the contained value or panics with a message.
    pub fn expect(self, msg: &str) -> T {
        match self {
            Option::Some(v) => v,
            Option::None => panic!("{}", msg),
        }
    }

    /// Maps `Option<T>` to `Option<U>` using a function.
    pub fn map<U, F>(self, f: F) -> Option<U>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Option::Some(v) => Option::Some(f(v)),
            Option::None => Option::None,
        }
    }

    /// Applies a function to the contained value (if any), or returns a default.
    pub fn map_or<U, F>(self, default: U, f: F) -> U
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Option::Some(v) => f(v),
            Option::None => default,
        }
    }

    /// Applies a function to the contained value (if any), or computes a default.
    pub fn map_or_else<U, D, F>(self, default: D, f: F) -> U
    where
        D: FnOnce() -> U,
        F: FnOnce(T) -> U,
    {
        match self {
            Option::Some(v) => f(v),
            Option::None => default(),
        }
    }

    /// Returns None if the option is None, otherwise calls f.
    pub fn flat_map<U, F>(self, f: F) -> Option<U>
    where
        F: FnOnce(T) -> Option<U>,
    {
        match self {
            Option::Some(v) => f(v),
            Option::None => Option::None,
        }
    }

    /// Returns the option if it contains a value, otherwise returns other.
    pub fn or(self, other: Option<T>) -> Option<T> {
        match self {
            Option::Some(_) => self,
            Option::None => other,
        }
    }

    /// Returns the option if it contains a value, otherwise calls f.
    pub fn or_else<F>(self, f: F) -> Option<T>
    where
        F: FnOnce() -> Option<T>,
    {
        match self {
            Option::Some(_) => self,
            Option::None => f(),
        }
    }

    /// Returns None if the option is None, otherwise returns other.
    pub fn and<U>(self, other: Option<U>) -> Option<U> {
        match self {
            Option::Some(_) => other,
            Option::None => Option::None,
        }
    }

    /// Returns None if the option is None, otherwise calls f.
    pub fn and_then<U, F>(self, f: F) -> Option<U>
    where
        F: FnOnce(T) -> Option<U>,
    {
        self.flat_map(f)
    }

    /// Filters the option based on a predicate.
    pub fn filter<F>(self, f: F) -> Option<T>
    where
        F: FnOnce(&T) -> bool,
    {
        match self {
            Option::Some(v) => {
                if f(&v) {
                    Option::Some(v)
                } else {
                    Option::None
                }
            }
            Option::None => Option::None,
        }
    }

    /// Returns None if the option is None, otherwise returns other.
    pub fn xor(self, other: Option<T>) -> Option<T> {
        match (self, other) {
            (Option::Some(_), Option::Some(_)) => Option::None,
            (Option::Some(a), Option::None) => Option::Some(a),
            (Option::None, Option::Some(b)) => Option::Some(b),
            (Option::None, Option::None) => Option::None,
        }
    }

    /// Transforms the `Option<T>` into a `Result<T, E>`.
    pub fn ok_or<E>(self, err: E) -> Result<T, E> {
        match self {
            Option::Some(v) => Result::Ok(v),
            Option::None => Result::Err(err),
        }
    }

    /// Transforms the `Option<T>` into a `Result<T, E>` using a function.
    pub fn ok_or_else<E, F>(self, err: F) -> Result<T, E>
    where
        F: FnOnce() -> E,
    {
        match self {
            Option::Some(v) => Result::Ok(v),
            Option::None => Result::Err(err()),
        }
    }

    /// Returns an iterator over the possibly contained value.
    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            inner: self.as_ref(),
        }
    }

    /// Returns an iterator over the possibly contained value (mutable).
    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
        IterMut {
            inner: self.as_mut(),
        }
    }

    /// Returns an Option with a reference to the value.
    pub fn as_ref(&self) -> Option<&T> {
        match self {
            Option::Some(v) => Option::Some(v),
            Option::None => Option::None,
        }
    }

    /// Returns an Option with a mutable reference to the value.
    pub fn as_mut(&mut self) -> Option<&mut T> {
        match self {
            Option::Some(v) => Option::Some(v),
            Option::None => Option::None,
        }
    }

    /// Zips two options into one.
    pub fn zip<U>(self, other: Option<U>) -> Option<(T, U)> {
        match (self, other) {
            (Option::Some(a), Option::Some(b)) => Option::Some((a, b)),
            _ => Option::None,
        }
    }

    /// Zips two options with a function.
    pub fn zip_with<U, R, F>(self, other: Option<U>, f: F) -> Option<R>
    where
        F: FnOnce(T, U) -> R,
    {
        self.zip(other).map(|(a, b)| f(a, b))
    }

    /// Transposes an Option of a Result into a Result of an Option.
    pub fn transpose<E, U>(self) -> Result<Option<U>, E>
    where
        T: Into<Result<U, E>>,
    {
        match self {
            Option::Some(v) => v.into().map(Option::Some),
            Option::None => Result::Ok(Option::None),
        }
    }

    /// Flattens an Option of an Option.
    pub fn flatten<U>(self) -> Option<U>
    where
        T: Into<Option<U>>,
    {
        self.flat_map(|v| v.into())
    }

    /// Returns the contained value if Some, otherwise inserts and returns the value.
    pub fn get_or_insert(&mut self, value: T) -> &mut T {
        match self {
            Option::Some(v) => v,
            Option::None => {
                *self = Option::Some(value);
                match self {
                    Option::Some(v) => v,
                    Option::None => unreachable!(),
                }
            }
        }
    }

    /// Returns the contained value if Some, otherwise inserts and returns the computed value.
    pub fn get_or_insert_with<F>(&mut self, f: F) -> &mut T
    where
        F: FnOnce() -> T,
    {
        match self {
            Option::Some(v) => v,
            Option::None => {
                *self = Option::Some(f());
                match self {
                    Option::Some(v) => v,
                    Option::None => unreachable!(),
                }
            }
        }
    }

    /// Takes the value out of the option, leaving None in its place.
    pub fn take(&mut self) -> Option<T> {
        std::mem::replace(self, Option::None)
    }

    /// Replaces the value in the option, returning the old value.
    pub fn replace(&mut self, value: T) -> Option<T> {
        std::mem::replace(self, Option::Some(value))
    }
}

impl<T> From<T> for Option<T> {
    fn from(value: T) -> Self {
        Option::Some(value)
    }
}

/// Iterator over an Option
pub struct Iter<'a, T> {
    inner: Option<&'a T>,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> std::option::Option<Self::Item> {
        match self.inner.take() {
            Option::Some(v) => std::option::Option::Some(v),
            Option::None => std::option::Option::None,
        }
    }

    fn size_hint(&self) -> (usize, std::option::Option<usize>) {
        let len = if self.inner.is_some() { 1 } else { 0 };
        (len, std::option::Option::Some(len))
    }
}

/// Mutable iterator over an Option
pub struct IterMut<'a, T> {
    inner: Option<&'a mut T>,
}

impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> std::option::Option<Self::Item> {
        match self.inner.take() {
            Option::Some(v) => std::option::Option::Some(v),
            Option::None => std::option::Option::None,
        }
    }

    fn size_hint(&self) -> (usize, std::option::Option<usize>) {
        let len = if self.inner.is_some() { 1 } else { 0 };
        (len, std::option::Option::Some(len))
    }
}

// C ABI exports

/// Creates a Some variant
#[no_mangle]
pub extern "C" fn jet_option_some(value: i64) -> *mut Option<i64> {
    Box::into_raw(Box::new(Option::Some(value)))
}

/// Creates a None variant
#[no_mangle]
pub extern "C" fn jet_option_none() -> *mut Option<i64> {
    Box::into_raw(Box::new(Option::None))
}

/// Frees an option
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_option_free`'s FFI contract.
pub unsafe extern "C" fn jet_option_free(opt: *mut Option<i64>) {
    if !opt.is_null() {
        unsafe { drop(Box::from_raw(opt)) };
    }
}

/// Returns true if the option is Some
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_option_is_some`'s FFI contract.
pub unsafe extern "C" fn jet_option_is_some(opt: *const Option<i64>) -> bool {
    unsafe { (*opt).is_some() }
}

/// Returns true if the option is None
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_option_is_none`'s FFI contract.
pub unsafe extern "C" fn jet_option_is_none(opt: *const Option<i64>) -> bool {
    unsafe { (*opt).is_none() }
}

/// Unwraps an option (returns 0 if None)
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_option_unwrap`'s FFI contract.
pub unsafe extern "C" fn jet_option_unwrap(opt: *const Option<i64>) -> i64 {
    unsafe {
        match &*opt {
            Option::Some(v) => *v,
            Option::None => 0,
        }
    }
}

/// Creates a Some variant for generic types
pub fn some<T>(value: T) -> Option<T> {
    Option::Some(value)
}

/// Creates a None variant for generic types
pub fn none<T>() -> Option<T> {
    Option::None
}

// Result type for use with Option::ok_or
pub enum Result<T, E> {
    Ok(T),
    Err(E),
}

impl<T, E> Result<T, E> {
    pub fn map<U, F>(self, f: F) -> Result<U, E>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Result::Ok(v) => Result::Ok(f(v)),
            Result::Err(e) => Result::Err(e),
        }
    }
}
