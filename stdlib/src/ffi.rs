//! FFI types for Jet
//!
//! This module provides types for interfacing with C code, including
//! C-style strings and platform-specific integer types.
//!
//! # Examples
//!
//! ```
//! use jet_stdlib::ffi::{CString, CStr};
//!
//! // Create a C string
//! let c_string = CString::new("hello").expect("Failed to create CString");
//!
//! // Get a borrowed reference
//! let c_str = c_string.as_c_str();
//! ```

use crate::string::JetString;
use std::ffi::c_char;
use std::ops::Deref;

/// Platform-specific C types
pub mod ctypes {
    pub use std::ffi::{
        c_char, c_double, c_float, c_int, c_long, c_longlong, c_schar, c_short, c_uchar, c_uint,
        c_ulong, c_ulonglong, c_ushort, c_void,
    };

    /// Platform-specific `size_t` type
    #[allow(non_camel_case_types)]
    pub type c_size_t = usize;

    /// Platform-specific `ssize_t` type
    #[allow(non_camel_case_types)]
    pub type c_ssize_t = isize;

    /// Platform-specific `ptrdiff_t` type
    #[allow(non_camel_case_types)]
    pub type c_ptrdiff_t = isize;
}

/// A type representing an owned, C-compatible, nul-terminated string.
///
/// `CString` is to C strings what `String` is to Rust strings. It provides
/// ownership and lifetime semantics for C strings, ensuring proper memory
/// management and nul-termination.
///
/// # Examples
///
/// ```
/// use jet_stdlib::ffi::CString;
///
/// let c_string = CString::new("hello").expect("Failed to create CString");
/// assert_eq!(c_string.as_bytes(), b"hello\0");
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CString {
    inner: Vec<u8>,
}

/// A type representing a borrowed C string.
///
/// This is the borrowed equivalent of `CString`. It wraps a raw C string
/// pointer and provides safe accessors.
///
/// # Safety
///
/// A `CStr` is valid only as long as the memory it points to is valid
/// and contains a nul terminator.
#[derive(Debug)]
pub struct CStr {
    inner: [u8],
}

/// An error indicating that a nul byte was found in the data.
///
/// This error is returned from `CString::new` when a nul byte is found
/// in the data provided.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NulError {
    position: usize,
}

/// An error indicating invalid UTF-8 in a C string.
///
/// This error is returned when attempting to convert a `CStr` to a Rust
/// string and the data is not valid UTF-8.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Utf8Error {
    valid_up_to: usize,
}

impl NulError {
    /// Returns the position in the input where the nul byte was found.
    pub fn nul_position(&self) -> usize {
        self.position
    }
}

impl std::fmt::Display for NulError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "nul byte found in data at position {}", self.position)
    }
}

impl std::error::Error for NulError {}

impl Utf8Error {
    /// Returns the index in the string up to which valid UTF-8 was verified.
    pub fn valid_up_to(&self) -> usize {
        self.valid_up_to
    }
}

impl std::fmt::Display for Utf8Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "invalid UTF-8 in C string at position {}",
            self.valid_up_to
        )
    }
}

impl std::error::Error for Utf8Error {}

impl CString {
    /// Creates a new C string from a container of bytes.
    ///
    /// # Errors
    ///
    /// Returns `NulError` if the data contains a nul byte.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::ffi::CString;
    ///
    /// let c_string = CString::new("hello").expect("Failed to create CString");
    /// ```
    pub fn new<T: Into<Vec<u8>>>(t: T) -> Result<Self, NulError> {
        let bytes = t.into();

        // Check for nul bytes
        if let Some(pos) = bytes.iter().position(|&b| b == 0) {
            return Err(NulError { position: pos });
        }

        let mut inner = bytes;
        inner.push(0); // Add nul terminator

        Ok(Self { inner })
    }

    /// Creates a new C string without checking for nul bytes.
    ///
    /// # Safety
    ///
    /// The caller must ensure that the data does not contain any nul bytes
    /// before the end of the data.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::ffi::CString;
    ///
    /// // SAFETY: We know "hello" contains no nul bytes
    /// let c_string = unsafe { CString::new_unchecked("hello") };
    /// ```
    pub unsafe fn new_unchecked<T: Into<Vec<u8>>>(t: T) -> Self {
        let mut inner = t.into();
        inner.push(0);
        Self { inner }
    }

    /// Returns the bytes of this C string including the nul terminator.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::ffi::CString;
    ///
    /// let c_string = CString::new("hello").unwrap();
    /// assert_eq!(c_string.as_bytes(), b"hello\0");
    /// ```
    pub fn as_bytes(&self) -> &[u8] {
        &self.inner
    }

    /// Returns the bytes of this C string without the nul terminator.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::ffi::CString;
    ///
    /// let c_string = CString::new("hello").unwrap();
    /// assert_eq!(c_string.as_bytes_with_nul(), b"hello\0");
    /// ```
    pub fn as_bytes_with_nul(&self) -> &[u8] {
        &self.inner
    }

    /// Returns a borrowed CStr.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::ffi::CString;
    ///
    /// let c_string = CString::new("hello").unwrap();
    /// let c_str = c_string.as_c_str();
    /// ```
    pub fn as_c_str(&self) -> &CStr {
        // SAFETY: We know our inner data is valid UTF-8-ish and nul-terminated
        unsafe { CStr::from_bytes_with_nul_unchecked(&self.inner) }
    }

    /// Converts this C string into a raw pointer.
    ///
    /// After calling this method, the caller is responsible for the memory.
    /// The memory must be freed using `CString::from_raw`.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::ffi::CString;
    ///
    /// let c_string = CString::new("hello").unwrap();
    /// let ptr = c_string.into_raw();
    /// // Later, reclaim the memory
    /// unsafe { CString::from_raw(ptr); }
    /// ```
    pub fn into_raw(self) -> *mut c_char {
        let ptr = self.inner.as_ptr() as *mut c_char;
        std::mem::forget(self);
        ptr
    }

    /// Retakes ownership of a CString that was transferred to C via `into_raw`.
    ///
    /// # Safety
    ///
    /// This is unsafe because improper values may lead to memory problems.
    /// The pointer must have been obtained from `CString::into_raw`.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::ffi::CString;
    ///
    /// let c_string = CString::new("hello").unwrap();
    /// let ptr = c_string.into_raw();
    /// unsafe {
    ///     let c_string = CString::from_raw(ptr);
    /// }
    /// ```
    pub unsafe fn from_raw(ptr: *mut c_char) -> Self {
        // Calculate length by finding the nul terminator
        let len = std::ffi::CStr::from_ptr(ptr).to_bytes_with_nul().len();
        let inner = Vec::from_raw_parts(ptr as *mut u8, len, len);
        Self { inner }
    }

    /// Converts this C string to a JetString.
    ///
    /// # Errors
    ///
    /// Returns `Utf8Error` if the string is not valid UTF-8.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::ffi::CString;
    ///
    /// let c_string = CString::new("hello").unwrap();
    /// let jet_string = c_string.to_jet_string().unwrap();
    /// ```
    pub fn to_jet_string(&self) -> Result<JetString, Utf8Error> {
        self.as_c_str().to_jet_string()
    }

    /// Converts this C string to a JetString, replacing invalid UTF-8 sequences.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::ffi::CString;
    ///
    /// let c_string = CString::new("hello").unwrap();
    /// let jet_string = c_string.to_jet_string_lossy();
    /// ```
    pub fn to_jet_string_lossy(&self) -> JetString {
        self.as_c_str().to_jet_string_lossy()
    }
}

impl Drop for CString {
    fn drop(&mut self) {
        // Vec's Drop will handle the memory
    }
}

impl CStr {
    /// Creates a C string reference from a byte slice.
    ///
    /// # Errors
    ///
    /// Returns an error if the slice is not nul-terminated or contains
    /// interior nul bytes.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::ffi::CStr;
    ///
    /// let c_str = CStr::from_bytes_with_nul(b"hello\0").unwrap();
    /// ```
    pub fn from_bytes_with_nul(bytes: &[u8]) -> Result<&Self, NulError> {
        if bytes.is_empty() {
            return Err(NulError { position: 0 });
        }
        if bytes[bytes.len() - 1] != 0 {
            return Err(NulError {
                position: bytes.len(),
            });
        }
        // Check for interior nul bytes
        if let Some(pos) = bytes[..bytes.len() - 1].iter().position(|&b| b == 0) {
            return Err(NulError { position: pos });
        }

        // SAFETY: We've validated the bytes
        Ok(unsafe { Self::from_bytes_with_nul_unchecked(bytes) })
    }

    /// Creates a C string reference from a byte slice without checking.
    ///
    /// # Safety
    ///
    /// The caller must ensure:
    /// - The slice ends with a nul byte
    /// - The slice does not contain any other nul bytes
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::ffi::CStr;
    ///
    /// // SAFETY: We know this is a valid C string
    /// let c_str = unsafe { CStr::from_bytes_with_nul_unchecked(b"hello\0") };
    /// ```
    pub unsafe fn from_bytes_with_nul_unchecked(bytes: &[u8]) -> &Self {
        // SAFETY: CStr is a transparent wrapper around [u8]
        std::mem::transmute(bytes)
    }

    /// Creates a C string reference from a raw pointer.
    ///
    /// # Safety
    ///
    /// The pointer must point to a valid, nul-terminated C string that
    /// is valid for the lifetime of the returned reference.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::ffi::CStr;
    /// use std::ffi::c_char;
    ///
    /// let c_string = std::ffi::CString::new("hello").unwrap();
    /// let ptr = c_string.as_ptr();
    /// // SAFETY: ptr is valid and nul-terminated
    /// let c_str = unsafe { CStr::from_ptr(ptr) };
    /// ```
    pub unsafe fn from_ptr<'a>(ptr: *const c_char) -> &'a Self {
        let len = std::ffi::CStr::from_ptr(ptr).to_bytes_with_nul().len();
        let bytes = std::slice::from_raw_parts(ptr as *const u8, len);
        Self::from_bytes_with_nul_unchecked(bytes)
    }

    /// Returns the bytes of this C string without the nul terminator.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::ffi::CString;
    ///
    /// let c_string = CString::new("hello").unwrap();
    /// assert_eq!(c_string.as_c_str().to_bytes(), b"hello");
    /// ```
    pub fn to_bytes(&self) -> &[u8] {
        let bytes = &self.inner;
        &bytes[..bytes.len() - 1] // Exclude nul terminator
    }

    /// Returns the bytes of this C string including the nul terminator.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::ffi::CString;
    ///
    /// let c_string = CString::new("hello").unwrap();
    /// assert_eq!(c_string.as_c_str().to_bytes_with_nul(), b"hello\0");
    /// ```
    pub fn to_bytes_with_nul(&self) -> &[u8] {
        &self.inner
    }

    /// Converts this C string to a Rust string slice.
    ///
    /// # Errors
    ///
    /// Returns `Utf8Error` if the string is not valid UTF-8.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::ffi::CString;
    ///
    /// let c_string = CString::new("hello").unwrap();
    /// let s = c_string.as_c_str().to_str().unwrap();
    /// assert_eq!(s, "hello");
    /// ```
    pub fn to_str(&self) -> Result<&str, Utf8Error> {
        match std::str::from_utf8(self.to_bytes()) {
            Ok(s) => Ok(s),
            Err(e) => Err(Utf8Error {
                valid_up_to: e.valid_up_to(),
            }),
        }
    }

    /// Converts this C string to a Rust string slice, replacing invalid UTF-8.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::ffi::CString;
    ///
    /// let c_string = CString::new("hello").unwrap();
    /// let s = c_string.as_c_str().to_string_lossy();
    /// ```
    pub fn to_string_lossy(&self) -> std::borrow::Cow<'_, str> {
        String::from_utf8_lossy(self.to_bytes())
    }

    /// Converts this C string to a JetString.
    ///
    /// # Errors
    ///
    /// Returns `Utf8Error` if the string is not valid UTF-8.
    pub fn to_jet_string(&self) -> Result<JetString, Utf8Error> {
        self.to_str().map(JetString::from_str)
    }

    /// Converts this C string to a JetString, replacing invalid UTF-8.
    pub fn to_jet_string_lossy(&self) -> JetString {
        JetString::from_str(&self.to_string_lossy())
    }

    /// Returns a raw pointer to the C string.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::ffi::CString;
    ///
    /// let c_string = CString::new("hello").unwrap();
    /// let ptr = c_string.as_c_str().as_ptr();
    /// ```
    pub fn as_ptr(&self) -> *const c_char {
        self.inner.as_ptr() as *const c_char
    }

    /// Returns true if the C string is empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::ffi::CString;
    ///
    /// let c_string = CString::new("").unwrap();
    /// assert!(c_string.as_c_str().is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.inner.len() <= 1 // Just the nul terminator
    }

    /// Returns the length of the C string in bytes (excluding nul terminator).
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_stdlib::ffi::CString;
    ///
    /// let c_string = CString::new("hello").unwrap();
    /// assert_eq!(c_string.as_c_str().len(), 5);
    /// ```
    pub fn len(&self) -> usize {
        self.inner.len().saturating_sub(1)
    }
}

impl Deref for CString {
    type Target = CStr;

    fn deref(&self) -> &CStr {
        self.as_c_str()
    }
}

impl AsRef<CStr> for CString {
    fn as_ref(&self) -> &CStr {
        self.as_c_str()
    }
}

impl AsRef<CStr> for CStr {
    fn as_ref(&self) -> &CStr {
        self
    }
}

// C ABI exports

/// Creates a new CString from a C string pointer
///
/// # Safety
/// The caller must ensure ptr is a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn jet_cstring_new(ptr: *const c_char) -> *mut CString {
    if ptr.is_null() {
        return std::ptr::null_mut();
    }
    let c_str = std::ffi::CStr::from_ptr(ptr);
    match CString::new(c_str.to_bytes()) {
        Ok(s) => Box::into_raw(Box::new(s)),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Frees a CString
#[no_mangle]
pub unsafe extern "C" fn jet_cstring_free(s: *mut CString) {
    if !s.is_null() {
        unsafe { drop(Box::from_raw(s)) };
    }
}

/// Returns the bytes of a CString
///
/// # Safety
/// The caller must ensure s is a valid pointer.
/// The returned pointer is valid as long as the CString is not modified.
#[no_mangle]
pub unsafe extern "C" fn jet_cstring_as_ptr(s: *const CString) -> *const c_char {
    if s.is_null() {
        return std::ptr::null();
    }
    unsafe { (*s).as_ptr() }
}

/// Returns the length of a CString (excluding nul terminator)
///
/// # Safety
/// The caller must ensure s is a valid pointer.
#[no_mangle]
pub unsafe extern "C" fn jet_cstring_len(s: *const CString) -> usize {
    if s.is_null() {
        return 0;
    }
    unsafe { (*s).len() }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cstring_new() {
        let c_string = CString::new("hello").unwrap();
        assert_eq!(c_string.as_bytes(), b"hello\0");
        assert_eq!(c_string.to_bytes(), b"hello");
    }

    #[test]
    fn test_cstring_nul_error() {
        let result = CString::new("hel\0lo");
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err.nul_position(), 3);
    }

    #[test]
    fn test_cstring_empty() {
        let c_string = CString::new("").unwrap();
        assert!(c_string.is_empty());
        assert_eq!(c_string.len(), 0);
    }

    #[test]
    fn test_cstr_from_bytes() {
        let c_str = CStr::from_bytes_with_nul(b"hello\0").unwrap();
        assert_eq!(c_str.to_bytes(), b"hello");
    }

    #[test]
    fn test_cstr_from_bytes_no_nul() {
        let result = CStr::from_bytes_with_nul(b"hello");
        assert!(result.is_err());
    }

    #[test]
    fn test_cstr_from_bytes_interior_nul() {
        let result = CStr::from_bytes_with_nul(b"hel\0lo\0");
        assert!(result.is_err());
    }

    #[test]
    fn test_cstring_to_str() {
        let c_string = CString::new("hello").unwrap();
        assert_eq!(c_string.to_str().unwrap(), "hello");
    }

    #[test]
    fn test_cstring_into_raw_from_raw() {
        let c_string = CString::new("hello").unwrap();
        let ptr = c_string.into_raw();

        // Reclaim ownership
        let c_string = unsafe { CString::from_raw(ptr) };
        assert_eq!(c_string.to_str().unwrap(), "hello");
    }

    #[test]
    fn test_cstring_to_jet_string() {
        let c_string = CString::new("hello").unwrap();
        let jet_string = c_string.to_jet_string().unwrap();
        assert_eq!(jet_string.as_str(), "hello");
    }

    #[test]
    fn test_ctypes() {
        // Just verify the types exist and have the right size
        assert_eq!(std::mem::size_of::<ctypes::c_char>(), 1);
        assert_eq!(std::mem::size_of::<ctypes::c_int>(), 4);
        assert_eq!(
            std::mem::size_of::<ctypes::c_size_t>(),
            std::mem::size_of::<usize>()
        );
        assert_eq!(
            std::mem::size_of::<ctypes::c_ssize_t>(),
            std::mem::size_of::<isize>()
        );
    }
}
