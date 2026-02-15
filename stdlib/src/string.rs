//! String - UTF-8 string implementation for Jet
//!
//! This module provides a UTF-8 encoded string type with small string optimization
//! (SSO) for strings up to 23 bytes on 64-bit systems.

use std::alloc::{alloc, dealloc, Layout};
use std::ptr;
use std::slice;
use std::str::{self, Utf8Error};

/// Small String Optimization (SSO) threshold
/// On 64-bit systems, we can store up to 23 bytes inline
const SSO_CAPACITY: usize = 23;

/// A UTF-8 encoded string with small string optimization.
///
/// For strings up to 23 bytes, the data is stored inline in the struct
/// without heap allocation. Larger strings are stored on the heap.
pub struct JetString {
    /// Inline storage for small strings (SSO)
    inline: [u8; SSO_CAPACITY],
    /// Pointer to heap-allocated buffer (null if using inline storage)
    heap_ptr: *mut u8,
    /// Capacity of the heap buffer (0 if using inline storage)
    heap_capacity: usize,
    /// Length of the string in bytes
    len: usize,
}

/// String builder for efficient string concatenation
pub struct StringBuilder {
    buffer: Vec<u8>,
}

impl JetString {
    /// Creates a new empty string.
    pub fn new() -> Self {
        JetString {
            inline: [0; SSO_CAPACITY],
            heap_ptr: ptr::null_mut(),
            heap_capacity: 0,
            len: 0,
        }
    }

    /// Creates a string with the specified capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        if capacity <= SSO_CAPACITY {
            return JetString::new();
        }

        let layout = Layout::array::<u8>(capacity).expect("Layout computation failed");
        let ptr = unsafe { alloc(layout) };

        if ptr.is_null() {
            panic!("Allocation failed");
        }

        JetString {
            inline: [0; SSO_CAPACITY],
            heap_ptr: ptr,
            heap_capacity: capacity,
            len: 0,
        }
    }

    /// Creates a string from a UTF-8 byte slice.
    pub fn from_utf8(bytes: &[u8]) -> Result<Self, Utf8Error> {
        str::from_utf8(bytes)?;
        Ok(unsafe { Self::from_utf8_unchecked(bytes) })
    }

    /// Creates a string from a UTF-8 byte slice without validation.
    ///
    /// # Safety
    /// The bytes must be valid UTF-8.
    pub unsafe fn from_utf8_unchecked(bytes: &[u8]) -> Self {
        let len = bytes.len();
        let mut s = JetString::with_capacity(len);
        s.len = len;

        if len <= SSO_CAPACITY {
            // Store inline
            ptr::copy_nonoverlapping(bytes.as_ptr(), s.inline.as_mut_ptr(), len);
        } else {
            // Store on heap
            ptr::copy_nonoverlapping(bytes.as_ptr(), s.heap_ptr, len);
        }

        s
    }

    /// Creates a string from a `&str`.
    #[allow(clippy::should_implement_trait)]
    pub fn from_str(s: &str) -> Self {
        unsafe { Self::from_utf8_unchecked(s.as_bytes()) }
    }

    /// Returns the length of the string in bytes.
    pub fn len(&self) -> usize {
        self.len
    }

    /// Returns true if the string is empty.
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Returns the capacity of the string.
    pub fn capacity(&self) -> usize {
        if self.is_inline() {
            SSO_CAPACITY
        } else {
            self.heap_capacity
        }
    }

    /// Returns true if using inline storage.
    fn is_inline(&self) -> bool {
        self.heap_ptr.is_null()
    }

    /// Returns a pointer to the string data.
    fn data_ptr(&self) -> *const u8 {
        if self.is_inline() {
            self.inline.as_ptr()
        } else {
            self.heap_ptr
        }
    }

    /// Returns a mutable pointer to the string data.
    fn data_ptr_mut(&mut self) -> *mut u8 {
        if self.is_inline() {
            self.inline.as_mut_ptr()
        } else {
            self.heap_ptr
        }
    }

    /// Returns a byte slice of the string's contents.
    pub fn as_bytes(&self) -> &[u8] {
        unsafe { slice::from_raw_parts(self.data_ptr(), self.len) }
    }

    /// Returns a mutable byte slice of the string's contents.
    #[allow(dead_code)]
    fn as_bytes_mut(&mut self) -> &mut [u8] {
        unsafe { slice::from_raw_parts_mut(self.data_ptr_mut(), self.len) }
    }

    /// Returns a string slice of the contents.
    pub fn as_str(&self) -> &str {
        unsafe { str::from_utf8_unchecked(self.as_bytes()) }
    }

    /// Appends a character to the end of the string.
    pub fn push(&mut self, ch: char) {
        let mut buf = [0; 4];
        let ch_bytes = ch.encode_utf8(&mut buf).as_bytes();
        self.push_str(unsafe { str::from_utf8_unchecked(ch_bytes) });
    }

    /// Appends a string slice to the end.
    pub fn push_str(&mut self, s: &str) {
        let new_len = self.len + s.len();

        if new_len > self.capacity() {
            self.grow(new_len);
        }

        unsafe {
            ptr::copy_nonoverlapping(
                s.as_bytes().as_ptr(),
                self.data_ptr_mut().add(self.len),
                s.len(),
            );
        }

        self.len = new_len;
    }

    /// Removes and returns the last character.
    pub fn pop(&mut self) -> Option<char> {
        if self.is_empty() {
            return None;
        }

        let bytes = self.as_bytes();
        let last_char_start = (0..bytes.len())
            .rev()
            .find(|&i| (bytes[i] & 0b11000000) != 0b10000000)?;

        let ch = unsafe {
            str::from_utf8_unchecked(&bytes[last_char_start..])
                .chars()
                .next()?
        };

        self.len = last_char_start;
        Some(ch)
    }

    /// Truncates the string to the specified length.
    pub fn truncate(&mut self, new_len: usize) {
        if new_len < self.len {
            // Ensure we don't truncate in the middle of a UTF-8 sequence
            let bytes = self.as_bytes();
            let valid_len = (0..=new_len)
                .rev()
                .find(|&i| i == 0 || (bytes[i] & 0b11000000) != 0b10000000)
                .unwrap_or(0);

            self.len = valid_len;
        }
    }

    /// Clears the string, removing all contents.
    pub fn clear(&mut self) {
        self.len = 0;
    }

    /// Returns true if the string contains the given pattern.
    pub fn contains(&self, pat: &str) -> bool {
        self.as_str().contains(pat)
    }

    /// Returns true if the string starts with the given pattern.
    pub fn starts_with(&self, pat: &str) -> bool {
        self.as_str().starts_with(pat)
    }

    /// Returns true if the string ends with the given pattern.
    pub fn ends_with(&self, pat: &str) -> bool {
        self.as_str().ends_with(pat)
    }

    /// Returns the byte index of the first occurrence of the pattern.
    pub fn find(&self, pat: &str) -> Option<usize> {
        self.as_str().find(pat)
    }

    /// Returns the byte index of the last occurrence of the pattern.
    pub fn rfind(&self, pat: &str) -> Option<usize> {
        self.as_str().rfind(pat)
    }

    /// Replaces all occurrences of a pattern with another string.
    pub fn replace(&self, from: &str, to: &str) -> JetString {
        let result = self.as_str().replace(from, to);
        JetString::from_str(&result)
    }

    /// Replaces the first N occurrences of a pattern.
    pub fn replacen(&self, from: &str, to: &str, count: usize) -> JetString {
        let result = self.as_str().replacen(from, to, count);
        JetString::from_str(&result)
    }

    /// Returns a lowercase version of the string.
    pub fn to_lowercase(&self) -> JetString {
        JetString::from_str(&self.as_str().to_lowercase())
    }

    /// Returns an uppercase version of the string.
    pub fn to_uppercase(&self) -> JetString {
        JetString::from_str(&self.as_str().to_uppercase())
    }

    /// Returns a string with leading and trailing whitespace removed.
    pub fn trim(&self) -> &str {
        self.as_str().trim()
    }

    /// Returns a string with leading whitespace removed.
    pub fn trim_start(&self) -> &str {
        self.as_str().trim_start()
    }

    /// Returns a string with trailing whitespace removed.
    pub fn trim_end(&self) -> &str {
        self.as_str().trim_end()
    }

    /// Returns an iterator over the characters in the string.
    pub fn chars(&self) -> std::str::Chars<'_> {
        self.as_str().chars()
    }

    /// Returns an iterator over the lines in the string.
    pub fn lines(&self) -> std::str::Lines<'_> {
        self.as_str().lines()
    }

    /// Returns an iterator over substrings split by whitespace.
    pub fn split_whitespace(&self) -> std::str::SplitWhitespace<'_> {
        self.as_str().split_whitespace()
    }

    /// Splits the string by the given pattern.
    pub fn split<'a>(&'a self, pat: &'a str) -> impl Iterator<Item = &'a str> + 'a {
        self.as_str().split(pat)
    }

    /// Returns a substring from start (inclusive) to end (exclusive).
    pub fn slice(&self, start: usize, end: usize) -> &str {
        &self.as_str()[start..end.min(self.len)]
    }

    /// Repeats the string n times.
    pub fn repeat(&self, n: usize) -> JetString {
        JetString::from_str(&self.as_str().repeat(n))
    }

    /// Returns true if all characters are whitespace.
    pub fn is_whitespace(&self) -> bool {
        self.as_str().chars().all(|c| c.is_whitespace())
    }

    /// Returns true if all characters are numeric.
    pub fn is_numeric(&self) -> bool {
        self.as_str().chars().all(|c| c.is_numeric())
    }

    /// Returns true if all characters are alphabetic.
    pub fn is_alphabetic(&self) -> bool {
        self.as_str().chars().all(|c| c.is_alphabetic())
    }

    /// Returns true if all characters are alphanumeric.
    pub fn is_alphanumeric(&self) -> bool {
        self.as_str().chars().all(|c| c.is_alphanumeric())
    }

    /// Returns the character at the given byte index.
    pub fn char_at(&self, index: usize) -> Option<char> {
        self.as_str().chars().nth(index)
    }

    /// Returns the byte length of the string.
    pub fn byte_len(&self) -> usize {
        self.len
    }

    /// Returns the character length of the string.
    pub fn char_len(&self) -> usize {
        self.as_str().chars().count()
    }

    // Private methods

    fn grow(&mut self, min_capacity: usize) {
        let new_capacity = (min_capacity * 2).max(64);
        let new_layout = Layout::array::<u8>(new_capacity).expect("Layout computation failed");

        let new_ptr = unsafe { alloc(new_layout) };
        if new_ptr.is_null() {
            panic!("Allocation failed");
        }

        // Copy existing data
        unsafe {
            ptr::copy_nonoverlapping(self.data_ptr(), new_ptr, self.len);
        }

        // If we were heap-allocated, free the old buffer
        if !self.is_inline() {
            let old_layout =
                Layout::array::<u8>(self.heap_capacity).expect("Layout computation failed");
            unsafe {
                dealloc(self.heap_ptr, old_layout);
            }
        }

        self.heap_ptr = new_ptr;
        self.heap_capacity = new_capacity;
    }
}

impl Default for JetString {
    fn default() -> Self {
        JetString::new()
    }
}

impl Clone for JetString {
    fn clone(&self) -> Self {
        unsafe { JetString::from_utf8_unchecked(self.as_bytes()) }
    }
}

impl Drop for JetString {
    fn drop(&mut self) {
        if !self.is_inline() {
            let layout =
                Layout::array::<u8>(self.heap_capacity).expect("Layout computation failed");
            unsafe {
                dealloc(self.heap_ptr, layout);
            }
        }
    }
}

impl PartialEq for JetString {
    fn eq(&self, other: &Self) -> bool {
        self.as_bytes() == other.as_bytes()
    }
}

impl Eq for JetString {}

impl std::hash::Hash for JetString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_bytes().hash(state);
    }
}

impl std::fmt::Display for JetString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl std::fmt::Debug for JetString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.as_str())
    }
}

impl From<&str> for JetString {
    fn from(s: &str) -> Self {
        JetString::from_str(s)
    }
}

impl std::str::FromStr for JetString {
    type Err = core::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(JetString::from_str(s))
    }
}

impl StringBuilder {
    /// Creates a new empty string builder.
    pub fn new() -> Self {
        StringBuilder { buffer: Vec::new() }
    }

    /// Creates a string builder with the specified capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        StringBuilder {
            buffer: Vec::with_capacity(capacity),
        }
    }

    /// Appends a string slice.
    pub fn append(&mut self, s: &str) {
        self.buffer.extend_from_slice(s.as_bytes());
    }

    /// Appends a character.
    pub fn append_char(&mut self, ch: char) {
        let mut buf = [0; 4];
        self.buffer
            .extend_from_slice(ch.encode_utf8(&mut buf).as_bytes());
    }

    /// Returns the current length.
    pub fn len(&self) -> usize {
        self.buffer.len()
    }

    /// Returns true if empty.
    pub fn is_empty(&self) -> bool {
        self.buffer.is_empty()
    }

    /// Clears the builder.
    pub fn clear(&mut self) {
        self.buffer.clear();
    }

    /// Builds the final string.
    pub fn build(self) -> JetString {
        unsafe { JetString::from_utf8_unchecked(&self.buffer) }
    }

    /// Returns the current contents as a string slice.
    pub fn as_str(&self) -> &str {
        unsafe { str::from_utf8_unchecked(&self.buffer) }
    }
}

impl Default for StringBuilder {
    fn default() -> Self {
        StringBuilder::new()
    }
}

// C ABI exports

/// Creates a new empty string
#[no_mangle]
pub extern "C" fn jet_string_new() -> *mut JetString {
    Box::into_raw(Box::new(JetString::new()))
}

/// Creates a string from a C string
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_string_from_c_str`'s FFI contract.
pub unsafe extern "C" fn jet_string_from_c_str(s: *const u8, len: usize) -> *mut JetString {
    if s.is_null() {
        return jet_string_new();
    }

    let slice = unsafe { slice::from_raw_parts(s, len) };
    match JetString::from_utf8(slice) {
        Ok(string) => Box::into_raw(Box::new(string)),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Frees a string
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_string_free`'s FFI contract.
pub unsafe extern "C" fn jet_string_free(s: *mut JetString) {
    if !s.is_null() {
        unsafe { drop(Box::from_raw(s)) };
    }
}

/// Returns the length of a string
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_string_len`'s FFI contract.
pub unsafe extern "C" fn jet_string_len(s: *const JetString) -> usize {
    unsafe { (*s).len() }
}

/// Returns true if the string is empty
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_string_is_empty`'s FFI contract.
pub unsafe extern "C" fn jet_string_is_empty(s: *const JetString) -> bool {
    unsafe { (*s).is_empty() }
}

/// Clears a string
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_string_clear`'s FFI contract.
pub unsafe extern "C" fn jet_string_clear(s: *mut JetString) {
    unsafe { (*s).clear() }
}

/// Creates a new string builder
#[no_mangle]
pub extern "C" fn jet_string_builder_new() -> *mut StringBuilder {
    Box::into_raw(Box::new(StringBuilder::new()))
}

/// Frees a string builder
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_string_builder_free`'s FFI contract.
pub unsafe extern "C" fn jet_string_builder_free(builder: *mut StringBuilder) {
    if !builder.is_null() {
        unsafe { drop(Box::from_raw(builder)) };
    }
}

/// Builds a string from a builder
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_string_builder_build`'s FFI contract.
pub unsafe extern "C" fn jet_string_builder_build(builder: *mut StringBuilder) -> *mut JetString {
    if builder.is_null() {
        return std::ptr::null_mut();
    }

    let builder = unsafe { Box::from_raw(builder) };
    Box::into_raw(Box::new(builder.build()))
}
