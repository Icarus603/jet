//! Traits - Core trait implementations for Jet
//!
//! This module provides runtime support for core traits used by the Jet
//! type system, including Eq, Ord, Hash, Show, etc.

use std::cmp::Ordering;

/// Types that can be compared for equality.
///
/// Two values are equal if they represent the same value.
/// Types implementing Eq must ensure:
/// - Reflexivity: a == a is always true
/// - Symmetry: a == b implies b == a
/// - Transitivity: a == b and b == c implies a == c
pub trait Eq {
    /// Returns true if self is equal to other.
    fn eq(&self, other: &Self) -> bool;

    /// Returns true if self is not equal to other.
    /// Default implementation uses eq.
    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}

/// Ordering of values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum JetOrdering {
    Less,
    Equal,
    Greater,
}

impl JetOrdering {
    /// Converts to std::cmp::Ordering
    pub fn to_std(self) -> Ordering {
        match self {
            JetOrdering::Less => Ordering::Less,
            JetOrdering::Equal => Ordering::Equal,
            JetOrdering::Greater => Ordering::Greater,
        }
    }

    /// Converts from std::cmp::Ordering
    pub fn from_std(ordering: Ordering) -> Self {
        match ordering {
            Ordering::Less => JetOrdering::Less,
            Ordering::Equal => JetOrdering::Equal,
            Ordering::Greater => JetOrdering::Greater,
        }
    }
}

/// Types that can be ordered.
///
/// Types implementing Ord must ensure:
/// - Totality: for any a, b: exactly one of a < b, a == b, a > b is true
/// - Antisymmetry: a < b implies not b < a
/// - Transitivity: a < b and b < c implies a < c
pub trait Ord: Eq {
    /// Compares self to other and returns the ordering.
    fn compare(&self, other: &Self) -> JetOrdering;

    /// Returns true if self is less than other.
    fn lt(&self, other: &Self) -> bool {
        matches!(self.compare(other), JetOrdering::Less)
    }

    /// Returns true if self is less than or equal to other.
    fn le(&self, other: &Self) -> bool {
        !matches!(self.compare(other), JetOrdering::Greater)
    }

    /// Returns true if self is greater than other.
    fn gt(&self, other: &Self) -> bool {
        matches!(self.compare(other), JetOrdering::Greater)
    }

    /// Returns true if self is greater than or equal to other.
    fn ge(&self, other: &Self) -> bool {
        !matches!(self.compare(other), JetOrdering::Less)
    }

    /// Returns the minimum of self and other.
    fn min<'a>(&'a self, other: &'a Self) -> &'a Self
    where
        Self: Sized,
    {
        if self.lt(other) {
            self
        } else {
            other
        }
    }

    /// Returns the maximum of self and other.
    fn max<'a>(&'a self, other: &'a Self) -> &'a Self
    where
        Self: Sized,
    {
        if self.gt(other) {
            self
        } else {
            other
        }
    }

    /// Clamps self between min and max.
    fn clamp<'a>(&'a self, min: &'a Self, max: &'a Self) -> &'a Self
    where
        Self: Sized,
    {
        if self.lt(min) {
            min
        } else if self.gt(max) {
            max
        } else {
            self
        }
    }
}

/// Types that can be displayed as strings.
///
/// Show is used for user-facing output and debugging.
pub trait Show {
    /// Returns a string representation of self.
    fn show(&self) -> String;
}

/// Types that can be converted to strings.
///
/// ToString is for types that can be converted to a string representation.
pub trait ToString {
    /// Converts self to a string.
    fn to_string(&self) -> String;
}

/// Types that can be parsed from strings.
///
/// FromStr is for types that can be parsed from string input.
pub trait FromStr: Sized {
    /// The error type returned when parsing fails.
    type Err;

    /// Parses a string to produce a value.
    fn from_str(s: &str) -> Result<Self, Self::Err>;
}

/// Types that can be cloned.
///
/// Clone creates a deep copy of a value.
/// Types implementing Clone must ensure the new value is independent.
pub trait Clone {
    /// Returns a clone of self.
    fn clone(&self) -> Self;
}

/// Types that can be copied bit-for-bit.
///
/// Copy is a marker trait for types that can be duplicated by simple bitwise copy.
/// All Copy types are also Clone.
pub trait Copy: Clone {}

/// Types that can be hashed.
///
/// Hash produces a deterministic hash code for a value.
/// Types implementing Hash must ensure:
/// - If a == b, then hash(a) == hash(b)
/// - The hash is consistent for the lifetime of the value
pub trait Hash {
    /// Returns a 64-bit hash of self.
    fn hash(&self) -> u64;

    /// Hashes self into a hasher.
    /// Default implementation uses hash.
    fn hash_with<H: Hasher>(&self, mut hasher: H) -> H {
        hasher.write_u64(self.hash());
        hasher
    }
}

/// A trait for hashing state.
///
/// Hasher is used to incrementally build a hash value.
pub trait Hasher {
    /// Writes a single byte.
    fn write_u8(&mut self, byte: u8) -> &mut Self;

    /// Writes a 64-bit integer.
    fn write_u64(&mut self, value: u64) -> &mut Self;

    /// Writes a slice of bytes.
    fn write_bytes(&mut self, bytes: &[u8]) -> &mut Self;

    /// Finishes and returns the hash value.
    fn finish(&self) -> u64;
}

/// Default hasher using FNV-1a
pub struct DefaultHasher {
    state: u64,
}

impl DefaultHasher {
    /// Creates a new default hasher.
    pub fn new() -> Self {
        DefaultHasher {
            state: 0xcbf29ce484222325, // FNV offset basis
        }
    }
}

impl std::default::Default for DefaultHasher {
    fn default() -> Self {
        Self::new()
    }
}

impl Hasher for DefaultHasher {
    fn write_u8(&mut self, byte: u8) -> &mut Self {
        self.state ^= byte as u64;
        self.state = self.state.wrapping_mul(0x100000001b3); // FNV prime
        self
    }

    fn write_u64(&mut self, value: u64) -> &mut Self {
        for i in 0..8 {
            self.write_u8((value >> (i * 8)) as u8);
        }
        self
    }

    fn write_bytes(&mut self, bytes: &[u8]) -> &mut Self {
        for &byte in bytes {
            self.write_u8(byte);
        }
        self
    }

    fn finish(&self) -> u64 {
        self.state
    }
}

/// Types that can be sent between threads.
///
/// Send is a marker trait indicating that values of this type can be safely
/// transferred across thread boundaries.
pub unsafe trait Send {}

/// Types that can be shared between threads.
///
/// Sync is a marker trait indicating that references to values of this type
/// can be safely shared between threads.
pub unsafe trait Sync {}

/// Types that can be dropped.
///
/// Drop is called when a value goes out of scope.
pub trait Drop {
    /// Called when the value is dropped.
    fn drop(&mut self);
}

/// Types that have a default value.
///
/// Default provides a way to create a default instance of a type.
pub trait Default {
    /// Returns the default value.
    fn default() -> Self;
}

/// Types that can be converted from another type.
///
/// From represents a conversion that always succeeds.
pub trait From<T>: Sized {
    /// Converts from T to Self.
    fn from(value: T) -> Self;
}

/// Types that can be converted into another type.
///
/// Into represents a conversion that always succeeds.
/// The inverse of From.
pub trait Into<T>: Sized {
    /// Converts self into T.
    fn into(self) -> T;
}

/// Types that can be converted from another type, possibly failing.
///
/// TryFrom represents a conversion that may fail.
pub trait TryFrom<T>: Sized {
    /// The error type returned when conversion fails.
    type Error;

    /// Attempts to convert from T to Self.
    fn try_from(value: T) -> Result<Self, Self::Error>;
}

/// Types that can be converted into another type, possibly failing.
///
/// TryInto represents a conversion that may fail.
/// The inverse of TryFrom.
pub trait TryInto<T>: Sized {
    /// The error type returned when conversion fails.
    type Error;

    /// Attempts to convert self into T.
    fn try_into(self) -> Result<T, Self::Error>;
}

/// Types that can be dereferenced.
///
/// Deref is used for smart pointer types.
pub trait Deref {
    /// The type of the value that Self dereferences to.
    type Target;

    /// Dereferences self to return a reference to Target.
    fn deref(&self) -> &Self::Target;
}

/// Types that can be mutably dereferenced.
///
/// DerefMut is used for mutable smart pointer types.
pub trait DerefMut: Deref {
    /// Mutably dereferences self.
    fn deref_mut(&mut self) -> &mut Self::Target;
}

/// Types that can be indexed.
///
/// Index provides immutable indexed access.
pub trait Index<Idx> {
    /// The type of the value returned by indexing.
    type Output;

    /// Returns a reference to the value at index.
    fn index(&self, index: Idx) -> &Self::Output;
}

/// Types that can be mutably indexed.
///
/// IndexMut provides mutable indexed access.
pub trait IndexMut<Idx>: Index<Idx> {
    /// Returns a mutable reference to the value at index.
    fn index_mut(&mut self, index: Idx) -> &mut Self::Output;
}

/// Types that have a length.
///
/// Len provides a way to get the number of elements.
pub trait Len {
    /// Returns the number of elements.
    fn len(&self) -> usize;

    /// Returns true if there are no elements.
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

/// Types that can be extended with elements.
///
/// Extend adds elements from an iterator to a collection.
pub trait Extend<T> {
    /// Extends self with elements from the iterator.
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I);
}

/// Types that can be collected from an iterator.
///
/// FromIter creates a collection from an iterator.
pub trait FromIter<T>: Sized {
    /// Creates a collection from an iterator.
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self;
}

/// Types that can be compared partially.
///
/// PartialEq is for types where not all values can be compared.
pub trait PartialEq {
    /// Returns true if self is equal to other.
    fn eq(&self, other: &Self) -> bool;

    /// Returns true if self is not equal to other.
    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}

/// Types that can be partially ordered.
///
/// PartialOrd is for types where not all values can be ordered.
pub trait PartialOrd: PartialEq {
    /// Compares self to other and returns the ordering if comparable.
    fn partial_compare(&self, other: &Self) -> Option<JetOrdering>;

    /// Returns true if self is less than other.
    fn partial_lt(&self, other: &Self) -> bool {
        matches!(self.partial_compare(other), Some(JetOrdering::Less))
    }

    /// Returns true if self is less than or equal to other.
    fn partial_le(&self, other: &Self) -> bool {
        matches!(
            self.partial_compare(other),
            Some(JetOrdering::Less) | Some(JetOrdering::Equal)
        )
    }

    /// Returns true if self is greater than other.
    fn partial_gt(&self, other: &Self) -> bool {
        matches!(self.partial_compare(other), Some(JetOrdering::Greater))
    }

    /// Returns true if self is greater than or equal to other.
    fn partial_ge(&self, other: &Self) -> bool {
        matches!(
            self.partial_compare(other),
            Some(JetOrdering::Greater) | Some(JetOrdering::Equal)
        )
    }
}

/// Types that can be added.
///
/// Add is the trait for the + operator.
pub trait Add<Rhs = Self> {
    /// The type of the result.
    type Output;

    /// Adds self and rhs.
    fn add(self, rhs: Rhs) -> Self::Output;
}

/// Types that can be subtracted.
///
/// Sub is the trait for the - operator.
pub trait Sub<Rhs = Self> {
    /// The type of the result.
    type Output;

    /// Subtracts rhs from self.
    fn sub(self, rhs: Rhs) -> Self::Output;
}

/// Types that can be multiplied.
///
/// Mul is the trait for the * operator.
pub trait Mul<Rhs = Self> {
    /// The type of the result.
    type Output;

    /// Multiplies self by rhs.
    fn mul(self, rhs: Rhs) -> Self::Output;
}

/// Types that can be divided.
///
/// Div is the trait for the / operator.
pub trait Div<Rhs = Self> {
    /// The type of the result.
    type Output;

    /// Divides self by rhs.
    fn div(self, rhs: Rhs) -> Self::Output;
}

/// Types that can have a remainder.
///
/// Rem is the trait for the % operator.
pub trait Rem<Rhs = Self> {
    /// The type of the result.
    type Output;

    /// Returns the remainder of self divided by rhs.
    fn rem(self, rhs: Rhs) -> Self::Output;
}

/// Types that can be negated.
///
/// Neg is the trait for unary -.
pub trait Neg {
    /// The type of the result.
    type Output;

    /// Negates self.
    fn neg(self) -> Self::Output;
}

/// Types that can be bitwise NOTed.
///
/// Not is the trait for unary ~.
pub trait Not {
    /// The type of the result.
    type Output;

    /// Returns the bitwise NOT of self.
    fn not(self) -> Self::Output;
}

/// Types that can be bitwise ANDed.
///
/// BitAnd is the trait for the & operator.
pub trait BitAnd<Rhs = Self> {
    /// The type of the result.
    type Output;

    /// Returns the bitwise AND of self and rhs.
    fn bitand(self, rhs: Rhs) -> Self::Output;
}

/// Types that can be bitwise ORed.
///
/// BitOr is the trait for the | operator.
pub trait BitOr<Rhs = Self> {
    /// The type of the result.
    type Output;

    /// Returns the bitwise OR of self and rhs.
    fn bitor(self, rhs: Rhs) -> Self::Output;
}

/// Types that can be bitwise XORed.
///
/// BitXor is the trait for the ^ operator.
pub trait BitXor<Rhs = Self> {
    /// The type of the result.
    type Output;

    /// Returns the bitwise XOR of self and rhs.
    fn bitxor(self, rhs: Rhs) -> Self::Output;
}

/// Types that can be left-shifted.
///
/// Shl is the trait for the << operator.
pub trait Shl<Rhs = Self> {
    /// The type of the result.
    type Output;

    /// Shifts self left by rhs bits.
    fn shl(self, rhs: Rhs) -> Self::Output;
}

/// Types that can be right-shifted.
///
/// Shr is the trait for the >> operator.
pub trait Shr<Rhs = Self> {
    /// The type of the result.
    type Output;

    /// Shifts self right by rhs bits.
    fn shr(self, rhs: Rhs) -> Self::Output;
}

// Implementations for primitive types

impl Eq for i32 {
    fn eq(&self, other: &Self) -> bool {
        *self == *other
    }
}

impl Eq for i64 {
    fn eq(&self, other: &Self) -> bool {
        *self == *other
    }
}

impl Eq for u32 {
    fn eq(&self, other: &Self) -> bool {
        *self == *other
    }
}

impl Eq for u64 {
    fn eq(&self, other: &Self) -> bool {
        *self == *other
    }
}

impl Eq for usize {
    fn eq(&self, other: &Self) -> bool {
        *self == *other
    }
}

impl Eq for f32 {
    fn eq(&self, other: &Self) -> bool {
        *self == *other
    }
}

impl Eq for f64 {
    fn eq(&self, other: &Self) -> bool {
        *self == *other
    }
}

impl Eq for bool {
    fn eq(&self, other: &Self) -> bool {
        *self == *other
    }
}

impl Eq for char {
    fn eq(&self, other: &Self) -> bool {
        *self == *other
    }
}

impl Ord for i32 {
    fn compare(&self, other: &Self) -> JetOrdering {
        JetOrdering::from_std(self.cmp(other))
    }
}

impl Ord for i64 {
    fn compare(&self, other: &Self) -> JetOrdering {
        JetOrdering::from_std(self.cmp(other))
    }
}

impl Ord for u32 {
    fn compare(&self, other: &Self) -> JetOrdering {
        JetOrdering::from_std(self.cmp(other))
    }
}

impl Ord for u64 {
    fn compare(&self, other: &Self) -> JetOrdering {
        JetOrdering::from_std(self.cmp(other))
    }
}

impl Ord for usize {
    fn compare(&self, other: &Self) -> JetOrdering {
        JetOrdering::from_std(self.cmp(other))
    }
}

impl Ord for char {
    fn compare(&self, other: &Self) -> JetOrdering {
        JetOrdering::from_std(self.cmp(other))
    }
}

impl Ord for bool {
    fn compare(&self, other: &Self) -> JetOrdering {
        JetOrdering::from_std(self.cmp(other))
    }
}

impl Hash for i32 {
    fn hash(&self) -> u64 {
        *self as u64
    }
}

impl Hash for i64 {
    fn hash(&self) -> u64 {
        *self as u64
    }
}

impl Hash for u32 {
    fn hash(&self) -> u64 {
        *self as u64
    }
}

impl Hash for u64 {
    fn hash(&self) -> u64 {
        *self
    }
}

impl Hash for usize {
    fn hash(&self) -> u64 {
        *self as u64
    }
}

impl Hash for &str {
    fn hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        hasher.write_bytes(self.as_bytes());
        hasher.finish()
    }
}

impl Hash for String {
    fn hash(&self) -> u64 {
        self.as_str().hash()
    }
}

// C ABI exports for trait operations

/// Compares two i64 values
#[no_mangle]
pub extern "C" fn jet_traits_compare_i64(a: i64, b: i64) -> i32 {
    match a.cmp(&b) {
        Ordering::Less => -1,
        Ordering::Equal => 0,
        Ordering::Greater => 1,
    }
}

/// Hashes an i64 value
#[no_mangle]
pub extern "C" fn jet_traits_hash_i64(value: i64) -> u64 {
    value as u64
}

/// Hashes a string
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_traits_hash_string`'s FFI contract.
pub unsafe extern "C" fn jet_traits_hash_string(s: *const u8, len: usize) -> u64 {
    if s.is_null() {
        return 0;
    }
    let slice = unsafe { std::slice::from_raw_parts(s, len) };
    let mut hasher = DefaultHasher::new();
    hasher.write_bytes(slice);
    hasher.finish()
}
