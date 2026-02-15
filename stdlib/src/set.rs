//! Set - Hash set implementation for Jet
//!
//! This module provides a hash set with efficient operations for membership
//! testing, insertion, and deletion. It is implemented as a `Map[T, ()]`.

use crate::map::{Hash, Map};

/// A hash set implementation.
///
/// Set is implemented as a `Map[T, ()]` where the keys are the set elements
/// and the values are the unit type (carrying no information).
pub struct Set<T>
where
    T: Eq,
{
    map: Map<T, ()>,
}

/// Iterator over set elements
pub struct Iter<'a, T>
where
    T: Eq,
{
    inner: crate::map::Keys<'a, T, ()>,
}

/// Difference between two sets (elements in self but not in other)
#[allow(dead_code)]
pub struct Difference<'a, T>
where
    T: Eq,
{
    /// Reference to the first set (reserved for future iterator implementation)
    self_set: &'a Set<T>,
    /// Reference to the second set (reserved for future iterator implementation)
    other_set: &'a Set<T>,
    /// Iterator over the first set (reserved for future iterator implementation)
    current_iter: Iter<'a, T>,
}

/// Intersection of two sets
#[allow(dead_code)]
pub struct Intersection<'a, T>
where
    T: Eq,
{
    /// Reference to the first set (reserved for future iterator implementation)
    self_set: &'a Set<T>,
    /// Reference to the second set (reserved for future iterator implementation)
    other_set: &'a Set<T>,
    /// Iterator over the first set (reserved for future iterator implementation)
    current_iter: Iter<'a, T>,
}

/// Union of two sets
#[allow(dead_code)]
pub struct Union<'a, T>
where
    T: Eq,
{
    /// Reference to the first set (reserved for future iterator implementation)
    self_set: &'a Set<T>,
    /// Reference to the second set (reserved for future iterator implementation)
    other_set: &'a Set<T>,
    /// Iterator over the first set (reserved for future iterator implementation)
    current_iter: Iter<'a, T>,
    /// Whether the first set iteration is complete (reserved for future iterator implementation)
    first_done: bool,
}

impl<T> Set<T>
where
    T: Eq,
{
    /// Creates an empty set.
    pub fn new() -> Self {
        Set { map: Map::new() }
    }

    /// Creates an empty set with the specified initial capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Set {
            map: Map::with_capacity(capacity),
        }
    }

    /// Creates a set from a slice.
    pub fn from_slice(slice: &[T]) -> Self
    where
        T: Hash + Clone,
    {
        let mut set = Set::new();
        for item in slice {
            set.insert(item.clone());
        }
        set
    }

    /// Inserts a value into the set.
    ///
    /// Returns true if the value was not already present in the set.
    pub fn insert(&mut self, value: T) -> bool
    where
        T: Hash,
    {
        self.map.insert(value, ()).is_none()
    }

    /// Returns true if the set contains the specified value.
    pub fn contains(&self, value: &T) -> bool
    where
        T: Hash,
    {
        self.map.contains_key(value)
    }

    /// Removes a value from the set.
    ///
    /// Returns true if the value was present in the set.
    pub fn remove(&mut self, value: &T) -> bool
    where
        T: Hash,
    {
        self.map.remove(value).is_some()
    }

    /// Returns the number of elements in the set.
    pub fn len(&self) -> usize {
        self.map.len()
    }

    /// Returns true if the set contains no elements.
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    /// Clears the set, removing all values.
    pub fn clear(&mut self) {
        self.map.clear()
    }

    /// Returns an iterator over the set.
    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            inner: self.map.keys(),
        }
    }

    /// Returns true if the set is a subset of another set.
    ///
    /// A set is a subset if all its elements are contained in the other set.
    pub fn is_subset(&self, other: &Set<T>) -> bool
    where
        T: Hash,
    {
        for value in self.iter() {
            if !other.contains(value) {
                return false;
            }
        }
        true
    }

    /// Returns true if the set is a superset of another set.
    ///
    /// A set is a superset if it contains all elements of the other set.
    pub fn is_superset(&self, other: &Set<T>) -> bool
    where
        T: Hash,
    {
        other.is_subset(self)
    }

    /// Returns true if the set has no elements in common with another set.
    pub fn is_disjoint(&self, other: &Set<T>) -> bool
    where
        T: Hash,
    {
        let smaller = if self.len() <= other.len() {
            self
        } else {
            other
        };
        let larger = if self.len() <= other.len() {
            other
        } else {
            self
        };

        for value in smaller.iter() {
            if larger.contains(value) {
                return false;
            }
        }
        true
    }

    /// Returns the union of two sets.
    ///
    /// The union contains all elements that are in either set.
    pub fn union(&self, other: &Set<T>) -> Set<T>
    where
        T: Hash + Clone,
    {
        let mut result = Set::new();
        for v in self.iter() {
            result.insert(v.clone());
        }
        for v in other.iter() {
            result.insert(v.clone());
        }
        result
    }

    /// Returns the intersection of two sets.
    ///
    /// The intersection contains all elements that are in both sets.
    pub fn intersection(&self, other: &Set<T>) -> Set<T>
    where
        T: Hash + Clone,
    {
        let mut result = Set::new();
        let smaller = if self.len() <= other.len() {
            self
        } else {
            other
        };
        let larger = if self.len() <= other.len() {
            other
        } else {
            self
        };

        for v in smaller.iter() {
            if larger.contains(v) {
                result.insert(v.clone());
            }
        }
        result
    }

    /// Returns the difference between two sets.
    ///
    /// The difference contains elements that are in self but not in other.
    pub fn difference(&self, other: &Set<T>) -> Set<T>
    where
        T: Hash + Clone,
    {
        let mut result = Set::new();
        for v in self.iter() {
            if !other.contains(v) {
                result.insert(v.clone());
            }
        }
        result
    }

    /// Returns the symmetric difference between two sets.
    ///
    /// The symmetric difference contains elements that are in exactly one of the sets.
    pub fn symmetric_difference(&self, other: &Set<T>) -> Set<T>
    where
        T: Hash + Clone,
    {
        let mut result = Set::new();
        for v in self.iter() {
            if !other.contains(v) {
                result.insert(v.clone());
            }
        }
        for v in other.iter() {
            if !self.contains(v) {
                result.insert(v.clone());
            }
        }
        result
    }

    /// Returns true if all elements satisfy the predicate.
    pub fn all<F>(&self, mut f: F) -> bool
    where
        F: FnMut(&T) -> bool,
    {
        for v in self.iter() {
            if !f(v) {
                return false;
            }
        }
        true
    }

    /// Returns true if any element satisfies the predicate.
    pub fn any<F>(&self, mut f: F) -> bool
    where
        F: FnMut(&T) -> bool,
    {
        for v in self.iter() {
            if f(v) {
                return true;
            }
        }
        false
    }

    /// Returns the first element that satisfies the predicate.
    pub fn find<F>(&self, mut f: F) -> Option<&T>
    where
        F: FnMut(&T) -> bool,
    {
        self.iter().find(|&v| f(v))
    }

    /// Applies a function to each element.
    pub fn for_each<F>(&self, mut f: F)
    where
        F: FnMut(&T),
    {
        for v in self.iter() {
            f(v);
        }
    }

    /// Folds the set into a single value.
    pub fn fold<U, F>(&self, initial: U, mut f: F) -> U
    where
        F: FnMut(U, &T) -> U,
    {
        let mut acc = initial;
        for v in self.iter() {
            acc = f(acc, v);
        }
        acc
    }

    /// Creates a new set with the results of applying the function.
    pub fn map<F, U>(&self, mut f: F) -> Set<U>
    where
        F: FnMut(&T) -> U,
        U: Eq + Hash,
    {
        let mut result = Set::new();
        for v in self.iter() {
            result.insert(f(v));
        }
        result
    }

    /// Creates a new set with only the elements that match the predicate.
    pub fn filter<F>(&self, f: F) -> Set<T>
    where
        F: Fn(&T) -> bool,
        T: Hash + Clone,
    {
        let mut result = Set::new();
        for v in self.iter() {
            if f(v) {
                result.insert(v.clone());
            }
        }
        result
    }

    /// Retains only the elements specified by the predicate.
    pub fn retain<F>(&mut self, f: F)
    where
        F: Fn(&T) -> bool,
        T: Hash + Clone,
    {
        let to_remove: Vec<T> = self.iter().filter(|v| !f(v)).cloned().collect();

        for v in to_remove {
            self.remove(&v);
        }
    }

    /// Takes an element from the set and returns it.
    ///
    /// Returns None if the set is empty.
    pub fn take(&mut self) -> Option<T>
    where
        T: Hash + Clone,
    {
        let first = self.iter().next()?.clone();
        self.remove(&first);
        Some(first)
    }

    /// Takes an element equal to `value` from the set.
    ///
    /// Returns None if no such element exists.
    pub fn take_value(&mut self, value: &T) -> Option<T>
    where
        T: Hash + Clone,
    {
        if self.contains(value) {
            let v = value.clone();
            self.remove(value);
            Some(v)
        } else {
            None
        }
    }

    /// Returns a vector containing all elements of the set.
    pub fn to_vec(&self) -> Vec<T>
    where
        T: Clone,
    {
        self.iter().cloned().collect()
    }

    /// Returns the number of elements that satisfy the predicate.
    pub fn count<F>(&self, mut f: F) -> usize
    where
        F: FnMut(&T) -> bool,
    {
        let mut count = 0;
        for v in self.iter() {
            if f(v) {
                count += 1;
            }
        }
        count
    }

    /// Partitions the set into two based on a predicate.
    pub fn partition<F>(&self, f: F) -> (Set<T>, Set<T>)
    where
        F: Fn(&T) -> bool,
        T: Hash + Clone,
    {
        let mut true_set = Set::new();
        let mut false_set = Set::new();

        for v in self.iter() {
            if f(v) {
                true_set.insert(v.clone());
            } else {
                false_set.insert(v.clone());
            }
        }

        (true_set, false_set)
    }

    /// Extends the set with elements from an iterator.
    pub fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = T>,
        T: Hash,
    {
        for item in iter {
            self.insert(item);
        }
    }
}

impl<T> Default for Set<T>
where
    T: Eq,
{
    fn default() -> Self {
        Set::new()
    }
}

impl<T> std::iter::FromIterator<T> for Set<T>
where
    T: Eq + Hash + Clone,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut set = Set::new();
        for item in iter {
            set.insert(item);
        }
        set
    }
}

impl<T> Clone for Set<T>
where
    T: Clone + Eq + crate::map::Hash,
{
    fn clone(&self) -> Self {
        Set {
            map: self.map.clone(),
        }
    }
}

impl<'a, T: Eq> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

impl<T> IntoIterator for Set<T>
where
    T: Eq + Clone,
{
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.to_vec().into_iter()
    }
}

impl<'a, T> IntoIterator for &'a Set<T>
where
    T: Eq,
{
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<T> PartialEq for Set<T>
where
    T: Eq + Hash,
{
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        self.is_subset(other)
    }
}

impl<T> Eq for Set<T> where T: Eq + Hash {}

// C ABI exports

/// Creates a new empty set
#[no_mangle]
pub extern "C" fn jet_set_new() -> *mut Set<i64> {
    Box::into_raw(Box::new(Set::new()))
}

/// Frees a set
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_set_free`'s FFI contract.
pub unsafe extern "C" fn jet_set_free(set: *mut Set<i64>) {
    if !set.is_null() {
        unsafe { drop(Box::from_raw(set)) };
    }
}

/// Returns the length of a set
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_set_len`'s FFI contract.
pub unsafe extern "C" fn jet_set_len(set: *const Set<i64>) -> usize {
    unsafe { (*set).len() }
}

/// Returns true if the set is empty
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_set_is_empty`'s FFI contract.
pub unsafe extern "C" fn jet_set_is_empty(set: *const Set<i64>) -> bool {
    unsafe { (*set).is_empty() }
}

/// Clears a set
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_set_clear`'s FFI contract.
pub unsafe extern "C" fn jet_set_clear(set: *mut Set<i64>) {
    unsafe { (*set).clear() }
}
