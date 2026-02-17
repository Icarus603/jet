//! Vec - Dynamic array implementation for Jet
//!
//! This module provides a growable array type with amortized O(1) push/pop
//! operations. It integrates with the Immix GC for memory management.

use crate::alloc::{gc_alloc, gc_free, gc_realloc};
use std::marker::PhantomData;
use std::ptr::{self, NonNull};

/// A contiguous growable array type.
///
/// Vec stores elements in a contiguous memory block and automatically
/// grows the allocation when capacity is exceeded.
pub struct Vec<T> {
    /// Pointer to the heap-allocated buffer
    ptr: NonNull<T>,
    /// Number of elements currently in the vector
    len: usize,
    /// Total capacity of the buffer
    capacity: usize,
    /// Phantom data to handle generic type T
    _marker: PhantomData<T>,
}

/// Iterator over Vec elements
pub struct Iter<'a, T> {
    vec: &'a Vec<T>,
    pos: usize,
}

/// Iterator over mutable references to Vec elements
pub struct IterMut<'a, T> {
    vec: &'a mut Vec<T>,
    pos: usize,
}

/// Iterator that consumes a Vec and yields its elements
pub struct IntoIter<T> {
    vec: Vec<T>,
    pos: usize,
}

impl<T> Vec<T> {
    /// Creates an empty vector.
    ///
    /// The vector will have zero capacity and will allocate on first push.
    pub fn new() -> Self {
        Vec {
            ptr: NonNull::dangling(),
            len: 0,
            capacity: 0,
            _marker: PhantomData,
        }
    }

    /// Creates a vector with the specified capacity.
    ///
    /// The vector will be able to hold `capacity` elements without reallocating.
    pub fn with_capacity(capacity: usize) -> Self {
        if capacity == 0 {
            return Vec::new();
        }

        let size = std::mem::size_of::<T>()
            .checked_mul(capacity)
            .expect("Size computation overflow");
        let align = std::mem::align_of::<T>();
        let ptr = unsafe { gc_alloc(size, align) };

        if ptr.is_null() {
            panic!("Allocation failed");
        }

        Vec {
            ptr: unsafe { NonNull::new_unchecked(ptr as *mut T) },
            len: 0,
            capacity,
            _marker: PhantomData,
        }
    }

    /// Returns the number of elements in the vector.
    pub fn len(&self) -> usize {
        self.len
    }

    /// Returns true if the vector contains no elements.
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Returns the number of elements the vector can hold without reallocating.
    pub fn capacity(&self) -> usize {
        self.capacity
    }

    /// Appends an element to the end of the vector.
    ///
    /// Amortized O(1) time complexity. May reallocate if capacity is exceeded.
    pub fn push(&mut self, value: T) {
        if self.len == self.capacity {
            self.grow();
        }

        unsafe {
            ptr::write(self.ptr.as_ptr().add(self.len), value);
        }
        self.len += 1;
    }

    /// Removes and returns the last element.
    ///
    /// Returns None if the vector is empty.
    pub fn pop(&mut self) -> Option<T> {
        if self.len == 0 {
            return None;
        }

        self.len -= 1;
        Some(unsafe { ptr::read(self.ptr.as_ptr().add(self.len)) })
    }

    /// Returns a reference to the element at the given index.
    ///
    /// Returns None if the index is out of bounds.
    pub fn get(&self, index: usize) -> Option<&T> {
        if index < self.len {
            Some(unsafe { &*self.ptr.as_ptr().add(index) })
        } else {
            None
        }
    }

    /// Returns a mutable reference to the element at the given index.
    ///
    /// Returns None if the index is out of bounds.
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        if index < self.len {
            Some(unsafe { &mut *self.ptr.as_ptr().add(index) })
        } else {
            None
        }
    }

    /// Returns a reference to the first element.
    pub fn first(&self) -> Option<&T> {
        self.get(0)
    }

    /// Returns a mutable reference to the first element.
    pub fn first_mut(&mut self) -> Option<&mut T> {
        self.get_mut(0)
    }

    /// Returns a reference to the last element.
    pub fn last(&self) -> Option<&T> {
        if self.len == 0 {
            None
        } else {
            self.get(self.len - 1)
        }
    }

    /// Returns a mutable reference to the last element.
    pub fn last_mut(&mut self) -> Option<&mut T> {
        if self.len == 0 {
            None
        } else {
            self.get_mut(self.len - 1)
        }
    }

    /// Swaps two elements in the vector.
    pub fn swap(&mut self, i: usize, j: usize) {
        if i >= self.len || j >= self.len {
            return;
        }

        unsafe {
            let ptr = self.ptr.as_ptr();
            ptr::swap(ptr.add(i), ptr.add(j));
        }
    }

    /// Clears the vector, removing all values.
    ///
    /// Note that this has no effect on the allocated capacity of the vector.
    pub fn clear(&mut self) {
        // Drop all elements
        for i in 0..self.len {
            unsafe {
                ptr::drop_in_place(self.ptr.as_ptr().add(i));
            }
        }
        self.len = 0;
    }

    /// Reserves capacity for at least `additional` more elements.
    ///
    /// The collection may reserve more space to avoid frequent reallocations.
    pub fn reserve(&mut self, additional: usize) {
        let new_cap = self.len.saturating_add(additional);
        if new_cap > self.capacity {
            self.reallocate(new_cap);
        }
    }

    /// Reserves capacity for exactly `additional` more elements.
    pub fn reserve_exact(&mut self, additional: usize) {
        let new_cap = self.len.saturating_add(additional);
        if new_cap > self.capacity {
            self.reallocate_exact(new_cap);
        }
    }

    /// Shrinks the capacity of the vector as much as possible.
    pub fn shrink_to_fit(&mut self) {
        if self.capacity > self.len {
            self.reallocate(self.len);
        }
    }

    /// Shrinks the capacity to at least `min_capacity`.
    pub fn shrink_to(&mut self, min_capacity: usize) {
        if self.capacity > min_capacity {
            let new_cap = self.len.max(min_capacity);
            if new_cap < self.capacity {
                self.reallocate(new_cap);
            }
        }
    }

    /// Removes and returns the element at position `index` within the vector.
    ///
    /// This is O(n) as it shifts all elements after the removed element.
    pub fn remove(&mut self, index: usize) -> T {
        if index >= self.len {
            panic!("remove index out of bounds");
        }

        unsafe {
            let ptr = self.ptr.as_ptr();
            let value = ptr::read(ptr.add(index));

            // Shift elements to the left
            ptr::copy(ptr.add(index + 1), ptr.add(index), self.len - index - 1);
            self.len -= 1;

            value
        }
    }

    /// Removes an element at position `index` and returns it, swapping with the last element.
    ///
    /// This is O(1) but changes the order of elements.
    pub fn swap_remove(&mut self, index: usize) -> T {
        if index >= self.len {
            panic!("swap_remove index out of bounds");
        }

        let last_idx = self.len - 1;
        self.swap(index, last_idx);
        self.pop().unwrap()
    }

    /// Inserts an element at position `index`.
    ///
    /// Shifts the element currently at that position and all elements after it
    /// to the right.
    pub fn insert(&mut self, index: usize, value: T) {
        if index > self.len {
            panic!("insert index out of bounds");
        }

        if self.len == self.capacity {
            self.grow();
        }

        unsafe {
            let ptr = self.ptr.as_ptr();
            // Shift elements to the right
            ptr::copy(ptr.add(index), ptr.add(index + 1), self.len - index);
            ptr::write(ptr.add(index), value);
        }
        self.len += 1;
    }

    /// Appends all elements from another vector.
    pub fn append(&mut self, other: &mut Vec<T>) {
        self.reserve(other.len);
        for i in 0..other.len {
            unsafe {
                self.push(ptr::read(other.ptr.as_ptr().add(i)));
            }
        }
        other.len = 0;
    }

    /// Returns true if the vector contains the specified element.
    pub fn contains(&self, value: &T) -> bool
    where
        T: PartialEq,
    {
        for i in 0..self.len {
            unsafe {
                if &*self.ptr.as_ptr().add(i) == value {
                    return true;
                }
            }
        }
        false
    }

    /// Returns the first element that satisfies the predicate.
    pub fn find<P>(&self, predicate: P) -> Option<&T>
    where
        P: Fn(&T) -> bool,
    {
        for i in 0..self.len {
            unsafe {
                let elem = &*self.ptr.as_ptr().add(i);
                if predicate(elem) {
                    return Some(elem);
                }
            }
        }
        None
    }

    /// Returns the index of the first occurrence of the value.
    pub fn index_of(&self, value: &T) -> Option<usize>
    where
        T: PartialEq,
    {
        for i in 0..self.len {
            unsafe {
                if &*self.ptr.as_ptr().add(i) == value {
                    return Some(i);
                }
            }
        }
        None
    }

    /// Returns an iterator over the vector.
    pub fn iter(&self) -> Iter<'_, T> {
        Iter { vec: self, pos: 0 }
    }

    /// Returns an iterator over mutable references to the vector.
    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
        IterMut { vec: self, pos: 0 }
    }

    /// Returns a slice of the vector's elements.
    pub fn as_slice(&self) -> &[T] {
        unsafe { std::slice::from_raw_parts(self.ptr.as_ptr(), self.len) }
    }

    /// Returns a mutable slice of the vector's elements.
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        unsafe { std::slice::from_raw_parts_mut(self.ptr.as_ptr(), self.len) }
    }

    /// Retains only the elements specified by the predicate.
    pub fn retain<F>(&mut self, mut f: F)
    where
        F: FnMut(&T) -> bool,
    {
        let mut i = 0;
        while i < self.len {
            unsafe {
                if !f(&*self.ptr.as_ptr().add(i)) {
                    self.remove(i);
                } else {
                    i += 1;
                }
            }
        }
    }

    /// Reverses the order of elements in the vector.
    pub fn reverse(&mut self) {
        let mut left = 0;
        let mut right = self.len;
        while left < right {
            right -= 1;
            if left < right {
                self.swap(left, right);
                left += 1;
            }
        }
    }

    /// Sorts the vector.
    pub fn sort(&mut self)
    where
        T: Ord,
    {
        self.sort_by(|a, b| a.cmp(b));
    }

    /// Sorts the vector with a custom comparator.
    pub fn sort_by<F>(&mut self, mut compare: F)
    where
        F: FnMut(&T, &T) -> std::cmp::Ordering,
    {
        if self.len <= 1 {
            return;
        }
        self.quick_sort(0, self.len - 1, &mut compare);
    }

    /// Performs a binary search on the sorted vector.
    ///
    /// Returns the index of the element if found, or the index where it should be inserted.
    pub fn binary_search(&self, value: &T) -> Result<usize, usize>
    where
        T: Ord,
    {
        self.binary_search_by(|probe| probe.cmp(value))
    }

    /// Performs a binary search with a custom comparator.
    pub fn binary_search_by<F>(&self, mut f: F) -> Result<usize, usize>
    where
        F: FnMut(&T) -> std::cmp::Ordering,
    {
        let mut left = 0;
        let mut right = self.len;

        while left < right {
            let mid = left + (right - left) / 2;
            unsafe {
                let probe = &*self.ptr.as_ptr().add(mid);
                match f(probe) {
                    std::cmp::Ordering::Equal => return Ok(mid),
                    std::cmp::Ordering::Less => left = mid + 1,
                    std::cmp::Ordering::Greater => right = mid,
                }
            }
        }
        Err(left)
    }

    /// Partitions the vector based on a predicate.
    ///
    /// Returns the index of the first element that does not satisfy the predicate.
    pub fn partition<F>(&mut self, mut f: F) -> usize
    where
        F: FnMut(&T) -> bool,
    {
        let mut i = 0;
        for j in 0..self.len {
            unsafe {
                let val = &*self.ptr.as_ptr().add(j);
                if f(val) {
                    self.swap(i, j);
                    i += 1;
                }
            }
        }
        i
    }

    // Private methods

    fn grow(&mut self) {
        let new_cap = if self.capacity == 0 {
            4
        } else {
            self.capacity * 2
        };
        self.reallocate(new_cap);
    }

    fn reallocate(&mut self, new_cap: usize) {
        if new_cap == 0 {
            return;
        }

        let elem_size = std::mem::size_of::<T>();
        let align = std::mem::align_of::<T>();
        let new_size = elem_size
            .checked_mul(new_cap)
            .expect("Size computation overflow");
        let old_size = elem_size * self.capacity;

        let new_ptr = if self.capacity == 0 {
            unsafe { gc_alloc(new_size, align) }
        } else {
            unsafe { gc_realloc(self.ptr.as_ptr() as *mut u8, old_size, new_size, align) }
        };

        if new_ptr.is_null() {
            panic!("Reallocation failed");
        }

        self.ptr = unsafe { NonNull::new_unchecked(new_ptr as *mut T) };
        self.capacity = new_cap;
    }

    fn reallocate_exact(&mut self, new_cap: usize) {
        self.reallocate(new_cap);
    }

    fn quick_sort<F>(&mut self, low: usize, high: usize, compare: &mut F)
    where
        F: FnMut(&T, &T) -> std::cmp::Ordering,
    {
        if low < high {
            let pi = self.partition_sort(low, high, compare);
            if pi > 0 {
                self.quick_sort(low, pi - 1, compare);
            }
            self.quick_sort(pi + 1, high, compare);
        }
    }

    fn partition_sort<F>(&mut self, low: usize, high: usize, compare: &mut F) -> usize
    where
        F: FnMut(&T, &T) -> std::cmp::Ordering,
    {
        unsafe {
            let pivot_ptr = self.ptr.as_ptr().add(high);
            let mut i = low;

            for j in low..high {
                let current_ptr = self.ptr.as_ptr().add(j);
                if compare(&*current_ptr, &*pivot_ptr) != std::cmp::Ordering::Greater {
                    if i != j {
                        ptr::swap(self.ptr.as_ptr().add(i), current_ptr);
                    }
                    i += 1;
                }
            }

            if i != high {
                ptr::swap(self.ptr.as_ptr().add(i), pivot_ptr);
            }
            i
        }
    }
}

impl<T> Default for Vec<T> {
    fn default() -> Self {
        Vec::new()
    }
}

impl<T> Drop for Vec<T> {
    fn drop(&mut self) {
        if self.capacity > 0 {
            self.clear();
            let elem_size = std::mem::size_of::<T>();
            let align = std::mem::align_of::<T>();
            let size = elem_size * self.capacity;
            unsafe {
                gc_free(self.ptr.as_ptr() as *mut u8, size, align);
            }
        }
    }
}

impl<T: Clone> Clone for Vec<T> {
    fn clone(&self) -> Self {
        let mut new_vec = Vec::with_capacity(self.capacity);
        for i in 0..self.len {
            unsafe {
                new_vec.push((*self.ptr.as_ptr().add(i)).clone());
            }
        }
        new_vec
    }
}

impl<T: PartialEq> PartialEq for Vec<T> {
    fn eq(&self, other: &Self) -> bool {
        if self.len != other.len {
            return false;
        }
        for i in 0..self.len {
            unsafe {
                if *self.ptr.as_ptr().add(i) != *other.ptr.as_ptr().add(i) {
                    return false;
                }
            }
        }
        true
    }
}

impl<T: Eq> Eq for Vec<T> {}

impl<T> IntoIterator for Vec<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> IntoIter<T> {
        IntoIter { vec: self, pos: 0 }
    }
}

impl<'a, T> IntoIterator for &'a Vec<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Iter<'a, T> {
        self.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut Vec<T> {
    type Item = &'a mut T;
    type IntoIter = IterMut<'a, T>;

    fn into_iter(self) -> IterMut<'a, T> {
        self.iter_mut()
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos < self.vec.len {
            let val = unsafe { &*self.vec.ptr.as_ptr().add(self.pos) };
            self.pos += 1;
            Some(val)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.vec.len - self.pos;
        (remaining, Some(remaining))
    }
}

impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos < self.vec.len {
            let val = unsafe { &mut *self.vec.ptr.as_ptr().add(self.pos) };
            self.pos += 1;
            Some(val)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.vec.len - self.pos;
        (remaining, Some(remaining))
    }
}

impl<T> Iterator for IntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos < self.vec.len {
            let val = unsafe { ptr::read(self.vec.ptr.as_ptr().add(self.pos)) };
            self.pos += 1;
            Some(val)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.vec.len - self.pos;
        (remaining, Some(remaining))
    }
}

impl<T> Drop for IntoIter<T> {
    fn drop(&mut self) {
        // Drop remaining elements
        for i in self.pos..self.vec.len {
            unsafe {
                ptr::drop_in_place(self.vec.ptr.as_ptr().add(i));
            }
        }
        self.vec.len = 0;
    }
}

// C ABI exports for Jet code generation

/// Creates a new empty vector
#[no_mangle]
pub extern "C" fn jet_vec_new() -> *mut Vec<u8> {
    Box::into_raw(Box::new(Vec::new()))
}

/// Creates a vector with specified capacity
#[no_mangle]
pub extern "C" fn jet_vec_with_capacity(capacity: usize) -> *mut Vec<u8> {
    Box::into_raw(Box::new(Vec::with_capacity(capacity)))
}

/// Frees a vector
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_vec_free`'s FFI contract.
pub unsafe extern "C" fn jet_vec_free(vec: *mut Vec<u8>) {
    if !vec.is_null() {
        unsafe { drop(Box::from_raw(vec)) };
    }
}

/// Returns the length of a vector
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_vec_len`'s FFI contract.
pub unsafe extern "C" fn jet_vec_len(vec: *const Vec<u8>) -> usize {
    unsafe { (*vec).len() }
}

/// Returns the capacity of a vector
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_vec_capacity`'s FFI contract.
pub unsafe extern "C" fn jet_vec_capacity(vec: *const Vec<u8>) -> usize {
    unsafe { (*vec).capacity() }
}

/// Returns true if the vector is empty
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_vec_is_empty`'s FFI contract.
pub unsafe extern "C" fn jet_vec_is_empty(vec: *const Vec<u8>) -> bool {
    unsafe { (*vec).is_empty() }
}

/// Clears a vector
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_vec_clear`'s FFI contract.
pub unsafe extern "C" fn jet_vec_clear(vec: *mut Vec<u8>) {
    unsafe { (*vec).clear() }
}
