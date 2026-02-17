//! GC Allocator Integration for Jet Standard Library
//!
//! This module provides the bridge between the standard library collections
//! and the Immix garbage collector. It replaces std::alloc with GC-managed
//! allocation for all collection types.

use std::alloc::Layout;
use std::ptr::NonNull;

/// Thread-local GC heap reference
///
/// This provides access to the GC heap for allocations. In a full implementation,
/// this would be provided by the runtime. For now, we use a simplified approach
/// that falls back to std::alloc when the GC heap is not available.
thread_local! {
    static GC_HEAP_AVAILABLE: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Mark the GC heap as available for this thread
pub fn enable_gc_alloc() {
    GC_HEAP_AVAILABLE.with(|available| available.set(true));
}

/// Mark the GC heap as unavailable for this thread
pub fn disable_gc_alloc() {
    GC_HEAP_AVAILABLE.with(|available| available.set(false));
}

/// Check if GC allocation is available
pub fn is_gc_available() -> bool {
    GC_HEAP_AVAILABLE.with(|available| available.get())
}

/// Allocate memory using the GC
///
/// This function attempts to allocate memory through the Immix GC. If the GC
/// is not available, it falls back to the system allocator.
///
/// # Arguments
/// * `size` - Size in bytes to allocate
/// * `align` - Alignment requirement (must be power of 2)
///
/// # Returns
/// A pointer to the allocated memory, or null on failure
///
/// # Safety
/// The returned pointer is not initialized. The caller must properly
/// initialize the memory before use.
pub unsafe fn gc_alloc(size: usize, align: usize) -> *mut u8 {
    if size == 0 {
        return NonNull::dangling().as_ptr();
    }

    let layout = match Layout::from_size_align(size, align) {
        Ok(l) => l,
        Err(_) => return std::ptr::null_mut(),
    };

    // For now, use system allocator with GC tracking
    // In a full implementation, this would call into the Immix heap
    let ptr = unsafe { std::alloc::alloc(layout) };

    if !ptr.is_null() {
        // Register allocation for GC tracking
        register_allocation(ptr, size);
    }

    ptr
}

/// Reallocate memory using the GC
///
/// # Arguments
/// * `ptr` - Pointer to previously allocated memory
/// * `old_size` - Previous allocation size
/// * `new_size` - New size in bytes
/// * `align` - Alignment requirement
///
/// # Returns
/// A pointer to the reallocated memory, or null on failure
///
/// # Safety
/// If reallocation succeeds, the old pointer is invalidated. If it fails,
/// the old pointer remains valid.
pub unsafe fn gc_realloc(ptr: *mut u8, old_size: usize, new_size: usize, align: usize) -> *mut u8 {
    if ptr.is_null() {
        return unsafe { gc_alloc(new_size, align) };
    }

    if new_size == 0 {
        unsafe { gc_free(ptr, old_size, align) };
        return NonNull::dangling().as_ptr();
    }

    let layout = match Layout::from_size_align(old_size, align) {
        Ok(l) => l,
        Err(_) => return std::ptr::null_mut(),
    };

    // Unregister old allocation
    unregister_allocation(ptr);

    let new_ptr = unsafe { std::alloc::realloc(ptr, layout, new_size) };

    if !new_ptr.is_null() {
        // Register new allocation
        register_allocation(new_ptr, new_size);
    } else {
        // Reallocation failed, re-register old allocation
        register_allocation(ptr, old_size);
    }

    new_ptr
}

/// Free memory allocated by the GC
///
/// # Arguments
/// * `ptr` - Pointer to allocated memory
/// * `size` - Size of the allocation
/// * `align` - Alignment requirement
///
/// # Safety
/// The pointer must have been allocated by gc_alloc or gc_realloc.
/// After this call, the pointer is invalid and must not be used.
pub unsafe fn gc_free(ptr: *mut u8, size: usize, align: usize) {
    if ptr.is_null() || size == 0 {
        return;
    }

    let layout = match Layout::from_size_align(size, align) {
        Ok(l) => l,
        Err(_) => return,
    };

    // Unregister allocation
    unregister_allocation(ptr);

    unsafe {
        std::alloc::dealloc(ptr, layout);
    }
}

/// Register an allocation with the GC
///
/// This is a placeholder for full GC integration. In a complete implementation,
/// this would register the allocation with the GC's root set.
fn register_allocation(_ptr: *mut u8, _size: usize) {
    // Placeholder: In full implementation, register with GC heap
    // This would add the pointer to the GC's allocation tracking
}

/// Unregister an allocation from the GC
///
/// This is a placeholder for full GC integration.
fn unregister_allocation(_ptr: *mut u8) {
    // Placeholder: In full implementation, unregister from GC heap
}

/// A smart pointer for GC-managed memory
///
/// GcBox provides a Box-like interface for GC-allocated memory.
/// The memory is automatically freed when the GcBox is dropped,
/// unless it has been rooted for GC collection cycles.
pub struct GcBox<T> {
    ptr: NonNull<T>,
    _marker: std::marker::PhantomData<T>,
}

impl<T> GcBox<T> {
    /// Allocate a new GcBox with the given value
    pub fn new(value: T) -> Option<Self> {
        let size = std::mem::size_of::<T>();
        let align = std::mem::align_of::<T>();

        let ptr = unsafe { gc_alloc(size, align) as *mut T };

        if ptr.is_null() {
            return None;
        }

        unsafe {
            ptr.write(value);
        }

        Some(GcBox {
            ptr: unsafe { NonNull::new_unchecked(ptr) },
            _marker: std::marker::PhantomData,
        })
    }

    /// Create a GcBox from a raw pointer
    ///
    /// # Safety
    /// The pointer must have been allocated by gc_alloc and properly initialized.
    pub unsafe fn from_raw(ptr: *mut T) -> Self {
        GcBox {
            ptr: unsafe { NonNull::new_unchecked(ptr) },
            _marker: std::marker::PhantomData,
        }
    }

    /// Consume the GcBox and return the raw pointer
    ///
    /// After calling this, the caller is responsible for freeing the memory.
    pub fn into_raw(self) -> *mut T {
        let ptr = self.ptr.as_ptr();
        std::mem::forget(self);
        ptr
    }

    /// Get a reference to the contained value
    pub fn as_ref(&self) -> &T {
        unsafe { self.ptr.as_ref() }
    }

    /// Get a mutable reference to the contained value
    pub fn as_mut(&mut self) -> &mut T {
        unsafe { self.ptr.as_mut() }
    }

    /// Get the raw pointer
    pub fn as_ptr(&self) -> *mut T {
        self.ptr.as_ptr()
    }
}

impl<T> std::ops::Deref for GcBox<T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.as_ref()
    }
}

impl<T> std::ops::DerefMut for GcBox<T> {
    fn deref_mut(&mut self) -> &mut T {
        self.as_mut()
    }
}

impl<T> Drop for GcBox<T> {
    fn drop(&mut self) {
        let size = std::mem::size_of::<T>();
        let align = std::mem::align_of::<T>();

        unsafe {
            std::ptr::drop_in_place(self.ptr.as_ptr());
            gc_free(self.ptr.as_ptr() as *mut u8, size, align);
        }
    }
}

impl<T: Clone> Clone for GcBox<T> {
    fn clone(&self) -> Self {
        GcBox::new(self.as_ref().clone()).expect("Failed to clone GcBox")
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for GcBox<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_ref().fmt(f)
    }
}

impl<T: std::fmt::Display> std::fmt::Display for GcBox<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_ref().fmt(f)
    }
}

impl<T: PartialEq> PartialEq for GcBox<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl<T: Eq> Eq for GcBox<T> {}

/// GC root handle for keeping objects alive during collection
///
/// When a value is rooted, it is guaranteed to survive GC collection cycles.
/// Roots are typically used for values that are reachable from the stack
/// or global variables.
pub struct GcRoot<T> {
    ptr: NonNull<T>,
}

impl<T> GcRoot<T> {
    /// Create a new GC root
    ///
    /// # Safety
    /// The pointer must be a valid GC-allocated object.
    pub unsafe fn new(ptr: *mut T) -> Self {
        let root = GcRoot {
            ptr: unsafe { NonNull::new_unchecked(ptr) },
        };
        root.register();
        root
    }

    /// Get a reference to the rooted value
    pub fn as_ref(&self) -> &T {
        unsafe { self.ptr.as_ref() }
    }

    /// Get a mutable reference to the rooted value
    pub fn as_mut(&mut self) -> &mut T {
        unsafe { self.ptr.as_mut() }
    }

    fn register(&self) {
        // Placeholder: Register with GC root set
    }

    fn unregister(&self) {
        // Placeholder: Unregister from GC root set
    }
}

impl<T> Drop for GcRoot<T> {
    fn drop(&mut self) {
        self.unregister();
    }
}

/// Allocate an array using the GC
///
/// # Arguments
/// * `len` - Number of elements
/// * `elem_size` - Size of each element
/// * `align` - Alignment requirement
///
/// # Returns
/// A pointer to the allocated array, or null on failure
///
/// # Safety
/// The returned memory is not initialized.
pub unsafe fn gc_alloc_array(len: usize, elem_size: usize, align: usize) -> *mut u8 {
    let size = len.checked_mul(elem_size).unwrap_or(0);
    if size == 0 {
        return NonNull::dangling().as_ptr();
    }
    unsafe { gc_alloc(size, align) }
}

/// Reallocate an array using the GC
///
/// # Safety
/// The old pointer must have been allocated by gc_alloc_array.
pub unsafe fn gc_realloc_array(
    ptr: *mut u8,
    old_len: usize,
    new_len: usize,
    elem_size: usize,
    align: usize,
) -> *mut u8 {
    let old_size = old_len * elem_size;
    let new_size = new_len * elem_size;
    unsafe { gc_realloc(ptr, old_size, new_size, align) }
}

/// Free an array allocated by the GC
///
/// # Safety
/// The pointer must have been allocated by gc_alloc_array.
pub unsafe fn gc_free_array(ptr: *mut u8, len: usize, elem_size: usize, align: usize) {
    let size = len * elem_size;
    unsafe { gc_free(ptr, size, align) }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gc_alloc_basic() {
        let ptr = unsafe { gc_alloc(100, 8) };
        assert!(!ptr.is_null());
        unsafe {
            gc_free(ptr, 100, 8);
        }
    }

    #[test]
    fn test_gc_alloc_zero_size() {
        let ptr = unsafe { gc_alloc(0, 8) };
        // Should return a dangling pointer, not null
        assert!(!ptr.is_null());
    }

    #[test]
    fn test_gc_realloc() {
        let ptr = unsafe { gc_alloc(100, 8) };
        assert!(!ptr.is_null());

        let new_ptr = unsafe { gc_realloc(ptr, 100, 200, 8) };
        assert!(!new_ptr.is_null());

        unsafe {
            gc_free(new_ptr, 200, 8);
        }
    }

    #[test]
    fn test_gc_box() {
        let gc_box = GcBox::new(42i64);
        assert!(gc_box.is_some());

        let mut gc_box = gc_box.unwrap();
        assert_eq!(*gc_box, 42);

        *gc_box = 100;
        assert_eq!(*gc_box, 100);
    }

    #[test]
    fn test_gc_box_clone() {
        let gc_box = GcBox::new(42i64).unwrap();
        let cloned = gc_box.clone();
        assert_eq!(*cloned, 42);
    }

    #[test]
    fn test_gc_box_debug() {
        let gc_box = GcBox::new(42i64).unwrap();
        let debug_str = format!("{:?}", gc_box);
        assert!(debug_str.contains("42"));
    }

    #[test]
    fn test_gc_alloc_array() {
        let ptr = unsafe { gc_alloc_array(10, std::mem::size_of::<i64>(), 8) };
        assert!(!ptr.is_null());

        unsafe {
            gc_free_array(ptr, 10, std::mem::size_of::<i64>(), 8);
        }
    }
}
