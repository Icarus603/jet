//! Stack management for tasks.
//!
//! Provides stack allocation and management for user-space tasks.
//! Each task gets its own stack for local variables and function calls.

use std::alloc::{alloc, dealloc, Layout};

/// Default stack size for tasks (1 MiB).
pub const DEFAULT_STACK_SIZE: usize = 1024 * 1024;

/// Minimum stack size (64 KiB).
pub const MIN_STACK_SIZE: usize = 64 * 1024;

/// A stack allocation for a task.
///
/// Each task has its own stack for storing local variables and
/// making function calls. The stack grows downward on x86_64.
pub struct Stack {
    /// Base address of the stack allocation (lowest address).
    pub base: *mut u8,
    /// Total size of the stack in bytes.
    pub size: usize,
}

impl Stack {
    /// Creates a new stack with the given size.
    ///
    /// The size will be rounded up to a page boundary and must be
    /// at least `MIN_STACK_SIZE`.
    ///
    /// # Panics
    ///
    /// Panics if allocation fails.
    pub fn new(size: usize) -> Self {
        let size = size.max(MIN_STACK_SIZE);
        // Round up to page size (4096 bytes)
        let page_size = 4096;
        let size = (size + page_size - 1) & !(page_size - 1);

        let layout = Layout::from_size_align(size, page_size).expect("Invalid stack layout");

        let base = unsafe { alloc(layout) };
        if base.is_null() {
            std::alloc::handle_alloc_error(layout);
        }

        Self { base, size }
    }

    /// Returns a pointer to the top of the stack.
    ///
    /// On x86_64, the stack grows downward, so the "top" is actually
    /// the highest address in the allocation. The returned pointer
    /// points just past the last valid byte (suitable for use as
    /// the initial stack pointer).
    pub fn top(&self) -> *mut u8 {
        unsafe { self.base.add(self.size) }
    }

    /// Returns the current stack pointer adjusted for red zone.
    ///
    /// On x86_64 System V ABI, the 128 bytes below the stack pointer
    /// are reserved for the red zone.
    pub fn top_with_red_zone(&self) -> *mut u8 {
        unsafe { self.top().sub(128) }
    }

    /// Checks if a pointer is within this stack's bounds.
    pub fn contains(&self, ptr: *const u8) -> bool {
        let ptr = ptr as usize;
        let base = self.base as usize;
        ptr >= base && ptr < base + self.size
    }

    /// Returns the remaining stack space from the given pointer.
    ///
    /// # Safety
    ///
    /// The pointer must be within the stack's bounds.
    pub unsafe fn remaining_from(&self, ptr: *const u8) -> usize {
        let ptr = ptr as usize;
        let base = self.base as usize;
        ptr - base
    }
}

impl Drop for Stack {
    fn drop(&mut self) {
        if !self.base.is_null() {
            let page_size = 4096;
            let layout =
                Layout::from_size_align(self.size, page_size).expect("Invalid stack layout");
            unsafe {
                dealloc(self.base, layout);
            }
        }
    }
}

// Stack is not Send/Sync by default because it contains a raw pointer.
// However, the pointer is to thread-local memory, so it's safe to send
// across threads as long as only one thread accesses it at a time.
unsafe impl Send for Stack {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stack_new() {
        let stack = Stack::new(DEFAULT_STACK_SIZE);
        assert!(!stack.base.is_null());
        assert!(stack.size >= DEFAULT_STACK_SIZE);
    }

    #[test]
    fn test_stack_minimum_size() {
        let stack = Stack::new(100);
        assert!(stack.size >= MIN_STACK_SIZE);
    }

    #[test]
    fn test_stack_top() {
        let stack = Stack::new(DEFAULT_STACK_SIZE);
        let top = stack.top();
        let base = stack.base;
        assert!(top > base);
        assert_eq!(top as usize - base as usize, stack.size);
    }

    #[test]
    fn test_stack_contains() {
        let stack = Stack::new(DEFAULT_STACK_SIZE);
        assert!(stack.contains(stack.base));
        assert!(!stack.contains(stack.top()));
        assert!(stack.contains(unsafe { stack.top().sub(1) }));
    }

    #[test]
    fn test_stack_alignment() {
        let stack = Stack::new(DEFAULT_STACK_SIZE);
        let base_addr = stack.base as usize;
        assert_eq!(base_addr % 4096, 0, "Stack should be page-aligned");
    }
}
