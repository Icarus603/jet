//! GC integration for the scheduler.
//!
//! This module provides integration between the scheduler and the garbage
//! collector. It handles root scanning from task stacks and coordination
//! during garbage collection.
//!
//! # Root Scanning
//!
//! The GC needs to identify all live objects (roots) before collection.
//! Roots come from:
//!
//! 1. **Task stacks**: Local variables in each task's stack
//! 2. **Registers**: Saved register state during collection
//! 3. **Global roots**: Static data and global variables
//! 4. **Thread-local storage**: Per-task thread-local data
//!
//! # Examples
//!
//! ```
//! use jet_rt_sched::gc_integration::RootScanner;
//!
//! let scanner = RootScanner::new();
//! // Register tasks and global roots, then scan
//! // let roots = unsafe { scanner.scan_task_roots() };
//! ```

use crate::task::{Task, TaskState};
use jet_rt_gc::ObjectHeader;
use std::sync::{Arc, Mutex};

/// A root pointer that the GC should trace from.
#[derive(Debug, Clone, Copy)]
pub struct Root {
    /// Pointer to the object header.
    pub header: *mut ObjectHeader,
    /// Source of this root (for debugging).
    pub source: RootSource,
}

// SAFETY: Root is Send because we only use the pointer during GC
// when all threads are stopped at safe points
unsafe impl Send for Root {}

// SAFETY: Root is Sync because it's immutable after creation
unsafe impl Sync for Root {}

/// The source of a GC root.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RootSource {
    /// Root from a task's stack.
    TaskStack { task_id: u64 },
    /// Root from saved registers.
    Registers { task_id: u64 },
    /// Root from global/static data.
    Global,
    /// Root from thread-local storage.
    ThreadLocal { task_id: u64 },
}

/// Scanner for finding GC roots.
///
/// The root scanner traverses all task stacks and other root sources
/// to find live objects that the GC should preserve.
///
/// # Examples
///
/// ```
/// use jet_rt_sched::gc_integration::RootScanner;
///
/// let scanner = RootScanner::new();
/// ```
pub struct RootScanner {
    /// Registered tasks for root scanning.
    tasks: Mutex<Vec<Task>>,
    /// Global roots (static data, etc.).
    global_roots: Mutex<Vec<Root>>,
}

impl RootScanner {
    /// Creates a new root scanner.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sched::gc_integration::RootScanner;
    ///
    /// let scanner = RootScanner::new();
    /// ```
    pub fn new() -> Self {
        Self {
            tasks: Mutex::new(Vec::new()),
            global_roots: Mutex::new(Vec::new()),
        }
    }

    /// Registers a task for root scanning.
    ///
    /// The task's stack will be scanned during GC to find live objects.
    ///
    /// # Arguments
    ///
    /// * `task` - The task to register.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sched::gc_integration::RootScanner;
    /// use jet_rt_sched::Task;
    ///
    /// let scanner = RootScanner::new();
    /// let task = Task::new(|| {}, None);
    /// scanner.register_task(task);
    /// ```
    pub fn register_task(&self, task: Task) {
        let mut tasks = self.tasks.lock().unwrap();
        tasks.push(task);
    }

    /// Unregisters a task from root scanning.
    ///
    /// # Arguments
    ///
    /// * `task_id` - The ID of the task to unregister.
    ///
    /// # Returns
    ///
    /// Returns true if the task was found and removed.
    pub fn unregister_task(&self, task_id: u64) -> bool {
        let mut tasks = self.tasks.lock().unwrap();
        let len_before = tasks.len();
        tasks.retain(|t| t.id != task_id);
        tasks.len() < len_before
    }

    /// Adds a global root.
    ///
    /// Global roots are always considered live during GC.
    ///
    /// # Arguments
    ///
    /// * `header` - Pointer to the object header.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sched::gc_integration::{RootScanner, RootSource};
    ///
    /// let scanner = RootScanner::new();
    /// // scanner.add_global_root(header_ptr);
    /// ```
    pub fn add_global_root(&self, header: *mut ObjectHeader) {
        let mut roots = self.global_roots.lock().unwrap();
        roots.push(Root {
            header,
            source: RootSource::Global,
        });
    }

    /// Removes a global root.
    ///
    /// # Arguments
    ///
    /// * `header` - Pointer to the object header to remove.
    ///
    /// # Returns
    ///
    /// Returns true if the root was found and removed.
    pub fn remove_global_root(&self, header: *mut ObjectHeader) -> bool {
        let mut roots = self.global_roots.lock().unwrap();
        let len_before = roots.len();
        roots.retain(|r| r.header != header);
        roots.len() < len_before
    }

    /// Scans all registered tasks for roots.
    ///
    /// This method iterates through all registered tasks and scans their
    /// stacks for potential GC roots.
    ///
    /// # Returns
    ///
    /// Returns a vector of all found roots.
    ///
    /// # Safety
    ///
    /// This method should only be called when all tasks are stopped (STW phase).
    pub unsafe fn scan_task_roots(&self) -> Vec<Root> {
        let mut roots = Vec::new();
        let tasks = self.tasks.lock().unwrap();

        for task in tasks.iter() {
            // Only scan tasks that are running or blocked
            if task.state != TaskState::Completed {
                let task_roots = self.scan_task_stack(task);
                roots.extend(task_roots);
            }
        }

        roots
    }

    /// Scans a single task's stack for roots.
    ///
    /// # Arguments
    ///
    /// * `task` - The task whose stack to scan.
    ///
    /// # Returns
    ///
    /// Returns a vector of roots found on the task's stack.
    ///
    /// # Safety
    ///
    /// The task must be stopped during scanning.
    unsafe fn scan_task_stack(&self, task: &Task) -> Vec<Root> {
        let mut roots = Vec::new();

        // Get the stack bounds
        let stack_top = task.stack_top();
        let stack_bottom = task.stack.base;

        // Scan the stack for potential pointers
        // In a real implementation, we would use stack maps to precisely
        // identify which stack slots contain pointers.
        // For now, we do a conservative scan.
        let mut current = stack_bottom;
        while current < stack_top {
            // Read the value at this stack location
            let value = *(current as *const usize);

            // Check if it looks like a heap pointer
            if Self::is_potential_heap_pointer(value) {
                // Try to convert to an object header pointer
                if let Some(header) = Self::pointer_to_header(value as *mut u8) {
                    roots.push(Root {
                        header,
                        source: RootSource::TaskStack { task_id: task.id },
                    });
                }
            }

            current = current.add(std::mem::size_of::<usize>());
        }

        roots
    }

    /// Checks if a value looks like a heap pointer.
    ///
    /// This is a heuristic check used for conservative stack scanning.
    fn is_potential_heap_pointer(value: usize) -> bool {
        // Check alignment (heap pointers are typically 8-byte aligned)
        if !value.is_multiple_of(8) {
            return false;
        }

        // Check if it's in a reasonable address range
        // This is platform-specific and approximate
        if value < 0x1000 {
            return false; // Too low (null page)
        }

        // Check if it's in the kernel space (on 64-bit systems)
        #[cfg(target_pointer_width = "64")]
        if value > 0x0000_7FFF_FFFF_FFFF {
            return false; // Likely kernel address
        }

        true
    }

    /// Converts a potential pointer to an object header pointer.
    ///
    /// # Safety
    ///
    /// This method assumes the pointer may be a valid heap object.
    unsafe fn pointer_to_header(_ptr: *mut u8) -> Option<*mut ObjectHeader> {
        // In a real implementation, we would:
        // 1. Check if the pointer is within the heap bounds
        // 2. Align down to find the object header
        // 3. Verify the header is valid

        // For now, return None as we can't verify without heap context
        None
    }

    /// Scans saved registers for roots.
    ///
    /// # Arguments
    ///
    /// * `context` - The saved CPU context.
    /// * `task_id` - The ID of the task these registers belong to.
    ///
    /// # Returns
    ///
    /// Returns a vector of roots found in the registers.
    ///
    /// # Safety
    ///
    /// This method should only be called when the task is stopped during GC.
    pub unsafe fn scan_registers(
        &self,
        _context: &crate::context::Context,
        _task_id: u64,
    ) -> Vec<Root> {
        // In a real implementation, we would examine each register
        // that could contain a pointer and check if it points to the heap.

        // For now, return an empty vector
        Vec::new()
    }

    /// Returns all global roots.
    pub fn global_roots(&self) -> Vec<Root> {
        self.global_roots.lock().unwrap().clone()
    }

    /// Returns the number of registered tasks.
    pub fn num_registered_tasks(&self) -> usize {
        self.tasks.lock().unwrap().len()
    }

    /// Clears all registered tasks.
    pub fn clear_tasks(&self) {
        self.tasks.lock().unwrap().clear();
    }

    /// Clears all global roots.
    pub fn clear_global_roots(&self) {
        self.global_roots.lock().unwrap().clear();
    }
}

impl Default for RootScanner {
    fn default() -> Self {
        Self::new()
    }
}

/// A handle for registering roots that automatically unregisters on drop.
pub struct RootHandle {
    /// The scanner this root is registered with.
    scanner: Arc<RootScanner>,
    /// The header pointer.
    header: *mut ObjectHeader,
    /// Whether this is a global root.
    is_global: bool,
}

impl RootHandle {
    /// Creates a new global root handle.
    ///
    /// The root will be automatically unregistered when the handle is dropped.
    pub fn new_global(scanner: Arc<RootScanner>, header: *mut ObjectHeader) -> Self {
        scanner.add_global_root(header);
        Self {
            scanner,
            header,
            is_global: true,
        }
    }
}

impl Drop for RootHandle {
    fn drop(&mut self) {
        if self.is_global {
            self.scanner.remove_global_root(self.header);
        }
    }
}

// RootHandle is Send but not Sync since it represents ownership
unsafe impl Send for RootHandle {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_root_scanner_new() {
        let scanner = RootScanner::new();
        assert_eq!(scanner.num_registered_tasks(), 0);
    }

    #[test]
    fn test_register_unregister_task() {
        let scanner = RootScanner::new();
        let task = Task::new(|| {}, None);
        let task_id = task.id;

        scanner.register_task(task);
        assert_eq!(scanner.num_registered_tasks(), 1);

        assert!(scanner.unregister_task(task_id));
        assert_eq!(scanner.num_registered_tasks(), 0);

        // Unregistering again should return false
        assert!(!scanner.unregister_task(task_id));
    }

    #[test]
    fn test_global_roots() {
        let scanner = RootScanner::new();

        // We can't test with real headers without the heap,
        // but we can test the API
        let dummy_header = std::ptr::null_mut();
        scanner.add_global_root(dummy_header);

        let roots = scanner.global_roots();
        assert_eq!(roots.len(), 1);
        assert_eq!(roots[0].source, RootSource::Global);

        assert!(scanner.remove_global_root(dummy_header));
        assert!(!scanner.remove_global_root(dummy_header));
    }

    #[test]
    #[allow(clippy::arc_with_non_send_sync)]
    fn test_root_handle() {
        let scanner = Arc::new(RootScanner::new());
        let dummy_header = std::ptr::null_mut();

        {
            let _handle = RootHandle::new_global(scanner.clone(), dummy_header);
            assert_eq!(scanner.global_roots().len(), 1);
        }

        // After the handle is dropped, the root should be unregistered
        assert_eq!(scanner.global_roots().len(), 0);
    }

    #[test]
    fn test_is_potential_heap_pointer() {
        // Null should not be a heap pointer
        assert!(!RootScanner::is_potential_heap_pointer(0));

        // Misaligned values should not be heap pointers
        assert!(!RootScanner::is_potential_heap_pointer(0x1001));

        // Very low addresses should not be heap pointers
        assert!(!RootScanner::is_potential_heap_pointer(0x100));

        // Aligned, reasonable address should be a potential pointer
        assert!(RootScanner::is_potential_heap_pointer(0x10000));
    }

    #[test]
    fn test_clear_tasks() {
        let scanner = RootScanner::new();
        let task = Task::new(|| {}, None);

        scanner.register_task(task);
        assert_eq!(scanner.num_registered_tasks(), 1);

        scanner.clear_tasks();
        assert_eq!(scanner.num_registered_tasks(), 0);
    }

    #[test]
    fn test_clear_global_roots() {
        let scanner = RootScanner::new();
        let dummy_header = std::ptr::null_mut();

        scanner.add_global_root(dummy_header);
        assert_eq!(scanner.global_roots().len(), 1);

        scanner.clear_global_roots();
        assert_eq!(scanner.global_roots().len(), 0);
    }

    #[test]
    fn test_root_source_debug() {
        let source = RootSource::Global;
        let debug_str = format!("{:?}", source);
        assert!(debug_str.contains("Global"));

        let source = RootSource::TaskStack { task_id: 42 };
        let debug_str = format!("{:?}", source);
        assert!(debug_str.contains("TaskStack"));
        assert!(debug_str.contains("42"));
    }
}
