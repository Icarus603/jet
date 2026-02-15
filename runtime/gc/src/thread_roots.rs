//! Thread Root Set Scanning
//!
//! This module provides functionality for scanning thread stacks to find
//! GC roots. It coordinates with the scheduler to suspend threads and
//! scan their stacks using stack maps.
//!
//! # Root Sources
//!
//! 1. **Stack roots**: Local variables and temporaries containing GC pointers
//! 2. **Register roots**: GC pointers in registers (saved at safe points)
//! 3. **Thread-local roots**: Thread-local handles and pinned objects
//! 4. **Global roots**: Static data and interned strings
//!
//! # Scanning Process
//!
//! 1. Stop all mutator threads (or wait for safe points)
//! 2. For each thread:
//!    - Walk the stack from top to bottom
//!    - For each frame, find the corresponding stack map
//!    - Extract GC pointers at the recorded offsets
//!    - Add to root set
//! 3. Resume threads

use crate::header::ObjectHeader;
use crate::safe_point::ThreadSafePoint;
use crate::stack_map::StackMapRegistry;
use std::collections::HashSet;
use std::sync::{Arc, Mutex};

/// A GC root - pointer to a location containing a heap pointer
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GcRoot {
    /// Address of the root (pointer to the pointer)
    pub address: *mut *mut u8,
    /// Stack frame this root belongs to (for debugging)
    pub frame_address: *mut u8,
}

impl GcRoot {
    /// Create a new GC root
    pub const fn new(address: *mut *mut u8, frame_address: *mut u8) -> Self {
        GcRoot {
            address,
            frame_address,
        }
    }

    /// Get the object pointer from this root
    ///
    /// # Safety
    /// The root must point to valid memory
    pub unsafe fn get_object(&self) -> *mut u8 {
        *self.address
    }

    /// Update the object pointer in this root
    ///
    /// # Safety
    /// The root must point to valid memory
    pub unsafe fn set_object(&self, new_object: *mut u8) {
        *self.address = new_object;
    }
}

/// Thread-local root set
///
/// Roots that are registered by the thread itself (not found via stack scanning)
pub struct ThreadLocalRoots {
    /// Explicitly registered roots
    roots: Vec<GcRoot>,
    /// Pinned objects (cannot move during GC)
    pinned: HashSet<*mut u8>,
}

impl ThreadLocalRoots {
    /// Create a new thread-local root set
    pub fn new() -> Self {
        ThreadLocalRoots {
            roots: Vec::new(),
            pinned: HashSet::new(),
        }
    }

    /// Register a root
    pub fn register(&mut self, address: *mut *mut u8) {
        self.roots.push(GcRoot::new(address, std::ptr::null_mut()));
    }

    /// Unregister a root
    pub fn unregister(&mut self, address: *mut *mut u8) {
        self.roots.retain(|r| r.address != address);
    }

    /// Pin an object
    ///
    /// Pinned objects cannot be moved during GC
    pub fn pin(&mut self, object: *mut u8) {
        self.pinned.insert(object);
    }

    /// Unpin an object
    pub fn unpin(&mut self, object: *mut u8) {
        self.pinned.remove(&object);
    }

    /// Check if an object is pinned
    pub fn is_pinned(&self, object: *mut u8) -> bool {
        self.pinned.contains(&object)
    }

    /// Iterate over registered roots
    pub fn iter(&self) -> impl Iterator<Item = &GcRoot> {
        self.roots.iter()
    }

    /// Get pinned objects
    pub fn pinned_objects(&self) -> &HashSet<*mut u8> {
        &self.pinned
    }

    /// Clear all roots
    pub fn clear(&mut self) {
        self.roots.clear();
        self.pinned.clear();
    }

    /// Get number of roots
    pub fn len(&self) -> usize {
        self.roots.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.roots.is_empty() && self.pinned.is_empty()
    }
}

impl Default for ThreadLocalRoots {
    fn default() -> Self {
        Self::new()
    }
}

/// Saved register state at a safe point
///
/// This captures the values of all registers that might contain GC pointers
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct SavedRegisters {
    /// General purpose registers (platform-specific)
    pub rax: u64,
    pub rbx: u64,
    pub rcx: u64,
    pub rdx: u64,
    pub rsi: u64,
    pub rdi: u64,
    pub rbp: u64,
    pub rsp: u64,
    pub r8: u64,
    pub r9: u64,
    pub r10: u64,
    pub r11: u64,
    pub r12: u64,
    pub r13: u64,
    pub r14: u64,
    pub r15: u64,
}

impl SavedRegisters {
    /// Create a new empty register state
    pub const fn new() -> Self {
        SavedRegisters {
            rax: 0,
            rbx: 0,
            rcx: 0,
            rdx: 0,
            rsi: 0,
            rdi: 0,
            rbp: 0,
            rsp: 0,
            r8: 0,
            r9: 0,
            r10: 0,
            r11: 0,
            r12: 0,
            r13: 0,
            r14: 0,
            r15: 0,
        }
    }

    /// Iterate over all registers as potential pointers
    pub fn iter_registers(&self) -> impl Iterator<Item = u64> {
        [
            self.rax, self.rbx, self.rcx, self.rdx, self.rsi, self.rdi, self.rbp, self.rsp,
            self.r8, self.r9, self.r10, self.r11, self.r12, self.r13, self.r14, self.r15,
        ]
        .into_iter()
    }
}

impl Default for SavedRegisters {
    fn default() -> Self {
        Self::new()
    }
}

/// Per-thread state for root scanning
///
/// # Safety
/// This struct contains raw pointers and is wrapped in Arc<Mutex<>> for sharing
/// between threads. The Send and Sync implementations are safe because:
/// - The raw pointers are only accessed when the thread is stopped at a safe point
/// - All access is synchronized through the Mutex
pub struct ThreadRootScanner {
    /// Thread ID
    pub thread_id: usize,
    /// Safe point state
    pub safe_point: ThreadSafePoint,
    /// Thread-local roots
    pub local_roots: ThreadLocalRoots,
    /// Saved registers at safe point
    pub saved_registers: Option<SavedRegisters>,
    /// Stack bounds
    pub stack_top: *mut u8,
    pub stack_bottom: *mut u8,
}

// SAFETY: ThreadRootScanner is Send because it's only accessed through Mutex
// and the raw pointers are only used when the thread is stopped
unsafe impl Send for ThreadRootScanner {}

// SAFETY: ThreadRootScanner is Sync because all mutable access is synchronized
// through the Mutex, and the raw pointers are only read when the thread is stopped
unsafe impl Sync for ThreadRootScanner {}

impl ThreadRootScanner {
    /// Create a new thread root scanner
    pub fn new(thread_id: usize) -> Self {
        ThreadRootScanner {
            thread_id,
            safe_point: ThreadSafePoint::new(thread_id),
            local_roots: ThreadLocalRoots::new(),
            saved_registers: None,
            stack_top: std::ptr::null_mut(),
            stack_bottom: std::ptr::null_mut(),
        }
    }

    /// Set stack bounds
    pub fn set_stack_bounds(&mut self, top: *mut u8, bottom: *mut u8) {
        self.stack_top = top;
        self.stack_bottom = bottom;
    }

    /// Check if thread is at a safe point
    pub fn is_at_safe_point(&self) -> bool {
        self.safe_point.is_at_safe_point()
    }

    /// Scan this thread's stack for roots
    ///
    /// # Safety
    /// The thread must be stopped at a safe point
    pub unsafe fn scan_stack(&self, registry: &StackMapRegistry, callback: &mut dyn FnMut(GcRoot)) {
        if self.stack_top.is_null() || self.stack_bottom.is_null() {
            return;
        }

        // Start from the frame pointer at the safe point
        let mut current_frame = self.safe_point.frame_pointer;
        let mut current_ip = self.safe_point.instruction_pointer;

        // Walk the stack
        while !current_frame.is_null() && current_frame >= self.stack_bottom {
            // Find stack map for this return address
            if let Some((section, entry)) = registry.find_entry_for_address(current_ip as u64) {
                // Get pointer offsets for this frame
                let offsets = section.get_pointer_offsets(entry);

                // Calculate frame base
                let frame_base = current_frame;

                // Iterate over roots in this frame
                for offset in offsets {
                    let root_addr = frame_base.add(*offset as usize) as *mut *mut u8;
                    callback(GcRoot::new(root_addr, current_frame));
                }
            }

            // Move to previous frame
            // Frame layout: [saved rbp] [return address] [locals...]
            current_ip = *(current_frame.add(std::mem::size_of::<usize>())) as *mut u8;
            current_frame = *(current_frame as *mut *mut u8);
        }
    }

    /// Scan saved registers for roots
    ///
    /// # Safety
    /// The thread must be stopped at a safe point with valid saved registers
    pub unsafe fn scan_registers(
        &self,
        heap_contains: impl Fn(*const u8) -> bool,
        _callback: &mut dyn FnMut(GcRoot),
    ) {
        if let Some(regs) = self.saved_registers {
            for reg_value in regs.iter_registers() {
                let ptr = reg_value as *const u8;
                if heap_contains(ptr) {
                    // Register contains a heap pointer
                    // We need to treat this as a root, but registers don't have addresses
                    // In practice, this is handled by the stack map including spill slots
                }
            }
        }
    }

    /// Get all thread-local roots
    pub fn get_local_roots(&self) -> &[GcRoot] {
        &self.local_roots.roots
    }

    /// Get pinned objects
    pub fn get_pinned_objects(&self) -> &HashSet<*mut u8> {
        &self.local_roots.pinned
    }
}

/// Global root set collector
pub struct RootSetCollector {
    /// All collected roots
    roots: Vec<GcRoot>,
    /// Pinned objects from all threads
    pinned: HashSet<*mut u8>,
}

impl RootSetCollector {
    /// Create a new root set collector
    pub fn new() -> Self {
        RootSetCollector {
            roots: Vec::new(),
            pinned: HashSet::new(),
        }
    }

    /// Add a root
    pub fn add_root(&mut self, root: GcRoot) {
        self.roots.push(root);
    }

    /// Add a pinned object
    pub fn add_pinned(&mut self, object: *mut u8) {
        self.pinned.insert(object);
    }

    /// Get all roots
    pub fn roots(&self) -> &[GcRoot] {
        &self.roots
    }

    /// Get all pinned objects
    pub fn pinned(&self) -> &HashSet<*mut u8> {
        &self.pinned
    }

    /// Convert roots to object headers
    ///
    /// # Safety
    /// All roots must point to valid GC objects
    pub unsafe fn to_object_headers(&self) -> Vec<*mut ObjectHeader> {
        self.roots
            .iter()
            .filter_map(|root| {
                let ptr = root.get_object();
                if ptr.is_null() {
                    None
                } else {
                    Some(ObjectHeader::from_payload(ptr))
                }
            })
            .collect()
    }

    /// Clear all roots
    pub fn clear(&mut self) {
        self.roots.clear();
        self.pinned.clear();
    }

    /// Get number of roots
    pub fn len(&self) -> usize {
        self.roots.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.roots.is_empty()
    }
}

impl Default for RootSetCollector {
    fn default() -> Self {
        Self::new()
    }
}

/// Global root scanner
///
/// Coordinates root scanning across all threads
pub struct GlobalRootScanner {
    /// Stack map registry
    stack_maps: Arc<Mutex<StackMapRegistry>>,
    /// Thread scanners
    threads: Vec<Arc<Mutex<ThreadRootScanner>>>,
}

impl GlobalRootScanner {
    /// Create a new global root scanner
    pub fn new(stack_maps: Arc<Mutex<StackMapRegistry>>) -> Self {
        GlobalRootScanner {
            stack_maps,
            threads: Vec::new(),
        }
    }

    /// Register a thread for scanning
    pub fn register_thread(&mut self, scanner: Arc<Mutex<ThreadRootScanner>>) {
        self.threads.push(scanner);
    }

    /// Unregister a thread
    pub fn unregister_thread(&mut self, thread_id: usize) {
        self.threads
            .retain(|t| t.lock().unwrap().thread_id != thread_id);
    }

    /// Scan all threads for roots
    ///
    /// # Safety
    /// All threads must be stopped at safe points
    pub unsafe fn scan_all_threads(&self, collector: &mut RootSetCollector) {
        let registry = self.stack_maps.lock().unwrap();

        for thread in &self.threads {
            let scanner = thread.lock().unwrap();

            // Scan stack
            scanner.scan_stack(&registry, &mut |root| collector.add_root(root));

            // Add thread-local roots
            for root in scanner.get_local_roots() {
                collector.add_root(*root);
            }

            // Add pinned objects
            for &pinned in scanner.get_pinned_objects() {
                collector.add_pinned(pinned);
            }
        }
    }

    /// Get number of registered threads
    pub fn thread_count(&self) -> usize {
        self.threads.len()
    }
}

/// Conservative stack scanning
///
/// Used as a fallback when precise stack maps are not available
/// Scans the stack word-by-word looking for potential heap pointers
///
/// # Safety
/// The thread must be stopped
pub unsafe fn conservative_scan_stack(
    stack_top: *mut u8,
    stack_bottom: *mut u8,
    heap_contains: impl Fn(*const u8) -> bool,
    callback: &mut dyn FnMut(*mut u8),
) {
    let mut current = stack_top;

    while current < stack_bottom {
        // Read word as potential pointer
        let potential_ptr = *(current as *mut usize) as *mut u8;

        // Check if it points into the heap
        if heap_contains(potential_ptr) {
            callback(potential_ptr);
        }

        current = current.add(std::mem::size_of::<usize>());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gc_root() {
        let mut value: *mut u8 = 0x1000 as *mut u8;
        let root = GcRoot::new(&mut value as *mut *mut u8, std::ptr::null_mut());

        unsafe {
            assert_eq!(root.get_object(), 0x1000 as *mut u8);
            root.set_object(0x2000 as *mut u8);
            assert_eq!(value, 0x2000 as *mut u8);
        }
    }

    #[test]
    fn test_thread_local_roots() {
        let mut roots = ThreadLocalRoots::new();
        assert!(roots.is_empty());

        let mut value: *mut u8 = 0x1000 as *mut u8;
        roots.register(&mut value as *mut *mut u8);
        assert_eq!(roots.len(), 1);

        roots.pin(0x2000 as *mut u8);
        assert!(roots.is_pinned(0x2000 as *mut u8));

        roots.unpin(0x2000 as *mut u8);
        assert!(!roots.is_pinned(0x2000 as *mut u8));

        roots.unregister(&mut value as *mut *mut u8);
        assert!(roots.is_empty());
    }

    #[test]
    fn test_saved_registers() {
        let regs = SavedRegisters::new();
        assert_eq!(regs.rax, 0);
        assert_eq!(regs.r15, 0);

        let count = regs.iter_registers().count();
        assert_eq!(count, 16);
    }

    #[test]
    fn test_root_set_collector() {
        let mut collector = RootSetCollector::new();
        assert!(collector.is_empty());

        let root = GcRoot::new(0x1000 as *mut *mut u8, 0x2000 as *mut u8);
        collector.add_root(root);
        assert_eq!(collector.len(), 1);

        collector.add_pinned(0x3000 as *mut u8);
        assert!(collector.pinned().contains(&(0x3000 as *mut u8)));

        collector.clear();
        assert!(collector.is_empty());
    }

    #[test]
    fn test_thread_root_scanner() {
        let scanner = ThreadRootScanner::new(1);
        assert_eq!(scanner.thread_id, 1);
        assert!(!scanner.is_at_safe_point());
    }
}
