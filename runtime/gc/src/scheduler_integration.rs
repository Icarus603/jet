//! GC Integration with the M:N Scheduler
//!
//! This module provides integration between the garbage collector and
//! the Jet runtime's M:N scheduler. It coordinates:
//!
//! - Stopping all mutator threads for GC
//! - Scanning thread stacks for roots
//! - Coordinating with work-stealing queues
//! - Resuming threads after GC
//!
//! # Thread Coordination
//!
//! The GC uses a cooperative safe-point mechanism:
//! 1. GC requests a safe point (sets global flag)
//! 2. Mutator threads check flag at safe points
//! 3. Threads park themselves when safe point is requested
//! 4. GC scans stacks and performs collection
//! 5. GC releases safe point, threads resume

use crate::immix::ImmixHeap;
use crate::safe_point::SafePointCoordinator;
use crate::stack_map::StackMapRegistry;
use crate::thread_roots::{GlobalRootScanner, RootSetCollector, ThreadRootScanner};
use crate::write_barrier::GlobalRememberedSet;
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

/// GC state shared with the scheduler
pub struct GcSchedulerState {
    /// The Immix heap
    pub heap: Mutex<ImmixHeap>,
    /// Stack map registry for root scanning
    pub stack_maps: Arc<Mutex<StackMapRegistry>>,
    /// Safe point coordinator
    pub coordinator: SafePointCoordinator,
    /// Global root scanner
    pub root_scanner: Mutex<GlobalRootScanner>,
    /// Global remembered set
    pub remembered_set: Mutex<GlobalRememberedSet>,
    /// GC is active
    pub gc_active: AtomicBool,
    /// Number of collections performed
    pub collection_count: AtomicUsize,
    /// Thread-local root scanners (indexed by thread ID)
    pub thread_scanners: Mutex<HashMap<usize, Arc<Mutex<ThreadRootScanner>>>>,
}

impl GcSchedulerState {
    /// Create a new GC scheduler state
    pub fn new() -> Self {
        let stack_maps = Arc::new(Mutex::new(StackMapRegistry::new()));

        GcSchedulerState {
            heap: Mutex::new(ImmixHeap::new()),
            stack_maps: stack_maps.clone(),
            coordinator: SafePointCoordinator::new(),
            root_scanner: Mutex::new(GlobalRootScanner::new(stack_maps)),
            remembered_set: Mutex::new(GlobalRememberedSet::new()),
            gc_active: AtomicBool::new(false),
            collection_count: AtomicUsize::new(0),
            thread_scanners: Mutex::new(HashMap::new()),
        }
    }

    /// Register a thread with the GC
    pub fn register_thread(&self, thread_id: usize, scanner: Arc<Mutex<ThreadRootScanner>>) {
        let mut scanners = self.thread_scanners.lock().unwrap();
        scanners.insert(thread_id, scanner.clone());

        let mut root_scanner = self.root_scanner.lock().unwrap();
        root_scanner.register_thread(scanner);

        self.coordinator.register_thread();
    }

    /// Unregister a thread from the GC
    pub fn unregister_thread(&self, thread_id: usize) {
        let mut scanners = self.thread_scanners.lock().unwrap();
        scanners.remove(&thread_id);

        let mut root_scanner = self.root_scanner.lock().unwrap();
        root_scanner.unregister_thread(thread_id);

        self.coordinator.unregister_thread();
    }

    /// Request a GC collection
    ///
    /// This stops all threads and performs collection
    pub fn request_collection(&self) -> CollectionResult {
        // Check if GC is already active
        if self.gc_active.swap(true, Ordering::SeqCst) {
            return CollectionResult {
                success: false,
                objects_collected: 0,
                bytes_reclaimed: 0,
                pause_time_ms: 0.0,
            };
        }

        let start_time = std::time::Instant::now();

        // Stop the world
        let stopped = self.coordinator.stop_the_world(5000); // 5 second timeout

        if !stopped {
            self.gc_active.store(false, Ordering::SeqCst);
            return CollectionResult {
                success: false,
                objects_collected: 0,
                bytes_reclaimed: 0,
                pause_time_ms: start_time.elapsed().as_secs_f64() * 1000.0,
            };
        }

        // Perform collection
        let result = self.collect();

        // Resume the world
        self.coordinator.resume_the_world();
        self.gc_active.store(false, Ordering::SeqCst);

        CollectionResult {
            success: true,
            objects_collected: result.objects_collected,
            bytes_reclaimed: result.bytes_reclaimed,
            pause_time_ms: start_time.elapsed().as_secs_f64() * 1000.0,
        }
    }

    /// Perform GC collection
    ///
    /// Assumes all threads are stopped at safe points
    fn collect(&self) -> CollectionSummary {
        let mut collector = RootSetCollector::new();

        // Scan all thread stacks for roots
        unsafe {
            let root_scanner = self.root_scanner.lock().unwrap();
            root_scanner.scan_all_threads(&mut collector);
        }

        // Collect remembered set from all threads
        let remembered_set = {
            let mut global = self.remembered_set.lock().unwrap();
            std::mem::take(&mut *global)
        };

        // Add remembered set entries as roots
        // (they represent mature objects pointing to nursery)
        remembered_set.process_entries(|object, _field| {
            collector.add_pinned(object);
        });

        // Convert roots to object headers
        let roots = unsafe { collector.to_object_headers() };

        // Perform collection
        let mut heap = self.heap.lock().unwrap();
        let stats = heap.collect(&roots);

        self.collection_count.fetch_add(1, Ordering::SeqCst);

        CollectionSummary {
            objects_collected: stats.objects_marked,
            bytes_reclaimed: stats.bytes_reclaimed,
        }
    }

    /// Try to trigger a nursery collection
    ///
    /// Called when a thread's local allocation buffer is full
    pub fn try_nursery_collection(&self) -> bool {
        let heap = self.heap.lock().unwrap();
        if heap.needs_collection() {
            drop(heap);
            let result = self.request_collection();
            result.success
        } else {
            false
        }
    }

    /// Get collection count
    pub fn collection_count(&self) -> usize {
        self.collection_count.load(Ordering::Relaxed)
    }

    /// Check if GC is active
    pub fn is_gc_active(&self) -> bool {
        self.gc_active.load(Ordering::Acquire)
    }

    /// Get heap statistics
    pub fn heap_stats(&self) -> crate::immix::HeapStats {
        let heap = self.heap.lock().unwrap();
        heap.stats()
    }
}

impl Default for GcSchedulerState {
    fn default() -> Self {
        Self::new()
    }
}

/// Result of a collection request
#[derive(Debug, Clone, Copy)]
pub struct CollectionResult {
    /// Whether the collection succeeded
    pub success: bool,
    /// Number of objects collected
    pub objects_collected: usize,
    /// Bytes reclaimed
    pub bytes_reclaimed: usize,
    /// Pause time in milliseconds
    pub pause_time_ms: f64,
}

/// Summary of collection
#[derive(Debug, Clone, Copy)]
pub struct CollectionSummary {
    /// Number of objects collected
    pub objects_collected: usize,
    /// Bytes reclaimed
    pub bytes_reclaimed: usize,
}

/// Thread-local GC state
///
/// Each OS thread in the scheduler pool has one of these
pub struct ThreadLocalGcState {
    /// Thread ID
    pub thread_id: usize,
    /// Root scanner for this thread
    pub root_scanner: Arc<Mutex<ThreadRootScanner>>,
    /// Whether this thread is a mutator
    pub is_mutator: bool,
    /// Local allocation buffer (optional optimization)
    pub local_buffer: Option<ThreadLocalAllocBuffer>,
}

impl ThreadLocalGcState {
    /// Create a new thread-local GC state
    pub fn new(thread_id: usize) -> Self {
        ThreadLocalGcState {
            thread_id,
            root_scanner: Arc::new(Mutex::new(ThreadRootScanner::new(thread_id))),
            is_mutator: true,
            local_buffer: None,
        }
    }

    /// Register with the global GC state
    pub fn register(&self, global: &GcSchedulerState) {
        global.register_thread(self.thread_id, self.root_scanner.clone());
    }

    /// Unregister from the global GC state
    pub fn unregister(&self, global: &GcSchedulerState) {
        global.unregister_thread(self.thread_id);
    }

    /// Set stack bounds for root scanning
    pub fn set_stack_bounds(&self, top: *mut u8, bottom: *mut u8) {
        let mut scanner = self.root_scanner.lock().unwrap();
        scanner.set_stack_bounds(top, bottom);
    }
}

/// Thread-local allocation buffer
///
/// Cached allocation buffer to reduce contention on the global heap
pub struct ThreadLocalAllocBuffer {
    /// Current allocation pointer
    pub cursor: *mut u8,
    /// End of buffer
    pub limit: *mut u8,
    /// Buffer size
    pub size: usize,
}

impl ThreadLocalAllocBuffer {
    /// Create a new empty buffer
    pub const fn empty() -> Self {
        ThreadLocalAllocBuffer {
            cursor: std::ptr::null_mut(),
            limit: std::ptr::null_mut(),
            size: 0,
        }
    }

    /// Check if buffer has space for allocation
    pub fn has_space(&self, size: usize) -> bool {
        if self.cursor.is_null() {
            return false;
        }
        let new_cursor = unsafe { self.cursor.add(size) };
        new_cursor <= self.limit
    }

    /// Allocate from buffer
    ///
    /// Returns pointer or null if buffer is full
    pub fn allocate(&mut self, size: usize) -> Option<*mut u8> {
        if !self.has_space(size) {
            return None;
        }
        let ptr = self.cursor;
        self.cursor = unsafe { self.cursor.add(size) };
        Some(ptr)
    }

    /// Reset the buffer
    ///
    /// # Safety
    /// - `start` must be a valid pointer to a buffer of at least `size` bytes
    pub unsafe fn reset(&mut self, start: *mut u8, size: usize) {
        self.cursor = start;
        self.limit = start.add(size);
        self.size = size;
    }

    /// Check if buffer is empty
    pub fn is_empty(&self) -> bool {
        self.cursor.is_null() || self.cursor == self.limit
    }
}

impl Default for ThreadLocalAllocBuffer {
    fn default() -> Self {
        Self::empty()
    }
}

/// GC-aware work-stealing queue
///
/// Wraps a regular work-stealing queue with GC safe points
#[cfg(feature = "scheduler-integration")]
pub struct GcWorkStealingQueue<T> {
    /// Inner queue
    inner: crossbeam::deque::Worker<T>,
    /// Stealer for other threads
    stealer: crossbeam::deque::Stealer<T>,
}

#[cfg(feature = "scheduler-integration")]
impl<T> GcWorkStealingQueue<T> {
    /// Create a new GC-aware work-stealing queue
    pub fn new() -> Self {
        let worker = crossbeam::deque::Worker::new_fifo();
        let stealer = worker.stealer();

        GcWorkStealingQueue {
            inner: worker,
            stealer,
        }
    }

    /// Push a task onto the queue
    pub fn push(&self, task: T) {
        self.inner.push(task);
    }

    /// Pop a task from the queue
    pub fn pop(&self) -> Option<T> {
        self.inner.pop()
    }

    /// Get the stealer for this queue
    pub fn stealer(&self) -> &crossbeam::deque::Stealer<T> {
        &self.stealer
    }

    /// Check if queue is empty
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }
}

#[cfg(feature = "scheduler-integration")]
impl<T> Default for GcWorkStealingQueue<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// Stub implementation when scheduler-integration feature is disabled
#[cfg(not(feature = "scheduler-integration"))]
pub struct GcWorkStealingQueue<T> {
    _phantom: std::marker::PhantomData<T>,
}

#[cfg(not(feature = "scheduler-integration"))]
impl<T> GcWorkStealingQueue<T> {
    /// Create a new stub queue
    pub fn new() -> Self {
        GcWorkStealingQueue {
            _phantom: std::marker::PhantomData,
        }
    }

    /// Push is a no-op in stub
    pub fn push(&self, _task: T) {}

    /// Pop always returns None in stub
    pub fn pop(&self) -> Option<T> {
        None
    }

    /// Check if queue is empty
    pub fn is_empty(&self) -> bool {
        true
    }
}

#[cfg(not(feature = "scheduler-integration"))]
impl<T> Default for GcWorkStealingQueue<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// GC integration for the scheduler
///
/// This trait is implemented by the scheduler to provide GC coordination
pub trait GcSchedulerIntegration {
    /// Get the GC scheduler state
    fn gc_state(&self) -> &GcSchedulerState;

    /// Get current thread ID
    fn current_thread_id(&self) -> usize;

    /// Get thread-local GC state for current thread
    fn thread_local_gc_state(&self) -> &ThreadLocalGcState;

    /// Request a GC collection
    fn request_gc(&self) -> CollectionResult {
        self.gc_state().request_collection()
    }

    /// Check if GC is needed
    fn check_gc_needed(&self) -> bool {
        let state = self.gc_state();
        let heap = state.heap.lock().unwrap();
        heap.needs_collection()
    }

    /// Allocate an object
    fn allocate(&self, size: usize, align: usize) -> Option<*mut u8> {
        let mut heap = self.gc_state().heap.lock().unwrap();
        let ptr = heap.allocate(size, align);

        if ptr.is_null() {
            // Allocation failed - try collection
            drop(heap);
            let result = self.gc_state().request_collection();

            if result.success {
                let mut heap = self.gc_state().heap.lock().unwrap();
                let ptr = heap.allocate(size, align);
                if ptr.is_null() {
                    None
                } else {
                    Some(ptr)
                }
            } else {
                None
            }
        } else {
            Some(ptr)
        }
    }

    /// Allocate a typed object
    fn allocate_object(
        &self,
        size: usize,
        align: usize,
        type_id: crate::header::TypeId,
    ) -> Option<*mut u8> {
        let mut heap = self.gc_state().heap.lock().unwrap();
        let ptr = heap.allocate_object(size, align, type_id);
        if ptr.is_null() {
            None
        } else {
            Some(ptr)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gc_scheduler_state_creation() {
        let state = GcSchedulerState::new();
        assert!(!state.is_gc_active());
        assert_eq!(state.collection_count(), 0);
    }

    #[test]
    fn test_thread_local_gc_state() {
        let thread_state = ThreadLocalGcState::new(1);
        assert_eq!(thread_state.thread_id, 1);
        assert!(thread_state.is_mutator);
    }

    #[test]
    fn test_thread_local_alloc_buffer() {
        let mut buffer = ThreadLocalAllocBuffer::empty();
        assert!(buffer.is_empty());
        assert!(!buffer.has_space(100));

        // Create a buffer with actual memory
        let memory = vec![0u8; 1024];
        let ptr = memory.as_ptr() as *mut u8;
        unsafe {
            buffer.reset(ptr, 1024);
        }

        assert!(buffer.has_space(100));
        let alloc1 = buffer.allocate(100).unwrap();
        assert_eq!(alloc1, ptr);

        assert!(buffer.has_space(200));
        let alloc2 = buffer.allocate(200).unwrap();
        assert_eq!(alloc2, unsafe { ptr.add(100) });

        // Try to allocate more than remaining
        assert!(!buffer.has_space(1000));
    }

    #[test]
    fn test_collection_result() {
        let result = CollectionResult {
            success: true,
            objects_collected: 100,
            bytes_reclaimed: 4096,
            pause_time_ms: 10.5,
        };

        assert!(result.success);
        assert_eq!(result.objects_collected, 100);
        assert_eq!(result.bytes_reclaimed, 4096);
    }
}
