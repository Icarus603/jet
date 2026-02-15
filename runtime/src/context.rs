//! Runtime and thread context management.
//!
//! This module provides the runtime context shared across all threads
//! and thread-local context for each running thread.

use crate::config::RuntimeConfig;
use std::cell::RefCell;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};

/// The global runtime context shared across all threads.
///
/// This structure contains all the shared state that needs to be
/// accessible from any thread in the runtime.
pub struct RuntimeContext {
    /// Runtime configuration (immutable after creation)
    pub config: RuntimeConfig,
    /// Global task ID counter
    pub next_task_id: AtomicU64,
    /// Global object ID counter for GC
    pub next_object_id: AtomicU64,
    /// Current number of active threads
    pub active_threads: AtomicUsize,
    /// Total number of bytes allocated
    pub bytes_allocated: AtomicUsize,
    /// Total number of collections performed
    pub collection_count: AtomicUsize,
    /// Flag indicating if GC is requested
    pub gc_requested: std::sync::atomic::AtomicBool,
    /// Flag indicating if shutdown is requested
    pub shutdown_requested: std::sync::atomic::AtomicBool,
}

impl RuntimeContext {
    /// Creates a new runtime context with the given configuration.
    ///
    /// # Arguments
    ///
    /// * `config` - The runtime configuration.
    pub fn new(config: &RuntimeConfig) -> Self {
        Self {
            config: config.clone(),
            next_task_id: AtomicU64::new(1),
            next_object_id: AtomicU64::new(1),
            active_threads: AtomicUsize::new(0),
            bytes_allocated: AtomicUsize::new(0),
            collection_count: AtomicUsize::new(0),
            gc_requested: std::sync::atomic::AtomicBool::new(false),
            shutdown_requested: std::sync::atomic::AtomicBool::new(false),
        }
    }

    /// Generates a new unique task ID.
    pub fn next_task_id(&self) -> u64 {
        self.next_task_id.fetch_add(1, Ordering::Relaxed)
    }

    /// Generates a new unique object ID.
    pub fn next_object_id(&self) -> u64 {
        self.next_object_id.fetch_add(1, Ordering::Relaxed)
    }

    /// Increments the active thread count.
    pub fn inc_active_threads(&self) {
        self.active_threads.fetch_add(1, Ordering::Relaxed);
    }

    /// Decrements the active thread count.
    pub fn dec_active_threads(&self) {
        self.active_threads.fetch_sub(1, Ordering::Relaxed);
    }

    /// Returns the current number of active threads.
    pub fn active_thread_count(&self) -> usize {
        self.active_threads.load(Ordering::Relaxed)
    }

    /// Adds to the total bytes allocated.
    pub fn add_bytes_allocated(&self, bytes: usize) {
        self.bytes_allocated.fetch_add(bytes, Ordering::Relaxed);
    }

    /// Returns the total bytes allocated.
    pub fn total_bytes_allocated(&self) -> usize {
        self.bytes_allocated.load(Ordering::Relaxed)
    }

    /// Increments the collection count.
    pub fn inc_collection_count(&self) {
        self.collection_count.fetch_add(1, Ordering::Relaxed);
    }

    /// Returns the total number of collections.
    pub fn collection_count(&self) -> usize {
        self.collection_count.load(Ordering::Relaxed)
    }

    /// Requests a garbage collection cycle.
    pub fn request_gc(&self) {
        self.gc_requested.store(true, Ordering::Relaxed);
    }

    /// Checks if GC has been requested.
    pub fn is_gc_requested(&self) -> bool {
        self.gc_requested.load(Ordering::Relaxed)
    }

    /// Clears the GC request flag.
    pub fn clear_gc_request(&self) {
        self.gc_requested.store(false, Ordering::Relaxed);
    }

    /// Requests runtime shutdown.
    pub fn request_shutdown(&self) {
        self.shutdown_requested.store(true, Ordering::SeqCst);
    }

    /// Checks if shutdown has been requested.
    pub fn is_shutdown_requested(&self) -> bool {
        self.shutdown_requested.load(Ordering::SeqCst)
    }
}

/// Thread-local context for each runtime thread.
///
/// This structure contains all the state that is local to a specific thread.
/// It is accessed through thread-local storage for efficiency.
pub struct ThreadContext {
    /// The thread's ID
    pub thread_id: usize,
    /// The currently executing task (if any)
    pub current_task: Option<NonNull<jet_rt_sched::Task>>,
    /// Thread-local allocation buffer for GC
    pub tlab: jet_rt_gc::ThreadLocalBuffer,
    /// Roots registered by this thread
    pub roots: Vec<NonNull<jet_rt_gc::ObjectHeader>>,
    /// Flag indicating if this thread is at a safe point
    pub at_safe_point: bool,
    /// Flag indicating if this thread is a GC thread
    pub is_gc_thread: bool,
    /// Flag indicating if this thread is a worker thread
    pub is_worker_thread: bool,
}

impl ThreadContext {
    /// Creates a new thread context.
    pub fn new(thread_id: usize) -> Self {
        Self {
            thread_id,
            current_task: None,
            tlab: jet_rt_gc::ThreadLocalBuffer::new(),
            roots: Vec::new(),
            at_safe_point: false,
            is_gc_thread: false,
            is_worker_thread: false,
        }
    }

    /// Installs this thread context as the current thread's context.
    ///
    /// This should be called when a thread enters the runtime.
    pub fn install() {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static NEXT_THREAD_ID: AtomicUsize = AtomicUsize::new(1);

        THREAD_CONTEXT.with(|ctx| {
            let mut ctx = ctx.borrow_mut();
            if ctx.is_none() {
                let thread_id = NEXT_THREAD_ID.fetch_add(1, Ordering::Relaxed);
                *ctx = Some(Self::new(thread_id));
            }
        });
    }

    /// Uninstalls the thread context.
    ///
    /// This should be called when a thread exits the runtime.
    pub fn uninstall() {
        THREAD_CONTEXT.with(|ctx| {
            *ctx.borrow_mut() = None;
        });
    }

    /// Returns a reference to the current thread's context.
    ///
    /// # Panics
    ///
    /// Panics if called from a thread without an installed context.
    pub fn current<R, F: FnOnce(&ThreadContext) -> R>(f: F) -> R {
        THREAD_CONTEXT.with(|ctx| {
            let ctx_ref = std::cell::Ref::map(ctx.borrow(), |opt| {
                opt.as_ref().expect("No thread context installed")
            });
            f(&ctx_ref)
        })
    }

    /// Returns a mutable reference to the current thread's context.
    ///
    /// # Panics
    ///
    /// Panics if called from a thread without an installed context.
    pub fn current_mut<R, F: FnOnce(&mut ThreadContext) -> R>(f: F) -> R {
        THREAD_CONTEXT.with(|ctx| {
            let mut ctx_ref = std::cell::RefMut::map(ctx.borrow_mut(), |opt| {
                opt.as_mut().expect("No thread context installed")
            });
            f(&mut ctx_ref)
        })
    }

    /// Returns true if the current thread has a context installed.
    pub fn has_context() -> bool {
        THREAD_CONTEXT.with(|ctx| ctx.borrow().is_some())
    }

    /// Registers a GC root for this thread.
    ///
    /// # Safety
    ///
    /// The pointer must be a valid object header.
    pub unsafe fn add_root(&mut self, header: *mut jet_rt_gc::ObjectHeader) {
        self.roots.push(NonNull::new_unchecked(header));
    }

    /// Unregisters a GC root.
    pub fn remove_root(&mut self, header: *mut jet_rt_gc::ObjectHeader) {
        self.roots.retain(|r| r.as_ptr() != header);
    }

    /// Sets the current task.
    ///
    /// # Safety
    ///
    /// The pointer must remain valid for the duration of task execution.
    pub unsafe fn set_current_task(&mut self, task: Option<NonNull<jet_rt_sched::Task>>) {
        self.current_task = task;
    }

    /// Returns the current task ID if one is set.
    pub fn current_task_id(&self) -> Option<jet_rt_sched::TaskId> {
        self.current_task.map(|t| unsafe { t.as_ref().id })
    }

    /// Marks the thread as being at a safe point.
    pub fn enter_safe_point(&mut self) {
        self.at_safe_point = true;
    }

    /// Marks the thread as leaving a safe point.
    pub fn leave_safe_point(&mut self) {
        self.at_safe_point = false;
    }

    /// Returns true if the thread is at a safe point.
    pub fn is_at_safe_point(&self) -> bool {
        self.at_safe_point
    }
}

// Thread-local storage for the thread context
thread_local! {
    static THREAD_CONTEXT: RefCell<Option<ThreadContext>> = const { RefCell::new(None) };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_runtime_context() {
        let config = RuntimeConfig::default();
        let ctx = RuntimeContext::new(&config);

        assert_eq!(ctx.next_task_id(), 1);
        assert_eq!(ctx.next_task_id(), 2);

        assert_eq!(ctx.active_thread_count(), 0);
        ctx.inc_active_threads();
        assert_eq!(ctx.active_thread_count(), 1);
        ctx.dec_active_threads();
        assert_eq!(ctx.active_thread_count(), 0);
    }

    #[test]
    fn test_gc_request() {
        let config = RuntimeConfig::default();
        let ctx = RuntimeContext::new(&config);

        assert!(!ctx.is_gc_requested());
        ctx.request_gc();
        assert!(ctx.is_gc_requested());
        ctx.clear_gc_request();
        assert!(!ctx.is_gc_requested());
    }

    #[test]
    fn test_thread_context_install() {
        // Initially no context
        assert!(!ThreadContext::has_context());

        // Install context
        ThreadContext::install();
        assert!(ThreadContext::has_context());

        // Can access context
        ThreadContext::current(|ctx| {
            assert!(!ctx.is_at_safe_point());
        });

        // Uninstall context
        ThreadContext::uninstall();
        assert!(!ThreadContext::has_context());
    }

    #[test]
    fn test_safe_point() {
        ThreadContext::install();

        {
            ThreadContext::current_mut(|ctx| {
                assert!(!ctx.is_at_safe_point());

                ctx.enter_safe_point();
                assert!(ctx.is_at_safe_point());

                ctx.leave_safe_point();
                assert!(!ctx.is_at_safe_point());
            });
        }

        ThreadContext::uninstall();
    }
}
