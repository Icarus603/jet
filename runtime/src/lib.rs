//! Jet Runtime - Integrated Runtime System
//!
//! This crate provides the integrated runtime system for the Jet programming language,
//! combining garbage collection, task scheduling, synchronization primitives, and FFI
//! into a cohesive system.
//!
//! # Architecture
//!
//! The runtime consists of several integrated subsystems:
//!
//! - **GC**: Immix garbage collector with generational collection
//! - **Scheduler**: M:N threading with work-stealing
//! - **Sync**: Channels, mutexes, condition variables
//! - **FFI**: C interoperability with marshaling
//! - **Reactor**: Async I/O (epoll/kqueue/IOCP)
//!
//! # Usage
//!
//! ```rust,no_run
//! use jet_rt::{Runtime, RuntimeConfig};
//!
//! let config = RuntimeConfig::default();
//! let mut runtime = Runtime::new(config).expect("Failed to create runtime");
//!
//! runtime.run(|| {
//!     // Your Jet program runs here
//!     println!("Hello from Jet!");
//! }).expect("Runtime execution failed");
//! ```

#![warn(missing_docs)]
#![warn(rust_2018_idioms)]

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

// Re-export runtime components
pub use jet_rt_ffi as ffi;
pub use jet_rt_gc as gc;
pub use jet_rt_sched as sched;
pub use jet_rt_sync as sync;

// Internal modules
pub mod builtins;
mod config;
mod context;
mod effect;
mod init;
mod panic;
mod reactor;
mod safe_point;
mod shutdown;
mod signal;
mod thread_coord;

// Public API
pub use config::{GCConfig, RuntimeConfig, SchedulerConfig};
pub use context::{RuntimeContext, ThreadContext};
pub use effect::{
    async_integration, clear_current_effect_stack, current_effect_stack, perform_effect,
    set_current_effect_stack, AsyncEffectContext, AsyncEffectFuture, AsyncEffectHandle,
    AsyncEffectHandler, AsyncEffectRegistry, Continuation, ContinuationHandle, EffectData,
    EffectDispatch, EffectFrame, EffectHandler, EffectId, EffectRuntime, EffectStack, EffectTypeId,
    EffectValue, EffectfulFuture, HandlerId, HandlerResult, HandlerTable, OperationId,
    PerformResult, ResumeKind, ResumeResult, StackFrameId,
};
pub use init::{initialize, is_initialized};
pub use panic::{catch_panic, set_panic_handler, PanicInfo, PanicResult};
pub use reactor::{Reactor, ReactorHandle};
pub use safe_point::{enter_safe_point, is_at_safe_point, leave_safe_point, SafePointScope};
pub use shutdown::{is_shutdown_requested, request_shutdown, shutdown, wait_for_shutdown};

/// The main Jet runtime.
///
/// This struct represents the integrated runtime system that manages
/// all runtime components. It provides a unified interface for
/// initializing, running, and shutting down the runtime.
///
/// # Example
///
/// ```rust,no_run
/// use jet_rt::{Runtime, RuntimeConfig};
///
/// let mut runtime = Runtime::new(RuntimeConfig::default())
///     .expect("Failed to create runtime");
///
/// runtime.run(|| {
///     // Jet program code
/// }).expect("Runtime execution failed");
/// ```
pub struct Runtime {
    /// Runtime configuration
    #[allow(dead_code)]
    config: RuntimeConfig,
    /// Runtime context shared across all threads
    context: Arc<RuntimeContext>,
    /// The task scheduler
    scheduler: sched::Scheduler,
    /// The garbage collector heap
    heap: std::sync::Mutex<gc::ImmixHeap>,
    /// The I/O reactor
    reactor: Reactor,
    /// Flag indicating if the runtime is running
    running: AtomicBool,
}

/// Errors that can occur during runtime operations.
#[derive(Debug, thiserror::Error)]
pub enum RuntimeError {
    /// Runtime is already initialized
    #[error("Runtime is already initialized")]
    AlreadyInitialized,

    /// Runtime is not initialized
    #[error("Runtime is not initialized")]
    NotInitialized,

    /// Failed to create scheduler
    #[error("Failed to create scheduler: {0}")]
    SchedulerCreation(String),

    /// Failed to initialize GC
    #[error("Failed to initialize GC: {0}")]
    GcInit(String),

    /// Failed to initialize I/O reactor
    #[error("Failed to initialize I/O reactor: {0}")]
    ReactorInit(String),

    /// Runtime panic occurred
    #[error("Runtime panic: {0}")]
    Panic(String),

    /// Shutdown error
    #[error("Shutdown error: {0}")]
    Shutdown(String),
}

/// Result type for runtime operations.
pub type Result<T> = std::result::Result<T, RuntimeError>;

impl Runtime {
    /// Creates a new runtime with the given configuration.
    ///
    /// # Arguments
    ///
    /// * `config` - The runtime configuration.
    ///
    /// # Returns
    ///
    /// Returns a new `Runtime` instance or an error if initialization fails.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use jet_rt::{Runtime, RuntimeConfig};
    ///
    /// let config = RuntimeConfig::default();
    /// let mut runtime = Runtime::new(config).expect("Failed to create runtime");
    /// ```
    pub fn new(config: RuntimeConfig) -> Result<Self> {
        // Check if already initialized
        if is_initialized() {
            return Err(RuntimeError::AlreadyInitialized);
        }

        // Initialize signal handlers early
        signal::init();

        // Create the runtime context
        let context = Arc::new(RuntimeContext::new(&config));

        // Initialize the scheduler
        let scheduler = sched::Scheduler::new(config.scheduler.num_threads);

        // Initialize the GC heap
        let heap = gc::ImmixHeap::with_config(config.gc.into());

        // Initialize the I/O reactor
        let reactor =
            Reactor::new(&config).map_err(|e| RuntimeError::ReactorInit(e.to_string()))?;

        // Mark as initialized
        init::mark_initialized();

        Ok(Runtime {
            config,
            context,
            scheduler,
            heap: std::sync::Mutex::new(heap),
            reactor,
            running: AtomicBool::new(false),
        })
    }

    /// Runs the runtime with the given main function.
    ///
    /// This method starts the scheduler, runs the main function, and
    /// properly shuts down all runtime components when complete.
    ///
    /// # Arguments
    ///
    /// * `main` - The main function to run.
    ///
    /// # Returns
    ///
    /// Returns `Ok(())` on success, or an error if the runtime fails.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use jet_rt::{Runtime, RuntimeConfig};
    ///
    /// let mut runtime = Runtime::new(RuntimeConfig::default()).unwrap();
    ///
    /// runtime.run(|| {
    ///     println!("Hello from Jet runtime!");
    /// }).unwrap();
    /// ```
    pub fn run<F>(&mut self, main: F) -> Result<()>
    where
        F: FnOnce() + Send + 'static,
    {
        if self.running.load(Ordering::SeqCst) {
            return Err(RuntimeError::AlreadyInitialized);
        }

        self.running.store(true, Ordering::SeqCst);

        // Set up thread coordination between GC and scheduler
        thread_coord::init(self.context.clone());

        // Start the I/O reactor
        self.reactor.start()?;

        // Spawn the main task
        self.scheduler.spawn(move || {
            // Set up thread-local context
            ThreadContext::install();

            // Run the main function with panic handling
            use std::panic::AssertUnwindSafe;
            let result = catch_panic(AssertUnwindSafe(main));

            if let PanicResult::Panic(ref info) = result {
                eprintln!("Runtime panic: {:?}", info);
            }

            // Clean up thread-local context
            ThreadContext::uninstall();
        });

        // Run the scheduler (blocks until all tasks complete)
        self.scheduler.run();

        // Begin shutdown
        self.shutdown()
    }

    /// Spawns a new task onto the runtime.
    ///
    /// # Arguments
    ///
    /// * `f` - The function to execute.
    ///
    /// # Returns
    ///
    /// Returns the `TaskId` of the spawned task.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use jet_rt::Runtime;
    ///
    /// # let mut runtime = Runtime::new(Default::default()).unwrap();
    /// let task_id = runtime.spawn(|| {
    ///     println!("Running in a task!");
    /// });
    /// ```
    pub fn spawn<F>(&self, f: F) -> sched::TaskId
    where
        F: FnOnce() + Send + 'static,
    {
        self.scheduler.spawn(f)
    }

    /// Performs a garbage collection cycle.
    ///
    /// This is typically called automatically by the GC when needed,
    /// but can be called manually for testing or tuning purposes.
    ///
    /// # Safety
    ///
    /// This method is unsafe because it requires all threads to be
    /// at safe points. The runtime ensures this automatically.
    pub unsafe fn gc_collect(&self) -> gc::CollectionStats {
        let mut heap = self.heap.lock().unwrap();

        // Get roots from all tasks via the scheduler
        let task_roots = self.scheduler.scan_task_roots();
        let global_roots = self.scheduler.global_roots();

        // Convert Root pointers to ObjectHeader pointers
        let mut roots: Vec<*mut gc::ObjectHeader> =
            Vec::with_capacity(task_roots.len() + global_roots.len());

        for root in task_roots {
            roots.push(root.header);
        }
        for root in global_roots {
            roots.push(root.header);
        }

        heap.collect(&roots)
    }

    /// Returns runtime statistics.
    pub fn stats(&self) -> RuntimeStats {
        let heap = self.heap.lock().unwrap();

        RuntimeStats {
            heap_stats: heap.stats(),
            scheduler_workers: self.scheduler.num_workers(),
            tasks_spawned: self.scheduler.tasks_spawned(),
            tasks_completed: self.scheduler.tasks_completed(),
        }
    }

    /// Returns a handle to the I/O reactor.
    pub fn reactor(&self) -> &Reactor {
        &self.reactor
    }

    /// Shuts down the runtime gracefully.
    fn shutdown(&mut self) -> Result<()> {
        if !self.running.load(Ordering::SeqCst) {
            // Runtime instances that were created but never run must still
            // release global initialization state.
            init::mark_uninitialized();
            return Ok(());
        }

        // Signal shutdown
        self.running.store(false, Ordering::SeqCst);

        // Stop the I/O reactor
        self.reactor.stop()?;

        // Run final GC to clean up
        {
            let mut heap = self.heap.lock().unwrap();
            let _ = heap.collect_full(&[]);
        }

        // Clean up thread coordination
        thread_coord::cleanup();

        // Mark as uninitialized
        init::mark_uninitialized();

        Ok(())
    }
}

impl Drop for Runtime {
    fn drop(&mut self) {
        let _ = self.shutdown();
    }
}

/// Runtime statistics.
#[derive(Debug, Clone, Copy)]
pub struct RuntimeStats {
    /// Heap statistics
    pub heap_stats: gc::HeapStats,
    /// Number of scheduler worker threads
    pub scheduler_workers: usize,
    /// Total tasks spawned
    pub tasks_spawned: u64,
    /// Total tasks completed
    pub tasks_completed: u64,
}

/// Initializes the runtime with default configuration.
///
/// This is a convenience function for simple use cases.
/// For more control, use `Runtime::new()` directly.
///
/// # Example
///
/// ```rust,no_run
/// use jet_rt::init_runtime;
///
/// let runtime = init_runtime().expect("Failed to initialize runtime");
/// ```
pub fn init_runtime() -> Result<Runtime> {
    Runtime::new(RuntimeConfig::default())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex;

    static RUNTIME_TEST_LOCK: Mutex<()> = Mutex::new(());

    #[test]
    fn test_runtime_creation() {
        let _guard = RUNTIME_TEST_LOCK.lock().unwrap();
        init::mark_uninitialized();
        let runtime = Runtime::new(RuntimeConfig::default());
        assert!(runtime.is_ok());
        drop(runtime);
        init::mark_uninitialized();
    }

    #[test]
    fn test_runtime_stats() {
        let _guard = RUNTIME_TEST_LOCK.lock().unwrap();
        init::mark_uninitialized();
        let runtime = Runtime::new(RuntimeConfig::default()).unwrap();
        let stats = runtime.stats();
        assert_eq!(
            stats.scheduler_workers,
            RuntimeConfig::default().scheduler.num_threads
        );
        drop(runtime);
        init::mark_uninitialized();
    }
}
