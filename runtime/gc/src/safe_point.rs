//! Safe Points for Garbage Collection
//!
//! Safe points are locations in the code where the GC can safely pause
//! mutator threads. At safe points, the stack is in a well-defined state
//! and GC pointers are identifiable via stack maps.
//!
//! # Safe Point Locations
//!
//! Safe points are inserted at:
//! - Function entry (for stack overflow check)
//! - Allocation sites (may trigger GC)
//! - Call sites (may trigger GC via callee)
//! - Loop back edges (to ensure progress)
//! - Returns (for precise root scanning)
//!
//! # Implementation
//!
//! Safe points use a cooperative approach:
//! 1. GC sets a global "safe point requested" flag
//! 2. Mutators check this flag at safe points
//! 3. If set, mutators park themselves
//! 4. GC proceeds once all mutators are parked
//!
//! This avoids the need for signal-based preemption.

use std::sync::atomic::{AtomicU32, AtomicU64, AtomicUsize, Ordering};
use std::sync::{Condvar, Mutex};

/// Safe point state values
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SafePointState {
    /// Normal execution - no safe point requested
    Running = 0,
    /// Safe point requested - threads should park
    Requested = 1,
    /// GC in progress - threads are blocked
    InProgress = 2,
}

/// Global safe point state
///
/// This is checked by mutator threads at safe points
pub static SAFE_POINT_STATE: AtomicU32 = AtomicU32::new(SafePointState::Running as u32);

/// Global safe point counter
///
/// Incremented at each safe point for statistical purposes
pub static SAFE_POINT_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Number of threads parked at safe points
pub static PARKED_THREADS: AtomicUsize = AtomicUsize::new(0);

/// Total number of mutator threads
pub static TOTAL_THREADS: AtomicUsize = AtomicUsize::new(0);

/// Request a safe point
///
/// This is called by the GC thread to request all mutators to park
pub fn request_safe_point() {
    SAFE_POINT_STATE.store(SafePointState::Requested as u32, Ordering::SeqCst);
}

/// Begin GC at safe point
///
/// Called after all threads have parked
pub fn begin_gc_at_safe_point() {
    SAFE_POINT_STATE.store(SafePointState::InProgress as u32, Ordering::SeqCst);
}

/// Release safe point
///
/// Called by the GC thread to allow mutators to resume
pub fn release_safe_point() {
    PARKED_THREADS.store(0, Ordering::SeqCst);
    SAFE_POINT_STATE.store(SafePointState::Running as u32, Ordering::SeqCst);
}

/// Check if a safe point is requested
#[inline(always)]
pub fn is_safe_point_requested() -> bool {
    SAFE_POINT_STATE.load(Ordering::Acquire) != SafePointState::Running as u32
}

/// Check if GC is in progress
#[inline(always)]
pub fn is_gc_in_progress() -> bool {
    SAFE_POINT_STATE.load(Ordering::Acquire) == SafePointState::InProgress as u32
}

/// Thread state at safe points
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ThreadSafePointState {
    /// Thread is running normally
    Running = 0,
    /// Thread is at a safe point
    AtSafePoint = 1,
    /// Thread is parked for GC
    Parked = 2,
}

/// Per-thread safe point state
///
/// This is stored in thread-local storage
pub struct ThreadSafePoint {
    /// Current state
    pub state: ThreadSafePointState,
    /// Stack pointer at safe point
    pub stack_pointer: *mut u8,
    /// Frame pointer at safe point
    pub frame_pointer: *mut u8,
    /// Instruction pointer at safe point
    pub instruction_pointer: *mut u8,
    /// Number of safe points hit
    pub safe_point_count: u64,
    /// Thread ID
    pub thread_id: usize,
}

impl ThreadSafePoint {
    /// Create a new per-thread safe point state
    pub fn new(thread_id: usize) -> Self {
        ThreadSafePoint {
            state: ThreadSafePointState::Running,
            stack_pointer: std::ptr::null_mut(),
            frame_pointer: std::ptr::null_mut(),
            instruction_pointer: std::ptr::null_mut(),
            safe_point_count: 0,
            thread_id,
        }
    }

    /// Enter a safe point
    ///
    /// This is called at every safe point location
    #[inline(always)]
    pub fn enter(&mut self) {
        self.safe_point_count += 1;
        SAFE_POINT_COUNTER.fetch_add(1, Ordering::Relaxed);

        // Check if GC is requested
        if is_safe_point_requested() {
            self.park();
        }
    }

    /// Enter a safe point with stack state
    ///
    /// This variant captures the current stack state for GC root scanning
    #[inline(always)]
    pub fn enter_with_state(
        &mut self,
        stack_pointer: *mut u8,
        frame_pointer: *mut u8,
        instruction_pointer: *mut u8,
    ) {
        self.stack_pointer = stack_pointer;
        self.frame_pointer = frame_pointer;
        self.instruction_pointer = instruction_pointer;
        self.state = ThreadSafePointState::AtSafePoint;

        self.enter();

        self.state = ThreadSafePointState::Running;
    }

    /// Park the thread for GC
    fn park(&mut self) {
        self.state = ThreadSafePointState::Parked;
        PARKED_THREADS.fetch_add(1, Ordering::SeqCst);

        // Wait for GC to complete
        while is_gc_in_progress() || is_safe_point_requested() {
            // Spin or yield - in production, this might use futex/parking
            std::thread::yield_now();
        }

        PARKED_THREADS.fetch_sub(1, Ordering::SeqCst);
        self.state = ThreadSafePointState::Running;
    }

    /// Check if thread is at a safe point
    pub fn is_at_safe_point(&self) -> bool {
        self.state == ThreadSafePointState::AtSafePoint
            || self.state == ThreadSafePointState::Parked
    }
}

/// Safe point coordinator
///
/// Manages the global safe point process
pub struct SafePointCoordinator {
    /// Mutex for coordination
    mutex: Mutex<()>,
    /// Condition variable for waiting
    condvar: Condvar,
}

impl SafePointCoordinator {
    /// Create a new coordinator
    pub fn new() -> Self {
        SafePointCoordinator {
            mutex: Mutex::new(()),
            condvar: Condvar::new(),
        }
    }

    /// Request a global safe point and wait for all threads
    ///
    /// This is called by the GC thread to stop the world
    pub fn stop_the_world(&self, timeout_ms: u64) -> bool {
        let _guard = self.mutex.lock().unwrap();

        // Request safe point
        request_safe_point();

        // Wait for all threads to park
        let start = std::time::Instant::now();
        let timeout = std::time::Duration::from_millis(timeout_ms);

        while start.elapsed() < timeout {
            let parked = PARKED_THREADS.load(Ordering::Acquire);
            let total = TOTAL_THREADS.load(Ordering::Acquire);

            if parked >= total {
                begin_gc_at_safe_point();
                return true;
            }

            // Short sleep to avoid busy waiting
            std::thread::sleep(std::time::Duration::from_micros(100));
        }

        // Timeout - release safe point and return failure
        release_safe_point();
        false
    }

    /// Resume all threads after GC
    pub fn resume_the_world(&self) {
        let _guard = self.mutex.lock().unwrap();
        release_safe_point();
        self.condvar.notify_all();
    }

    /// Register a new mutator thread
    pub fn register_thread(&self) {
        TOTAL_THREADS.fetch_add(1, Ordering::SeqCst);
    }

    /// Unregister a mutator thread
    pub fn unregister_thread(&self) {
        TOTAL_THREADS.fetch_sub(1, Ordering::SeqCst);
    }

    /// Get the number of parked threads
    pub fn parked_threads(&self) -> usize {
        PARKED_THREADS.load(Ordering::Acquire)
    }

    /// Get the total number of threads
    pub fn total_threads(&self) -> usize {
        TOTAL_THREADS.load(Ordering::Acquire)
    }
}

impl Default for SafePointCoordinator {
    fn default() -> Self {
        Self::new()
    }
}

/// Safe point placement analysis
///
/// Determines where safe points should be inserted in generated code
pub struct SafePointPlacement;

impl SafePointPlacement {
    /// Check if a safe point is needed at function entry
    pub fn needs_safe_point_at_entry(_function_may_allocate: bool) -> bool {
        // Always need safe point at entry for stack overflow check
        // Also needed if function may allocate (and thus trigger GC)
        true
    }

    /// Check if a safe point is needed at call site
    pub fn needs_safe_point_at_call(callee_may_allocate: bool) -> bool {
        // Need safe point if callee may allocate
        callee_may_allocate
    }

    /// Check if a safe point is needed at loop back edge
    pub fn needs_safe_point_at_loop_back_edge(_loop_may_be_infinite: bool) -> bool {
        // Always need safe point at loop back edge to ensure GC progress
        // This prevents infinite loops from blocking GC indefinitely
        true
    }

    /// Check if a safe point is needed at return
    pub fn needs_safe_point_at_return(function_has_gc_roots: bool) -> bool {
        // Need safe point at return for precise root scanning
        function_has_gc_roots
    }
}

/// Statistics for safe points
#[derive(Debug, Clone, Default)]
pub struct SafePointStats {
    /// Total number of safe points hit
    pub total_safe_points: u64,
    /// Number of times GC was triggered
    pub gc_triggers: u64,
    /// Average time spent at safe points (microseconds)
    pub avg_safe_point_time_us: f64,
    /// Maximum time spent at a safe point (microseconds)
    pub max_safe_point_time_us: u64,
}

impl SafePointStats {
    /// Get global safe point statistics
    pub fn global() -> Self {
        SafePointStats {
            total_safe_points: SAFE_POINT_COUNTER.load(Ordering::Relaxed),
            gc_triggers: 0, // Would need additional tracking
            avg_safe_point_time_us: 0.0,
            max_safe_point_time_us: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_safe_point_states() {
        assert_eq!(SafePointState::Running as u32, 0);
        assert_eq!(SafePointState::Requested as u32, 1);
        assert_eq!(SafePointState::InProgress as u32, 2);
    }

    #[test]
    fn test_request_and_release() {
        // Initial state
        assert!(!is_safe_point_requested());
        assert!(!is_gc_in_progress());

        // Request safe point
        request_safe_point();
        assert!(is_safe_point_requested());
        assert!(!is_gc_in_progress());

        // Begin GC
        begin_gc_at_safe_point();
        assert!(is_safe_point_requested());
        assert!(is_gc_in_progress());

        // Release
        release_safe_point();
        assert!(!is_safe_point_requested());
        assert!(!is_gc_in_progress());
    }

    #[test]
    fn test_thread_safe_point() {
        let mut tsp = ThreadSafePoint::new(1);
        assert_eq!(tsp.state, ThreadSafePointState::Running);
        assert!(!tsp.is_at_safe_point());

        tsp.enter();
        assert_eq!(tsp.safe_point_count, 1);
    }

    #[test]
    fn test_safe_point_placement() {
        assert!(SafePointPlacement::needs_safe_point_at_entry(true));
        assert!(SafePointPlacement::needs_safe_point_at_entry(false));

        assert!(SafePointPlacement::needs_safe_point_at_call(true));
        assert!(!SafePointPlacement::needs_safe_point_at_call(false));

        assert!(SafePointPlacement::needs_safe_point_at_loop_back_edge(true));
        assert!(SafePointPlacement::needs_safe_point_at_loop_back_edge(
            false
        ));

        assert!(SafePointPlacement::needs_safe_point_at_return(true));
        assert!(!SafePointPlacement::needs_safe_point_at_return(false));
    }

    #[test]
    fn test_safe_point_stats() {
        let stats = SafePointStats::global();
        // Counter is a global value that may have been incremented by other tests
        // Just verify we can read it without panicking
        let _count = stats.total_safe_points;
    }
}
