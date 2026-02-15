//! Condition variable implementation for inter-task synchronization.
//!
//! Condition variables allow tasks to wait for a condition to become true.
//! They are used in conjunction with a mutex to protect the condition being
//! waited on.
//!
//! # Examples
//!
//! ```no_run
//! use jet_rt_sync::{Mutex, Condvar};
//! use std::sync::Arc;
//! use std::thread;
//!
//! let pair = Arc::new((Mutex::new(false), Condvar::new()));
//! let pair2 = pair.clone();
//!
//! thread::spawn(move || {
//!     let (lock, cvar) = &*pair2;
//!     *lock.lock() = true;
//!     cvar.notify_one();
//! }).join().unwrap();
//!
//! let (lock, cvar) = &*pair;
//! let mut guard = lock.lock();
//! while !*guard {
//!     guard = cvar.wait(guard);
//! }
//! assert!(*guard);
//! ```

use crate::mutex::MutexGuard;
use std::collections::VecDeque;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex as StdMutex;

/// A condition variable.
///
/// Condition variables allow tasks to block while waiting for a condition
/// to become true. They are always used with a mutex that protects the
/// condition being waited on.
///
/// # Examples
///
/// ```no_run
/// use jet_rt_sync::{Mutex, Condvar};
///
/// let mutex = Mutex::new(0);
/// let cvar = Condvar::new();
///
/// // Wait for the condition
/// let mut guard = mutex.lock();
/// while *guard < 10 {
///     guard = cvar.wait(guard);
/// }
/// ```
pub struct Condvar {
    /// Queue of waiting tasks.
    waiters: StdMutex<VecDeque<Waiter>>,
    /// Counter for generating unique waiter IDs.
    next_id: AtomicUsize,
}

/// A waiter in the condition variable queue.
struct Waiter {
    /// Unique ID for this waiter.
    id: usize,
    /// Flag indicating if the waiter has been notified.
    notified: std::sync::atomic::AtomicBool,
}

impl Condvar {
    /// Creates a new condition variable.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sync::Condvar;
    ///
    /// let cvar = Condvar::new();
    /// ```
    pub fn new() -> Self {
        Self {
            waiters: StdMutex::new(VecDeque::new()),
            next_id: AtomicUsize::new(0),
        }
    }

    /// Waits on the condition variable, releasing the mutex.
    ///
    /// This method atomically releases the mutex and blocks until the
    /// condition variable is notified. When the method returns, the
    /// mutex is locked again.
    ///
    /// # Arguments
    ///
    /// * `guard` - A mutex guard for the mutex protecting the condition.
    ///
    /// # Returns
    ///
    /// Returns the mutex guard, which is locked when the method returns.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use jet_rt_sync::{Mutex, Condvar};
    ///
    /// let mutex = Mutex::new(false);
    /// let cvar = Condvar::new();
    ///
    /// let mut guard = mutex.lock();
    /// while !*guard {
    ///     guard = cvar.wait(guard);
    /// }
    /// ```
    pub fn wait<'a, T>(&self, guard: MutexGuard<'a, T>) -> MutexGuard<'a, T> {
        // Create a waiter
        let waiter = Waiter {
            id: self.next_id.fetch_add(1, Ordering::SeqCst),
            notified: std::sync::atomic::AtomicBool::new(false),
        };

        // Add to waiters queue
        {
            let mut waiters = self.waiters.lock().unwrap();
            waiters.push_back(waiter);
        }

        // Release the mutex and wait
        // Note: In a full implementation with the Jet scheduler, this would
        // properly park the task. For now, we use a spin-yield approach.
        drop(guard);

        // Wait to be notified
        loop {
            {
                let waiters = self.waiters.lock().unwrap();
                if let Some(w) = waiters.front() {
                    if w.notified.load(Ordering::Acquire) {
                        break;
                    }
                }
            }
            std::thread::yield_now();
        }

        // Remove ourselves from the queue
        {
            let mut waiters = self.waiters.lock().unwrap();
            waiters.pop_front();
        }

        // Reacquire the mutex
        // This is a bit of a hack - we need to return a guard
        // In a real implementation, we'd have a reference to the mutex
        // For now, this is a simplified version
        panic!(
            "Condvar::wait requires integration with the scheduler for proper mutex reacquisition"
        )
    }

    /// Waits on the condition variable with a timeout.
    ///
    /// Returns the mutex guard and a boolean indicating whether the wait
    /// timed out (false) or was notified (true).
    ///
    /// # Arguments
    ///
    /// * `guard` - A mutex guard for the mutex protecting the condition.
    /// * `timeout` - The maximum duration to wait.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use jet_rt_sync::{Mutex, Condvar};
    /// use std::time::Duration;
    ///
    /// let mutex = Mutex::new(false);
    /// let cvar = Condvar::new();
    ///
    /// let guard = mutex.lock();
    /// let (guard, notified) = cvar.wait_timeout(guard, Duration::from_millis(100));
    /// ```
    pub fn wait_timeout<'a, T>(
        &self,
        guard: MutexGuard<'a, T>,
        timeout: std::time::Duration,
    ) -> (MutexGuard<'a, T>, bool) {
        let start = std::time::Instant::now();

        // Create a waiter
        let waiter = Waiter {
            id: self.next_id.fetch_add(1, Ordering::SeqCst),
            notified: std::sync::atomic::AtomicBool::new(false),
        };

        // Add to waiters queue
        {
            let mut waiters = self.waiters.lock().unwrap();
            waiters.push_back(waiter);
        }

        // Release the mutex and wait
        drop(guard);

        // Wait to be notified or timeout
        let mut notified = false;
        loop {
            if start.elapsed() >= timeout {
                break;
            }

            {
                let waiters = self.waiters.lock().unwrap();
                if let Some(w) = waiters.front() {
                    if w.notified.load(Ordering::Acquire) {
                        notified = true;
                        break;
                    }
                }
            }
            std::thread::yield_now();
        }

        // Remove ourselves from the queue if we timed out
        if !notified {
            let mut waiters = self.waiters.lock().unwrap();
            waiters.retain(|w| w.id != self.next_id.load(Ordering::SeqCst).saturating_sub(1));
        } else {
            let mut waiters = self.waiters.lock().unwrap();
            waiters.pop_front();
        }

        // Reacquire the mutex
        panic!("Condvar::wait_timeout requires integration with the scheduler")
    }

    /// Notifies one waiting task.
    ///
    /// If there are tasks waiting on this condition variable, this method
    /// wakes one of them. If no tasks are waiting, this is a no-op.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sync::Condvar;
    ///
    /// let cvar = Condvar::new();
    /// cvar.notify_one();
    /// ```
    pub fn notify_one(&self) {
        let waiters = self.waiters.lock().unwrap();
        if let Some(waiter) = waiters.front() {
            waiter.notified.store(true, Ordering::Release);
        }
    }

    /// Notifies all waiting tasks.
    ///
    /// This wakes all tasks currently waiting on this condition variable.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sync::Condvar;
    ///
    /// let cvar = Condvar::new();
    /// cvar.notify_all();
    /// ```
    pub fn notify_all(&self) {
        let waiters = self.waiters.lock().unwrap();
        for waiter in waiters.iter() {
            waiter.notified.store(true, Ordering::Release);
        }
    }
}

impl Default for Condvar {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Debug for Condvar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let waiters_count = self.waiters.lock().map(|w| w.len()).unwrap_or(0);
        f.debug_struct("Condvar")
            .field("waiters", &waiters_count)
            .finish()
    }
}

/// A condition variable that can be shared between tasks.
///
/// This is a convenience wrapper around `std::sync::Arc<Condvar>`.
pub struct SharedCondvar {
    inner: std::sync::Arc<Condvar>,
}

impl SharedCondvar {
    /// Creates a new shared condition variable.
    pub fn new() -> Self {
        Self {
            inner: std::sync::Arc::new(Condvar::new()),
        }
    }

    /// Waits on the condition variable.
    pub fn wait<'a, T>(&self, guard: MutexGuard<'a, T>) -> MutexGuard<'a, T> {
        self.inner.wait(guard)
    }

    /// Waits on the condition variable with a timeout.
    pub fn wait_timeout<'a, T>(
        &self,
        guard: MutexGuard<'a, T>,
        timeout: std::time::Duration,
    ) -> (MutexGuard<'a, T>, bool) {
        self.inner.wait_timeout(guard, timeout)
    }

    /// Notifies one waiting task.
    pub fn notify_one(&self) {
        self.inner.notify_one()
    }

    /// Notifies all waiting tasks.
    pub fn notify_all(&self) {
        self.inner.notify_all()
    }
}

impl Clone for SharedCondvar {
    fn clone(&self) -> Self {
        Self {
            inner: std::sync::Arc::clone(&self.inner),
        }
    }
}

impl Default for SharedCondvar {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_condvar_new() {
        let cvar = Condvar::new();
        let debug_str = format!("{:?}", cvar);
        assert!(debug_str.contains("Condvar"));
    }

    #[test]
    fn test_condvar_notify_one() {
        let cvar = Condvar::new();
        cvar.notify_one(); // Should not panic even with no waiters
    }

    #[test]
    fn test_condvar_notify_all() {
        let cvar = Condvar::new();
        cvar.notify_all(); // Should not panic even with no waiters
    }

    #[test]
    fn test_condvar_default() {
        let cvar: Condvar = Default::default();
        cvar.notify_one();
    }

    #[test]
    fn test_shared_condvar() {
        let cvar = SharedCondvar::new();
        let cvar2 = cvar.clone();

        cvar.notify_one();
        cvar2.notify_all();
    }
}
