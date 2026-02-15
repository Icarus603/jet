//! Mutex implementation for inter-task synchronization.
//!
//! This module provides a mutual exclusion primitive that can be used to
//! protect shared data between tasks. The mutex is designed to work with
//! the Jet scheduler's M:N threading model.
//!
//! # Examples
//!
//! ```
//! use jet_rt_sync::Mutex;
//! use std::sync::Arc;
//!
//! let mutex = Arc::new(Mutex::new(0));
//! let mutex2 = mutex.clone();
//!
//! std::thread::spawn(move || {
//!     *mutex2.lock() = 42;
//! }).join().unwrap();
//!
//! assert_eq!(*mutex.lock(), 42);
//! ```

use std::cell::UnsafeCell;
use std::ops::{Deref, DerefMut};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

/// A mutual exclusion primitive for protecting shared data.
///
/// The mutex ensures that only one task can access the protected data
/// at a time. When a task locks the mutex, it gains exclusive access
/// to the data until the lock is dropped.
///
/// # Type Parameters
///
/// * `T` - The type of data protected by the mutex.
///
/// # Examples
///
/// ```
/// use jet_rt_sync::Mutex;
///
/// let mutex = Mutex::new(vec![1, 2, 3]);
///
/// {
///     let mut guard = mutex.lock();
///     guard.push(4);
/// }
///
/// assert_eq!(mutex.lock().len(), 4);
/// ```
pub struct Mutex<T> {
    /// The inner data protected by the mutex.
    data: UnsafeCell<T>,
    /// Lock state - true if locked.
    locked: AtomicBool,
}

/// A guard that provides exclusive access to the data protected by a mutex.
///
/// When the guard is dropped, the mutex is automatically unlocked.
pub struct MutexGuard<'a, T> {
    mutex: &'a Mutex<T>,
}

// Mutex is Send if T is Send
unsafe impl<T: Send> Send for Mutex<T> {}

// Mutex is Sync if T is Send - this is the standard Rust mutex invariant
unsafe impl<T: Send> Sync for Mutex<T> {}

impl<T> Mutex<T> {
    /// Creates a new mutex protecting the given data.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sync::Mutex;
    ///
    /// let mutex = Mutex::new(42);
    /// ```
    pub fn new(data: T) -> Self {
        Self {
            data: UnsafeCell::new(data),
            locked: AtomicBool::new(false),
        }
    }

    /// Acquires the mutex, blocking until it is available.
    ///
    /// Returns a guard that provides exclusive access to the protected data.
    /// The mutex is automatically unlocked when the guard is dropped.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sync::Mutex;
    ///
    /// let mutex = Mutex::new(0);
    /// *mutex.lock() = 10;
    /// assert_eq!(*mutex.lock(), 10);
    /// ```
    pub fn lock(&self) -> MutexGuard<'_, T> {
        // Spin until we acquire the lock
        // In a full implementation with the Jet scheduler, this would
        // yield the task instead of spinning
        while self
            .locked
            .compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_err()
        {
            // Spin-wait with exponential backoff
            for _ in 0..100 {
                std::hint::spin_loop();
            }
            std::thread::yield_now();
        }

        MutexGuard { mutex: self }
    }

    /// Attempts to acquire the mutex without blocking.
    ///
    /// Returns `Some(MutexGuard)` if the lock was acquired, `None` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sync::Mutex;
    ///
    /// let mutex = Mutex::new(42);
    ///
    /// if let Some(guard) = mutex.try_lock() {
    ///     assert_eq!(*guard, 42);
    /// } else {
    ///     // Lock was not available
    /// };
    /// ```
    pub fn try_lock(&self) -> Option<MutexGuard<'_, T>> {
        if self
            .locked
            .compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_ok()
        {
            Some(MutexGuard { mutex: self })
        } else {
            None
        }
    }

    /// Returns a mutable reference to the underlying data.
    ///
    /// Since this call borrows the mutex mutably, no locking is required.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sync::Mutex;
    ///
    /// let mut mutex = Mutex::new(42);
    /// *mutex.get_mut() = 10;
    /// assert_eq!(*mutex.lock(), 10);
    /// ```
    pub fn get_mut(&mut self) -> &mut T {
        // Safe because we have exclusive access via &mut self
        unsafe { &mut *self.data.get() }
    }

    /// Consumes the mutex, returning the protected data.
    ///
    /// # Examples
    ///
    /// ```
    /// use jet_rt_sync::Mutex;
    ///
    /// let mutex = Mutex::new(42);
    /// assert_eq!(mutex.into_inner(), 42);
    /// ```
    pub fn into_inner(self) -> T {
        self.data.into_inner()
    }

    /// Returns true if the mutex is currently locked.
    ///
    /// This is a best-effort check and may return stale information.
    pub fn is_locked(&self) -> bool {
        self.locked.load(Ordering::Relaxed)
    }

    /// Unlocks the mutex.
    ///
    /// # Safety
    ///
    /// This should only be called by the MutexGuard when it is dropped.
    unsafe fn unlock(&self) {
        self.locked.store(false, Ordering::Release);
    }
}

impl<T: Default> Default for Mutex<T> {
    fn default() -> Self {
        Self::new(T::default())
    }
}

impl<T> From<T> for Mutex<T> {
    fn from(data: T) -> Self {
        Self::new(data)
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Mutex<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut d = f.debug_struct("Mutex");
        match self.try_lock() {
            Some(guard) => d.field("data", &*guard),
            None => d.field("data", &"<locked>"),
        };
        d.finish()
    }
}

impl<T> Deref for MutexGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        // Safe because we hold the lock
        unsafe { &*self.mutex.data.get() }
    }
}

impl<T> DerefMut for MutexGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        // Safe because we hold the lock
        unsafe { &mut *self.mutex.data.get() }
    }
}

impl<T> Drop for MutexGuard<'_, T> {
    fn drop(&mut self) {
        unsafe {
            self.mutex.unlock();
        }
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for MutexGuard<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MutexGuard").field("data", &**self).finish()
    }
}

/// A shared mutex that can be cloned and shared between tasks.
///
/// This is a convenience wrapper around `Arc<Mutex<T>>`.
///
/// # Examples
///
/// ```
/// use jet_rt_sync::SharedMutex;
/// use std::thread;
///
/// let mutex = SharedMutex::new(0);
/// let mutex2 = mutex.clone();
///
/// thread::spawn(move || {
///     *mutex2.lock() = 42;
/// }).join().unwrap();
///
/// assert_eq!(*mutex.lock(), 42);
/// ```
pub struct SharedMutex<T> {
    inner: Arc<Mutex<T>>,
}

impl<T> SharedMutex<T> {
    /// Creates a new shared mutex.
    pub fn new(data: T) -> Self {
        Self {
            inner: Arc::new(Mutex::new(data)),
        }
    }

    /// Locks the mutex.
    pub fn lock(&self) -> MutexGuard<'_, T> {
        self.inner.lock()
    }

    /// Tries to lock the mutex without blocking.
    pub fn try_lock(&self) -> Option<MutexGuard<'_, T>> {
        self.inner.try_lock()
    }

    /// Returns true if the mutex is locked.
    pub fn is_locked(&self) -> bool {
        self.inner.is_locked()
    }
}

impl<T> Clone for SharedMutex<T> {
    fn clone(&self) -> Self {
        Self {
            inner: Arc::clone(&self.inner),
        }
    }
}

impl<T> Deref for SharedMutex<T> {
    type Target = Mutex<T>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::thread;

    #[test]
    fn test_mutex_basic() {
        let mutex = Mutex::new(42);
        let guard = mutex.lock();
        assert_eq!(*guard, 42);
    }

    #[test]
    fn test_mutex_mutate() {
        let mutex = Mutex::new(0);
        *mutex.lock() = 10;
        assert_eq!(*mutex.lock(), 10);
    }

    #[test]
    fn test_mutex_try_lock() {
        let mutex = Mutex::new(42);

        let guard = mutex.try_lock();
        assert!(guard.is_some());
        assert_eq!(*guard.unwrap(), 42);

        // Second try_lock should succeed since we dropped the first guard
        let guard2 = mutex.try_lock();
        assert!(guard2.is_some());
    }

    #[test]
    fn test_mutex_concurrent() {
        let mutex = Arc::new(Mutex::new(0));
        let counter = Arc::new(AtomicUsize::new(0));

        let mut handles = vec![];

        for _ in 0..10 {
            let m = mutex.clone();
            let c = counter.clone();
            handles.push(thread::spawn(move || {
                let mut guard = m.lock();
                *guard += 1;
                c.fetch_add(1, Ordering::SeqCst);
            }));
        }

        for handle in handles {
            handle.join().unwrap();
        }

        assert_eq!(*mutex.lock(), 10);
        assert_eq!(counter.load(Ordering::SeqCst), 10);
    }

    #[test]
    fn test_mutex_get_mut() {
        let mut mutex = Mutex::new(42);
        *mutex.get_mut() = 10;
        assert_eq!(*mutex.lock(), 10);
    }

    #[test]
    fn test_mutex_into_inner() {
        let mutex = Mutex::new(42);
        assert_eq!(mutex.into_inner(), 42);
    }

    #[test]
    fn test_mutex_default() {
        let mutex: Mutex<i32> = Default::default();
        assert_eq!(*mutex.lock(), 0);
    }

    #[test]
    fn test_mutex_from() {
        let mutex: Mutex<i32> = 42.into();
        assert_eq!(*mutex.lock(), 42);
    }

    #[test]
    fn test_shared_mutex() {
        let mutex = SharedMutex::new(0);
        let mutex2 = mutex.clone();

        thread::spawn(move || {
            *mutex2.lock() = 42;
        })
        .join()
        .unwrap();

        assert_eq!(*mutex.lock(), 42);
    }

    #[test]
    fn test_mutex_debug() {
        let mutex = Mutex::new(42);
        let debug_str = format!("{:?}", mutex);
        assert!(debug_str.contains("Mutex"));
        assert!(debug_str.contains("42"));
    }

    #[test]
    fn test_mutex_guard_debug() {
        let mutex = Mutex::new(42);
        let guard = mutex.lock();
        let debug_str = format!("{:?}", guard);
        assert!(debug_str.contains("MutexGuard"));
        assert!(debug_str.contains("42"));
    }
}
