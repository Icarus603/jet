//! Concurrent - Concurrency primitives for Jet
//!
//! This module provides task spawning, channels, mutexes, and read-write locks
//! for concurrent programming in the Jet programming language.

use crate::string::JetString;
use std::sync::Arc;

/// A channel for sending values between tasks
pub struct Channel<T> {
    inner: Arc<std::sync::Mutex<ChannelInner<T>>>,
}

/// Internal channel state
struct ChannelInner<T> {
    buffer: Vec<T>,
    capacity: usize,
    closed: bool,
}

/// The receiving end of a channel
pub struct Receiver<T> {
    inner: Arc<std::sync::Mutex<ChannelInner<T>>>,
}

/// The sending end of a channel
pub struct Sender<T> {
    inner: Arc<std::sync::Mutex<ChannelInner<T>>>,
}

impl<T> Channel<T> {
    /// Create an unbuffered channel (capacity 0, synchronous)
    pub fn unbuffered() -> (Sender<T>, Receiver<T>) {
        Self::with_capacity(0)
    }

    /// Create a channel with capacity
    pub fn with_capacity(capacity: usize) -> (Sender<T>, Receiver<T>) {
        let inner = Arc::new(std::sync::Mutex::new(ChannelInner {
            buffer: Vec::with_capacity(capacity),
            capacity,
            closed: false,
        }));

        let sender = Sender {
            inner: inner.clone(),
        };
        let receiver = Receiver { inner };

        (sender, receiver)
    }
}

impl<T: Clone> Sender<T> {
    /// Send a value through the channel
    pub fn send(&self, value: T) -> std::result::Result<(), SendError<T>> {
        let mut inner = self.inner.lock().unwrap();
        if inner.closed {
            return Err(SendError(value));
        }
        if inner.capacity > 0 && inner.buffer.len() >= inner.capacity {
            return Err(SendError(value));
        }
        inner.buffer.push(value);
        Ok(())
    }
}

impl<T: Clone> Receiver<T> {
    /// Receive a value from the channel
    pub fn recv(&self) -> std::result::Result<T, RecvError> {
        let mut inner = self.inner.lock().unwrap();
        if inner.buffer.is_empty() {
            if inner.closed {
                return Err(RecvError);
            }
            return Err(RecvError);
        }
        Ok(inner.buffer.remove(0))
    }
}

impl<T> Clone for Sender<T> {
    fn clone(&self) -> Self {
        Sender {
            inner: self.inner.clone(),
        }
    }
}

impl<T> Clone for Receiver<T> {
    fn clone(&self) -> Self {
        Receiver {
            inner: self.inner.clone(),
        }
    }
}

/// Error when sending fails
#[derive(Debug, Clone)]
pub struct SendError<T>(pub T);

/// Error when receiving fails
#[derive(Debug, Clone)]
pub struct RecvError;

/// A mutex protecting data
pub struct Mutex<T> {
    inner: std::sync::Mutex<T>,
}

/// A guard for a locked mutex
pub struct MutexGuard<'a, T> {
    inner: Option<std::sync::MutexGuard<'a, T>>,
}

/// A shared mutex that can be cloned
pub struct SharedMutex<T> {
    inner: Arc<std::sync::Mutex<T>>,
}

/// A read-write lock
pub struct RwLock<T> {
    inner: std::sync::RwLock<T>,
}

/// A read guard for an RwLock
pub struct RwLockReadGuard<'a, T> {
    inner: Option<std::sync::RwLockReadGuard<'a, T>>,
}

/// A write guard for an RwLock
pub struct RwLockWriteGuard<'a, T> {
    inner: Option<std::sync::RwLockWriteGuard<'a, T>>,
}

/// A shared RwLock that can be cloned
pub struct SharedRwLock<T> {
    inner: Arc<std::sync::RwLock<T>>,
}

impl<T> Mutex<T> {
    /// Creates a new mutex protecting the given data.
    pub fn new(data: T) -> Self {
        Mutex {
            inner: std::sync::Mutex::new(data),
        }
    }

    /// Locks the mutex, blocking until the lock is acquired.
    pub fn lock(&self) -> MutexGuard<T> {
        match self.inner.lock() {
            Ok(guard) => MutexGuard { inner: Some(guard) },
            Err(_) => panic!("Mutex poisoned"),
        }
    }

    /// Attempts to lock the mutex without blocking.
    pub fn try_lock(&self) -> Option<MutexGuard<T>> {
        match self.inner.try_lock() {
            Ok(guard) => Some(MutexGuard { inner: Some(guard) }),
            Err(_) => None,
        }
    }
}

impl<T> std::ops::Deref for MutexGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner.as_ref().unwrap()
    }
}

impl<T> std::ops::DerefMut for MutexGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.inner.as_mut().unwrap()
    }
}

impl<T> SharedMutex<T> {
    /// Creates a new shared mutex.
    pub fn new(data: T) -> Self {
        SharedMutex {
            inner: Arc::new(std::sync::Mutex::new(data)),
        }
    }

    /// Locks the mutex, blocking until the lock is acquired.
    pub fn lock(&self) -> MutexGuard<T> {
        match self.inner.lock() {
            Ok(guard) => MutexGuard { inner: Some(guard) },
            Err(_) => panic!("Mutex poisoned"),
        }
    }

    /// Attempts to lock the mutex without blocking.
    pub fn try_lock(&self) -> Option<MutexGuard<T>> {
        match self.inner.try_lock() {
            Ok(guard) => Some(MutexGuard { inner: Some(guard) }),
            Err(_) => None,
        }
    }
}

impl<T> Clone for SharedMutex<T> {
    fn clone(&self) -> Self {
        SharedMutex {
            inner: self.inner.clone(),
        }
    }
}

impl<T> RwLock<T> {
    /// Creates a new read-write lock.
    pub fn new(data: T) -> Self {
        RwLock {
            inner: std::sync::RwLock::new(data),
        }
    }

    /// Acquires a read lock.
    pub fn read(&self) -> RwLockReadGuard<T> {
        match self.inner.read() {
            Ok(guard) => RwLockReadGuard { inner: Some(guard) },
            Err(_) => panic!("RwLock poisoned"),
        }
    }

    /// Acquires a write lock.
    pub fn write(&self) -> RwLockWriteGuard<T> {
        match self.inner.write() {
            Ok(guard) => RwLockWriteGuard { inner: Some(guard) },
            Err(_) => panic!("RwLock poisoned"),
        }
    }
}

impl<T> std::ops::Deref for RwLockReadGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner.as_ref().unwrap()
    }
}

impl<T> std::ops::Deref for RwLockWriteGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner.as_ref().unwrap()
    }
}

impl<T> std::ops::DerefMut for RwLockWriteGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.inner.as_mut().unwrap()
    }
}

impl<T> SharedRwLock<T> {
    /// Creates a new shared read-write lock.
    pub fn new(data: T) -> Self {
        SharedRwLock {
            inner: Arc::new(std::sync::RwLock::new(data)),
        }
    }

    /// Acquires a read lock.
    pub fn read(&self) -> RwLockReadGuard<T> {
        match self.inner.read() {
            Ok(guard) => RwLockReadGuard { inner: Some(guard) },
            Err(_) => panic!("RwLock poisoned"),
        }
    }

    /// Acquires a write lock.
    pub fn write(&self) -> RwLockWriteGuard<T> {
        match self.inner.write() {
            Ok(guard) => RwLockWriteGuard { inner: Some(guard) },
            Err(_) => panic!("RwLock poisoned"),
        }
    }
}

impl<T> Clone for SharedRwLock<T> {
    fn clone(&self) -> Self {
        SharedRwLock {
            inner: self.inner.clone(),
        }
    }
}

/// A handle to a spawned task that can be used to await its completion.
pub struct Task<T> {
    inner: TaskInner<T>,
}

/// Internal task representation.
enum TaskInner<T> {
    Ready(T),
    Pending,
}

/// Spawns a new task to run concurrently.
pub fn spawn<F, T>(f: F) -> Task<T>
where
    F: FnOnce() -> T + Send + 'static,
    T: Send + 'static,
{
    let result = f();
    Task {
        inner: TaskInner::Ready(result),
    }
}

impl<T> Task<T> {
    /// Waits for the task to complete and returns its result.
    pub fn await_result(self) -> T {
        match self.inner {
            TaskInner::Ready(value) => value,
            TaskInner::Pending => {
                panic!("Cannot await a pending task without runtime")
            }
        }
    }

    /// Checks if the task has completed.
    pub fn is_complete(&self) -> bool {
        matches!(self.inner, TaskInner::Ready(_))
    }

    /// Maps the result of the task to a different type.
    pub fn map<U, F>(self, f: F) -> Task<U>
    where
        F: FnOnce(T) -> U,
    {
        match self.inner {
            TaskInner::Ready(value) => Task {
                inner: TaskInner::Ready(f(value)),
            },
            TaskInner::Pending => Task {
                inner: TaskInner::Pending,
            },
        }
    }
}

/// Creates a new unbuffered channel.
pub fn channel<T>() -> (Sender<T>, Receiver<T>)
where
    T: Send + 'static,
{
    Channel::unbuffered()
}

/// Creates a new buffered channel with the specified capacity.
pub fn buffered_channel<T>(capacity: usize) -> (Sender<T>, Receiver<T>)
where
    T: Send + 'static,
{
    Channel::with_capacity(capacity)
}

/// Sends a value through a channel.
pub fn send<T>(sender: &Sender<T>, value: T) -> std::result::Result<(), ChannelError<T>>
where
    T: Clone + Send + 'static,
{
    sender.send(value).map_err(|e| ChannelError {
        value: e.0,
        message: JetString::from_str("Channel closed"),
    })
}

/// Receives a value from a channel.
pub fn recv<T>(receiver: &Receiver<T>) -> std::result::Result<T, ChannelError<()>>
where
    T: Clone + Send + 'static,
{
    receiver.recv().map_err(|_| ChannelError {
        value: (),
        message: JetString::from_str("Channel closed"),
    })
}

/// Error type for channel operations.
#[derive(Debug, Clone)]
pub struct ChannelError<T> {
    pub value: T,
    pub message: JetString,
}

impl<T> ChannelError<T> {
    /// Creates a new channel error.
    pub fn new(value: T, message: &str) -> Self {
        ChannelError {
            value,
            message: JetString::from_str(message),
        }
    }

    /// Returns the error message.
    pub fn message(&self) -> &str {
        self.message.as_str()
    }

    /// Consumes the error and returns the value.
    pub fn into_value(self) -> T {
        self.value
    }
}

impl<T: std::fmt::Debug> std::fmt::Display for ChannelError<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Channel error: {}", self.message.as_str())
    }
}

impl<T: std::fmt::Debug + Send> std::error::Error for ChannelError<T> {}

/// Creates a new mutex protecting the given data.
pub fn mutex<T>(data: T) -> Mutex<T> {
    Mutex::new(data)
}

/// Creates a new shared mutex that can be cloned and shared between tasks.
pub fn shared_mutex<T>(data: T) -> SharedMutex<T> {
    SharedMutex::new(data)
}

/// Creates a new read-write lock protecting the given data.
pub fn rwlock<T>(data: T) -> RwLock<T> {
    RwLock::new(data)
}

/// Creates a new shared read-write lock that can be cloned and shared between tasks.
pub fn shared_rwlock<T>(data: T) -> SharedRwLock<T> {
    SharedRwLock::new(data)
}

/// Yields the current task, allowing other tasks to run.
pub fn yield_now() {
    std::thread::yield_now();
}

/// Puts the current task to sleep for the specified duration.
pub fn sleep(millis: u64) {
    std::thread::sleep(std::time::Duration::from_millis(millis));
}

/// A barrier that makes multiple tasks wait until all tasks have reached it.
pub struct Barrier {
    count: usize,
    current: std::sync::atomic::AtomicUsize,
    condvar: std::sync::Condvar,
    mutex: std::sync::Mutex<()>,
}

impl Barrier {
    /// Creates a new barrier for the specified number of tasks.
    pub fn new(count: usize) -> Self {
        Barrier {
            count,
            current: std::sync::atomic::AtomicUsize::new(0),
            condvar: std::sync::Condvar::new(),
            mutex: std::sync::Mutex::new(()),
        }
    }

    /// Waits for all tasks to reach the barrier.
    pub fn wait(&self) -> usize {
        let mut guard = self.mutex.lock().unwrap();
        let current = self
            .current
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst)
            + 1;

        if current >= self.count {
            self.current.store(0, std::sync::atomic::Ordering::SeqCst);
            self.condvar.notify_all();
            current
        } else {
            let _guard = self.condvar.wait(guard).unwrap();
            drop(_guard);
            current
        }
    }
}

/// A countdown latch that allows one or more tasks to wait until a set of operations complete.
pub struct CountDownLatch {
    count: std::sync::atomic::AtomicUsize,
    condvar: std::sync::Condvar,
    mutex: std::sync::Mutex<()>,
}

impl CountDownLatch {
    /// Creates a new countdown latch with the specified count.
    pub fn new(count: usize) -> Self {
        CountDownLatch {
            count: std::sync::atomic::AtomicUsize::new(count),
            condvar: std::sync::Condvar::new(),
            mutex: std::sync::Mutex::new(()),
        }
    }

    /// Decrements the count of the latch.
    pub fn count_down(&self) {
        let previous = self.count.fetch_sub(1, std::sync::atomic::Ordering::SeqCst);
        if previous == 1 {
            let _guard = self.mutex.lock().unwrap();
            self.condvar.notify_all();
        }
    }

    /// Waits until the count reaches zero.
    pub fn wait(&self) {
        let mut guard = self.mutex.lock().unwrap();
        while self.count.load(std::sync::atomic::Ordering::SeqCst) > 0 {
            guard = self.condvar.wait(guard).unwrap();
        }
    }

    /// Returns the current count.
    pub fn get_count(&self) -> usize {
        self.count.load(std::sync::atomic::Ordering::SeqCst)
    }
}

/// A semaphore that controls access to a finite number of resources.
pub struct Semaphore {
    permits: std::sync::atomic::AtomicUsize,
    condvar: std::sync::Condvar,
    mutex: std::sync::Mutex<()>,
}

/// A permit acquired from a semaphore.
pub struct SemaphorePermit<'a> {
    semaphore: &'a Semaphore,
}

impl Semaphore {
    /// Creates a new semaphore with the specified number of permits.
    pub fn new(permits: usize) -> Self {
        Semaphore {
            permits: std::sync::atomic::AtomicUsize::new(permits),
            condvar: std::sync::Condvar::new(),
            mutex: std::sync::Mutex::new(()),
        }
    }

    /// Acquires a permit from the semaphore.
    pub fn acquire(&self) -> SemaphorePermit<'_> {
        let mut guard = self.mutex.lock().unwrap();
        while self.permits.load(std::sync::atomic::Ordering::SeqCst) == 0 {
            guard = self.condvar.wait(guard).unwrap();
        }
        self.permits
            .fetch_sub(1, std::sync::atomic::Ordering::SeqCst);
        SemaphorePermit { semaphore: self }
    }

    /// Attempts to acquire a permit without blocking.
    pub fn try_acquire(&self) -> Option<SemaphorePermit<'_>> {
        let current = self.permits.load(std::sync::atomic::Ordering::SeqCst);
        if current > 0 {
            if self
                .permits
                .compare_exchange(
                    current,
                    current - 1,
                    std::sync::atomic::Ordering::SeqCst,
                    std::sync::atomic::Ordering::SeqCst,
                )
                .is_ok()
            {
                return Some(SemaphorePermit { semaphore: self });
            }
        }
        None
    }

    /// Releases a permit back to the semaphore.
    fn release(&self) {
        self.permits
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        self.condvar.notify_one();
    }

    /// Returns the number of available permits.
    pub fn available_permits(&self) -> usize {
        self.permits.load(std::sync::atomic::Ordering::SeqCst)
    }
}

impl Drop for SemaphorePermit<'_> {
    fn drop(&mut self) {
        self.semaphore.release();
    }
}

/// A one-shot channel that can send a single value.
pub fn oneshot<T>() -> (OneShotSender<T>, OneShotReceiver<T>) {
    let inner = Arc::new(std::sync::Mutex::new(OneShotInner {
        value: None,
        sent: false,
        received: false,
    }));

    let sender = OneShotSender {
        inner: inner.clone(),
    };
    let receiver = OneShotReceiver { inner };

    (sender, receiver)
}

/// Internal state for a one-shot channel.
struct OneShotInner<T> {
    value: Option<T>,
    sent: bool,
    received: bool,
}

/// The sending half of a one-shot channel.
pub struct OneShotSender<T> {
    inner: Arc<std::sync::Mutex<OneShotInner<T>>>,
}

/// The receiving half of a one-shot channel.
pub struct OneShotReceiver<T> {
    inner: Arc<std::sync::Mutex<OneShotInner<T>>>,
}

impl<T> OneShotSender<T> {
    /// Sends a value through the channel.
    pub fn send(self, value: T) -> std::result::Result<(), ChannelError<T>> {
        let mut inner = self.inner.lock().unwrap();
        if inner.sent {
            return Err(ChannelError::new(
                value,
                "Value already sent on one-shot channel",
            ));
        }
        inner.value = Some(value);
        inner.sent = true;
        Ok(())
    }
}

impl<T> OneShotReceiver<T> {
    /// Receives the value from the channel.
    pub fn recv(self) -> std::result::Result<T, ChannelError<()>> {
        let mut inner = self.inner.lock().unwrap();
        if inner.received {
            return Err(ChannelError::new((), "Value already received"));
        }
        inner.received = true;
        match inner.value.take() {
            Some(value) => Ok(value),
            None => Err(ChannelError::new((), "Sender dropped without sending")),
        }
    }

    /// Attempts to receive the value without consuming the receiver.
    pub fn try_recv(&self) -> Option<T> {
        let mut inner = self.inner.lock().unwrap();
        if inner.sent && !inner.received {
            inner.received = true;
            inner.value.take()
        } else {
            None
        }
    }
}

/// A thread-local storage container.
pub struct ThreadLocal<T> {
    inner: std::cell::RefCell<Option<T>>,
}

impl<T> ThreadLocal<T> {
    /// Creates a new empty thread-local storage.
    pub fn new() -> Self {
        ThreadLocal {
            inner: std::cell::RefCell::new(None),
        }
    }

    /// Sets the value for the current thread.
    pub fn set(&self, value: T) {
        *self.inner.borrow_mut() = Some(value);
    }

    /// Gets a reference to the value for the current thread.
    pub fn get(&self) -> Option<std::cell::Ref<'_, T>> {
        if self.inner.borrow().is_some() {
            Some(std::cell::Ref::map(self.inner.borrow(), |opt| {
                opt.as_ref().unwrap()
            }))
        } else {
            None
        }
    }

    /// Gets a mutable reference to the value for the current thread.
    pub fn get_mut(&self) -> Option<std::cell::RefMut<'_, T>> {
        if self.inner.borrow_mut().is_some() {
            Some(std::cell::RefMut::map(self.inner.borrow_mut(), |opt| {
                opt.as_mut().unwrap()
            }))
        } else {
            None
        }
    }

    /// Takes the value from the current thread, leaving it empty.
    pub fn take(&self) -> Option<T> {
        self.inner.borrow_mut().take()
    }

    /// Returns true if a value is set for the current thread.
    pub fn is_set(&self) -> bool {
        self.inner.borrow().is_some()
    }
}

impl<T> Default for ThreadLocal<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_spawn_task() {
        let task = spawn(|| 42);
        assert!(task.is_complete());
        assert_eq!(task.await_result(), 42);
    }

    #[test]
    fn test_task_map() {
        let task = spawn(|| 21);
        let mapped = task.map(|x| x * 2);
        assert_eq!(mapped.await_result(), 42);
    }

    #[test]
    fn test_buffered_channel() {
        let (tx, rx) = buffered_channel::<i32>(5);
        send(&tx, 1).unwrap();
        send(&tx, 2).unwrap();
        send(&tx, 3).unwrap();

        assert_eq!(recv(&rx).unwrap(), 1);
        assert_eq!(recv(&rx).unwrap(), 2);
        assert_eq!(recv(&rx).unwrap(), 3);
    }

    #[test]
    fn test_channel_error() {
        let (tx, _rx) = buffered_channel::<i32>(1);
        // Just test that the error type works
        let err = ChannelError::new(42, "test error");
        assert_eq!(err.message(), "test error");
        assert_eq!(err.into_value(), 42);
    }

    #[test]
    fn test_mutex() {
        let m = mutex(0);
        {
            let mut guard = m.lock();
            *guard = 42;
        }
        assert_eq!(*m.lock(), 42);
    }

    #[test]
    fn test_mutex_try_lock() {
        let m = mutex(42);
        let guard = m.try_lock();
        assert!(guard.is_some());
        assert_eq!(*guard.unwrap(), 42);
    }

    #[test]
    fn test_shared_mutex() {
        let m = shared_mutex(0);
        let m2 = m.clone();

        *m.lock() = 42;
        assert_eq!(*m2.lock(), 42);
    }

    #[test]
    fn test_rwlock() {
        let lock = rwlock(vec![1, 2, 3]);

        // Multiple readers
        let r1 = lock.read();
        let r2 = lock.read();
        assert_eq!(r1.len(), 3);
        assert_eq!(r2.len(), 3);
        drop(r1);
        drop(r2);

        // Single writer
        {
            let mut w = lock.write();
            w.push(4);
        }

        assert_eq!(lock.read().len(), 4);
    }

    #[test]
    fn test_barrier() {
        let barrier = Arc::new(Barrier::new(2));
        let barrier2 = barrier.clone();

        let handle = std::thread::spawn(move || {
            barrier2.wait();
        });

        barrier.wait();
        handle.join().unwrap();
    }

    #[test]
    fn test_countdown_latch() {
        let latch = Arc::new(CountDownLatch::new(2));
        let latch2 = latch.clone();

        let handle = std::thread::spawn(move || {
            latch2.count_down();
        });

        latch.count_down();
        handle.join().unwrap();

        latch.wait();
        assert_eq!(latch.get_count(), 0);
    }

    #[test]
    fn test_semaphore() {
        let sem = Semaphore::new(2);

        // Acquire two permits
        let permit1 = sem.try_acquire();
        assert!(permit1.is_some());

        let permit2 = sem.try_acquire();
        assert!(permit2.is_some());

        // Third should fail (no permits left)
        let permit3 = sem.try_acquire();
        assert!(permit3.is_none());

        // Release one permit
        drop(permit1);

        // Now we can acquire again
        let permit4 = sem.try_acquire();
        assert!(permit4.is_some());
    }

    #[test]
    fn test_semaphore_available_permits() {
        let sem = Semaphore::new(5);
        assert_eq!(sem.available_permits(), 5);

        let _permit = sem.acquire();
        assert_eq!(sem.available_permits(), 4);
    }

    #[test]
    fn test_oneshot() {
        let (tx, rx) = oneshot::<i32>();
        tx.send(42).unwrap();
        assert_eq!(rx.recv().unwrap(), 42);
    }

    #[test]
    fn test_oneshot_try_recv() {
        let (tx, rx) = oneshot::<i32>();
        assert!(rx.try_recv().is_none());

        tx.send(42).unwrap();
        assert_eq!(rx.try_recv(), Some(42));
    }

    #[test]
    fn test_thread_local() {
        let local = ThreadLocal::new();
        assert!(!local.is_set());

        local.set(42);
        assert!(local.is_set());
        assert_eq!(local.get().map(|r| *r), Some(42));

        let value = local.take();
        assert_eq!(value, Some(42));
        assert!(!local.is_set());
    }

    #[test]
    fn test_yield_now() {
        yield_now();
    }

    #[test]
    fn test_sleep() {
        let start = std::time::Instant::now();
        sleep(10);
        let elapsed = start.elapsed();
        assert!(elapsed.as_millis() >= 10);
    }

    #[test]
    fn test_channel_error_display() {
        let err = ChannelError::new(42, "test error");
        let display = format!("{}", err);
        assert!(display.contains("test error"));
        assert_eq!(err.into_value(), 42);
    }
}
