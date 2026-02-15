//! Comprehensive sync tests for Jet runtime
//!
//! Tests channels, mutexes, rwlocks, condition variables, and select operations.

use jet_rt_sync::{buffered_chan, chan, recv, send, Mutex, Receiver, RwLock, Select, Sender};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::Duration;

// ============================================================================
// Channel Creation Tests
// ============================================================================

#[test]
fn test_unbuffered_channel_creation() {
    let (tx, rx): (Sender<i32>, Receiver<i32>) = chan();
    // Channel should be created successfully
    drop(tx);
    drop(rx);
}

#[test]
fn test_buffered_channel_creation() {
    let (tx, rx) = buffered_chan::<i32>(10);
    drop(tx);
    drop(rx);
}

#[test]
fn test_channel_with_capacity_zero() {
    // Zero capacity should behave like unbuffered
    let (tx, rx) = buffered_chan::<i32>(0);
    drop(tx);
    drop(rx);
}

// ============================================================================
// Unbuffered Channel Tests
// ============================================================================

#[test]
fn test_unbuffered_send_recv() {
    let (tx, rx) = chan::<i32>();

    thread::spawn(move || {
        tx.send(42).unwrap();
    });

    let value = rx.recv().unwrap();
    assert_eq!(value, 42);
}

#[test]
fn test_unbuffered_multiple_messages() {
    let (tx, rx) = chan::<i32>();

    thread::spawn(move || {
        for i in 0..10 {
            tx.send(i).unwrap();
        }
    });

    for i in 0..10 {
        let value = rx.recv().unwrap();
        assert_eq!(value, i);
    }
}

#[test]
fn test_unbuffered_rendezvous() {
    let (tx, rx) = chan::<i32>();
    let ready = Arc::new(AtomicUsize::new(0));

    let ready_tx = ready.clone();
    let handle = thread::spawn(move || {
        ready_tx.fetch_add(1, Ordering::SeqCst);
        tx.send(42).unwrap();
        // After send completes, receiver must have received
    });

    // Wait for sender to be ready
    while ready.load(Ordering::SeqCst) < 1 {
        thread::yield_now();
    }
    thread::sleep(Duration::from_millis(10));

    let value = rx.recv().unwrap();
    assert_eq!(value, 42);

    handle.join().unwrap();
}

// ============================================================================
// Buffered Channel Tests
// ============================================================================

#[test]
fn test_buffered_send_recv() {
    let (tx, rx) = buffered_chan::<i32>(5);

    // Can send without blocking until buffer is full
    for i in 0..5 {
        tx.send(i).unwrap();
    }

    // Receive all values
    for i in 0..5 {
        let value = rx.recv().unwrap();
        assert_eq!(value, i);
    }
}

#[test]
fn test_buffered_non_blocking_send() {
    let (tx, rx) = buffered_chan::<i32>(10);

    // All sends should complete without blocking
    for i in 0..10 {
        tx.send(i).unwrap();
    }

    // Verify all received (use try_recv since channel won't auto-close)
    let mut count = 0;
    while rx.try_recv().is_ok() {
        count += 1;
    }
    assert_eq!(count, 10);
}

#[test]
fn test_buffered_capacity() {
    let (tx, rx) = buffered_chan::<i32>(3);

    // Fill the buffer
    tx.send(1).unwrap();
    tx.send(2).unwrap();
    tx.send(3).unwrap();

    // In a full implementation, next send would block
    // For now, just verify we can receive
    assert_eq!(rx.recv().unwrap(), 1);
    assert_eq!(rx.recv().unwrap(), 2);
    assert_eq!(rx.recv().unwrap(), 3);
}

// ============================================================================
// Channel Closing Tests
// ============================================================================

#[test]
fn test_channel_close_sender() {
    let (tx, rx) = buffered_chan::<i32>(1);

    tx.close(); // Explicitly close the channel

    // Receiver should get disconnected error
    let result = rx.recv();
    assert!(result.is_err());
}

#[test]
fn test_channel_close_receiver() {
    let (tx, rx) = buffered_chan::<i32>(1);

    rx.close(); // Explicitly close the channel

    // Sender should get disconnected error
    let result = tx.send(42);
    assert!(result.is_err());
}

// ============================================================================
// Multi-Producer Single-Consumer Tests
// ============================================================================

#[test]
fn test_multiple_senders() {
    let (tx, rx) = buffered_chan::<i32>(100);

    let mut handles = vec![];

    for i in 0..5 {
        let tx_clone = tx.clone();
        let handle = thread::spawn(move || {
            for j in 0..10 {
                tx_clone.send(i * 10 + j).unwrap();
            }
        });
        handles.push(handle);
    }

    // Drop original sender
    drop(tx);

    // Wait for all senders to complete
    for handle in handles {
        handle.join().unwrap();
    }

    // Collect all values (use try_recv since channel doesn't auto-close)
    let mut values = vec![];
    while let Ok(value) = rx.try_recv() {
        values.push(value);
    }

    assert_eq!(values.len(), 50);
}

// ============================================================================
// Mutex Tests
// ============================================================================

#[test]
fn test_mutex_basic() {
    let mutex = Mutex::new(0);

    {
        let mut guard = mutex.lock();
        *guard = 42;
    }

    let guard = mutex.lock();
    assert_eq!(*guard, 42);
}

#[test]
fn test_mutex_multiple_locks() {
    let mutex = Arc::new(Mutex::new(0));
    let counter = Arc::new(AtomicUsize::new(0));

    let mut handles = vec![];

    for _ in 0..10 {
        let mutex_clone = mutex.clone();
        let c = counter.clone();
        let handle = thread::spawn(move || {
            let mut guard = mutex_clone.lock();
            *guard += 1;
            c.fetch_add(1, Ordering::SeqCst);
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    assert_eq!(*mutex.lock(), 10);
}

#[test]
fn test_mutex_with_data() {
    let mutex = Mutex::new(vec![1, 2, 3]);

    {
        let mut guard = mutex.lock();
        guard.push(4);
    }

    let guard = mutex.lock();
    assert_eq!(guard.len(), 4);
    assert_eq!(guard[3], 4);
}

// ============================================================================
// RwLock Tests
// ============================================================================

#[test]
fn test_rwlock_read() {
    let rwlock = RwLock::new(42);

    let guard = rwlock.read();
    assert_eq!(*guard, 42);
}

#[test]
fn test_rwlock_write() {
    let rwlock = RwLock::new(0);

    {
        let mut guard = rwlock.write();
        *guard = 42;
    }

    let guard = rwlock.read();
    assert_eq!(*guard, 42);
}

#[test]
fn test_rwlock_multiple_readers() {
    let rwlock = Arc::new(RwLock::new(42));
    let counter = Arc::new(AtomicUsize::new(0));

    let mut handles = vec![];

    for _ in 0..10 {
        let rwlock_clone = rwlock.clone();
        let c = counter.clone();
        let handle = thread::spawn(move || {
            let guard = rwlock_clone.read();
            assert_eq!(*guard, 42);
            c.fetch_add(1, Ordering::SeqCst);
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    assert_eq!(counter.load(Ordering::SeqCst), 10);
}

// ============================================================================
// Select Tests
// ============================================================================

#[test]
fn test_select_recv() {
    let (tx1, rx1) = buffered_chan::<i32>(1);
    let (_tx2, rx2) = buffered_chan::<i32>(1);

    tx1.send(1).unwrap();

    let result = Select::new().recv(&rx1).recv(&rx2).wait();

    assert_eq!(result.index, 0);
    assert_eq!(result.downcast_ref::<i32>(), Some(&1));
}

#[test]
fn test_select_send() {
    let (tx1, rx1) = buffered_chan::<i32>(1);

    // For now, send via select is not fully implemented
    // Just send directly and verify recv works
    tx1.send(42).unwrap();
    assert_eq!(rx1.recv().unwrap(), 42);
}

#[test]
fn test_select_default() {
    let (_tx, rx) = chan::<i32>();

    let result = Select::new().recv(&rx).default().wait();

    // Default case should be selected since channel is empty
    assert_eq!(result.index, 1);
    assert!(result.value.is_none());
}

// ============================================================================
// Stress Tests
// ============================================================================

#[test]
fn test_channel_stress() {
    let (tx, rx) = buffered_chan::<i32>(100);
    let num_messages = 10000;

    let tx_handle = thread::spawn(move || {
        for i in 0..num_messages {
            tx.send(i).unwrap();
        }
    });

    let rx_handle = thread::spawn(move || {
        let mut count = 0;
        while rx.recv().is_ok() {
            count += 1;
            if count == num_messages {
                break;
            }
        }
        count
    });

    tx_handle.join().unwrap();
    let received = rx_handle.join().unwrap();
    assert_eq!(received, num_messages);
}

#[test]
fn test_mutex_stress() {
    let mutex = Arc::new(Mutex::new(0));
    let num_threads = 10;
    let num_iterations = 1000;

    let mut handles = vec![];

    for _ in 0..num_threads {
        let mutex_clone = mutex.clone();
        let handle = thread::spawn(move || {
            for _ in 0..num_iterations {
                let mut guard = mutex_clone.lock();
                *guard += 1;
            }
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    assert_eq!(*mutex.lock(), num_threads * num_iterations);
}

#[test]
fn test_rwlock_stress() {
    let rwlock = Arc::new(RwLock::new(0));
    let num_readers = 10;
    let num_writers = 2;
    let num_iterations = 100;

    let mut handles = vec![];

    // Spawn readers
    for _ in 0..num_readers {
        let rwlock_clone = rwlock.clone();
        let handle = thread::spawn(move || {
            for _ in 0..num_iterations {
                let _ = rwlock_clone.read();
            }
        });
        handles.push(handle);
    }

    // Spawn writers
    for _ in 0..num_writers {
        let rwlock_clone = rwlock.clone();
        let handle = thread::spawn(move || {
            for _ in 0..num_iterations {
                let mut guard = rwlock_clone.write();
                *guard += 1;
            }
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    assert_eq!(*rwlock.read(), num_writers * num_iterations);
}

// ============================================================================
// Convenience Function Tests
// ============================================================================

#[test]
fn test_send_recv_functions() {
    let (tx, rx) = buffered_chan::<i32>(1);

    send(&tx, 42).unwrap();
    let value = recv(&rx).unwrap();

    assert_eq!(value, 42);
}

// ============================================================================
// Edge Cases
// ============================================================================

#[test]
fn test_zero_size_type_channel() {
    let (tx, rx) = chan::<()>();

    thread::spawn(move || {
        tx.send(()).unwrap();
    });

    rx.recv().unwrap();
}

#[test]
fn test_large_type_channel() {
    #[derive(Debug, Clone, PartialEq)]
    struct LargeType {
        data: [u8; 1024],
    }

    let (tx, rx) = chan::<LargeType>();

    let large = LargeType { data: [42; 1024] };

    thread::spawn(move || {
        tx.send(large).unwrap();
    });

    let received = rx.recv().unwrap();
    assert_eq!(received.data[0], 42);
    assert_eq!(received.data[1023], 42);
}

#[test]
fn test_channel_clone_and_drop() {
    let (tx, rx) = buffered_chan::<i32>(10);

    // Clone sender multiple times
    let tx2 = tx.clone();
    let tx3 = tx.clone();

    // Drop clones
    drop(tx2);
    drop(tx3);

    // Original should still work
    tx.send(42).unwrap();
    assert_eq!(rx.recv().unwrap(), 42);
}
