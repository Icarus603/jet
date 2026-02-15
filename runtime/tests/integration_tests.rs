//! Integration tests for the Jet runtime

use jet_rt::{Runtime, RuntimeConfig};
use std::sync::Mutex;

static RUNTIME_TEST_LOCK: Mutex<()> = Mutex::new(());

#[test]
fn test_runtime_initialization() {
    let _guard = RUNTIME_TEST_LOCK.lock().unwrap();
    let config = RuntimeConfig::default();
    let runtime = Runtime::new(config);
    assert!(runtime.is_ok());
    drop(runtime);
}

#[test]
fn test_runtime_threads() {
    let _guard = RUNTIME_TEST_LOCK.lock().unwrap();
    let config = RuntimeConfig::default();
    let runtime = Runtime::new(config).expect("Failed to create runtime");
    // Verify runtime can spawn worker threads
    let stats = runtime.stats();
    assert!(stats.scheduler_workers > 0);
    drop(runtime);
}
