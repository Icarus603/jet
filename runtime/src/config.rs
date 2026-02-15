//! Runtime configuration.
//!
//! This module provides configuration options for the Jet runtime,
//! including GC, scheduler, and I/O settings.

/// Runtime configuration.
#[derive(Debug, Clone)]
pub struct RuntimeConfig {
    /// Garbage collector configuration.
    pub gc: GCConfig,
    /// Scheduler configuration.
    pub scheduler: SchedulerConfig,
    /// Initial heap size in bytes.
    pub initial_heap_size: usize,
    /// Maximum heap size in bytes (0 = unlimited).
    pub max_heap_size: usize,
    /// Default task stack size in bytes.
    pub stack_size: usize,
    /// Enable signal handlers.
    pub enable_signal_handlers: bool,
    /// Enable panic recovery.
    pub enable_panic_recovery: bool,
}

impl RuntimeConfig {
    /// Creates a new runtime config with the specified number of worker threads.
    ///
    /// # Arguments
    ///
    /// * `num_threads` - Number of scheduler worker threads.
    ///
    /// # Example
    ///
    /// ```
    /// use jet_rt::RuntimeConfig;
    ///
    /// let config = RuntimeConfig::with_threads(4);
    /// assert_eq!(config.scheduler.num_threads, 4);
    /// ```
    pub fn with_threads(num_threads: usize) -> Self {
        Self {
            scheduler: SchedulerConfig {
                num_threads,
                ..Default::default()
            },
            ..Default::default()
        }
    }

    /// Sets the initial heap size.
    ///
    /// # Arguments
    ///
    /// * `size` - Heap size in bytes.
    ///
    /// # Example
    ///
    /// ```
    /// use jet_rt::RuntimeConfig;
    ///
    /// let config = RuntimeConfig::default()
    ///     .with_heap_size(256 * 1024 * 1024); // 256 MB
    /// ```
    pub fn with_heap_size(mut self, size: usize) -> Self {
        self.initial_heap_size = size;
        self.gc.nursery_threshold = size / 8;
        self.gc.mature_threshold = size / 2;
        self
    }

    /// Sets the maximum heap size.
    ///
    /// # Arguments
    ///
    /// * `size` - Maximum heap size in bytes (0 = unlimited).
    pub fn with_max_heap_size(mut self, size: usize) -> Self {
        self.max_heap_size = size;
        self
    }

    /// Sets the default stack size for tasks.
    ///
    /// # Arguments
    ///
    /// * `size` - Stack size in bytes.
    pub fn with_stack_size(mut self, size: usize) -> Self {
        self.stack_size = size;
        self.scheduler.stack_size = size;
        self
    }

    /// Enables or disables panic recovery.
    ///
    /// # Arguments
    ///
    /// * `enabled` - Whether to enable panic recovery.
    pub fn with_panic_recovery(mut self, enabled: bool) -> Self {
        self.enable_panic_recovery = enabled;
        self
    }
}

impl Default for RuntimeConfig {
    fn default() -> Self {
        Self {
            gc: GCConfig::default(),
            scheduler: SchedulerConfig::default(),
            initial_heap_size: 256 * 1024 * 1024, // 256 MB
            max_heap_size: 0,                     // Unlimited
            stack_size: 64 * 1024,                // 64 KB
            enable_signal_handlers: true,
            enable_panic_recovery: true,
        }
    }
}

/// Garbage collector configuration.
#[derive(Debug, Clone, Copy)]
pub struct GCConfig {
    /// Nursery collection threshold in bytes.
    pub nursery_threshold: usize,
    /// Mature space collection threshold in bytes.
    pub mature_threshold: usize,
    /// Large object threshold in bytes.
    pub large_object_threshold: usize,
    /// Enable evacuation during full collections.
    pub enable_evacuation: bool,
    /// Evacuation fragmentation threshold (0.0 - 1.0).
    pub evacuation_threshold: f32,
    /// Enable parallel marking.
    pub parallel_marking: bool,
    /// Number of GC threads (0 = auto).
    pub gc_threads: usize,
    /// Enable incremental collection.
    pub incremental: bool,
    /// Collection target pause time in milliseconds.
    pub target_pause_ms: u32,
}

impl GCConfig {
    /// Creates a low-latency GC configuration.
    ///
    /// This configuration prioritizes short pause times over throughput.
    ///
    /// # Example
    ///
    /// ```
    /// use jet_rt::GCConfig;
    ///
    /// let config = GCConfig::low_latency();
    /// assert!(config.incremental);
    /// assert!(config.target_pause_ms <= 10);
    /// ```
    pub fn low_latency() -> Self {
        Self {
            nursery_threshold: 16 * 1024 * 1024, // 16 MB
            mature_threshold: 128 * 1024 * 1024, // 128 MB
            large_object_threshold: 64 * 1024,   // 64 KB
            enable_evacuation: true,
            evacuation_threshold: 0.6,
            parallel_marking: true,
            gc_threads: 2,
            incremental: true,
            target_pause_ms: 5,
        }
    }

    /// Creates a high-throughput GC configuration.
    ///
    /// This configuration prioritizes throughput over pause times.
    pub fn high_throughput() -> Self {
        Self {
            nursery_threshold: 64 * 1024 * 1024, // 64 MB
            mature_threshold: 512 * 1024 * 1024, // 512 MB
            large_object_threshold: 256 * 1024,  // 256 KB
            enable_evacuation: true,
            evacuation_threshold: 0.8,
            parallel_marking: true,
            gc_threads: 0, // Auto
            incremental: false,
            target_pause_ms: 100,
        }
    }
}

impl Default for GCConfig {
    fn default() -> Self {
        Self {
            nursery_threshold: 32 * 1024 * 1024, // 32 MB
            mature_threshold: 256 * 1024 * 1024, // 256 MB
            large_object_threshold: 16 * 1024,   // 16 KB
            enable_evacuation: true,
            evacuation_threshold: 0.7,
            parallel_marking: true,
            gc_threads: 0, // Auto-detect
            incremental: false,
            target_pause_ms: 50,
        }
    }
}

impl From<GCConfig> for jet_rt_gc::ImmixConfig {
    fn from(config: GCConfig) -> Self {
        jet_rt_gc::ImmixConfig {
            nursery_threshold: config.nursery_threshold,
            mature_threshold: config.mature_threshold,
            large_object_threshold: config.large_object_threshold,
            enable_evacuation: config.enable_evacuation,
            evacuation_threshold: config.evacuation_threshold,
        }
    }
}

/// Scheduler configuration.
#[derive(Debug, Clone, Copy)]
pub struct SchedulerConfig {
    /// Number of worker threads (0 = auto-detect).
    pub num_threads: usize,
    /// Default task stack size in bytes.
    pub stack_size: usize,
    /// Maximum task stack size in bytes.
    pub max_stack_size: usize,
    /// Worker steal batch size.
    pub steal_batch_size: usize,
    /// Enable work stealing.
    pub enable_work_stealing: bool,
    /// Park timeout in microseconds.
    pub park_timeout_us: u64,
    /// Global queue capacity.
    pub global_queue_capacity: usize,
}

impl SchedulerConfig {
    /// Creates a single-threaded scheduler configuration.
    ///
    /// Useful for debugging or environments with only one CPU.
    pub fn single_threaded() -> Self {
        Self {
            num_threads: 1,
            enable_work_stealing: false,
            ..Default::default()
        }
    }

    /// Auto-detects the optimal number of worker threads.
    fn auto_threads() -> usize {
        std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(1)
    }
}

impl Default for SchedulerConfig {
    fn default() -> Self {
        Self {
            num_threads: Self::auto_threads(),
            stack_size: 64 * 1024,           // 64 KB
            max_stack_size: 8 * 1024 * 1024, // 8 MB
            steal_batch_size: 4,
            enable_work_stealing: true,
            park_timeout_us: 100,
            global_queue_capacity: 1024,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = RuntimeConfig::default();
        assert_eq!(config.initial_heap_size, 256 * 1024 * 1024);
        assert_eq!(config.stack_size, 64 * 1024);
        assert!(config.enable_signal_handlers);
    }

    #[test]
    fn test_with_threads() {
        let config = RuntimeConfig::with_threads(4);
        assert_eq!(config.scheduler.num_threads, 4);
    }

    #[test]
    fn test_with_heap_size() {
        let config = RuntimeConfig::default().with_heap_size(512 * 1024 * 1024);
        assert_eq!(config.initial_heap_size, 512 * 1024 * 1024);
    }

    #[test]
    fn test_gc_low_latency() {
        let config = GCConfig::low_latency();
        assert!(config.incremental);
        assert_eq!(config.target_pause_ms, 5);
    }

    #[test]
    fn test_gc_high_throughput() {
        let config = GCConfig::high_throughput();
        assert!(!config.incremental);
        assert_eq!(config.target_pause_ms, 100);
    }

    #[test]
    fn test_scheduler_single_threaded() {
        let config = SchedulerConfig::single_threaded();
        assert_eq!(config.num_threads, 1);
        assert!(!config.enable_work_stealing);
    }

    #[test]
    fn test_gc_config_conversion() {
        let gc_config = GCConfig::default();
        let immix_config: jet_rt_gc::ImmixConfig = gc_config.into();
        assert_eq!(immix_config.nursery_threshold, gc_config.nursery_threshold);
        assert_eq!(immix_config.enable_evacuation, gc_config.enable_evacuation);
    }
}
