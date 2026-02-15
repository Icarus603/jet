//! Cache management for incremental compilation
//!
//! This module provides:
//! - CacheKey computation for source files
//! - Cache directory management
//! - Cache hit/miss tracking

use sha2::{Digest, Sha256};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use crate::error::{LinkerError, LinkerResult};
use crate::platform::TargetConfig;

/// Key used to identify cached artifacts
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CacheKey {
    /// Hash of source file content
    pub source_hash: String,
    /// Hash of compiler version
    pub compiler_version: String,
    /// Hash of compilation flags
    pub flags_hash: String,
    /// Hash of dependency artifacts
    pub deps_hash: String,
    /// Target triple
    pub target: String,
}

impl CacheKey {
    /// Create a new cache key from components
    pub fn new(
        source: &str,
        compiler_version: &str,
        flags: &CompileFlags,
        deps: &[DependencyHash],
        target: &TargetConfig,
    ) -> Self {
        let source_hash = hash_string(source);
        let compiler_version = hash_string(compiler_version);
        let flags_hash = hash_flags(flags);
        let deps_hash = hash_dependencies(deps);

        CacheKey {
            source_hash,
            compiler_version,
            flags_hash,
            deps_hash,
            target: target.triple.clone(),
        }
    }

    /// Compute the cache key for a file
    pub fn for_file(
        path: &Path,
        compiler_version: &str,
        flags: &CompileFlags,
        deps: &[DependencyHash],
        target: &TargetConfig,
    ) -> LinkerResult<Self> {
        let source = fs::read_to_string(path)
            .map_err(|e| LinkerError::Io(format!("Failed to read {}: {}", path.display(), e)))?;

        Ok(Self::new(&source, compiler_version, flags, deps, target))
    }

    /// Get the filename for this cache key
    pub fn filename(&self) -> String {
        // Combine all hashes into a single hash for the filename
        let combined = format!(
            "{}:{}:{}:{}:{}",
            self.source_hash, self.compiler_version, self.flags_hash, self.deps_hash, self.target
        );
        hash_string(&combined)
    }

    /// Get the object filename for this cache key
    pub fn object_filename(&self) -> String {
        format!("{}.o", self.filename())
    }

    /// Get the metadata filename for this cache key
    pub fn metadata_filename(&self) -> String {
        format!("{}.json", self.filename())
    }
}

/// Hash of a dependency for cache invalidation
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DependencyHash {
    /// Path to the dependency
    pub path: PathBuf,
    /// Hash of the dependency content
    pub hash: String,
}

impl DependencyHash {
    /// Compute the hash for a file dependency
    pub fn for_file(path: &Path) -> LinkerResult<Self> {
        let content = fs::read(path)
            .map_err(|e| LinkerError::Io(format!("Failed to read {}: {}", path.display(), e)))?;

        let hash = hash_bytes(&content);
        Ok(DependencyHash {
            path: path.to_path_buf(),
            hash,
        })
    }

    /// Compute the hash from a string
    pub fn from_string(path: PathBuf, content: &str) -> Self {
        DependencyHash {
            path,
            hash: hash_string(content),
        }
    }
}

/// Compilation flags that affect caching
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct CompileFlags {
    /// Optimization level (0-3, s, z)
    pub opt_level: String,
    /// Debug info level (0-2)
    pub debug_level: u8,
    /// Target features enabled
    pub target_features: Vec<String>,
    /// Additional flags
    pub extra_flags: Vec<String>,
}

impl CompileFlags {
    /// Create flags for debug build
    pub fn debug() -> Self {
        CompileFlags {
            opt_level: "0".to_string(),
            debug_level: 2,
            target_features: vec![],
            extra_flags: vec![],
        }
    }

    /// Create flags for release build
    pub fn release() -> Self {
        CompileFlags {
            opt_level: "3".to_string(),
            debug_level: 0,
            target_features: vec![],
            extra_flags: vec![],
        }
    }

    /// Add a target feature
    pub fn with_feature(mut self, feature: &str) -> Self {
        self.target_features.push(feature.to_string());
        self
    }
}

/// Cache statistics for tracking performance
#[derive(Debug, Clone, Default)]
pub struct CacheStats {
    /// Number of cache hits
    pub hits: usize,
    /// Number of cache misses
    pub misses: usize,
    /// Total bytes read from cache
    pub bytes_read: u64,
    /// Total bytes written to cache
    pub bytes_written: u64,
}

impl CacheStats {
    /// Get the hit rate as a percentage
    pub fn hit_rate(&self) -> f64 {
        let total = self.hits + self.misses;
        if total == 0 {
            0.0
        } else {
            (self.hits as f64 / total as f64) * 100.0
        }
    }

    /// Record a cache hit
    pub fn record_hit(&mut self, bytes: u64) {
        self.hits += 1;
        self.bytes_read += bytes;
    }

    /// Record a cache miss
    pub fn record_miss(&mut self) {
        self.misses += 1;
    }

    /// Record bytes written
    pub fn record_write(&mut self, bytes: u64) {
        self.bytes_written += bytes;
    }
}

/// Cache manager for storing and retrieving compilation artifacts
pub struct Cache {
    /// Root directory for the cache
    root: PathBuf,
    /// Current statistics
    stats: CacheStats,
    /// Maximum cache size in bytes (0 = unlimited)
    max_size: u64,
}

impl Cache {
    /// Create a new cache at the given path
    pub fn new(root: PathBuf) -> LinkerResult<Self> {
        // Ensure the cache directory exists
        fs::create_dir_all(&root).map_err(|e| {
            LinkerError::CacheError(format!("Failed to create cache directory: {}", e))
        })?;

        // Create subdirectories
        fs::create_dir_all(root.join("objects"))?;
        fs::create_dir_all(root.join("metadata"))?;

        Ok(Cache {
            root,
            stats: CacheStats::default(),
            max_size: 10 * 1024 * 1024 * 1024, // 10 GB default
        })
    }

    /// Create a cache in the default location
    pub fn default_cache() -> LinkerResult<Self> {
        let cache_dir = default_cache_dir();
        Self::new(cache_dir)
    }

    /// Set the maximum cache size
    pub fn with_max_size(mut self, max_size: u64) -> Self {
        self.max_size = max_size;
        self
    }

    /// Get the path to a cached object file
    pub fn get_object_path(&self, key: &CacheKey) -> PathBuf {
        self.root.join("objects").join(key.object_filename())
    }

    /// Get the path to cached metadata
    pub fn get_metadata_path(&self, key: &CacheKey) -> PathBuf {
        self.root.join("metadata").join(key.metadata_filename())
    }

    /// Check if an artifact is cached
    pub fn contains(&self, key: &CacheKey) -> bool {
        let obj_path = self.get_object_path(key);
        let meta_path = self.get_metadata_path(key);

        obj_path.exists() && meta_path.exists()
    }

    /// Get a cached object file
    pub fn get(&mut self, key: &CacheKey) -> LinkerResult<Option<CachedArtifact>> {
        if !self.contains(key) {
            self.stats.record_miss();
            return Ok(None);
        }

        let obj_path = self.get_object_path(key);
        let meta_path = self.get_metadata_path(key);

        // Read the object file
        let object_data = fs::read(&obj_path)
            .map_err(|e| LinkerError::CacheError(format!("Failed to read cached object: {}", e)))?;

        // Read the metadata
        let metadata_str = fs::read_to_string(&meta_path).map_err(|e| {
            LinkerError::CacheError(format!("Failed to read cached metadata: {}", e))
        })?;

        let metadata: ArtifactMetadata = serde_json::from_str(&metadata_str).map_err(|e| {
            LinkerError::CacheError(format!("Failed to parse cached metadata: {}", e))
        })?;

        self.stats
            .record_hit(object_data.len() as u64 + metadata_str.len() as u64);

        Ok(Some(CachedArtifact {
            object_data,
            metadata,
        }))
    }

    /// Store an artifact in the cache
    pub fn put(
        &mut self,
        key: &CacheKey,
        object_data: Vec<u8>,
        metadata: ArtifactMetadata,
    ) -> LinkerResult<()> {
        let obj_path = self.get_object_path(key);
        let meta_path = self.get_metadata_path(key);

        // Write the object file
        fs::write(&obj_path, &object_data).map_err(|e| {
            LinkerError::CacheError(format!("Failed to write cached object: {}", e))
        })?;

        // Write the metadata
        let metadata_str = serde_json::to_string_pretty(&metadata)
            .map_err(|e| LinkerError::CacheError(format!("Failed to serialize metadata: {}", e)))?;

        fs::write(&meta_path, &metadata_str).map_err(|e| {
            LinkerError::CacheError(format!("Failed to write cached metadata: {}", e))
        })?;

        self.stats
            .record_write(object_data.len() as u64 + metadata_str.len() as u64);

        // Clean up if we're over the size limit
        if self.max_size > 0 {
            self.enforce_size_limit()?;
        }

        Ok(())
    }

    /// Invalidate a cached entry
    pub fn invalidate(&self, key: &CacheKey) -> LinkerResult<()> {
        let obj_path = self.get_object_path(key);
        let meta_path = self.get_metadata_path(key);

        if obj_path.exists() {
            fs::remove_file(obj_path)?;
        }

        if meta_path.exists() {
            fs::remove_file(meta_path)?;
        }

        Ok(())
    }

    /// Clear all cached entries
    pub fn clear(&self) -> LinkerResult<()> {
        let objects_dir = self.root.join("objects");
        let metadata_dir = self.root.join("metadata");

        if objects_dir.exists() {
            fs::remove_dir_all(&objects_dir)?;
            fs::create_dir_all(&objects_dir)?;
        }

        if metadata_dir.exists() {
            fs::remove_dir_all(&metadata_dir)?;
            fs::create_dir_all(&metadata_dir)?;
        }

        Ok(())
    }

    /// Get current cache statistics
    pub fn stats(&self) -> &CacheStats {
        &self.stats
    }

    /// Get the total size of the cache in bytes
    pub fn size(&self) -> LinkerResult<u64> {
        let mut total = 0u64;

        for entry in walkdir::WalkDir::new(&self.root) {
            let entry = entry.map_err(|e| LinkerError::Io(e.to_string()))?;
            if entry.file_type().is_file() {
                total += entry.metadata().map(|m| m.len()).unwrap_or(0);
            }
        }

        Ok(total)
    }

    /// Enforce the maximum cache size by removing oldest entries
    fn enforce_size_limit(&self) -> LinkerResult<()> {
        let current_size = self.size()?;

        if current_size <= self.max_size {
            return Ok(());
        }

        // Collect all cache entries with their modification times
        let mut entries: Vec<(PathBuf, SystemTime)> = Vec::new();

        for entry in walkdir::WalkDir::new(self.root.join("objects")) {
            let entry = entry.map_err(|e| LinkerError::Io(e.to_string()))?;
            if entry.file_type().is_file() {
                let modified = entry
                    .metadata()
                    .ok()
                    .and_then(|m| m.modified().ok())
                    .unwrap_or(SystemTime::UNIX_EPOCH);
                entries.push((entry.path().to_path_buf(), modified));
            }
        }

        // Sort by modification time (oldest first)
        entries.sort_by(|a, b| a.1.cmp(&b.1));

        // Remove oldest entries until we're under the limit
        let target_size = (self.max_size as f64 * 0.8) as u64; // Target 80% of max
        let mut size = current_size;

        for (path, _) in entries {
            if size <= target_size {
                break;
            }

            if let Ok(metadata) = fs::metadata(&path) {
                size -= metadata.len();
            }

            // Also remove the corresponding metadata file
            let meta_path = self.root.join("metadata").join(
                path.file_stem()
                    .unwrap_or_default()
                    .to_string_lossy()
                    .to_string()
                    + ".json",
            );

            let _ = fs::remove_file(&path);
            let _ = fs::remove_file(meta_path);
        }

        Ok(())
    }
}

/// A cached compilation artifact
#[derive(Debug, Clone)]
pub struct CachedArtifact {
    /// The object file data
    pub object_data: Vec<u8>,
    /// Metadata about the artifact
    pub metadata: ArtifactMetadata,
}

/// Metadata stored with a cached artifact
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ArtifactMetadata {
    /// When the artifact was created
    pub created_at: SystemTime,
    /// Source file path
    pub source_path: PathBuf,
    /// Source file hash
    pub source_hash: String,
    /// Compiler version used
    pub compiler_version: String,
    /// Target triple
    pub target: String,
    /// Dependencies
    pub dependencies: Vec<DependencyInfo>,
    /// Size of the object file
    pub object_size: u64,
}

/// Information about a dependency
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct DependencyInfo {
    /// Path to the dependency
    pub path: PathBuf,
    /// Hash of the dependency
    pub hash: String,
}

/// Get the default cache directory
pub fn default_cache_dir() -> PathBuf {
    // Check for JET_CACHE_DIR environment variable
    if let Ok(dir) = std::env::var("JET_CACHE_DIR") {
        return PathBuf::from(dir);
    }

    // Use platform-specific cache directory
    let cache_base = if cfg!(target_os = "macos") {
        dirs::cache_dir().unwrap_or_else(|| {
            PathBuf::from(std::env::var("HOME").unwrap_or_default()).join("Library/Caches")
        })
    } else if cfg!(target_os = "windows") {
        dirs::cache_dir().unwrap_or_else(|| {
            PathBuf::from(std::env::var("LOCALAPPDATA").unwrap_or_default()).join("cache")
        })
    } else {
        // Linux and other Unix
        PathBuf::from(std::env::var("HOME").unwrap_or_default()).join(".cache")
    };

    cache_base.join("jet").join("build-cache")
}

/// Hash a string using SHA-256
fn hash_string(s: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(s.as_bytes());
    hex::encode(hasher.finalize())[..16].to_string() // Use first 16 chars for shorter keys
}

/// Hash bytes using SHA-256
fn hash_bytes(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    hex::encode(hasher.finalize())[..16].to_string()
}

/// Hash compilation flags
fn hash_flags(flags: &CompileFlags) -> String {
    let flags_str = format!(
        "{}:{}:{}:{}",
        flags.opt_level,
        flags.debug_level,
        flags.target_features.join(","),
        flags.extra_flags.join(",")
    );
    hash_string(&flags_str)
}

/// Hash dependencies
fn hash_dependencies(deps: &[DependencyHash]) -> String {
    let mut hasher = Sha256::new();
    for dep in deps {
        hasher.update(dep.path.to_string_lossy().as_bytes());
        hasher.update(dep.hash.as_bytes());
    }
    hex::encode(hasher.finalize())[..16].to_string()
}

// Simple dirs implementation to avoid extra dependencies
mod dirs {
    use std::path::PathBuf;

    pub fn cache_dir() -> Option<PathBuf> {
        if cfg!(target_os = "macos") {
            std::env::var("HOME")
                .ok()
                .map(|h| PathBuf::from(h).join("Library/Caches"))
        } else if cfg!(target_os = "windows") {
            std::env::var("LOCALAPPDATA").ok().map(PathBuf::from)
        } else {
            std::env::var("HOME")
                .ok()
                .map(|h| PathBuf::from(h).join(".cache"))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_cache_key_creation() {
        let flags = CompileFlags::debug();
        let target = TargetConfig::native();
        let key = CacheKey::new("source code", "1.0.0", &flags, &[], &target);

        assert!(!key.source_hash.is_empty());
        assert!(!key.compiler_version.is_empty());
        assert!(!key.flags_hash.is_empty());
        assert!(!key.deps_hash.is_empty());
    }

    #[test]
    fn test_cache_key_consistency() {
        let flags = CompileFlags::debug();
        let target = TargetConfig::native();

        let key1 = CacheKey::new("source", "1.0.0", &flags, &[], &target);
        let key2 = CacheKey::new("source", "1.0.0", &flags, &[], &target);

        assert_eq!(key1, key2);
    }

    #[test]
    fn test_cache_key_different_source() {
        let flags = CompileFlags::debug();
        let target = TargetConfig::native();

        let key1 = CacheKey::new("source1", "1.0.0", &flags, &[], &target);
        let key2 = CacheKey::new("source2", "1.0.0", &flags, &[], &target);

        assert_ne!(key1.source_hash, key2.source_hash);
    }

    #[test]
    fn test_compile_flags_debug() {
        let flags = CompileFlags::debug();
        assert_eq!(flags.opt_level, "0");
        assert_eq!(flags.debug_level, 2);
    }

    #[test]
    fn test_compile_flags_release() {
        let flags = CompileFlags::release();
        assert_eq!(flags.opt_level, "3");
        assert_eq!(flags.debug_level, 0);
    }

    #[test]
    fn test_cache_stats() {
        let mut stats = CacheStats::default();
        assert_eq!(stats.hit_rate(), 0.0);

        stats.record_hit(100);
        assert_eq!(stats.hits, 1);
        assert_eq!(stats.bytes_read, 100);

        stats.record_miss();
        assert_eq!(stats.misses, 1);

        assert!((stats.hit_rate() - 50.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_cache_operations() {
        let temp_dir = TempDir::new().unwrap();
        let mut cache = Cache::new(temp_dir.path().to_path_buf()).unwrap();

        let key = CacheKey::new(
            "test",
            "1.0",
            &CompileFlags::debug(),
            &[],
            &TargetConfig::native(),
        );

        // Initially not in cache
        assert!(!cache.contains(&key));
        assert!(cache.get(&key).unwrap().is_none());

        // Store artifact
        let metadata = ArtifactMetadata {
            created_at: SystemTime::now(),
            source_path: PathBuf::from("test.jet"),
            source_hash: "abc".to_string(),
            compiler_version: "1.0".to_string(),
            target: "native".to_string(),
            dependencies: vec![],
            object_size: 100,
        };

        cache
            .put(&key, vec![1, 2, 3, 4, 5], metadata.clone())
            .unwrap();

        // Now should be in cache
        assert!(cache.contains(&key));

        let artifact = cache.get(&key).unwrap().unwrap();
        assert_eq!(artifact.object_data, vec![1, 2, 3, 4, 5]);

        // Invalidate
        cache.invalidate(&key).unwrap();
        assert!(!cache.contains(&key));
    }

    #[test]
    fn test_cache_clear() {
        let temp_dir = TempDir::new().unwrap();
        let mut cache = Cache::new(temp_dir.path().to_path_buf()).unwrap();

        let key1 = CacheKey::new(
            "test1",
            "1.0",
            &CompileFlags::debug(),
            &[],
            &TargetConfig::native(),
        );
        let key2 = CacheKey::new(
            "test2",
            "1.0",
            &CompileFlags::debug(),
            &[],
            &TargetConfig::native(),
        );

        let metadata = ArtifactMetadata {
            created_at: SystemTime::now(),
            source_path: PathBuf::from("test.jet"),
            source_hash: "abc".to_string(),
            compiler_version: "1.0".to_string(),
            target: "native".to_string(),
            dependencies: vec![],
            object_size: 100,
        };

        cache.put(&key1, vec![1, 2, 3], metadata.clone()).unwrap();
        cache.put(&key2, vec![4, 5, 6], metadata.clone()).unwrap();

        assert!(cache.contains(&key1));
        assert!(cache.contains(&key2));

        cache.clear().unwrap();

        assert!(!cache.contains(&key1));
        assert!(!cache.contains(&key2));
    }
}
