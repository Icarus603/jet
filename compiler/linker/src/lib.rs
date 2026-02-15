//! Jet Linker Crate
//!
//! This crate provides the linker and build system for the Jet programming language.
//!
//! # Components
//!
//! - `linker`: System linker invocation and executable generation
//! - `build`: Build orchestration and compilation unit management
//! - `incremental`: Incremental compilation with dependency tracking
//! - `cache`: Artifact caching and cache management
//! - `platform`: Platform-specific support (Linux, macOS, Windows)
//! - `error`: Error types for linking and building
//!
//! # Example
//!
//! ```rust,no_run
//! use jet_linker::{Linker, LinkerConfig, TargetConfig};
//!
//! async fn link_example() {
//!     let target = TargetConfig::native();
//!     let config = LinkerConfig::new(target, "myapp".to_string());
//!     let mut linker = Linker::new(config);
//!
//!     linker.add_object("main.o");
//!     linker.add_object("lib.o");
//!
//!     let output = linker.link().await.unwrap();
//!     println!("Linked: {}", output.display());
//! }
//! ```

pub mod build;
pub mod cache;
pub mod error;
pub mod incremental;
pub mod linker;
pub mod platform;

// Re-export commonly used types
pub use build::{BuildCommand, BuildConfig, BuildProfile, BuildSystem, BuildTarget};
pub use cache::{Cache, CacheKey, CompileFlags};
pub use error::{BuildError, BuildResult, LinkerError, LinkerResult};
pub use incremental::{DependencyGraph, IncrementalState, Module, ModuleId};
pub use linker::{find_runtime_lib, link_executable, Linker, LinkerBuilder, LinkerConfig};
pub use platform::{find_system_linker, supports_cross_compilation, Platform, TargetConfig};

/// Version of the jet-linker crate
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Check if the linker is properly configured
pub fn check_linker() -> Result<(), String> {
    let target = TargetConfig::native();
    match find_system_linker(target.platform) {
        Ok(path) => {
            println!("Found linker: {}", path.display());
            Ok(())
        }
        Err(e) => Err(format!("Linker not found: {}", e)),
    }
}

/// Get information about the current platform
pub fn platform_info() -> String {
    let target = TargetConfig::native();
    format!(
        "Platform: {}\nArchitecture: {}\nOS: {}\nTriple: {}",
        target.platform, target.arch, target.os, target.triple
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version() {
        assert!(!VERSION.is_empty());
    }

    #[test]
    fn test_platform_info() {
        let info = platform_info();
        assert!(info.contains("Platform:"));
        assert!(info.contains("Architecture:"));
    }
}
