//! Platform-specific support for linking
//!
//! This module provides platform detection and configuration for:
//! - Linux (ELF)
//! - macOS (Mach-O)
//! - Windows (PE/COFF)
//! - Cross-compilation support

use crate::error::{LinkerError, LinkerResult};
use std::path::PathBuf;

/// Supported target platforms
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Platform {
    /// Linux with ELF format
    Linux,
    /// macOS with Mach-O format
    MacOS,
    /// Windows with PE/COFF format
    Windows,
}

impl Platform {
    /// Detect the current host platform
    pub fn host() -> Self {
        #[cfg(target_os = "linux")]
        return Platform::Linux;
        #[cfg(target_os = "macos")]
        return Platform::MacOS;
        #[cfg(target_os = "windows")]
        return Platform::Windows;
    }

    /// Parse a platform from a target triple string
    pub fn from_target_triple(triple: &str) -> LinkerResult<Self> {
        if triple.contains("linux") {
            Ok(Platform::Linux)
        } else if triple.contains("darwin") || triple.contains("macos") {
            Ok(Platform::MacOS)
        } else if triple.contains("windows") || triple.contains("win32") {
            Ok(Platform::Windows)
        } else {
            Err(LinkerError::TargetNotSupported {
                triple: triple.to_string(),
            })
        }
    }

    /// Get the default executable extension for this platform
    pub fn exe_extension(&self) -> &'static str {
        match self {
            Platform::Windows => ".exe",
            _ => "",
        }
    }

    /// Get the default object file extension for this platform
    pub fn obj_extension(&self) -> &'static str {
        match self {
            Platform::Windows => ".obj",
            _ => ".o",
        }
    }

    /// Get the default static library extension for this platform
    pub fn static_lib_extension(&self) -> &'static str {
        match self {
            Platform::Windows => ".lib",
            _ => ".a",
        }
    }

    /// Get the default dynamic library extension for this platform
    pub fn dynamic_lib_extension(&self) -> &'static str {
        match self {
            Platform::Linux => ".so",
            Platform::MacOS => ".dylib",
            Platform::Windows => ".dll",
        }
    }

    /// Get the default linker name for this platform
    pub fn default_linker(&self) -> &'static str {
        match self {
            Platform::Linux => "ld",
            Platform::MacOS => "ld",
            Platform::Windows => "link.exe",
        }
    }

    /// Check if this platform supports position-independent executables (PIE)
    pub fn supports_pie(&self) -> bool {
        match self {
            Platform::Linux => true,
            Platform::MacOS => true,
            Platform::Windows => false, // Windows uses different mechanisms
        }
    }

    /// Get the default library search paths for this platform
    pub fn default_lib_paths(&self) -> Vec<PathBuf> {
        match self {
            Platform::Linux => vec![
                PathBuf::from("/usr/lib"),
                PathBuf::from("/usr/local/lib"),
                PathBuf::from("/lib"),
            ],
            Platform::MacOS => vec![
                PathBuf::from("/usr/lib"),
                PathBuf::from("/usr/local/lib"),
                PathBuf::from("/opt/homebrew/lib"), // Apple Silicon Homebrew
                PathBuf::from("/opt/local/lib"),    // MacPorts
            ],
            Platform::Windows => vec![
                PathBuf::from("C:\\Windows\\System32"),
                PathBuf::from("C:\\Windows"),
            ],
        }
    }

    /// Get the runtime library name for this platform
    pub fn runtime_lib_name(&self) -> String {
        format!("libjet_rt{}", self.static_lib_extension())
    }

    /// Get platform-specific linker flags for building an executable
    pub fn exe_linker_flags(&self) -> Vec<String> {
        match self {
            Platform::Linux => vec![
                "-dynamic-linker".to_string(),
                "/lib64/ld-linux-x86-64.so.2".to_string(),
            ],
            Platform::MacOS => vec![
                "-arch".to_string(),
                Self::macos_arch_name(),
                "-macos_version_min".to_string(),
                "11.0".to_string(),
                "-lSystem".to_string(),
            ],
            Platform::Windows => vec![
                "/SUBSYSTEM:CONSOLE".to_string(),
                "/ENTRY:mainCRTStartup".to_string(),
            ],
        }
    }

    /// Get the architecture name for macOS linker (ld expects 'arm64' not 'aarch64')
    fn macos_arch_name() -> String {
        // Rust's std::env::consts::ARCH returns "aarch64" but macOS ld expects "arm64"
        match std::env::consts::ARCH {
            "aarch64" => "arm64".to_string(),
            arch => arch.to_string(),
        }
    }

    /// Get platform-specific C runtime libraries to link
    pub fn crt_libs(&self) -> Vec<String> {
        match self {
            Platform::Linux => vec![
                "-lc".to_string(),
                "-lm".to_string(),
                "-lpthread".to_string(),
                "-ldl".to_string(),
                "-lrt".to_string(),
            ],
            Platform::MacOS => vec![
                "-lc".to_string(),
                "-lm".to_string(),
                "-lpthread".to_string(),
            ],
            Platform::Windows => vec![
                "kernel32.lib".to_string(),
                "ucrt.lib".to_string(),
                "vcruntime.lib".to_string(),
                "msvcrt.lib".to_string(),
            ],
        }
    }
}

impl std::fmt::Display for Platform {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Platform::Linux => write!(f, "linux"),
            Platform::MacOS => write!(f, "macos"),
            Platform::Windows => write!(f, "windows"),
        }
    }
}

/// Target configuration for cross-compilation
#[derive(Debug, Clone)]
pub struct TargetConfig {
    /// Target platform
    pub platform: Platform,
    /// Target triple (e.g., "x86_64-unknown-linux-gnu")
    pub triple: String,
    /// CPU architecture
    pub arch: String,
    /// Vendor (e.g., "unknown", "apple", "pc")
    pub vendor: String,
    /// Operating system
    pub os: String,
    /// Environment (e.g., "gnu", "musl", "msvc")
    pub env: Option<String>,
}

impl TargetConfig {
    /// Create a target config from a target triple string
    pub fn from_triple(triple: &str) -> LinkerResult<Self> {
        let platform = Platform::from_target_triple(triple)?;

        // Parse the triple (arch-vendor-os-env)
        let parts: Vec<&str> = triple.split('-').collect();
        if parts.len() < 3 {
            return Err(LinkerError::TargetNotSupported {
                triple: triple.to_string(),
            });
        }

        let arch = parts[0].to_string();
        let vendor = parts[1].to_string();
        let os = parts[2].to_string();
        let env = parts.get(3).map(|s| s.to_string());

        Ok(TargetConfig {
            platform,
            triple: triple.to_string(),
            arch,
            vendor,
            os,
            env,
        })
    }

    /// Create a native target config for the host
    pub fn native() -> Self {
        let platform = Platform::host();
        let triple = std::env::consts::ARCH.to_string()
            + "-"
            + if cfg!(target_os = "macos") {
                "apple"
            } else {
                "unknown"
            }
            + "-"
            + std::env::consts::OS;

        TargetConfig {
            platform,
            triple,
            arch: std::env::consts::ARCH.to_string(),
            vendor: if cfg!(target_os = "macos") {
                "apple"
            } else {
                "unknown"
            }
            .to_string(),
            os: std::env::consts::OS.to_string(),
            env: None,
        }
    }

    /// Check if this is a cross-compilation target
    pub fn is_cross_compile(&self) -> bool {
        let host = TargetConfig::native();
        self.triple != host.triple
    }

    /// Get the LLVM target triple
    pub fn llvm_target_triple(&self) -> String {
        self.triple.clone()
    }

    /// Get the architecture name for the linker
    /// On macOS, ld expects "arm64" instead of "aarch64"
    pub fn linker_arch(&self) -> String {
        if self.platform == Platform::MacOS && self.arch == "aarch64" {
            "arm64".to_string()
        } else {
            self.arch.clone()
        }
    }

    /// Get the sysroot path for this target (if cross-compiling)
    pub fn sysroot(&self) -> Option<PathBuf> {
        if !self.is_cross_compile() {
            return None;
        }

        // Look for sysroot in common locations
        let sysroot_paths = vec![
            PathBuf::from(format!("/usr/{}", self.triple)),
            PathBuf::from(format!("/opt/cross/{}", self.triple)),
            std::env::var("JET_SYSROOT")
                .map(PathBuf::from)
                .unwrap_or_default(),
        ];

        sysroot_paths.into_iter().find(|p| p.exists())
    }
}

/// Get the path to the system linker
pub fn find_system_linker(platform: Platform) -> LinkerResult<PathBuf> {
    let linker_name = platform.default_linker();

    // First, check if JET_LINKER environment variable is set
    if let Ok(linker) = std::env::var("JET_LINKER") {
        let path = PathBuf::from(linker);
        if path.exists() {
            return Ok(path);
        }
    }

    // Try to find the linker in PATH
    if let Ok(path) = which::which(linker_name) {
        return Ok(path);
    }

    // Platform-specific fallback paths
    let fallback_paths: Vec<PathBuf> = match platform {
        Platform::Linux => vec![
            PathBuf::from("/usr/bin/ld"),
            PathBuf::from("/usr/bin/ld.lld"),
            PathBuf::from("/usr/bin/gold"),
        ],
        Platform::MacOS => vec![
            PathBuf::from("/usr/bin/ld"),
            PathBuf::from("/Library/Developer/CommandLineTools/usr/bin/ld"),
            PathBuf::from("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ld"),
        ],
        Platform::Windows => vec![
            PathBuf::from("C:\\Program Files\\Microsoft Visual Studio\\2022\\Community\\VC\\Tools\\MSVC\\14.35.32215\\bin\\Hostx64\\x64\\link.exe"),
            PathBuf::from("C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\VC\\Tools\\MSVC\\14.29.30133\\bin\\Hostx64\\x64\\link.exe"),
        ],
    };

    for path in fallback_paths {
        if path.exists() {
            return Ok(path);
        }
    }

    Err(LinkerError::LinkerInvocation(format!(
        "Could not find linker '{}' for platform {}",
        linker_name, platform
    )))
}

/// Check if cross-compilation is supported for a given target
pub fn supports_cross_compilation(target: &TargetConfig) -> bool {
    let host = TargetConfig::native();

    // Same platform, no cross-compilation needed
    if target.platform == host.platform {
        return true;
    }

    // Check for cross-compilation tools
    match (host.platform, target.platform) {
        // macOS can cross-compile to Linux with the right toolchain
        (Platform::MacOS, Platform::Linux) => {
            // Check for musl-cross or similar
            std::env::var("JET_CROSS_COMPILE").is_ok()
                || which::which(&format!("{}-gcc", target.triple)).is_ok()
        }
        // Linux can cross-compile to various targets
        (Platform::Linux, Platform::Windows) => {
            which::which("x86_64-w64-mingw32-gcc").is_ok()
                || std::env::var("JET_CROSS_COMPILE").is_ok()
        }
        (Platform::Linux, Platform::MacOS) => {
            // Cross-compiling to macOS requires osxcross
            std::env::var("OSXCROSS_TARGET").is_ok() || which::which("o64-clang").is_ok()
        }
        // Windows cross-compilation is limited
        (Platform::Windows, _) => false,
        _ => false,
    }
}

// Simple which implementation to avoid extra dependency
mod which {
    use std::path::PathBuf;

    pub fn which(name: &str) -> Result<PathBuf, std::io::Error> {
        let path_var = std::env::var("PATH").unwrap_or_default();
        let paths: Vec<&str> = if cfg!(windows) {
            path_var.split(';').collect()
        } else {
            path_var.split(':').collect()
        };

        let extensions: Vec<&str> = if cfg!(windows) {
            vec![".exe", ".cmd", ".bat"]
        } else {
            vec![""]
        };

        for path in paths {
            for ext in &extensions {
                let full_path = PathBuf::from(path).join(format!("{}{}", name, ext));
                if full_path.exists() {
                    return Ok(full_path);
                }
            }
        }

        Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("{} not found in PATH", name),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_platform_host() {
        let host = Platform::host();
        // Should match the current platform
        assert!(
            (cfg!(target_os = "linux") && host == Platform::Linux)
                || (cfg!(target_os = "macos") && host == Platform::MacOS)
                || (cfg!(target_os = "windows") && host == Platform::Windows)
        );
    }

    #[test]
    fn test_platform_extensions() {
        assert_eq!(Platform::Windows.exe_extension(), ".exe");
        assert_eq!(Platform::Linux.exe_extension(), "");
        assert_eq!(Platform::MacOS.exe_extension(), "");

        assert_eq!(Platform::Windows.obj_extension(), ".obj");
        assert_eq!(Platform::Linux.obj_extension(), ".o");
        assert_eq!(Platform::MacOS.obj_extension(), ".o");
    }

    #[test]
    fn test_target_config_from_triple() {
        let config = TargetConfig::from_triple("x86_64-unknown-linux-gnu").unwrap();
        assert_eq!(config.platform, Platform::Linux);
        assert_eq!(config.arch, "x86_64");
        assert_eq!(config.vendor, "unknown");
        assert_eq!(config.os, "linux");
        assert_eq!(config.env, Some("gnu".to_string()));
    }

    #[test]
    fn test_target_config_macos() {
        let config = TargetConfig::from_triple("aarch64-apple-darwin").unwrap();
        assert_eq!(config.platform, Platform::MacOS);
        assert_eq!(config.arch, "aarch64");
        assert_eq!(config.vendor, "apple");
        assert_eq!(config.os, "darwin");
    }

    #[test]
    fn test_invalid_target() {
        let result = TargetConfig::from_triple("invalid");
        assert!(result.is_err());
    }

    #[test]
    fn test_native_target() {
        let native = TargetConfig::native();
        assert!(!native.triple.is_empty());
        assert!(!native.arch.is_empty());
    }
}
