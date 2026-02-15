//! Linker implementation for Jet
//!
//! This module provides:
//! - Linker struct with configuration
//! - System linker invocation (ld, lld, or link.exe)
//! - Object file collection
//! - Runtime library linking
//! - Output executable generation

use std::path::{Path, PathBuf};
use std::process::Stdio;

use tokio::process::Command;

use crate::error::{LinkerError, LinkerResult};
use crate::platform::{find_system_linker, Platform, TargetConfig};

/// Configuration for the linker
#[derive(Debug, Clone)]
pub struct LinkerConfig {
    /// Target platform
    pub target: TargetConfig,
    /// Path to the linker executable (None for system default)
    pub linker_path: Option<PathBuf>,
    /// Additional library search paths
    pub lib_paths: Vec<PathBuf>,
    /// Additional libraries to link
    pub libraries: Vec<String>,
    /// Runtime library path
    pub runtime_lib_path: Option<PathBuf>,
    /// Output file name
    pub output_name: String,
    /// Output directory
    pub output_dir: PathBuf,
    /// Whether to link statically
    pub static_linking: bool,
    /// Whether to strip symbols
    pub strip_symbols: bool,
    /// Additional linker arguments
    pub extra_args: Vec<String>,
    /// Debug information level
    pub debug_level: u8,
}

impl LinkerConfig {
    /// Create a new linker configuration for the given target
    pub fn new(target: TargetConfig, output_name: String) -> Self {
        LinkerConfig {
            target,
            linker_path: None,
            lib_paths: vec![],
            libraries: vec![],
            runtime_lib_path: None,
            output_name,
            output_dir: PathBuf::from("target/debug"),
            static_linking: false,
            strip_symbols: false,
            extra_args: vec![],
            debug_level: 0,
        }
    }

    /// Set a custom linker path
    pub fn with_linker(mut self, path: impl AsRef<Path>) -> Self {
        self.linker_path = Some(path.as_ref().to_path_buf());
        self
    }

    /// Add a library search path
    pub fn add_lib_path(mut self, path: impl AsRef<Path>) -> Self {
        self.lib_paths.push(path.as_ref().to_path_buf());
        self
    }

    /// Add a library to link
    pub fn add_library(mut self, lib: impl Into<String>) -> Self {
        self.libraries.push(lib.into());
        self
    }

    /// Set the runtime library path
    pub fn with_runtime_lib(mut self, path: impl AsRef<Path>) -> Self {
        self.runtime_lib_path = Some(path.as_ref().to_path_buf());
        self
    }

    /// Set the output directory
    pub fn with_output_dir(mut self, path: impl AsRef<Path>) -> Self {
        self.output_dir = path.as_ref().to_path_buf();
        self
    }

    /// Enable static linking
    pub fn static_linking(mut self) -> Self {
        self.static_linking = true;
        self
    }

    /// Enable symbol stripping
    pub fn strip_symbols(mut self) -> Self {
        self.strip_symbols = true;
        self
    }

    /// Add extra linker arguments
    pub fn add_extra_arg(mut self, arg: impl Into<String>) -> Self {
        self.extra_args.push(arg.into());
        self
    }

    /// Set debug information level
    pub fn with_debug_level(mut self, level: u8) -> Self {
        self.debug_level = level;
        self
    }

    /// Get the full output path for the executable
    pub fn output_path(&self) -> PathBuf {
        let exe_name = format!(
            "{}{}",
            self.output_name,
            self.target.platform.exe_extension()
        );
        self.output_dir.join(exe_name)
    }
}

impl Default for LinkerConfig {
    fn default() -> Self {
        LinkerConfig::new(TargetConfig::native(), "main".to_string())
    }
}

/// The linker for Jet
pub struct Linker {
    /// Linker configuration
    config: LinkerConfig,
    /// Object files to link
    object_files: Vec<PathBuf>,
    /// Additional linker arguments
    link_args: Vec<String>,
}

impl Linker {
    /// Create a new linker with the given configuration
    pub fn new(config: LinkerConfig) -> Self {
        Linker {
            config,
            object_files: vec![],
            link_args: vec![],
        }
    }

    /// Add an object file to link
    pub fn add_object(&mut self, path: impl AsRef<Path>) -> &mut Self {
        self.object_files.push(path.as_ref().to_path_buf());
        self
    }

    /// Add multiple object files
    pub fn add_objects(&mut self, paths: &[PathBuf]) -> &mut Self {
        self.object_files.extend(paths.iter().cloned());
        self
    }

    /// Add a linker argument
    pub fn add_arg(&mut self, arg: impl Into<String>) -> &mut Self {
        self.link_args.push(arg.into());
        self
    }

    /// Link all object files into an executable
    pub async fn link(&self) -> LinkerResult<PathBuf> {
        // Ensure output directory exists
        std::fs::create_dir_all(&self.config.output_dir)?;

        // Get the linker path
        let linker_path = self.config.linker_path.clone().unwrap_or_else(|| {
            find_system_linker(self.config.target.platform)
                .unwrap_or_else(|_| PathBuf::from(self.config.target.platform.default_linker()))
        });

        // Build the linker command
        let mut cmd = Command::new(&linker_path);

        // Add platform-specific arguments
        self.add_platform_args(&mut cmd);

        // Add object files
        for obj in &self.object_files {
            if !obj.exists() {
                return Err(LinkerError::ObjectFileNotFound(obj.clone()));
            }
            cmd.arg(obj);
        }

        // Add library search paths
        for path in &self.config.lib_paths {
            match self.config.target.platform {
                Platform::Windows => {
                    cmd.arg(format!("/LIBPATH:{}", path.display()));
                }
                _ => {
                    cmd.arg("-L");
                    cmd.arg(path);
                }
            }
        }

        // Add default library paths
        for path in self.config.target.platform.default_lib_paths() {
            if path.exists() {
                match self.config.target.platform {
                    Platform::Windows => {
                        cmd.arg(format!("/LIBPATH:{}", path.display()));
                    }
                    _ => {
                        cmd.arg("-L");
                        cmd.arg(path);
                    }
                }
            }
        }

        // Add runtime library if specified
        if let Some(ref runtime_path) = self.config.runtime_lib_path {
            if runtime_path.exists() {
                match self.config.target.platform {
                    Platform::Windows => {
                        cmd.arg(runtime_path);
                    }
                    _ => {
                        cmd.arg(runtime_path);
                    }
                }
            }
        }

        // Add libraries
        for lib in &self.config.libraries {
            match self.config.target.platform {
                Platform::Windows => {
                    cmd.arg(format!("{}.lib", lib));
                }
                _ => {
                    cmd.arg("-l");
                    cmd.arg(lib);
                }
            }
        }

        // Add C runtime libraries
        if !self.config.static_linking {
            for lib in self.config.target.platform.crt_libs() {
                cmd.arg(&lib);
            }
        }

        // Add extra arguments
        for arg in &self.config.extra_args {
            cmd.arg(arg);
        }

        // Add custom link args
        for arg in &self.link_args {
            cmd.arg(arg);
        }

        // Set output file
        let output_path = self.config.output_path();
        match self.config.target.platform {
            Platform::Windows => {
                cmd.arg(format!("/OUT:{}", output_path.display()));
            }
            _ => {
                cmd.arg("-o");
                cmd.arg(&output_path);
            }
        }

        // Execute the linker
        let output = cmd
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .await
            .map_err(|e| LinkerError::LinkerInvocation(e.to_string()))?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr).to_string();
            return Err(LinkerError::LinkerFailed {
                exit_code: output.status.code().unwrap_or(-1),
                stderr,
            });
        }

        // Make executable on Unix
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = std::fs::metadata(&output_path)?.permissions();
            perms.set_mode(0o755);
            std::fs::set_permissions(&output_path, perms)?;
        }

        Ok(output_path)
    }

    /// Add platform-specific linker arguments
    fn add_platform_args(&self, cmd: &mut Command) {
        match self.config.target.platform {
            Platform::Linux => {
                // Our object files are currently emitted as non-PIE.
                // Enabling -pie here causes relocation failures in integration builds.

                // Dynamic linker
                cmd.arg("-dynamic-linker");
                cmd.arg("/lib64/ld-linux-x86-64.so.2");

                let arch_dir = Self::linux_multiarch_dir();
                cmd.arg(format!("-L/usr/lib/{}", arch_dir));
                cmd.arg(format!("-L/lib/{}", arch_dir));

                // Start files
                cmd.arg(Self::find_linux_startup_object("crt1.o"));
                cmd.arg(Self::find_linux_startup_object("crti.o"));

                // Static linking
                if self.config.static_linking {
                    cmd.arg("-static");
                }

                // Strip symbols
                if self.config.strip_symbols {
                    cmd.arg("-s");
                }

                // Debug info
                if self.config.debug_level > 0 {
                    // Keep debug info
                }

                // Runtime/system libraries expected by Rust-generated objects.
                if let Some(libgcc) = Self::find_linux_runtime_lib("libgcc_s.so.1") {
                    cmd.arg(libgcc);
                } else {
                    cmd.arg("-lgcc_s");
                }
                cmd.arg("-lutil");
                cmd.arg("-lrt");
                cmd.arg("-lpthread");
                cmd.arg("-lm");
                cmd.arg("-ldl");

                // End files
                cmd.arg("-lc");
                cmd.arg(Self::find_linux_startup_object("crtn.o"));
            }
            Platform::MacOS => {
                // Architecture
                cmd.arg("-arch");
                cmd.arg(self.config.target.linker_arch());

                // macOS version
                cmd.arg("-macos_version_min");
                cmd.arg("11.0");

                // System library
                cmd.arg("-lSystem");

                // Library search path for system libraries
                cmd.arg("-L/usr/lib");
                cmd.arg("-L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib");

                // Static linking
                if self.config.static_linking {
                    cmd.arg("-static");
                }

                // Strip symbols
                if self.config.strip_symbols {
                    cmd.arg("-x"); // Strip local symbols
                }

                // Debug info
                if self.config.debug_level > 0 {
                    cmd.arg("-no_deduplicate");
                }
            }
            Platform::Windows => {
                // Subsystem
                cmd.arg("/SUBSYSTEM:CONSOLE");

                // Entry point
                cmd.arg("/ENTRY:mainCRTStartup");

                // Debug info
                if self.config.debug_level > 0 {
                    cmd.arg("/DEBUG");
                }

                // Strip symbols
                if self.config.strip_symbols {
                    cmd.arg("/STRIP");
                }

                // Default libraries
                cmd.arg("kernel32.lib");
                cmd.arg("ucrt.lib");
                cmd.arg("vcruntime.lib");
                cmd.arg("msvcrt.lib");
            }
        }
    }

    fn find_linux_startup_object(object: &str) -> String {
        let arch_dir = Self::linux_multiarch_dir();

        let candidates = [
            format!("/usr/lib/{}/{}", arch_dir, object),
            format!("/lib/{}/{}", arch_dir, object),
            format!("/usr/lib64/{}", object),
            format!("/usr/lib/{}", object),
            format!("/lib/{}", object),
        ];

        for candidate in candidates {
            if std::path::Path::new(&candidate).exists() {
                return candidate;
            }
        }

        format!("/usr/lib/{}", object)
    }

    fn linux_multiarch_dir() -> &'static str {
        match std::env::consts::ARCH {
            "x86_64" => "x86_64-linux-gnu",
            "aarch64" => "aarch64-linux-gnu",
            other => other,
        }
    }

    fn find_linux_runtime_lib(lib_file: &str) -> Option<String> {
        let arch_dir = Self::linux_multiarch_dir();
        let candidates = [
            format!("/lib/{}/{}", arch_dir, lib_file),
            format!("/usr/lib/{}/{}", arch_dir, lib_file),
            format!("/lib64/{}", lib_file),
            format!("/usr/lib64/{}", lib_file),
        ];

        candidates
            .iter()
            .find(|candidate| std::path::Path::new(candidate.as_str()).exists())
            .cloned()
    }

    /// Get the configuration
    pub fn config(&self) -> &LinkerConfig {
        &self.config
    }

    /// Get a mutable reference to the configuration
    pub fn config_mut(&mut self) -> &mut LinkerConfig {
        &mut self.config
    }

    /// Get the list of object files
    pub fn object_files(&self) -> &[PathBuf] {
        &self.object_files
    }
}

/// Builder for creating linkers with common configurations
pub struct LinkerBuilder {
    config: LinkerConfig,
}

impl LinkerBuilder {
    /// Create a new linker builder
    pub fn new(target: TargetConfig, output_name: impl Into<String>) -> Self {
        LinkerBuilder {
            config: LinkerConfig::new(target, output_name.into()),
        }
    }

    /// Create a debug linker
    pub fn debug(target: TargetConfig, output_name: impl Into<String>) -> Self {
        let mut builder = Self::new(target, output_name);
        builder.config.output_dir = PathBuf::from("target/debug");
        builder.config.debug_level = 2;
        builder
    }

    /// Create a release linker
    pub fn release(target: TargetConfig, output_name: impl Into<String>) -> Self {
        let mut builder = Self::new(target, output_name);
        builder.config.output_dir = PathBuf::from("target/release");
        builder.config.strip_symbols = true;
        builder.config.static_linking = false;
        builder
    }

    /// Set a custom linker
    pub fn with_linker(mut self, path: impl AsRef<Path>) -> Self {
        self.config.linker_path = Some(path.as_ref().to_path_buf());
        self
    }

    /// Add a library search path
    pub fn add_lib_path(mut self, path: impl AsRef<Path>) -> Self {
        self.config.lib_paths.push(path.as_ref().to_path_buf());
        self
    }

    /// Add a library
    pub fn add_library(mut self, lib: impl Into<String>) -> Self {
        self.config.libraries.push(lib.into());
        self
    }

    /// Set the runtime library path
    pub fn with_runtime_lib(mut self, path: impl AsRef<Path>) -> Self {
        self.config.runtime_lib_path = Some(path.as_ref().to_path_buf());
        self
    }

    /// Set the output directory
    pub fn with_output_dir(mut self, path: impl AsRef<Path>) -> Self {
        self.config.output_dir = path.as_ref().to_path_buf();
        self
    }

    /// Enable static linking
    pub fn static_linking(mut self) -> Self {
        self.config.static_linking = true;
        self
    }

    /// Build the linker
    pub fn build(self) -> Linker {
        Linker::new(self.config)
    }
}

/// Link multiple object files into a single executable
pub async fn link_executable(
    object_files: &[PathBuf],
    output_path: &Path,
    target: &TargetConfig,
) -> LinkerResult<PathBuf> {
    let output_name = output_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main")
        .to_string();

    let output_dir = output_path
        .parent()
        .map(|p| p.to_path_buf())
        .unwrap_or_else(|| PathBuf::from("."));

    let config = LinkerConfig {
        target: target.clone(),
        linker_path: None,
        lib_paths: vec![],
        libraries: vec![],
        runtime_lib_path: None,
        output_name,
        output_dir,
        static_linking: false,
        strip_symbols: false,
        extra_args: vec![],
        debug_level: 0,
    };

    let mut linker = Linker::new(config);
    linker.add_objects(object_files);
    linker.link().await
}

/// Find the Jet runtime library
pub fn find_runtime_lib(platform: Platform) -> LinkerResult<PathBuf> {
    // Check JET_RUNTIME_LIB environment variable
    if let Ok(path) = std::env::var("JET_RUNTIME_LIB") {
        let path = PathBuf::from(path);
        if path.exists() {
            return Ok(path);
        }
    }

    // Search in common locations
    let lib_name = platform.runtime_lib_name();

    // Try to find the workspace root by looking for Cargo.toml with [workspace]
    let mut workspace_root = None;
    let mut current_dir = std::env::current_dir().ok();

    while let Some(dir) = current_dir {
        let cargo_toml = dir.join("Cargo.toml");
        if cargo_toml.exists() {
            if let Ok(content) = std::fs::read_to_string(&cargo_toml) {
                if content.contains("[workspace]") {
                    workspace_root = Some(dir.clone());
                    break;
                }
            }
        }

        // Also check if this is the jet compiler root (has runtime/ directory)
        if dir.join("runtime").join("Cargo.toml").exists() {
            workspace_root = Some(dir.clone());
            break;
        }

        current_dir = dir.parent().map(|p| p.to_path_buf());
    }

    let mut search_paths = vec![
        // Current directory (prefer debug first for development workflows)
        PathBuf::from("target/debug").join(&lib_name),
        PathBuf::from("target/release").join(&lib_name),
        // Parent directory (for sub-projects)
        PathBuf::from("../target/debug").join(&lib_name),
        PathBuf::from("../target/release").join(&lib_name),
    ];

    // Add paths near the current executable (important when running `jet` from a different CWD)
    if let Ok(current_exe) = std::env::current_exe() {
        if let Some(exe_dir) = current_exe.parent() {
            search_paths.push(exe_dir.join(&lib_name));
            if let Some(parent) = exe_dir.parent() {
                search_paths.push(parent.join("target/debug").join(&lib_name));
                search_paths.push(parent.join("target/release").join(&lib_name));
            }
        }
    }

    // Add workspace root paths if found
    if let Some(root) = workspace_root {
        search_paths.push(root.join("target/debug").join(&lib_name));
        search_paths.push(root.join("target/release").join(&lib_name));
    }

    // System locations
    search_paths.push(PathBuf::from("/usr/local/lib/jet").join(&lib_name));
    search_paths.push(PathBuf::from("/usr/lib/jet").join(&lib_name));

    for path in &search_paths {
        if path.exists() {
            return Ok(path.clone());
        }
    }

    Err(LinkerError::RuntimeLibraryNotFound {
        library: lib_name,
        search_paths: search_paths.clone(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_linker_config_default() {
        let config = LinkerConfig::default();
        assert_eq!(config.output_name, "main");
        assert!(!config.static_linking);
        assert!(!config.strip_symbols);
    }

    #[test]
    fn test_linker_config_builder() {
        let config = LinkerConfig::new(TargetConfig::native(), "myapp".to_string())
            .with_output_dir("/tmp/build")
            .static_linking()
            .strip_symbols()
            .with_debug_level(2);

        assert_eq!(config.output_name, "myapp");
        assert_eq!(config.output_dir, PathBuf::from("/tmp/build"));
        assert!(config.static_linking);
        assert!(config.strip_symbols);
        assert_eq!(config.debug_level, 2);
    }

    #[test]
    fn test_linker_config_output_path() {
        let config = LinkerConfig::new(TargetConfig::native(), "myapp".to_string())
            .with_output_dir("/tmp/build");

        let output = config.output_path();
        assert!(output.to_string_lossy().contains("myapp"));
        assert!(output.parent().unwrap() == std::path::Path::new("/tmp/build"));
    }

    #[test]
    fn test_linker_builder_debug() {
        let builder = LinkerBuilder::debug(TargetConfig::native(), "app");
        assert_eq!(builder.config.output_dir, PathBuf::from("target/debug"));
        assert_eq!(builder.config.debug_level, 2);
    }

    #[test]
    fn test_linker_builder_release() {
        let builder = LinkerBuilder::release(TargetConfig::native(), "app");
        assert_eq!(builder.config.output_dir, PathBuf::from("target/release"));
        assert!(builder.config.strip_symbols);
    }

    #[tokio::test]
    async fn test_linker_add_object() {
        let config = LinkerConfig::new(TargetConfig::native(), "test".to_string());
        let mut linker = Linker::new(config);

        linker.add_object("/tmp/test.o");
        assert_eq!(linker.object_files().len(), 1);
    }
}
