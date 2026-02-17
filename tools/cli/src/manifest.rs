//! Jet project manifest (jet.toml) parsing and management
//!
//! This module provides comprehensive support for parsing and working with
//! Jet project configuration files.

#![allow(dead_code)]

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// The main project manifest parsed from jet.toml
#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct Manifest {
    /// Package metadata
    #[serde(default)]
    pub package: Package,

    /// Build configuration
    #[serde(default)]
    pub build: BuildConfig,

    /// Feature flags
    #[serde(default)]
    pub features: HashMap<String, Vec<String>>,

    /// Dependencies
    #[serde(default)]
    pub dependencies: HashMap<String, Dependency>,

    /// Development dependencies
    #[serde(rename = "dev-dependencies")]
    #[serde(default)]
    pub dev_dependencies: HashMap<String, Dependency>,

    /// Build dependencies
    #[serde(rename = "build-dependencies")]
    #[serde(default)]
    pub build_dependencies: HashMap<String, Dependency>,

    /// Workspace configuration
    #[serde(default)]
    pub workspace: Option<Workspace>,

    /// Target-specific dependencies
    #[serde(default)]
    pub target: HashMap<String, HashMap<String, Dependency>>,

    /// Package metadata for registry
    #[serde(rename = "package.metadata")]
    #[serde(default)]
    pub metadata: Option<serde_json::Value>,

    /// Lint configuration
    #[serde(default)]
    pub lints: Option<LintConfig>,
}

/// Package metadata
#[derive(Deserialize, Serialize, Debug, Clone, Default)]
pub struct Package {
    /// Package name
    pub name: String,

    /// Package version (semver)
    pub version: String,

    /// Language edition
    #[serde(default = "default_edition")]
    pub edition: String,

    /// Package authors
    #[serde(default)]
    pub authors: Vec<String>,

    /// Brief description
    #[serde(default)]
    pub description: Option<String>,

    /// License (SPDX identifier)
    #[serde(default)]
    pub license: Option<String>,

    /// License file path
    #[serde(rename = "license-file")]
    #[serde(default)]
    pub license_file: Option<PathBuf>,

    /// Repository URL
    #[serde(default)]
    pub repository: Option<String>,

    /// Homepage URL
    #[serde(default)]
    pub homepage: Option<String>,

    /// Documentation URL
    #[serde(default)]
    pub documentation: Option<String>,

    /// Keywords for registry search
    #[serde(default)]
    pub keywords: Vec<String>,

    /// Categories for registry browsing
    #[serde(default)]
    pub categories: Vec<String>,

    /// README file path
    #[serde(default)]
    pub readme: Option<PathBuf>,

    /// Build script path
    #[serde(default)]
    pub build: Option<PathBuf>,

    /// Whether this package can be published
    #[serde(default = "default_true")]
    pub publish: bool,

    /// Default run target
    #[serde(rename = "default-run")]
    #[serde(default)]
    pub default_run: Option<String>,

    /// Minimum supported Jet version
    #[serde(rename = "jet-version")]
    #[serde(default)]
    pub jet_version: Option<String>,

    /// Rust-style features that are always enabled
    #[serde(default)]
    pub default_features: Vec<String>,
}

/// Build configuration
#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct BuildConfig {
    /// Target directory
    #[serde(rename = "target-dir")]
    #[serde(default = "default_target_dir")]
    pub target_dir: PathBuf,

    /// Default build profile
    #[serde(default)]
    pub profile: Option<String>,

    /// Compiler wrapper (e.g., sccache)
    #[serde(rename = "rustc-wrapper")]
    #[serde(default)]
    pub rustc_wrapper: Option<String>,

    /// Linker to use
    #[serde(default)]
    pub linker: Option<String>,

    /// Additional linker flags
    #[serde(rename = "linker-flaqs")]
    #[serde(default)]
    pub linker_flags: Vec<String>,

    /// Build profiles
    #[serde(default)]
    pub profiles: HashMap<String, Profile>,
}

impl Default for BuildConfig {
    fn default() -> Self {
        Self {
            target_dir: default_target_dir(),
            profile: None,
            rustc_wrapper: None,
            linker: None,
            linker_flags: vec![],
            profiles: HashMap::new(),
        }
    }
}

/// Build profile configuration
#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct Profile {
    /// Inherits from another profile
    #[serde(default)]
    pub inherits: Option<String>,

    /// Optimization level (0-3, s, z)
    #[serde(rename = "opt-level")]
    #[serde(default)]
    pub opt_level: Option<String>,

    /// Debug info level (0-2)
    #[serde(default)]
    pub debug: Option<u8>,

    /// Link-time optimization
    #[serde(default)]
    pub lto: Option<bool>,

    /// Number of codegen units
    #[serde(rename = "codegen-units")]
    #[serde(default)]
    pub codegen_units: Option<usize>,

    /// Panic strategy (unwind or abort)
    #[serde(default)]
    pub panic: Option<String>,

    /// Strip symbols
    #[serde(default)]
    pub strip: Option<bool>,

    /// Enable incremental compilation
    #[serde(default)]
    pub incremental: Option<bool>,

    /// Additional compiler flags
    #[serde(default)]
    pub flags: Vec<String>,
}

/// Dependency specification
#[derive(Deserialize, Serialize, Debug, Clone)]
#[serde(untagged)]
pub enum Dependency {
    /// Simple version requirement
    Simple(String),
    /// Detailed dependency specification
    Detailed(DetailedDependency),
}

impl Dependency {
    /// Get the version requirement
    pub fn version(&self) -> Option<&str> {
        match self {
            Dependency::Simple(v) => Some(v),
            Dependency::Detailed(d) => d.version.as_deref(),
        }
    }

    /// Check if this is an optional dependency
    pub fn is_optional(&self) -> bool {
        match self {
            Dependency::Simple(_) => false,
            Dependency::Detailed(d) => d.optional.unwrap_or(false),
        }
    }

    /// Get the features to enable
    pub fn features(&self) -> &[String] {
        match self {
            Dependency::Simple(_) => &[],
            Dependency::Detailed(d) => &d.features,
        }
    }
}

/// Detailed dependency specification
#[derive(Deserialize, Serialize, Debug, Clone, Default)]
pub struct DetailedDependency {
    /// Version requirement
    #[serde(default)]
    pub version: Option<String>,

    /// Git repository URL
    #[serde(default)]
    pub git: Option<String>,

    /// Git branch
    #[serde(default)]
    pub branch: Option<String>,

    /// Git tag
    #[serde(default)]
    pub tag: Option<String>,

    /// Git revision
    #[serde(default)]
    pub rev: Option<String>,

    /// Local path
    #[serde(default)]
    pub path: Option<PathBuf>,

    /// Registry URL
    #[serde(default)]
    pub registry: Option<String>,

    /// Whether this is an optional dependency
    #[serde(default)]
    pub optional: Option<bool>,

    /// Features to enable
    #[serde(default)]
    pub features: Vec<String>,

    /// Whether to use default features
    #[serde(rename = "default-features")]
    #[serde(default = "default_true")]
    pub default_features: bool,

    /// Target platform
    #[serde(default)]
    pub target: Option<String>,
}

/// Workspace configuration
#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct Workspace {
    /// Workspace name
    #[serde(default)]
    pub name: Option<String>,

    /// Workspace members (glob patterns)
    #[serde(default)]
    pub members: Vec<String>,

    /// Excluded paths
    #[serde(default)]
    pub exclude: Vec<String>,

    /// Resolver version
    #[serde(default = "default_resolver")]
    pub resolver: String,

    /// Shared package metadata
    #[serde(rename = "package")]
    #[serde(default)]
    pub package: Option<WorkspacePackage>,

    /// Shared dependencies
    #[serde(rename = "dependencies")]
    #[serde(default)]
    pub dependencies: HashMap<String, Dependency>,
}

/// Workspace package defaults
#[derive(Deserialize, Serialize, Debug, Clone, Default)]
pub struct WorkspacePackage {
    pub version: Option<String>,
    pub edition: Option<String>,
    pub authors: Option<Vec<String>>,
    pub license: Option<String>,
    pub repository: Option<String>,
    pub homepage: Option<String>,
    pub documentation: Option<String>,
    pub keywords: Option<Vec<String>>,
    pub categories: Option<Vec<String>>,
    pub readme: Option<PathBuf>,
}

/// Lint configuration
#[derive(Deserialize, Serialize, Debug, Clone, Default)]
pub struct LintConfig {
    /// Lints to allow
    #[serde(default)]
    pub allow: Vec<String>,

    /// Lints to warn
    #[serde(default)]
    pub warn: Vec<String>,

    /// Lints to deny
    #[serde(default)]
    pub deny: Vec<String>,

    /// Lints to forbid
    #[serde(default)]
    pub forbid: Vec<String>,

    /// Lint groups
    #[serde(default)]
    pub clippy: Option<ClippyLints>,
}

/// Clippy lint groups
#[derive(Deserialize, Serialize, Debug, Clone, Default)]
pub struct ClippyLints {
    pub correctness: Option<String>,
    pub suspicious: Option<String>,
    pub style: Option<String>,
    pub performance: Option<String>,
    pub complexity: Option<String>,
    pub pedantic: Option<String>,
}

impl Manifest {
    /// Read and parse a jet.toml file
    pub fn from_file(path: &Path) -> Result<Self> {
        let content = std::fs::read_to_string(path)
            .with_context(|| format!("Failed to read manifest at {}", path.display()))?;

        Self::from_str(&content)
    }

    /// Parse a manifest from a string
    pub fn from_str(content: &str) -> Result<Self> {
        let manifest: Manifest = toml::from_str(content).context("Failed to parse jet.toml")?;

        manifest.validate()?;

        Ok(manifest)
    }

    /// Write the manifest to a file
    pub fn to_file(&self, path: &Path) -> Result<()> {
        let content = toml::to_string_pretty(self).context("Failed to serialize manifest")?;

        std::fs::write(path, content)
            .with_context(|| format!("Failed to write manifest to {}", path.display()))?;

        Ok(())
    }

    /// Validate the manifest
    fn validate(&self) -> Result<()> {
        // Validate package name
        if self.package.name.is_empty() {
            anyhow::bail!("Package name cannot be empty");
        }

        if !is_valid_package_name(&self.package.name) {
            anyhow::bail!(
                "Invalid package name '{}'. Package names must start with a letter \
                 and contain only alphanumeric characters, hyphens, and underscores.",
                self.package.name
            );
        }

        // Validate version
        if !is_valid_version(&self.package.version) {
            anyhow::bail!(
                "Invalid version '{}'. Versions must follow semantic versioning.",
                self.package.version
            );
        }

        // Validate edition
        let valid_editions = ["2024"];
        if !valid_editions.contains(&self.package.edition.as_str()) {
            anyhow::bail!(
                "Invalid edition '{}'. Supported editions: {:?}",
                self.package.edition,
                valid_editions
            );
        }

        Ok(())
    }

    /// Get the target directory
    pub fn target_dir(&self) -> &Path {
        &self.build.target_dir
    }

    /// Get the full target directory for a profile
    pub fn target_dir_for_profile(&self, profile: &str) -> PathBuf {
        self.build.target_dir.join(profile)
    }

    /// Get all dependencies (including dev and build)
    pub fn all_dependencies(&self) -> impl Iterator<Item = (&String, &Dependency)> {
        self.dependencies
            .iter()
            .chain(self.dev_dependencies.iter())
            .chain(self.build_dependencies.iter())
    }

    /// Check if this is a workspace root
    pub fn is_workspace(&self) -> bool {
        self.workspace.is_some()
    }

    /// Get workspace members
    pub fn workspace_members(&self) -> Option<&[String]> {
        self.workspace.as_ref().map(|w| w.members.as_slice())
    }

    /// Get the default profile configuration
    pub fn default_profile(&self) -> Profile {
        let mut profile = Profile {
            inherits: None,
            opt_level: Some("0".to_string()),
            debug: Some(2),
            lto: Some(false),
            codegen_units: Some(256),
            panic: Some("unwind".to_string()),
            strip: Some(false),
            incremental: Some(true),
            flags: vec![],
        };

        // Apply profile-specific defaults
        if let Some(profiles) = &self.build.profiles.get("dev") {
            profile.merge(profiles);
        }

        profile
    }

    /// Get the release profile configuration
    pub fn release_profile(&self) -> Profile {
        let mut profile = Profile {
            inherits: None,
            opt_level: Some("3".to_string()),
            debug: Some(0),
            lto: Some(true),
            codegen_units: Some(1),
            panic: Some("abort".to_string()),
            strip: Some(true),
            incremental: Some(false),
            flags: vec![],
        };

        // Apply profile-specific defaults
        if let Some(profiles) = &self.build.profiles.get("release") {
            profile.merge(profiles);
        }

        profile
    }

    /// Get a profile by name
    pub fn get_profile(&self, name: &str) -> Option<Profile> {
        match name {
            "dev" => Some(self.default_profile()),
            "release" => Some(self.release_profile()),
            _ => self.build.profiles.get(name).cloned(),
        }
    }
}

impl Profile {
    /// Merge another profile into this one
    fn merge(&mut self, other: &Profile) {
        if let Some(ref v) = other.opt_level {
            self.opt_level = Some(v.clone());
        }
        if let Some(v) = other.debug {
            self.debug = Some(v);
        }
        if let Some(v) = other.lto {
            self.lto = Some(v);
        }
        if let Some(v) = other.codegen_units {
            self.codegen_units = Some(v);
        }
        if let Some(ref v) = other.panic {
            self.panic = Some(v.clone());
        }
        if let Some(v) = other.strip {
            self.strip = Some(v);
        }
        if let Some(v) = other.incremental {
            self.incremental = Some(v);
        }
        self.flags.extend(other.flags.clone());
    }

    /// Get the optimization level as a number
    pub fn opt_level_num(&self) -> u8 {
        match self.opt_level.as_deref() {
            Some("0") => 0,
            Some("1") => 1,
            Some("2") => 2,
            Some("3") => 3,
            Some("s") => 2, // Size optimization
            Some("z") => 2, // Aggressive size
            _ => 0,
        }
    }
}

/// Check if a package name is valid
fn is_valid_package_name(name: &str) -> bool {
    if name.is_empty() {
        return false;
    }

    let first = name.chars().next().unwrap();
    if !first.is_ascii_alphabetic() {
        return false;
    }

    name.chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
}

/// Check if a version string is valid (basic semver check)
fn is_valid_version(version: &str) -> bool {
    // Basic semver: MAJOR.MINOR.PATCH[-PRERELEASE][+BUILD]
    let parts: Vec<&str> = version.split(&['-', '+'][..]).collect();
    let version_part = parts[0];

    let nums: Vec<&str> = version_part.split('.').collect();
    if nums.len() != 3 {
        return false;
    }

    nums.iter().all(|n| n.parse::<u64>().is_ok())
}

fn default_edition() -> String {
    "2024".to_string()
}

fn default_target_dir() -> PathBuf {
    PathBuf::from("target")
}

fn default_true() -> bool {
    true
}

fn default_resolver() -> String {
    "2".to_string()
}

/// Find the project root by looking for jet.toml
pub fn find_project_root(start_dir: &Path) -> Option<PathBuf> {
    let mut current = Some(start_dir);

    while let Some(dir) = current {
        let manifest_path = dir.join("jet.toml");
        if manifest_path.exists() {
            return Some(dir.to_path_buf());
        }

        current = dir.parent();
    }

    None
}

/// Read the manifest from the current project
pub fn read_project_manifest() -> Result<(Manifest, PathBuf)> {
    let current_dir = std::env::current_dir()?;

    let project_root = find_project_root(&current_dir).ok_or_else(|| {
        anyhow::anyhow!("Could not find jet.toml. Are you in a Jet project directory?")
    })?;

    let manifest_path = project_root.join("jet.toml");
    let manifest = Manifest::from_file(&manifest_path)?;

    Ok((manifest, project_root))
}

/// Create a new manifest for a binary project
pub fn new_binary_manifest(name: &str) -> Manifest {
    Manifest {
        package: Package {
            name: name.to_string(),
            version: "0.1.0".to_string(),
            edition: "2024".to_string(),
            authors: vec![],
            description: None,
            license: None,
            license_file: None,
            repository: None,
            homepage: None,
            documentation: None,
            keywords: vec![],
            categories: vec![],
            readme: None,
            build: None,
            publish: true,
            default_run: None,
            jet_version: None,
            default_features: vec![],
        },
        build: BuildConfig::default(),
        features: HashMap::new(),
        dependencies: HashMap::new(),
        dev_dependencies: HashMap::new(),
        build_dependencies: HashMap::new(),
        workspace: None,
        target: HashMap::new(),
        metadata: None,
        lints: None,
    }
}

/// Create a new manifest for a library project
pub fn new_library_manifest(name: &str) -> Manifest {
    let mut manifest = new_binary_manifest(name);
    manifest.package.description = Some(format!("The {} library", name));
    manifest
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_package_names() {
        assert!(is_valid_package_name("my_project"));
        assert!(is_valid_package_name("my-project"));
        assert!(is_valid_package_name("myProject"));
        assert!(is_valid_package_name("MyProject123"));
    }

    #[test]
    fn test_invalid_package_names() {
        assert!(!is_valid_package_name(""));
        assert!(!is_valid_package_name("123project"));
        assert!(!is_valid_package_name("my project"));
        assert!(!is_valid_package_name("my.project"));
    }

    #[test]
    fn test_valid_versions() {
        assert!(is_valid_version("1.0.0"));
        assert!(is_valid_version("0.1.0"));
        assert!(is_valid_version("1.2.3-alpha"));
        assert!(is_valid_version("1.0.0+build123"));
    }

    #[test]
    fn test_invalid_versions() {
        assert!(!is_valid_version("1.0"));
        assert!(!is_valid_version("1"));
        assert!(!is_valid_version("abc"));
        assert!(!is_valid_version("1.0.0.0"));
    }

    #[test]
    fn test_parse_simple_manifest() {
        let toml = r#"
[package]
name = "test-project"
version = "1.0.0"
edition = "2024"

[dependencies]
serde = "1.0"
"#;

        let manifest = Manifest::from_str(toml).unwrap();
        assert_eq!(manifest.package.name, "test-project");
        assert_eq!(manifest.package.version, "1.0.0");
        assert_eq!(manifest.package.edition, "2024");
        assert_eq!(manifest.dependencies.len(), 1);
    }

    #[test]
    fn test_parse_detailed_dependency() {
        let toml = r#"
[package]
name = "test"
version = "1.0.0"

[dependencies]
serde = { version = "1.0", features = ["derive"] }
local-lib = { path = "../local-lib" }
git-lib = { git = "https://github.com/user/repo", branch = "main" }
"#;

        let manifest = Manifest::from_str(toml).unwrap();
        assert_eq!(manifest.dependencies.len(), 3);

        let serde = manifest.dependencies.get("serde").unwrap();
        assert!(!serde.is_optional());
        assert_eq!(serde.features(), &["derive"]);
    }

    #[test]
    fn test_workspace_manifest() {
        let toml = r#"
[package]
name = "workspace-root"
version = "1.0.0"

[workspace]
members = ["crates/*", "apps/*"]
resolver = "2"
"#;

        let manifest = Manifest::from_str(toml).unwrap();
        assert!(manifest.is_workspace());
        assert_eq!(
            manifest.workspace_members().unwrap(),
            &["crates/*", "apps/*"]
        );
    }

    #[test]
    fn test_profile_merge() {
        let mut base = Profile {
            inherits: None,
            opt_level: Some("0".to_string()),
            debug: Some(2),
            lto: Some(false),
            codegen_units: Some(256),
            panic: Some("unwind".to_string()),
            strip: Some(false),
            incremental: Some(true),
            flags: vec![],
        };

        let override_profile = Profile {
            inherits: None,
            opt_level: Some("3".to_string()),
            debug: None,
            lto: Some(true),
            codegen_units: None,
            panic: None,
            strip: None,
            incremental: None,
            flags: vec!["-C".to_string(), "target-cpu=native".to_string()],
        };

        base.merge(&override_profile);

        assert_eq!(base.opt_level, Some("3".to_string()));
        assert_eq!(base.debug, Some(2)); // Not overridden
        assert_eq!(base.lto, Some(true));
        assert_eq!(base.flags.len(), 2);
    }
}
