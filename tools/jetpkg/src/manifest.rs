//! Jet package manifest parsing
//!
//! Handles parsing of `jet.toml` files with full support for:
//! - Package metadata
//! - Dependencies (simple and detailed)
//! - Dev dependencies
//! - Build dependencies
//! - Features
//! - Workspace configuration
//! - Platform-specific dependencies

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Package manifest (jet.toml)
#[derive(Debug, Clone, Deserialize, Serialize)]
#[allow(dead_code)]
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
    /// Dev dependencies
    #[serde(rename = "dev-dependencies", default)]
    pub dev_dependencies: HashMap<String, Dependency>,
    /// Build dependencies
    #[serde(rename = "build-dependencies", default)]
    pub build_dependencies: HashMap<String, Dependency>,
    /// Target-specific dependencies
    #[serde(default)]
    pub target: HashMap<String, TargetDependencies>,
    /// Workspace configuration
    #[serde(default)]
    pub workspace: Option<WorkspaceConfig>,
    /// Package metadata for registry
    #[serde(rename = "package.metadata", default)]
    pub metadata: Option<toml::Value>,
}

/// Package metadata
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
#[allow(dead_code)]
pub struct Package {
    pub name: String,
    pub version: String,
    #[serde(default)]
    pub edition: Option<String>,
    #[serde(default)]
    pub authors: Vec<String>,
    #[serde(default)]
    pub description: Option<String>,
    #[serde(default)]
    pub license: Option<String>,
    #[serde(default)]
    pub repository: Option<String>,
    #[serde(default)]
    pub homepage: Option<String>,
    #[serde(default)]
    pub documentation: Option<String>,
    #[serde(default)]
    pub keywords: Vec<String>,
    #[serde(default)]
    pub categories: Vec<String>,
    #[serde(default)]
    pub readme: Option<String>,
    #[serde(default = "default_true")]
    pub publish: bool,
    #[serde(default)]
    pub exclude: Vec<String>,
    #[serde(default)]
    pub include: Vec<String>,
    #[serde(default)]
    pub build: Option<String>,
    #[serde(default)]
    pub links: Option<String>,
}

/// Build configuration
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
#[allow(dead_code)]
pub struct BuildConfig {
    #[serde(rename = "target-dir", default)]
    pub target_dir: Option<PathBuf>,
    #[serde(default)]
    pub profile: Option<String>,
    #[serde(rename = "rustc-wrapper", default)]
    pub rustc_wrapper: Option<String>,
}

/// Dependency specification
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(untagged)]
#[allow(dead_code)]
pub enum Dependency {
    /// Simple version string (e.g., "1.0.0")
    Simple(String),
    /// Detailed dependency specification
    Detailed(DetailedDependency),
}

#[allow(dead_code)]
impl Dependency {
    /// Get the version requirement for this dependency
    pub fn version(&self) -> Option<&str> {
        match self {
            Dependency::Simple(v) => Some(v.as_str()),
            Dependency::Detailed(d) => d.version.as_deref(),
        }
    }

    /// Check if this is a path dependency
    pub fn is_path(&self) -> bool {
        matches!(self, Dependency::Detailed(d) if d.path.is_some())
    }

    /// Check if this is a git dependency
    pub fn is_git(&self) -> bool {
        matches!(self, Dependency::Detailed(d) if d.git.is_some())
    }

    /// Get the path if this is a path dependency
    pub fn path(&self) -> Option<&Path> {
        match self {
            Dependency::Detailed(d) => d.path.as_deref(),
            _ => None,
        }
    }

    /// Get the git URL if this is a git dependency
    pub fn git(&self) -> Option<&str> {
        match self {
            Dependency::Detailed(d) => d.git.as_deref(),
            _ => None,
        }
    }

    /// Get the features for this dependency
    pub fn features(&self) -> &[String] {
        match self {
            Dependency::Detailed(d) => &d.features,
            _ => &[],
        }
    }

    /// Check if this dependency is optional
    pub fn is_optional(&self) -> bool {
        match self {
            Dependency::Detailed(d) => d.optional,
            _ => false,
        }
    }
}

/// Detailed dependency specification
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct DetailedDependency {
    #[serde(default)]
    pub version: Option<String>,
    #[serde(default)]
    pub path: Option<PathBuf>,
    #[serde(default)]
    pub git: Option<String>,
    #[serde(default)]
    pub branch: Option<String>,
    #[serde(default)]
    pub tag: Option<String>,
    #[serde(default)]
    pub rev: Option<String>,
    #[serde(default)]
    pub registry: Option<String>,
    #[serde(default)]
    pub features: Vec<String>,
    #[serde(rename = "default-features", default = "default_true")]
    pub default_features: bool,
    #[serde(default)]
    pub optional: bool,
    #[serde(rename = "workspace", default)]
    pub workspace: bool,
}

/// Target-specific dependencies
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct TargetDependencies {
    #[serde(default)]
    pub dependencies: HashMap<String, Dependency>,
}

/// Workspace configuration
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct WorkspaceConfig {
    /// Workspace members
    #[serde(default)]
    pub members: Vec<String>,
    /// Excluded paths
    #[serde(default)]
    pub exclude: Vec<String>,
    /// Workspace resolver version
    #[serde(default)]
    pub resolver: Option<String>,
    /// Shared workspace package metadata
    #[serde(default)]
    pub package: Option<WorkspacePackage>,
    /// Shared workspace dependencies
    #[serde(default)]
    pub dependencies: HashMap<String, Dependency>,
}

/// Workspace package defaults
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct WorkspacePackage {
    #[serde(default)]
    pub version: Option<String>,
    #[serde(default)]
    pub edition: Option<String>,
    #[serde(default)]
    pub authors: Option<Vec<String>>,
    #[serde(default)]
    pub license: Option<String>,
    #[serde(default)]
    pub repository: Option<String>,
    #[serde(default)]
    pub homepage: Option<String>,
    #[serde(default)]
    pub documentation: Option<String>,
    #[serde(default)]
    pub keywords: Option<Vec<String>>,
    #[serde(default)]
    pub categories: Option<Vec<String>>,
    #[serde(default)]
    pub readme: Option<String>,
}

impl Manifest {
    /// Load a manifest from a file path
    #[allow(dead_code)]
    pub fn from_file(path: &Path) -> Result<Self> {
        let content = std::fs::read_to_string(path)
            .with_context(|| format!("Failed to read manifest from {}", path.display()))?;

        Self::from_str(&content)
    }

    /// Parse a manifest from a string
    #[allow(dead_code)]
    pub fn from_str(content: &str) -> Result<Self> {
        let manifest: Manifest =
            toml::from_str(content).with_context(|| "Failed to parse jet.toml")?;

        Ok(manifest)
    }

    /// Save the manifest to a file
    #[allow(dead_code)]
    pub fn to_file(&self, path: &Path) -> Result<()> {
        let content =
            toml::to_string_pretty(self).with_context(|| "Failed to serialize manifest")?;

        std::fs::write(path, content)
            .with_context(|| format!("Failed to write manifest to {}", path.display()))?;

        Ok(())
    }

    /// Get all dependencies (regular, dev, and build)
    #[allow(dead_code)]
    pub fn all_dependencies(&self) -> HashMap<String, &Dependency> {
        let mut deps = HashMap::new();

        for (name, dep) in &self.dependencies {
            deps.insert(name.clone(), dep);
        }

        for (name, dep) in &self.dev_dependencies {
            deps.insert(name.clone(), dep);
        }

        for (name, dep) in &self.build_dependencies {
            deps.insert(name.clone(), dep);
        }

        deps
    }

    /// Check if this is a workspace root
    #[allow(dead_code)]
    pub fn is_workspace_root(&self) -> bool {
        self.workspace.is_some()
    }

    /// Get workspace members
    #[allow(dead_code)]
    pub fn workspace_members(&self) -> Option<&[String]> {
        self.workspace.as_ref().map(|w| w.members.as_slice())
    }

    /// Resolve workspace dependencies for a member
    #[allow(dead_code)]
    pub fn resolve_workspace_deps(&self) -> HashMap<String, Dependency> {
        let mut resolved = self.dependencies.clone();

        if let Some(ref workspace) = self.workspace {
            // Resolve workspace = true dependencies
            for (name, dep) in &mut resolved {
                if let Dependency::Detailed(ref mut d) = dep {
                    if d.workspace {
                        if let Some(workspace_deps) = workspace.dependencies.get(name) {
                            // Replace with workspace dependency
                            *dep = workspace_deps.clone();
                        }
                    }
                }
            }
        }

        resolved
    }
}

fn default_true() -> bool {
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_manifest() {
        let toml = r#"
[package]
name = "my-package"
version = "1.0.0"

[dependencies]
serde = "1.0"
"#;

        let manifest = Manifest::from_str(toml).unwrap();
        assert_eq!(manifest.package.name, "my-package");
        assert_eq!(manifest.package.version, "1.0.0");
        assert_eq!(manifest.dependencies.len(), 1);

        let serde_dep = manifest.dependencies.get("serde").unwrap();
        assert_eq!(serde_dep.version(), Some("1.0"));
    }

    #[test]
    fn test_parse_detailed_dependency() {
        let toml = r#"
[package]
name = "my-package"
version = "1.0.0"

[dependencies]
serde = { version = "1.0", features = ["derive"] }
tokio = { version = "1.0", optional = true }
local = { path = "../local" }
git-dep = { git = "https://github.com/user/repo", branch = "main" }
"#;

        let manifest = Manifest::from_str(toml).unwrap();
        assert_eq!(manifest.dependencies.len(), 4);

        let serde_dep = manifest.dependencies.get("serde").unwrap();
        assert!(matches!(serde_dep, Dependency::Detailed(_)));
        assert_eq!(serde_dep.features(), &["derive"]);

        let tokio_dep = manifest.dependencies.get("tokio").unwrap();
        assert!(tokio_dep.is_optional());

        let local_dep = manifest.dependencies.get("local").unwrap();
        assert!(local_dep.is_path());

        let git_dep = manifest.dependencies.get("git-dep").unwrap();
        assert!(git_dep.is_git());
    }

    #[test]
    fn test_parse_workspace() {
        let toml = r#"
[workspace]
members = ["crates/*", "apps/*"]
exclude = ["experimental"]
resolver = "2"

[workspace.package]
version = "1.0.0"
edition = "2024"
authors = ["Team <team@example.com>"]
license = "MIT"

[workspace.dependencies]
serde = "1.0.200"
tokio = { version = "1.35", features = ["full"] }
"#;

        let manifest = Manifest::from_str(toml).unwrap();
        assert!(manifest.is_workspace_root());

        let workspace = manifest.workspace.unwrap();
        assert_eq!(workspace.members, vec!["crates/*", "apps/*"]);
        assert_eq!(workspace.exclude, vec!["experimental"]);
        assert_eq!(workspace.resolver, Some("2".to_string()));

        let package = workspace.package.unwrap();
        assert_eq!(package.version, Some("1.0.0".to_string()));
        assert_eq!(package.license, Some("MIT".to_string()));
    }

    #[test]
    fn test_parse_features() {
        let toml = r#"
[package]
name = "my-package"
version = "1.0.0"

[features]
default = ["std", "logging"]
std = []
logging = ["log", "tracing"]
async = ["tokio", "futures"]
"#;

        let manifest = Manifest::from_str(toml).unwrap();
        assert_eq!(manifest.features.len(), 4);

        let default = manifest.features.get("default").unwrap();
        assert_eq!(default, &["std", "logging"]);

        let logging = manifest.features.get("logging").unwrap();
        assert_eq!(logging, &["log", "tracing"]);
    }

    #[test]
    fn test_parse_target_deps() {
        let toml = r#"
[package]
name = "my-package"
version = "1.0.0"

[target.'cfg(unix)'.dependencies]
libc = "0.2"

[target.'cfg(windows)'.dependencies]
winapi = "0.3"
"#;

        let manifest = Manifest::from_str(toml).unwrap();
        assert_eq!(manifest.target.len(), 2);

        let unix_deps = manifest.target.get("cfg(unix)").unwrap();
        assert!(unix_deps.dependencies.contains_key("libc"));

        let windows_deps = manifest.target.get("cfg(windows)").unwrap();
        assert!(windows_deps.dependencies.contains_key("winapi"));
    }
}
