//! Dependency resolution and management for Jet projects
//!
//! This module provides:
//! - Dependency resolution from registries
//! - Git dependency fetching
//! - Local path dependency handling
//! - Lock file generation and validation
//! - Dependency graph construction

#![allow(dead_code)]

use anyhow::{Context, Result};
use semver::{Version, VersionReq};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

use crate::manifest::{Dependency, DetailedDependency, Manifest};

/// A resolved dependency
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolvedDependency {
    /// Package name
    pub name: String,
    /// Resolved version
    pub version: String,
    /// Source of the dependency
    pub source: DependencySource,
    /// Checksum for verification
    pub checksum: Option<String>,
    /// Dependencies of this dependency
    pub dependencies: Vec<String>,
    /// Features enabled
    pub features: Vec<String>,
}

/// Source of a dependency
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum DependencySource {
    /// Registry dependency
    #[serde(rename = "registry")]
    Registry {
        /// Registry URL
        url: String,
    },
    /// Git repository
    #[serde(rename = "git")]
    Git {
        /// Repository URL
        url: String,
        /// Specific revision
        rev: Option<String>,
        /// Branch name
        branch: Option<String>,
        /// Tag name
        tag: Option<String>,
    },
    /// Local path
    #[serde(rename = "path")]
    Path {
        /// Path to the dependency
        path: PathBuf,
    },
}

impl DependencySource {
    /// Get a human-readable description
    pub fn description(&self) -> String {
        match self {
            DependencySource::Registry { url } => format!("registry {}", url),
            DependencySource::Git {
                url,
                rev,
                branch,
                tag,
            } => {
                let mut desc = format!("git {}", url);
                if let Some(r) = rev {
                    desc.push_str(&format!(" (rev: {})", &r[..r.len().min(8)]));
                } else if let Some(b) = branch {
                    desc.push_str(&format!(" (branch: {})", b));
                } else if let Some(t) = tag {
                    desc.push_str(&format!(" (tag: {})", t));
                }
                desc
            }
            DependencySource::Path { path } => format!("path {}", path.display()),
        }
    }
}

/// The dependency resolver
pub struct DependencyResolver {
    /// Registry URL
    registry_url: String,
    /// Cache directory
    cache_dir: PathBuf,
    /// Resolved dependencies
    resolved: HashMap<String, ResolvedDependency>,
    /// Resolution stack (for cycle detection)
    resolution_stack: Vec<String>,
}

/// Lock file format
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockFile {
    /// Lock file version
    pub version: u32,
    /// All resolved packages
    pub packages: Vec<ResolvedDependency>,
}

/// Dependency graph for topological sorting
#[derive(Debug, Default)]
pub struct DependencyGraph {
    /// Nodes (package names)
    nodes: HashSet<String>,
    /// Edges (package -> dependencies)
    edges: HashMap<String, Vec<String>>,
}

#[derive(Debug, Deserialize)]
struct RegistryCrateResponse {
    versions: Vec<RegistryVersionEntry>,
}

#[derive(Debug, Deserialize)]
struct RegistryVersionEntry {
    num: String,
    #[serde(default)]
    yanked: bool,
}

impl DependencyResolver {
    /// Create a new dependency resolver
    pub fn new() -> Result<Self> {
        let cache_dir = dirs::cache_dir()
            .unwrap_or_else(|| {
                PathBuf::from(std::env::var("HOME").unwrap_or_default()).join(".cache")
            })
            .join("jet")
            .join("registry");

        std::fs::create_dir_all(&cache_dir)?;

        Ok(Self {
            registry_url: std::env::var("JET_REGISTRY")
                .unwrap_or_else(|_| "https://crates.jet-lang.org".to_string()),
            cache_dir,
            resolved: HashMap::new(),
            resolution_stack: vec![],
        })
    }

    /// Set a custom registry URL
    pub fn with_registry(mut self, url: impl Into<String>) -> Self {
        self.registry_url = url.into();
        self
    }

    /// Resolve all dependencies for a manifest
    pub async fn resolve(&mut self, manifest: &Manifest) -> Result<Vec<ResolvedDependency>> {
        self.resolved.clear();
        self.resolution_stack.clear();

        // Resolve regular dependencies
        for (name, dep) in &manifest.dependencies {
            self.resolve_dependency(name, dep).await?;
        }

        // Resolve dev dependencies
        for (name, dep) in &manifest.dev_dependencies {
            self.resolve_dependency(name, dep).await?;
        }

        // Resolve build dependencies
        for (name, dep) in &manifest.build_dependencies {
            self.resolve_dependency(name, dep).await?;
        }

        // Resolve target-specific dependencies
        for deps in manifest.target.values() {
            for (name, dep) in deps {
                self.resolve_dependency(name, dep).await?;
            }
        }

        Ok(self.resolved.values().cloned().collect())
    }

    /// Resolve a single dependency
    async fn resolve_dependency(&mut self, name: &str, dep: &Dependency) -> Result<()> {
        // Check for cycles
        if self.resolution_stack.contains(&name.to_string()) {
            anyhow::bail!(
                "Dependency cycle detected: {} -> {}",
                self.resolution_stack.join(" -> "),
                name
            );
        }

        // Check if already resolved
        if self.resolved.contains_key(name) {
            return Ok(());
        }

        self.resolution_stack.push(name.to_string());

        let resolved = match dep {
            Dependency::Simple(version) => {
                self.resolve_registry_dependency(name, version, &[]).await?
            }
            Dependency::Detailed(detailed) => {
                self.resolve_detailed_dependency(name, detailed).await?
            }
        };

        self.resolved.insert(name.to_string(), resolved);
        self.resolution_stack.pop();

        Ok(())
    }

    /// Resolve a registry dependency
    async fn resolve_registry_dependency(
        &mut self,
        name: &str,
        version_req: &str,
        features: &[String],
    ) -> Result<ResolvedDependency> {
        let version = self
            .resolve_version_from_registry(name, version_req)
            .await?;

        Ok(ResolvedDependency {
            name: name.to_string(),
            version,
            source: DependencySource::Registry {
                url: self.registry_url.clone(),
            },
            checksum: None,
            dependencies: vec![],
            features: features.to_vec(),
        })
    }

    /// Resolve a detailed dependency
    async fn resolve_detailed_dependency(
        &mut self,
        name: &str,
        dep: &DetailedDependency,
    ) -> Result<ResolvedDependency> {
        // Path dependency
        if let Some(ref path) = dep.path {
            return self
                .resolve_path_dependency(name, path, &dep.features)
                .await;
        }

        // Git dependency
        if let Some(ref url) = dep.git {
            return self
                .resolve_git_dependency(
                    name,
                    url,
                    dep.branch.clone(),
                    dep.tag.clone(),
                    dep.rev.clone(),
                    &dep.features,
                )
                .await;
        }

        // Registry dependency with version
        if let Some(ref version) = dep.version {
            return self
                .resolve_registry_dependency(name, version, &dep.features)
                .await;
        }

        anyhow::bail!("Dependency {} has no source specified", name)
    }

    /// Resolve a path dependency
    async fn resolve_path_dependency(
        &self,
        name: &str,
        path: &Path,
        features: &[String],
    ) -> Result<ResolvedDependency> {
        let manifest_path = path.join("jet.toml");
        if !manifest_path.exists() {
            anyhow::bail!(
                "Path dependency {} does not have a jet.toml at {}",
                name,
                path.display()
            );
        }

        let manifest = Manifest::from_file(&manifest_path)?;

        Ok(ResolvedDependency {
            name: name.to_string(),
            version: manifest.package.version,
            source: DependencySource::Path {
                path: path.to_path_buf(),
            },
            checksum: None,
            dependencies: manifest.dependencies.keys().cloned().collect(),
            features: features.to_vec(),
        })
    }

    /// Resolve a git dependency
    async fn resolve_git_dependency(
        &self,
        name: &str,
        url: &str,
        branch: Option<String>,
        tag: Option<String>,
        rev: Option<String>,
        features: &[String],
    ) -> Result<ResolvedDependency> {
        let checkout =
            self.ensure_git_checkout(url, branch.as_deref(), tag.as_deref(), rev.as_deref())?;
        let manifest_path = checkout.join("jet.toml");
        if !manifest_path.exists() {
            anyhow::bail!(
                "Git dependency {} does not contain a jet.toml at {}",
                name,
                manifest_path.display()
            );
        }
        let manifest = Manifest::from_file(&manifest_path)?;

        Ok(ResolvedDependency {
            name: name.to_string(),
            version: manifest.package.version,
            source: DependencySource::Git {
                url: url.to_string(),
                rev,
                branch,
                tag,
            },
            checksum: None,
            dependencies: manifest.dependencies.keys().cloned().collect(),
            features: features.to_vec(),
        })
    }

    /// Resolve a version from the registry
    async fn resolve_version_from_registry(&self, name: &str, version_req: &str) -> Result<String> {
        let normalized = normalize_version_req(version_req)?;
        let req = VersionReq::parse(&normalized)
            .with_context(|| format!("invalid version requirement: {}", version_req))?;
        let versions = self.fetch_registry_versions(name).await?;

        versions
            .into_iter()
            .filter(|v| req.matches(v))
            .max()
            .map(|v| v.to_string())
            .with_context(|| {
                format!(
                    "no matching version for {} {} in registry {}",
                    name, version_req, self.registry_url
                )
            })
    }

    fn ensure_git_checkout(
        &self,
        url: &str,
        branch: Option<&str>,
        tag: Option<&str>,
        rev: Option<&str>,
    ) -> Result<PathBuf> {
        let mut hasher = Sha256::new();
        hasher.update(url.as_bytes());
        if let Some(branch) = branch {
            hasher.update(b"branch:");
            hasher.update(branch.as_bytes());
        }
        if let Some(tag) = tag {
            hasher.update(b"tag:");
            hasher.update(tag.as_bytes());
        }
        if let Some(rev) = rev {
            hasher.update(b"rev:");
            hasher.update(rev.as_bytes());
        }
        let digest = hex::encode(hasher.finalize());
        let repo_dir = self.cache_dir.join("git").join(&digest[..16]);
        if !repo_dir.exists() {
            std::fs::create_dir_all(repo_dir.parent().unwrap_or(&self.cache_dir))?;
            run_git_command(
                Command::new("git")
                    .arg("clone")
                    .arg("--quiet")
                    .arg(url)
                    .arg(&repo_dir),
                "clone git dependency",
            )?;
        } else {
            run_git_command(
                Command::new("git")
                    .current_dir(&repo_dir)
                    .arg("fetch")
                    .arg("--all")
                    .arg("--tags")
                    .arg("--quiet"),
                "fetch git dependency",
            )?;
        }

        if let Some(rev) = rev {
            run_git_command(
                Command::new("git")
                    .current_dir(&repo_dir)
                    .arg("checkout")
                    .arg("--quiet")
                    .arg(rev),
                "checkout git revision",
            )?;
        } else if let Some(tag) = tag {
            run_git_command(
                Command::new("git")
                    .current_dir(&repo_dir)
                    .arg("checkout")
                    .arg("--quiet")
                    .arg(format!("tags/{}", tag)),
                "checkout git tag",
            )?;
        } else if let Some(branch) = branch {
            run_git_command(
                Command::new("git")
                    .current_dir(&repo_dir)
                    .arg("checkout")
                    .arg("--quiet")
                    .arg(branch),
                "checkout git branch",
            )?;
        } else {
            run_git_command(
                Command::new("git")
                    .current_dir(&repo_dir)
                    .arg("checkout")
                    .arg("--quiet")
                    .arg("HEAD"),
                "checkout git HEAD",
            )?;
        }

        Ok(repo_dir)
    }

    async fn fetch_registry_versions(&self, name: &str) -> Result<Vec<Version>> {
        let endpoint = format!(
            "{}/api/v1/crates/{}",
            self.registry_url.trim_end_matches('/'),
            name
        );
        let client = reqwest::Client::builder()
            .timeout(Duration::from_secs(10))
            .build()
            .context("failed to create registry HTTP client")?;
        let response = client
            .get(&endpoint)
            .send()
            .await
            .with_context(|| format!("failed to query registry endpoint {}", endpoint))?;
        if !response.status().is_success() {
            anyhow::bail!(
                "registry request failed for {}: HTTP {}",
                endpoint,
                response.status()
            );
        }
        let crate_response: RegistryCrateResponse = response
            .json()
            .await
            .context("failed to decode registry response")?;
        let mut versions = crate_response
            .versions
            .into_iter()
            .filter(|v| !v.yanked)
            .filter_map(|v| Version::parse(&v.num).ok())
            .collect::<Vec<_>>();
        versions.sort();
        Ok(versions)
    }

    /// Get the cache directory
    pub fn cache_dir(&self) -> &Path {
        &self.cache_dir
    }
}

impl Default for DependencyResolver {
    fn default() -> Self {
        Self::new().expect("Failed to create dependency resolver")
    }
}

fn normalize_version_req(version_req: &str) -> Result<String> {
    let req = version_req.trim();
    if req == "*" {
        return Ok("*".to_string());
    }
    if req.starts_with(['^', '~', '=', '>', '<']) || req.contains(',') {
        return Ok(req.to_string());
    }
    let parsed = Version::parse(req)
        .with_context(|| format!("invalid semver version in dependency requirement: {}", req))?;
    Ok(format!("^{}", parsed))
}

fn run_git_command(cmd: &mut Command, action: &str) -> Result<()> {
    let output = cmd
        .output()
        .with_context(|| format!("failed to {}: git executable not available", action))?;
    if output.status.success() {
        return Ok(());
    }
    let stderr = String::from_utf8_lossy(&output.stderr);
    anyhow::bail!("failed to {}: {}", action, stderr.trim())
}

impl LockFile {
    /// Create a new lock file from resolved dependencies
    pub fn new(packages: Vec<ResolvedDependency>) -> Self {
        Self {
            version: 1,
            packages,
        }
    }

    /// Read a lock file from disk
    pub fn from_file(path: &Path) -> Result<Self> {
        let content = std::fs::read_to_string(path)
            .with_context(|| format!("Failed to read lock file at {}", path.display()))?;

        Self::from_str(&content)
    }

    /// Parse a lock file from a string
    pub fn from_str(content: &str) -> Result<Self> {
        let lockfile: LockFile = toml::from_str(content).context("Failed to parse lock file")?;
        Ok(lockfile)
    }

    /// Write the lock file to disk
    pub fn to_file(&self, path: &Path) -> Result<()> {
        let content = self.to_string()?;
        std::fs::write(path, content)
            .with_context(|| format!("Failed to write lock file to {}", path.display()))?;
        Ok(())
    }

    /// Serialize to string
    pub fn to_string(&self) -> Result<String> {
        let mut content = String::new();
        content.push_str("# This file is automatically generated by Jet.\n");
        content.push_str("# It is not intended for manual editing.\n");
        content.push_str(&format!("version = {}\n\n", self.version));

        for package in &self.packages {
            content.push_str("[[package]]\n");
            content.push_str(&format!("name = \"{}\"\n", package.name));
            content.push_str(&format!("version = \"{}\"\n", package.version));
            content.push_str(&format!("source = \"{}\"\n", package.source.description()));

            if let Some(ref checksum) = package.checksum {
                content.push_str(&format!("checksum = \"{}\"\n", checksum));
            }

            if !package.dependencies.is_empty() {
                content.push_str("dependencies = [\n");
                for dep in &package.dependencies {
                    content.push_str(&format!("    \"{}\",\n", dep));
                }
                content.push_str("]\n");
            }

            if !package.features.is_empty() {
                content.push_str(&format!("features = {:?}\n", package.features));
            }

            content.push('\n');
        }

        Ok(content)
    }

    /// Find a package in the lock file
    pub fn find_package(&self, name: &str) -> Option<&ResolvedDependency> {
        self.packages.iter().find(|p| p.name == name)
    }

    /// Check if the lock file is up to date with the manifest
    pub fn is_up_to_date(&self, manifest: &Manifest) -> bool {
        let manifest_deps: HashSet<&String> = manifest
            .dependencies
            .keys()
            .chain(manifest.dev_dependencies.keys())
            .chain(manifest.build_dependencies.keys())
            .collect();

        let locked_deps: HashSet<&String> = self.packages.iter().map(|p| &p.name).collect();

        manifest_deps == locked_deps
    }
}

impl DependencyGraph {
    /// Create a new dependency graph from resolved dependencies
    pub fn from_resolved(deps: &[ResolvedDependency]) -> Self {
        let mut graph = Self::default();

        for dep in deps {
            graph.nodes.insert(dep.name.clone());
            graph
                .edges
                .insert(dep.name.clone(), dep.dependencies.clone());
        }

        graph
    }

    /// Add a node to the graph
    pub fn add_node(&mut self, name: impl Into<String>) {
        self.nodes.insert(name.into());
    }

    /// Add an edge to the graph
    pub fn add_edge(&mut self, from: impl Into<String>, to: impl Into<String>) {
        let from = from.into();
        let to = to.into();
        self.nodes.insert(from.clone());
        self.nodes.insert(to.clone());
        self.edges.entry(from).or_default().push(to);
    }

    /// Get dependencies of a node
    pub fn dependencies(&self, name: &str) -> &[String] {
        self.edges.get(name).map(|v| v.as_slice()).unwrap_or(&[])
    }

    /// Perform topological sort
    pub fn topological_sort(&self) -> Result<Vec<String>> {
        let mut sorted = Vec::new();
        let mut visited = HashSet::new();
        let mut temp_mark = HashSet::new();

        for node in &self.nodes {
            if !visited.contains(node) {
                self.visit(node, &mut visited, &mut temp_mark, &mut sorted)?;
            }
        }

        Ok(sorted)
    }

    fn visit(
        &self,
        node: &str,
        visited: &mut HashSet<String>,
        temp_mark: &mut HashSet<String>,
        sorted: &mut Vec<String>,
    ) -> Result<()> {
        if temp_mark.contains(node) {
            anyhow::bail!("Dependency cycle detected involving {}", node);
        }

        if visited.contains(node) {
            return Ok(());
        }

        temp_mark.insert(node.to_string());

        for dep in self.dependencies(node) {
            self.visit(dep, visited, temp_mark, sorted)?;
        }

        temp_mark.remove(node);
        visited.insert(node.to_string());
        sorted.push(node.to_string());

        Ok(())
    }
}

/// Update the lock file for a project
pub async fn update_lock_file(project_root: &Path, manifest: &Manifest) -> Result<LockFile> {
    let mut resolver = DependencyResolver::new()?;
    let resolved = resolver.resolve(manifest).await?;

    let lockfile = LockFile::new(resolved);
    let lockfile_path = project_root.join("jet.lock");
    lockfile.to_file(&lockfile_path)?;

    Ok(lockfile)
}

/// Read or create the lock file
pub fn read_or_create_lock_file(project_root: &Path, manifest: &Manifest) -> Result<LockFile> {
    let lockfile_path = project_root.join("jet.lock");

    if lockfile_path.exists() {
        let lockfile = LockFile::from_file(&lockfile_path)?;

        if !lockfile.is_up_to_date(manifest) {
            println!("    Lock file is out of date, consider running `jet update`");
        }

        Ok(lockfile)
    } else {
        println!("    No lock file found, creating one...");
        // Create an empty lock file
        let lockfile = LockFile::new(vec![]);
        lockfile.to_file(&lockfile_path)?;
        Ok(lockfile)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dependency_source_description() {
        let registry = DependencySource::Registry {
            url: "https://crates.jet-lang.org".to_string(),
        };
        assert!(registry.description().contains("registry"));

        let git = DependencySource::Git {
            url: "https://github.com/user/repo".to_string(),
            rev: Some("abc123def456".to_string()),
            branch: None,
            tag: None,
        };
        assert!(git.description().contains("git"));
        assert!(git.description().contains("abc123de")); // First 8 chars of rev

        let path = DependencySource::Path {
            path: PathBuf::from("../local"),
        };
        assert!(path.description().contains("path"));
    }

    #[test]
    fn test_lock_file_roundtrip() {
        let packages = vec![ResolvedDependency {
            name: "serde".to_string(),
            version: "1.0.200".to_string(),
            source: DependencySource::Registry {
                url: "https://crates.jet-lang.org".to_string(),
            },
            checksum: Some("sha256:abc123".to_string()),
            dependencies: vec!["serde_derive".to_string()],
            features: vec!["derive".to_string()],
        }];

        let lockfile = LockFile::new(packages);
        let serialized = lockfile.to_string().unwrap();
        assert!(serialized.contains("serde"));
        assert!(serialized.contains("1.0.200"));
    }

    #[test]
    fn test_dependency_graph_topological_sort() {
        let mut graph = DependencyGraph::default();

        // a -> b -> c
        //  \-> d
        graph.add_edge("a", "b");
        graph.add_edge("b", "c");
        graph.add_edge("a", "d");

        let sorted = graph.topological_sort().unwrap();

        // c should come before b, b before a
        let pos_a = sorted.iter().position(|x| x == "a").unwrap();
        let pos_b = sorted.iter().position(|x| x == "b").unwrap();
        let pos_c = sorted.iter().position(|x| x == "c").unwrap();
        let pos_d = sorted.iter().position(|x| x == "d").unwrap();

        assert!(pos_c < pos_b);
        assert!(pos_b < pos_a);
        assert!(pos_d < pos_a);
    }

    #[test]
    fn test_dependency_graph_cycle_detection() {
        let mut graph = DependencyGraph::default();

        // a -> b -> c -> a (cycle)
        graph.add_edge("a", "b");
        graph.add_edge("b", "c");
        graph.add_edge("c", "a");

        assert!(graph.topological_sort().is_err());
    }

    #[test]
    fn test_normalize_version_req() {
        assert_eq!(normalize_version_req("1.2.3").unwrap(), "^1.2.3");
        assert_eq!(normalize_version_req("^1.2.3").unwrap(), "^1.2.3");
        assert_eq!(
            normalize_version_req(">=1.2.3, <2.0.0").unwrap(),
            ">=1.2.3, <2.0.0"
        );
        assert_eq!(normalize_version_req("*").unwrap(), "*");
        assert!(normalize_version_req("not-a-version").is_err());
    }
}
