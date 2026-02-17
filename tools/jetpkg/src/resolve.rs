//! Dependency resolution for the Jet package manager
//!
//! Implements SemVer-compatible dependency resolution with conflict resolution
//! and lock file generation.

use crate::registry::{PackageMetadata, RegistryClient};
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::path::Path;

/// A resolved dependency graph
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ResolvedGraph {
    /// Map of package names to resolved versions
    pub packages: HashMap<String, ResolvedPackage>,
    /// Root package name
    pub root: String,
    /// Root package version
    pub root_version: String,
}

/// A resolved package
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolvedPackage {
    /// Package name
    pub name: String,
    /// Resolved version
    pub version: String,
    /// Dependencies (package name -> version requirement)
    pub dependencies: HashMap<String, String>,
    /// Source information
    pub source: PackageSource,
    /// Checksum for verification
    #[serde(skip_serializing_if = "Option::is_none")]
    pub checksum: Option<String>,
}

/// Package source
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum PackageSource {
    /// From registry
    Registry { url: String },
    /// From git repository
    Git { url: String, reference: String },
    /// From local path
    Path { path: String },
}

impl Default for PackageSource {
    fn default() -> Self {
        PackageSource::Registry {
            url: crate::registry::DEFAULT_REGISTRY.to_string(),
        }
    }
}

/// Lock file format
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockFile {
    /// Lock file version
    pub version: u32,
    /// Root package
    pub root: ResolvedPackage,
    /// Resolved dependencies
    pub packages: Vec<ResolvedPackage>,
    /// Metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<LockMetadata>,
}

/// Lock file metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockMetadata {
    /// When the lock file was generated
    pub generated_at: String,
    /// Which registry was used
    pub registry: String,
}

/// Dependency resolver
pub struct Resolver {
    /// Registry client
    registry: RegistryClient,
    /// Cache of fetched packages
    package_cache: HashMap<String, PackageMetadata>,
    /// Resolution constraints
    constraints: HashMap<String, Vec<VersionReq>>,
    /// Already resolved packages
    resolved: HashMap<String, String>,
    /// Resolution stack (for detecting cycles)
    resolution_stack: Vec<String>,
}

/// Version requirement
#[derive(Debug, Clone)]
struct VersionReq {
    /// The requirement string (e.g., "^1.0.0", "~>2.0", ">=1.0.0 <2.0.0")
    req: String,
    /// Which package required this
    #[allow(dead_code)]
    required_by: String,
}

/// SemVer version
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Default)]
struct SemVer {
    major: u64,
    minor: u64,
    patch: u64,
    pre: Option<String>,
    build: Option<String>,
}

impl Resolver {
    /// Create a new resolver
    pub fn new(registry: RegistryClient) -> Self {
        Self {
            registry,
            package_cache: HashMap::new(),
            constraints: HashMap::new(),
            resolved: HashMap::new(),
            resolution_stack: Vec::new(),
        }
    }

    /// Resolve dependencies for a package
    pub async fn resolve(
        &mut self,
        root_name: &str,
        root_version: &str,
        dependencies: &HashMap<String, crate::Dependency>,
    ) -> Result<ResolvedGraph> {
        // Clear state
        self.constraints.clear();
        self.resolved.clear();
        self.resolution_stack.clear();

        // Collect all constraints
        self.collect_constraints(root_name, dependencies).await?;

        // Resolve each package
        let mut packages = HashMap::new();

        // Collect constraints first to avoid borrow issues
        let constraints: Vec<(String, Vec<VersionReq>)> = self
            .constraints
            .iter()
            .filter(|(name, _)| *name != root_name)
            .map(|(name, reqs)| (name.clone(), reqs.clone()))
            .collect();

        for (name, reqs) in constraints {
            let version = self.resolve_package(&name, &reqs).await?;
            let package = self.fetch_package(&name, &version).await?;

            let deps: HashMap<String, String> = package
                .dependencies
                .iter()
                .map(|d| (d.name.clone(), d.version_req.clone()))
                .collect();

            packages.insert(
                name.clone(),
                ResolvedPackage {
                    name: name.clone(),
                    version: version.clone(),
                    dependencies: deps,
                    source: PackageSource::Registry {
                        url: self.registry.base_url().to_string(),
                    },
                    checksum: package.checksum,
                },
            );

            self.resolved.insert(name.clone(), version);
        }

        Ok(ResolvedGraph {
            packages,
            root: root_name.to_string(),
            root_version: root_version.to_string(),
        })
    }

    /// Collect constraints from dependencies
    async fn collect_constraints(
        &mut self,
        _root_name: &str,
        dependencies: &HashMap<String, crate::Dependency>,
    ) -> Result<()> {
        let mut to_process: Vec<(String, String, String)> = Vec::new();

        // Add root dependencies
        for (name, dep) in dependencies {
            let version_req = match dep {
                crate::Dependency::Simple(v) => v.clone(),
                crate::Dependency::Detailed(d) => {
                    if let Some(v) = &d.version {
                        v.clone()
                    } else {
                        "*".to_string()
                    }
                }
            };

            to_process.push((name.clone(), version_req, "root".to_string()));
        }

        // Process dependencies recursively
        let mut processed = HashSet::new();

        while let Some((name, version_req, required_by)) = to_process.pop() {
            if processed.contains(&(name.clone(), version_req.clone())) {
                continue;
            }

            processed.insert((name.clone(), version_req.clone()));

            // Add constraint
            self.constraints
                .entry(name.clone())
                .or_default()
                .push(VersionReq {
                    req: version_req.clone(),
                    required_by: required_by.clone(),
                });

            // Fetch package to get its dependencies
            let versions = self.registry.get_versions(&name).await?;

            // Find best matching version
            let semver_req = parse_semver_req(&version_req)?;
            let matching_version = versions
                .into_iter()
                .filter(|v| {
                    parse_semver(&v.version)
                        .map(|sv| semver_req.matches(&sv))
                        .unwrap_or(false)
                })
                .max_by(|a, b| {
                    parse_semver(&a.version)
                        .unwrap_or_default()
                        .cmp(&parse_semver(&b.version).unwrap_or_default())
                });

            if let Some(version_info) = matching_version {
                let package = self.fetch_package(&name, &version_info.version).await?;

                // Add transitive dependencies
                for dep in &package.dependencies {
                    to_process.push((dep.name.clone(), dep.version_req.clone(), name.clone()));
                }
            }
        }

        Ok(())
    }

    /// Resolve a single package version from constraints
    async fn resolve_package(&self, name: &str, reqs: &[VersionReq]) -> Result<String> {
        // Fetch available versions
        let versions = self.registry.get_versions(name).await?;

        // Parse all requirements
        let semver_reqs: Vec<SemverReq> = reqs
            .iter()
            .map(|r| parse_semver_req(&r.req))
            .collect::<Result<Vec<_>>>()?;

        // Find the best version that satisfies all requirements
        let mut candidates: Vec<SemVer> = versions
            .into_iter()
            .filter_map(|v| parse_semver(&v.version).ok())
            .filter(|sv| semver_reqs.iter().all(|r| r.matches(sv)))
            .collect();

        // Sort by version (highest first)
        candidates.sort_by(|a, b| b.cmp(a));

        // Prefer stable versions over pre-release
        let stable: Vec<_> = candidates
            .iter()
            .filter(|v| v.pre.is_none())
            .cloned()
            .collect();

        let best = if !stable.is_empty() {
            stable.first().unwrap()
        } else {
            candidates.first().ok_or_else(|| {
                anyhow::anyhow!(
                    "No version of {} satisfies all requirements: {:?}",
                    name,
                    reqs
                )
            })?
        };

        Ok(format!("{}.{}{}", best.major, best.minor, best.patch))
    }

    /// Fetch package metadata (with caching)
    async fn fetch_package(&mut self, name: &str, version: &str) -> Result<PackageMetadata> {
        let key = format!("{}@{}", name, version);

        if let Some(cached) = self.package_cache.get(&key) {
            return Ok(cached.clone());
        }

        let package = self
            .registry
            .fetch_package(name, Some(version))
            .await
            .with_context(|| format!("Failed to fetch package {} {}", name, version))?;

        self.package_cache.insert(key, package.clone());
        Ok(package)
    }

    /// Generate a lock file from the resolved graph
    pub fn generate_lock_file(&self, graph: &ResolvedGraph) -> LockFile {
        let packages: Vec<ResolvedPackage> = graph.packages.values().cloned().collect();

        LockFile {
            version: 1,
            root: ResolvedPackage {
                name: graph.root.clone(),
                version: graph.root_version.clone(),
                dependencies: HashMap::new(),
                source: PackageSource::default(),
                checksum: None,
            },
            packages,
            metadata: Some(LockMetadata {
                generated_at: chrono::Utc::now().to_rfc3339(),
                registry: self.registry.base_url().to_string(),
            }),
        }
    }
}

/// Parse a SemVer version string
fn parse_semver(version: &str) -> Result<SemVer> {
    let version = version.trim();

    // Handle pre-release and build metadata
    let (version, pre) = if let Some(idx) = version.find('-') {
        let (v, p) = version.split_at(idx);
        (v, Some(p[1..].to_string()))
    } else {
        (version, None)
    };

    let (version, build) = if let Some(idx) = version.find('+') {
        let (v, b) = version.split_at(idx);
        (v, Some(b[1..].to_string()))
    } else {
        (version, None)
    };

    let parts: Vec<&str> = version.split('.').collect();

    if parts.len() != 3 {
        anyhow::bail!("Invalid semver format: {}", version);
    }

    let major = parts[0].parse().context("Invalid major version")?;
    let minor = parts[1].parse().context("Invalid minor version")?;
    let patch = parts[2].parse().context("Invalid patch version")?;

    Ok(SemVer {
        major,
        minor,
        patch,
        pre,
        build,
    })
}

/// SemVer requirement
#[derive(Debug, Clone)]
enum SemverReq {
    /// Exact version (=1.2.3)
    Exact(SemVer),
    /// Greater than or equal (>=1.2.3)
    Gte(SemVer),
    /// Less than (<1.2.3)
    Lt(SemVer),
    /// Greater than (>1.2.3)
    Gt(SemVer),
    /// Less than or equal (<=1.2.3)
    Lte(SemVer),
    /// Tilde range (~>1.2.0)
    Tilde(SemVer),
    /// Caret range (^1.2.0)
    Caret(SemVer),
    /// Wildcard (*)
    Wildcard,
    /// Range combination
    Range(Box<SemverReq>, Box<SemverReq>),
}

impl SemverReq {
    /// Check if a version matches this requirement
    fn matches(&self, version: &SemVer) -> bool {
        match self {
            SemverReq::Exact(req) => version == req,
            SemverReq::Gte(req) => version >= req,
            SemverReq::Lt(req) => version < req,
            SemverReq::Gt(req) => version > req,
            SemverReq::Lte(req) => version <= req,
            SemverReq::Tilde(req) => {
                // ~>1.2.3 matches >=1.2.3 and <1.3.0
                // ~>1.2 matches >=1.2.0 and <1.3.0
                // ~>1 matches >=1.0.0 and <2.0.0
                version >= req
                    && version.major == req.major
                    && (req.minor != 0 || version.minor == req.minor)
            }
            SemverReq::Caret(req) => {
                // ^1.2.3 matches >=1.2.3 and <2.0.0
                // ^0.2.3 matches >=0.2.3 and <0.3.0
                // ^0.0.3 matches >=0.0.3 and <0.0.4
                version >= req
                    && (version.major != 0
                        || (req.minor == 0 && version.minor == 0)
                        || (version.minor == req.minor && version.patch >= req.patch))
            }
            SemverReq::Wildcard => true,
            SemverReq::Range(left, right) => left.matches(version) && right.matches(version),
        }
    }
}

/// Parse a SemVer requirement string
fn parse_semver_req(req: &str) -> Result<SemverReq> {
    let req = req.trim();

    if req == "*" || req.is_empty() {
        return Ok(SemverReq::Wildcard);
    }

    // Handle ranges with space (e.g., ">=1.0.0 <2.0.0")
    if req.contains(' ') {
        let parts: Vec<&str> = req.split_whitespace().collect();
        if parts.len() == 2 {
            let left = parse_semver_req(parts[0])?;
            let right = parse_semver_req(parts[1])?;
            return Ok(SemverReq::Range(Box::new(left), Box::new(right)));
        }
    }

    // Handle prefix operators
    if req.starts_with("~=") {
        let version = parse_semver(&req[2..])?;
        return Ok(SemverReq::Tilde(version));
    }

    if req.starts_with('^') {
        let version = parse_semver(&req[1..])?;
        return Ok(SemverReq::Caret(version));
    }

    if req.starts_with(">=") {
        let version = parse_semver(&req[2..])?;
        return Ok(SemverReq::Gte(version));
    }

    if req.starts_with("<=") {
        let version = parse_semver(&req[2..])?;
        return Ok(SemverReq::Lte(version));
    }

    if req.starts_with('~') {
        let version = parse_semver(&req[1..])?;
        return Ok(SemverReq::Tilde(version));
    }

    if req.starts_with('>') {
        let version = parse_semver(&req[1..])?;
        return Ok(SemverReq::Gt(version));
    }

    if req.starts_with('<') {
        let version = parse_semver(&req[1..])?;
        return Ok(SemverReq::Lt(version));
    }

    if req.starts_with('=') {
        let version = parse_semver(&req[1..])?;
        return Ok(SemverReq::Exact(version));
    }

    // Plain version is treated as caret (^) for compatibility
    let version = parse_semver(req)?;
    Ok(SemverReq::Caret(version))
}

/// Write a lock file to disk
pub async fn write_lock_file(path: &Path, lock: &LockFile) -> Result<()> {
    let content = toml::to_string_pretty(lock).context("Failed to serialize lock file")?;

    tokio::fs::write(path, content)
        .await
        .context("Failed to write lock file")?;

    Ok(())
}

/// Read a lock file from disk
pub async fn read_lock_file(path: &Path) -> Result<LockFile> {
    let content = tokio::fs::read_to_string(path)
        .await
        .context("Failed to read lock file")?;

    let lock: LockFile = toml::from_str(&content).context("Failed to parse lock file")?;

    Ok(lock)
}

/// Check if a lock file exists and is up to date
pub async fn is_lock_file_up_to_date(lock_path: &Path, manifest_path: &Path) -> bool {
    let lock_modified = match tokio::fs::metadata(lock_path).await {
        Ok(m) => m.modified().ok(),
        Err(_) => return false,
    };

    let manifest_modified = match tokio::fs::metadata(manifest_path).await {
        Ok(m) => m.modified().ok(),
        Err(_) => return false,
    };

    match (lock_modified, manifest_modified) {
        (Some(l), Some(m)) => l >= m,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_semver() {
        let v = parse_semver("1.2.3").unwrap();
        assert_eq!(v.major, 1);
        assert_eq!(v.minor, 2);
        assert_eq!(v.patch, 3);
        assert!(v.pre.is_none());

        let v = parse_semver("1.2.3-alpha").unwrap();
        assert_eq!(v.pre, Some("alpha".to_string()));

        let v = parse_semver("1.2.3+build123").unwrap();
        assert_eq!(v.build, Some("build123".to_string()));
    }

    #[test]
    fn test_semver_comparison() {
        let v1 = parse_semver("1.0.0").unwrap();
        let v2 = parse_semver("1.0.1").unwrap();
        let v3 = parse_semver("1.1.0").unwrap();
        let v4 = parse_semver("2.0.0").unwrap();

        assert!(v1 < v2);
        assert!(v2 < v3);
        assert!(v3 < v4);
    }

    #[test]
    fn test_parse_semver_req() {
        assert!(matches!(
            parse_semver_req("*").unwrap(),
            SemverReq::Wildcard
        ));
        assert!(matches!(parse_semver_req("").unwrap(), SemverReq::Wildcard));

        let req = parse_semver_req("^1.0.0").unwrap();
        assert!(req.matches(&parse_semver("1.0.0").unwrap()));
        assert!(req.matches(&parse_semver("1.5.0").unwrap()));
        assert!(!req.matches(&parse_semver("2.0.0").unwrap()));

        let req = parse_semver_req(">=1.0.0").unwrap();
        assert!(req.matches(&parse_semver("1.0.0").unwrap()));
        assert!(req.matches(&parse_semver("2.0.0").unwrap()));
        assert!(!req.matches(&parse_semver("0.9.0").unwrap()));
    }

    #[test]
    fn test_semver_req_caret() {
        let req = parse_semver_req("^1.2.3").unwrap();

        assert!(req.matches(&parse_semver("1.2.3").unwrap()));
        assert!(req.matches(&parse_semver("1.3.0").unwrap()));
        assert!(req.matches(&parse_semver("1.9.9").unwrap()));
        assert!(!req.matches(&parse_semver("2.0.0").unwrap()));
        assert!(!req.matches(&parse_semver("1.2.2").unwrap()));
    }

    #[test]
    fn test_semver_req_tilde() {
        let req = parse_semver_req("~>1.2.3").unwrap();

        assert!(req.matches(&parse_semver("1.2.3").unwrap()));
        assert!(req.matches(&parse_semver("1.2.9").unwrap()));
        assert!(!req.matches(&parse_semver("1.3.0").unwrap()));
        assert!(!req.matches(&parse_semver("2.0.0").unwrap()));
    }

    #[test]
    fn test_lock_file_serialization() {
        let lock = LockFile {
            version: 1,
            root: ResolvedPackage {
                name: "my-package".to_string(),
                version: "0.1.0".to_string(),
                dependencies: HashMap::new(),
                source: PackageSource::default(),
                checksum: None,
            },
            packages: vec![ResolvedPackage {
                name: "dep1".to_string(),
                version: "1.0.0".to_string(),
                dependencies: HashMap::new(),
                source: PackageSource::Registry {
                    url: "https://crates.jet-lang.org".to_string(),
                },
                checksum: Some("abc123".to_string()),
            }],
            metadata: Some(LockMetadata {
                generated_at: "2024-01-01T00:00:00Z".to_string(),
                registry: "https://crates.jet-lang.org".to_string(),
            }),
        };

        let toml_str = toml::to_string_pretty(&lock).unwrap();
        assert!(toml_str.contains("version = 1"));
        assert!(toml_str.contains("my-package"));
        assert!(toml_str.contains("dep1"));
    }
}
