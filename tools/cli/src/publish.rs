//! Package publishing for the Jet package manager (jetpkg)
//!
//! This module provides functionality for:
//! - Validating packages before publishing
//! - Creating package archives
//! - Uploading to registries
//! - Managing authentication
//! - Version verification

#![allow(dead_code)]

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::io::Cursor;
use std::path::{Path, PathBuf};
use std::time::Duration;

use crate::manifest::Manifest;
use crate::project::Project;

/// Registry configuration
#[allow(dead_code)]
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct RegistryConfig {
    /// Registry name
    pub name: String,
    /// Registry URL
    pub url: String,
    /// Authentication token
    #[serde(skip_serializing)]
    pub token: Option<String>,
}

/// Package validation result
#[derive(Debug, Clone)]
pub struct ValidationResult {
    /// Whether validation passed
    pub valid: bool,
    /// Validation errors
    pub errors: Vec<String>,
    /// Validation warnings
    pub warnings: Vec<String>,
}

/// Publish configuration
#[derive(Debug, Clone, Default)]
pub struct PublishConfig {
    /// Registry to publish to
    pub registry: Option<String>,
    /// Whether to perform a dry run
    pub dry_run: bool,
    /// Whether to allow uncommitted changes
    pub allow_dirty: bool,
    /// Whether to skip verification
    pub no_verify: bool,
    /// Authentication token
    pub token: Option<String>,
    /// Whether to be verbose
    pub verbose: bool,
}

/// Credentials stored in ~/.jet/credentials.toml
#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct Credentials {
    /// Registry tokens
    #[serde(default)]
    pub registries: HashMap<String, RegistryCredential>,
}

/// Credentials for a specific registry
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct RegistryCredential {
    /// Authentication token
    pub token: String,
}

/// The package publisher
pub struct Publisher {
    /// Publish configuration
    config: PublishConfig,
    /// Credentials
    credentials: Credentials,
}

/// Package archive (tar.gz)
pub struct PackageArchive {
    /// Package name
    pub name: String,
    /// Package version
    pub version: String,
    /// Archive data
    pub data: Vec<u8>,
    /// Files included
    pub files: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct RegistrySearchResponse {
    #[serde(default)]
    crates: Vec<RegistrySearchEntry>,
    #[serde(default)]
    packages: Vec<RegistrySearchEntry>,
}

#[derive(Debug, Deserialize)]
struct RegistrySearchEntry {
    name: String,
    #[serde(default)]
    description: Option<String>,
    #[serde(default)]
    max_version: Option<String>,
    #[serde(default)]
    version: Option<String>,
    #[serde(default)]
    downloads: u64,
}

fn registry_client() -> Result<reqwest::Client> {
    reqwest::Client::builder()
        .timeout(Duration::from_secs(20))
        .user_agent(format!("jet-cli/{}", env!("CARGO_PKG_VERSION")))
        .build()
        .context("failed to create registry HTTP client")
}

impl Publisher {
    /// Create a new publisher
    pub fn new(config: PublishConfig) -> Result<Self> {
        let credentials = Self::load_credentials()?;

        Ok(Self {
            config,
            credentials,
        })
    }

    /// Load credentials from ~/.jet/credentials.toml
    fn load_credentials() -> Result<Credentials> {
        let credentials_path = Self::credentials_path()?;

        if !credentials_path.exists() {
            return Ok(Credentials::default());
        }

        let content = fs::read_to_string(&credentials_path).with_context(|| {
            format!(
                "Failed to read credentials from {}",
                credentials_path.display()
            )
        })?;

        let credentials: Credentials =
            toml::from_str(&content).context("Failed to parse credentials.toml")?;

        Ok(credentials)
    }

    /// Save credentials to ~/.jet/credentials.toml
    fn save_credentials(&self) -> Result<()> {
        let credentials_path = Self::credentials_path()?;

        // Ensure directory exists
        if let Some(parent) = credentials_path.parent() {
            fs::create_dir_all(parent)?;
        }

        let content =
            toml::to_string_pretty(&self.credentials).context("Failed to serialize credentials")?;

        fs::write(&credentials_path, content).with_context(|| {
            format!(
                "Failed to write credentials to {}",
                credentials_path.display()
            )
        })?;

        // Set restrictive permissions on Unix
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = fs::metadata(&credentials_path)?.permissions();
            perms.set_mode(0o600);
            fs::set_permissions(&credentials_path, perms)?;
        }

        Ok(())
    }

    /// Get the path to the credentials file
    fn credentials_path() -> Result<PathBuf> {
        let home = dirs::home_dir()
            .ok_or_else(|| anyhow::anyhow!("Could not determine home directory"))?;
        Ok(home.join(".jet").join("credentials.toml"))
    }

    /// Login to a registry
    pub fn login(&mut self, registry: &str, token: &str) -> Result<()> {
        self.credentials.registries.insert(
            registry.to_string(),
            RegistryCredential {
                token: token.to_string(),
            },
        );

        self.save_credentials()?;

        println!("Logged in to registry: {}", registry);
        Ok(())
    }

    /// Logout from a registry
    pub fn logout(&mut self, registry: &str) -> Result<()> {
        self.credentials.registries.remove(registry);
        self.save_credentials()?;

        println!("Logged out from registry: {}", registry);
        Ok(())
    }

    /// Publish a package
    pub async fn publish(&self, project: &Project) -> Result<()> {
        // Validate the package
        if !self.config.no_verify {
            let validation = self.validate_package(project)?;
            if !validation.valid {
                println!("Package validation failed:");
                for error in &validation.errors {
                    println!("  error: {}", error);
                }
                anyhow::bail!("Cannot publish invalid package");
            }

            if !validation.warnings.is_empty() {
                println!("Warnings:");
                for warning in &validation.warnings {
                    println!("  warning: {}", warning);
                }
            }
        }

        // Check for uncommitted changes
        if !self.config.allow_dirty {
            self.check_git_status(&project.root)?;
        }

        // Get registry URL
        let registry = self
            .config
            .registry
            .clone()
            .or_else(|| std::env::var("JET_REGISTRY").ok())
            .unwrap_or_else(|| "https://crates.jet-lang.org".to_string());

        // Get authentication token
        let token = self
            .config
            .token
            .clone()
            .or_else(|| {
                self.credentials
                    .registries
                    .get(&registry)
                    .map(|c| c.token.clone())
            })
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "No authentication token for registry {}. Run `jet login` first.",
                    registry
                )
            })?;

        // Create package archive
        let archive = self.create_archive(project)?;

        println!(
            "Publishing {} v{} to {}",
            archive.name, archive.version, registry
        );
        println!("  Included files: {}", archive.files.len());

        if self.config.dry_run {
            println!("  (Dry run - not actually publishing)");
            return Ok(());
        }

        // Upload to registry
        self.upload_package(&registry, &token, &archive).await?;

        println!(
            "Successfully published {} v{}",
            archive.name, archive.version
        );

        Ok(())
    }

    /// Validate a package before publishing
    fn validate_package(&self, project: &Project) -> Result<ValidationResult> {
        let mut errors = Vec::new();
        let mut warnings = Vec::new();

        let manifest = &project.manifest;

        // Check required fields
        if manifest.package.name.is_empty() {
            errors.push("Package name is required".to_string());
        }

        if manifest.package.version.is_empty() {
            errors.push("Package version is required".to_string());
        }

        if manifest.package.description.is_none() {
            warnings.push("Package description is recommended".to_string());
        }

        if manifest.package.license.is_none() && manifest.package.license_file.is_none() {
            warnings.push("Package should have a license".to_string());
        }

        if manifest.package.repository.is_none() {
            warnings.push("Repository URL is recommended".to_string());
        }

        // Check for valid version
        if !is_valid_semver(&manifest.package.version) {
            errors.push(format!(
                "Invalid version: {}. Must follow semantic versioning.",
                manifest.package.version
            ));
        }

        // Check for README
        let readme_path = project.root.join("README.md");
        if !readme_path.exists() {
            warnings.push("README.md is recommended".to_string());
        }

        // Check for LICENSE
        let license_path = project.root.join("LICENSE");
        let license_mit = project.root.join("LICENSE-MIT");
        let license_apache = project.root.join("LICENSE-APACHE");
        if !license_path.exists() && !license_mit.exists() && !license_apache.exists() {
            warnings.push("LICENSE file is recommended".to_string());
        }

        // Check that source files exist
        if project.source_files.is_empty() {
            errors.push("No source files found".to_string());
        }

        // Check that entry point exists
        if project.entry_point.is_none() {
            errors.push("No entry point found (main.jet or lib.jet)".to_string());
        }

        // Check for publish flag
        if !manifest.package.publish {
            errors.push("Package has publish = false in manifest".to_string());
        }

        // Try to parse as valid Jet code
        for source_file in &project.source_files {
            let source = fs::read_to_string(&source_file.path)?;

            // Quick lex check
            let mut lexer = jet_lexer::Lexer::new(&source);
            let tokens = lexer.tokenize();

            for token in &tokens {
                if let jet_lexer::Token::Error(ref msg) = token.token {
                    errors.push(format!(
                        "Lex error in {}: {}",
                        source_file.relative_path.display(),
                        msg
                    ));
                }
            }

            // Quick parse check
            let mut parser = jet_parser::Parser::new(tokens);
            if let Err(e) = parser.parse_module() {
                errors.push(format!(
                    "Parse error in {}: {:?}",
                    source_file.relative_path.display(),
                    e
                ));
            }
        }

        let valid = errors.is_empty();

        Ok(ValidationResult {
            valid,
            errors,
            warnings,
        })
    }

    /// Check git status for uncommitted changes
    fn check_git_status(&self, project_root: &Path) -> Result<()> {
        let git_dir = project_root.join(".git");

        if !git_dir.exists() {
            // Not a git repository, that's fine
            return Ok(());
        }

        // Check for uncommitted changes using git status --porcelain
        let output = std::process::Command::new("git")
            .args(["status", "--porcelain"])
            .current_dir(project_root)
            .output()
            .context("Failed to run git status")?;

        if !output.stdout.is_empty() {
            anyhow::bail!(
                "Uncommitted changes detected. Use --allow-dirty to publish anyway.\n{}",
                String::from_utf8_lossy(&output.stdout)
            );
        }

        // Check if we're on a tag that matches the version
        let manifest_version = &self.read_manifest(project_root)?.package.version;

        let tag_output = std::process::Command::new("git")
            .args(["describe", "--tags", "--exact-match"])
            .current_dir(project_root)
            .output()?;

        if tag_output.status.success() {
            let tag = String::from_utf8_lossy(&tag_output.stdout)
                .trim()
                .to_string();
            let expected_tag = format!("v{}", manifest_version);

            if tag != expected_tag {
                println!(
                    "  Warning: Current tag ({}) doesn't match manifest version ({})",
                    tag, expected_tag
                );
            }
        } else {
            println!("  Warning: Not on a tag matching v{}", manifest_version);
        }

        Ok(())
    }

    /// Create a package archive
    fn create_archive(&self, project: &Project) -> Result<PackageArchive> {
        let mut files = Vec::new();
        let mut archive_data = Vec::new();

        // Collect files to include
        let _manifest_path = project.root.join("jet.toml");
        files.push("jet.toml".to_string());

        // Source files
        for source_file in &project.source_files {
            let relative = source_file
                .path
                .strip_prefix(&project.root)
                .unwrap_or(&source_file.path)
                .to_string_lossy()
                .to_string();
            files.push(relative);
        }

        // README
        let readme_path = project.root.join("README.md");
        if readme_path.exists() {
            files.push("README.md".to_string());
        }

        // LICENSE files
        for license_name in &["LICENSE", "LICENSE-MIT", "LICENSE-APACHE", "COPYING"] {
            let license_path = project.root.join(license_name);
            if license_path.exists() {
                files.push(license_name.to_string());
            }
        }

        // Build script
        if let Some(ref build_script) = project.build_script {
            let relative = build_script
                .strip_prefix(&project.root)
                .unwrap_or(build_script)
                .to_string_lossy()
                .to_string();
            files.push(relative);
        }

        // Create tar.gz archive
        {
            let encoder =
                flate2::write::GzEncoder::new(&mut archive_data, flate2::Compression::default());
            let mut tar = tar::Builder::new(encoder);

            for file in &files {
                let file_path = project.root.join(file);
                if file_path.exists() {
                    tar.append_path_with_name(&file_path, file)?;
                }
            }

            // Finish the archive
            let encoder = tar.into_inner()?;
            encoder.finish()?;
        }

        Ok(PackageArchive {
            name: project.name().to_string(),
            version: project.version().to_string(),
            data: archive_data,
            files,
        })
    }

    /// Upload a package to the registry
    async fn upload_package(
        &self,
        registry: &str,
        token: &str,
        archive: &PackageArchive,
    ) -> Result<()> {
        let upload_url = format!("{}/api/v1/crates/new", registry);

        if self.config.verbose {
            println!("  Uploading to: {}", upload_url);
            println!("  Package size: {} bytes", archive.data.len());
        }

        let client = registry_client()?;
        let response = client
            .put(&upload_url)
            .header("Authorization", format!("Bearer {}", token))
            .header("Content-Type", "application/gzip")
            .body(archive.data.clone())
            .send()
            .await
            .context("failed to upload package to registry")?;
        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            anyhow::bail!("upload failed: {} {}", status, body);
        }

        Ok(())
    }

    /// Read the manifest from a project root
    fn read_manifest(&self, project_root: &Path) -> Result<Manifest> {
        let manifest_path = project_root.join("jet.toml");
        Manifest::from_file(&manifest_path)
    }

    fn config(&self) -> &PublishConfig {
        &self.config
    }
}

/// Check if a version string is valid semver
fn is_valid_semver(version: &str) -> bool {
    // Basic semver check: MAJOR.MINOR.PATCH[-PRERELEASE][+BUILD]
    let parts: Vec<&str> = version.split(&['-', '+'][..]).collect();
    let version_part = parts[0];

    let nums: Vec<&str> = version_part.split('.').collect();
    if nums.len() != 3 {
        return false;
    }

    nums.iter().all(|n| n.parse::<u64>().is_ok())
}

/// Yank a package version
pub async fn yank_package(
    registry: &str,
    token: &str,
    name: &str,
    version: &str,
    undo: bool,
) -> Result<()> {
    let action = if undo { "unyank" } else { "yank" };

    println!("{} {} v{} from {}", action, name, version, registry);
    let client = registry_client()?;
    let url = format!("{}/api/v1/crates/{}/{}/{}", registry, name, version, action);
    let response = client
        .put(&url)
        .header("Authorization", format!("Bearer {}", token))
        .send()
        .await
        .with_context(|| format!("failed to send {} request", action))?;
    if !response.status().is_success() {
        let status = response.status();
        let body = response.text().await.unwrap_or_default();
        anyhow::bail!("{} failed: {} {}", action, status, body);
    }

    Ok(())
}

/// Search for packages in the registry
pub async fn search_packages(
    registry: &str,
    query: &str,
    limit: usize,
) -> Result<Vec<SearchResult>> {
    println!("Searching for '{}' in {}...", query, registry);
    let client = registry_client()?;
    let url = format!("{}/api/v1/crates", registry.trim_end_matches('/'));
    let response = client
        .get(&url)
        .query(&[
            ("q", query.to_string()),
            ("per_page", limit.max(1).to_string()),
        ])
        .send()
        .await
        .context("failed to query registry search endpoint")?;
    if !response.status().is_success() {
        let status = response.status();
        let body = response.text().await.unwrap_or_default();
        anyhow::bail!("search failed: {} {}", status, body);
    }
    let parsed: RegistrySearchResponse = response
        .json()
        .await
        .context("failed to decode search response")?;
    let entries = if parsed.crates.is_empty() {
        parsed.packages
    } else {
        parsed.crates
    };
    Ok(entries
        .into_iter()
        .map(|entry| SearchResult {
            name: entry.name,
            version: entry
                .version
                .or(entry.max_version)
                .unwrap_or_else(|| "unknown".to_string()),
            description: entry.description,
            downloads: entry.downloads,
        })
        .collect())
}

/// Search result
#[derive(Debug, Clone)]
pub struct SearchResult {
    pub name: String,
    pub version: String,
    pub description: Option<String>,
    pub downloads: u64,
}

/// Install a package from the registry
pub async fn install_package(
    registry: &str,
    name: &str,
    version: Option<&str>,
    install_dir: &Path,
) -> Result<()> {
    let metadata = get_package_metadata(registry, name).await?;
    let selected_version = version
        .map(ToOwned::to_owned)
        .or_else(|| metadata.versions.first().cloned())
        .ok_or_else(|| anyhow::anyhow!("No versions available for package '{}'", name))?;
    println!(
        "Installing {} {} from {}...",
        name, selected_version, registry
    );

    let client = registry_client()?;
    let download_url = format!(
        "{}/api/v1/crates/{}/{}/download",
        registry.trim_end_matches('/'),
        name,
        selected_version
    );
    let response = client
        .get(&download_url)
        .send()
        .await
        .with_context(|| format!("failed to download package from {}", download_url))?;
    if !response.status().is_success() {
        let status = response.status();
        let body = response.text().await.unwrap_or_default();
        anyhow::bail!("download failed: {} {}", status, body);
    }
    let bytes = response
        .bytes()
        .await
        .context("failed to read downloaded package bytes")?;

    let package_dir = install_dir.join(name);
    fs::create_dir_all(&package_dir)?;
    let tar = flate2::read::GzDecoder::new(Cursor::new(bytes));
    let mut archive = tar::Archive::new(tar);
    archive
        .unpack(&package_dir)
        .with_context(|| format!("failed to extract package into {}", package_dir.display()))?;

    println!("  Installed to: {}", package_dir.display());

    Ok(())
}

/// Get package metadata from registry
pub async fn get_package_metadata(registry: &str, name: &str) -> Result<PackageMetadata> {
    let client = registry_client()?;
    let url = format!("{}/api/v1/crates/{}", registry.trim_end_matches('/'), name);
    let response = client
        .get(&url)
        .send()
        .await
        .with_context(|| format!("failed to query package metadata at {}", url))?;
    if !response.status().is_success() {
        let status = response.status();
        let body = response.text().await.unwrap_or_default();
        anyhow::bail!("metadata query failed: {} {}", status, body);
    }
    let body: serde_json::Value = response
        .json()
        .await
        .context("failed to decode package metadata response")?;
    let crate_obj = body.get("crate").unwrap_or(&body);
    let versions = body
        .get("versions")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|entry| {
                    entry
                        .get("num")
                        .and_then(|n| n.as_str())
                        .or_else(|| entry.get("version").and_then(|n| n.as_str()))
                        .map(ToOwned::to_owned)
                })
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();
    Ok(PackageMetadata {
        name: crate_obj
            .get("name")
            .and_then(|v| v.as_str())
            .unwrap_or(name)
            .to_string(),
        versions,
        description: crate_obj
            .get("description")
            .and_then(|v| v.as_str())
            .map(ToOwned::to_owned),
        repository: crate_obj
            .get("repository")
            .and_then(|v| v.as_str())
            .map(ToOwned::to_owned),
        documentation: crate_obj
            .get("documentation")
            .and_then(|v| v.as_str())
            .map(ToOwned::to_owned),
        downloads: crate_obj
            .get("downloads")
            .and_then(|v| v.as_u64())
            .unwrap_or(0),
        authors: crate_obj
            .get("authors")
            .and_then(|v| v.as_array())
            .map(|arr| {
                arr.iter()
                    .filter_map(|a| a.as_str().map(ToOwned::to_owned))
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default(),
        license: crate_obj
            .get("license")
            .and_then(|v| v.as_str())
            .map(ToOwned::to_owned),
    })
}

/// Package metadata from registry
#[derive(Debug, Clone)]
pub struct PackageMetadata {
    pub name: String,
    pub versions: Vec<String>,
    pub description: Option<String>,
    pub repository: Option<String>,
    pub documentation: Option<String>,
    pub downloads: u64,
    pub authors: Vec<String>,
    pub license: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn create_test_project(dir: &TempDir) -> Project {
        let project_dir = dir.path().join("test-pkg");
        let src_dir = project_dir.join("src");
        fs::create_dir_all(&src_dir).unwrap();

        // Create jet.toml
        fs::write(
            project_dir.join("jet.toml"),
            r#"[package]
name = "test-pkg"
version = "1.0.0"
edition = "2024"
description = "A test package"
license = "MIT"
"#,
        )
        .unwrap();

        // Create main.jet
        fs::write(
            src_dir.join("main.jet"),
            "fn main():\n    print(\"Hello\")\n",
        )
        .unwrap();

        // Create README
        fs::write(project_dir.join("README.md"), "# Test Package\n").unwrap();

        Project::discover(&project_dir).unwrap()
    }

    #[test]
    fn test_semver_validation() {
        assert!(is_valid_semver("1.0.0"));
        assert!(is_valid_semver("0.1.0"));
        assert!(is_valid_semver("1.2.3-alpha"));
        assert!(is_valid_semver("1.0.0+build123"));

        assert!(!is_valid_semver("1.0"));
        assert!(!is_valid_semver("1"));
        assert!(!is_valid_semver("abc"));
    }

    #[test]
    fn test_package_validation() {
        let temp_dir = TempDir::new().unwrap();
        let project = create_test_project(&temp_dir);

        let config = PublishConfig::default();
        let publisher = Publisher::new(config).unwrap();

        let result = publisher.validate_package(&project).unwrap();

        assert!(result.valid, "Validation failed: {:?}", result.errors);
        assert!(result.errors.is_empty());
    }

    #[test]
    fn test_create_archive() {
        let temp_dir = TempDir::new().unwrap();
        let project = create_test_project(&temp_dir);

        let config = PublishConfig::default();
        let publisher = Publisher::new(config).unwrap();

        let archive = publisher.create_archive(&project).unwrap();

        assert_eq!(archive.name, "test-pkg");
        assert_eq!(archive.version, "1.0.0");
        assert!(!archive.files.is_empty());
        assert!(archive.files.contains(&"jet.toml".to_string()));
        assert!(!archive.data.is_empty());
    }
}
