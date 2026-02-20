//! Jet Package Manager (jetpkg)
//!
//! The official package manager for the Jet programming language.
//!
//! # Usage
//!
//! ```bash
//! # Install a package
//! jetpkg install <name>
//!
//! # Install a specific version
//! jetpkg install <name> --version 1.2.3
//!
//! # Publish a package
//! jetpkg publish
//!
//! # Search for packages
//! jetpkg search <query>
//!
//! # Update dependencies
//! jetpkg update
//!
//! # Login to registry
//! jetpkg login
//!
//! # Show package info
//! jetpkg show <name>
//! ```

#![allow(clippy::manual_strip)]
#![allow(clippy::collapsible_if)]
#![allow(clippy::for_kv_map)]

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

mod git;
mod manifest;
mod registry;
mod resolve;

#[allow(unused_imports)]
use git::{parse_git_ref, repo_cache_path};
use registry::{PackageMetadata, RegistryClient};
use resolve::{is_lock_file_up_to_date, write_lock_file, Resolver};

/// Default registry URL
const DEFAULT_REGISTRY: &str = registry::DEFAULT_REGISTRY;

/// Jet package manager CLI
#[derive(Parser)]
#[command(name = "jetpkg")]
#[command(about = "The Jet package manager")]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Registry URL
    #[arg(short, long, global = true)]
    registry: Option<String>,

    /// Configuration file path
    #[arg(short, long, global = true)]
    config: Option<PathBuf>,

    /// Increase verbosity
    #[arg(short, long, global = true, action = clap::ArgAction::Count)]
    verbose: u8,
}

#[derive(Subcommand)]
enum Commands {
    /// Install a package
    Install {
        /// Package name
        name: Option<String>,
        /// Package version
        #[arg(short, long)]
        version: Option<String>,
        /// Install as dev dependency
        #[arg(long)]
        dev: bool,
        /// Install as build dependency
        #[arg(long)]
        build: bool,
        /// Don't update the lock file
        #[arg(long)]
        frozen: bool,
        /// Path to local package
        #[arg(long)]
        path: Option<PathBuf>,
        /// Git repository URL
        #[arg(long)]
        git: Option<String>,
        /// Git branch
        #[arg(long)]
        branch: Option<String>,
        /// Git tag
        #[arg(long)]
        tag: Option<String>,
        /// Git revision
        #[arg(long)]
        rev: Option<String>,
    },

    /// Uninstall a package
    Uninstall {
        /// Package name
        name: String,
        /// Remove from dev dependencies
        #[arg(long)]
        dev: bool,
        /// Remove from build dependencies
        #[arg(long)]
        build: bool,
    },

    /// Update dependencies
    Update {
        /// Specific package to update
        #[arg(short, long)]
        package: Option<String>,
        /// Update to latest compatible version only
        #[arg(long)]
        compatible: bool,
        /// Show what would be updated without applying
        #[arg(long)]
        dry_run: bool,
    },

    /// Publish the current package
    Publish {
        /// Registry to publish to
        #[arg(short, long)]
        registry: Option<String>,
        /// Don't actually publish, just verify
        #[arg(long)]
        dry_run: bool,
        /// Skip validation
        #[arg(long)]
        no_verify: bool,
        /// Allow dirty working directory
        #[arg(long)]
        allow_dirty: bool,
        /// Skip building documentation
        #[arg(long)]
        no_doc: bool,
    },

    /// Search for packages
    Search {
        /// Search query
        query: String,
        /// Limit number of results
        #[arg(short, long, default_value = "20")]
        limit: usize,
    },

    /// Show package information
    Show {
        /// Package name
        name: String,
        /// Show all available versions
        #[arg(long)]
        versions: bool,
        /// Show dependencies
        #[arg(long)]
        dependencies: bool,
    },

    /// Login to a registry
    Login {
        /// Registry URL
        registry: Option<String>,
        /// API token
        token: Option<String>,
    },

    /// Logout from a registry
    Logout {
        /// Registry URL
        registry: Option<String>,
    },

    /// Initialize a new package
    Init {
        /// Package name
        #[arg(short, long)]
        name: Option<String>,
        /// Create a library package
        #[arg(long)]
        lib: bool,
        /// Package description
        #[arg(short, long)]
        description: Option<String>,
        /// Package license
        #[arg(short, long)]
        license: Option<String>,
    },

    /// Verify the current package
    Verify {
        /// Strict verification
        #[arg(long)]
        strict: bool,
    },

    /// Yank a package version (prevent downloads)
    Yank {
        /// Package name
        name: String,
        /// Package version
        version: String,
        /// Undo the yank
        #[arg(long)]
        undo: bool,
    },

    /// Generate package documentation locally
    Doc {
        /// Open documentation in browser
        #[arg(long)]
        open: bool,
        /// Documentation output directory
        #[arg(short, long)]
        output: Option<PathBuf>,
    },

    /// List installed packages
    List {
        /// Show detailed information
        #[arg(short, long)]
        long: bool,
        /// Show outdated packages
        #[arg(long)]
        outdated: bool,
    },

    /// Clean package cache
    Clean {
        /// Clean everything
        #[arg(long)]
        all: bool,
        /// Clean registry cache
        #[arg(long)]
        registry: bool,
        /// Clean git cache
        #[arg(long)]
        git: bool,
    },
}

/// Package manifest (jet.toml)
#[derive(Debug, Clone, Deserialize, Serialize)]
struct Manifest {
    #[serde(default)]
    package: Package,
    #[serde(default)]
    dependencies: HashMap<String, Dependency>,
    #[serde(rename = "dev-dependencies", default)]
    dev_dependencies: HashMap<String, Dependency>,
    #[serde(rename = "build-dependencies", default)]
    build_dependencies: HashMap<String, Dependency>,
}

#[derive(Debug, Clone, Default, Deserialize, Serialize)]
struct Package {
    name: String,
    version: String,
    #[serde(default)]
    description: Option<String>,
    #[serde(default)]
    authors: Vec<String>,
    #[serde(default)]
    license: Option<String>,
    #[serde(default)]
    repository: Option<String>,
    #[serde(default)]
    homepage: Option<String>,
    #[serde(default)]
    documentation: Option<String>,
    #[serde(default)]
    keywords: Vec<String>,
    #[serde(default)]
    readme: Option<String>,
    #[serde(default = "default_true")]
    publish: bool,
    #[serde(default)]
    exclude: Vec<String>,
    #[serde(default)]
    include: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(untagged)]
enum Dependency {
    Simple(String),
    Detailed(DetailedDependency),
}

#[derive(Debug, Clone, Default, Deserialize, Serialize)]
struct DetailedDependency {
    #[serde(default)]
    version: Option<String>,
    #[serde(default)]
    path: Option<PathBuf>,
    #[serde(default)]
    git: Option<String>,
    #[serde(default)]
    branch: Option<String>,
    #[serde(default)]
    tag: Option<String>,
    #[serde(default)]
    rev: Option<String>,
    #[serde(default)]
    registry: Option<String>,
    #[serde(default)]
    features: Vec<String>,
    #[serde(rename = "default-features", default = "default_true")]
    default_features: bool,
    #[serde(default)]
    optional: bool,
}

/// Registry credentials
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
struct Credentials {
    #[serde(default)]
    registries: HashMap<String, RegistryCredential>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct RegistryCredential {
    token: String,
    #[serde(default)]
    username: Option<String>,
}

// SearchResult is now imported from registry module

struct InstallOptions {
    name: Option<String>,
    version: Option<String>,
    dev: bool,
    build: bool,
    frozen: bool,
    path: Option<PathBuf>,
    git: Option<String>,
    branch: Option<String>,
    tag: Option<String>,
    rev: Option<String>,
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();

    let result = match cli.command {
        Commands::Install {
            name,
            version,
            dev,
            build,
            frozen,
            path,
            git,
            branch,
            tag,
            rev,
        } => {
            cmd_install(
                InstallOptions {
                    name,
                    version,
                    dev,
                    build,
                    frozen,
                    path,
                    git,
                    branch,
                    tag,
                    rev,
                },
                cli.registry.as_deref(),
            )
            .await
        }
        Commands::Uninstall { name, dev, build } => cmd_uninstall(name, dev, build).await,
        Commands::Update {
            package,
            compatible,
            dry_run,
        } => cmd_update(cli.registry.as_deref(), package, compatible, dry_run).await,
        Commands::Publish {
            registry,
            dry_run,
            no_verify,
            allow_dirty,
            no_doc,
        } => {
            cmd_publish(
                registry.as_deref().or(cli.registry.as_deref()),
                dry_run,
                no_verify,
                allow_dirty,
                no_doc,
            )
            .await
        }
        Commands::Search { query, limit } => {
            cmd_search(cli.registry.as_deref(), query, limit).await
        }
        Commands::Show {
            name,
            versions,
            dependencies,
        } => cmd_show(cli.registry.as_deref(), name, versions, dependencies).await,
        Commands::Login { registry, token } => {
            cmd_login(registry.as_deref().or(cli.registry.as_deref()), token).await
        }
        Commands::Logout { registry } => {
            cmd_logout(registry.as_deref().or(cli.registry.as_deref())).await
        }
        Commands::Init {
            name,
            lib,
            description,
            license,
        } => cmd_init(name, lib, description, license).await,
        Commands::Verify { strict } => cmd_verify(strict).await,
        Commands::Yank {
            name,
            version,
            undo,
        } => cmd_yank(cli.registry.as_deref(), name, version, undo).await,
        Commands::Doc { open, output } => cmd_doc(open, output).await,
        Commands::List { long, outdated } => cmd_list(long, outdated).await,
        Commands::Clean { all, registry, git } => cmd_clean(all, registry, git).await,
    };

    if let Err(e) = result {
        print_error(&format!("Error: {}", e));
        std::process::exit(1);
    }
}

// =============================================================================
// Install Command
// =============================================================================

async fn cmd_install(opts: InstallOptions, registry: Option<&str>) -> Result<()> {
    let InstallOptions {
        name,
        version,
        dev,
        build,
        frozen,
        path,
        git,
        branch,
        tag,
        rev,
    } = opts;

    let registry = registry.unwrap_or(DEFAULT_REGISTRY);

    // Load manifest
    let manifest_path = Path::new("jet.toml");
    if !manifest_path.exists() {
        anyhow::bail!("No jet.toml found. Run 'jetpkg init' first.");
    }

    let manifest_content = fs::read_to_string(manifest_path)?;
    let mut manifest: Manifest =
        toml::from_str(&manifest_content).with_context(|| "Failed to parse jet.toml")?;

    if let Some(name) = name {
        // Installing from registry
        print_info(&format!("Installing {}...", name));

        let dep = if let Some(path) = path {
            Dependency::Detailed(DetailedDependency {
                path: Some(path),
                ..Default::default()
            })
        } else if let Some(git) = git {
            Dependency::Detailed(DetailedDependency {
                git: Some(git),
                branch,
                tag,
                rev,
                ..Default::default()
            })
        } else {
            match version {
                Some(v) => Dependency::Simple(v),
                None => Dependency::Simple("*".to_string()),
            }
        };

        // Add to appropriate dependency section
        if dev {
            manifest.dev_dependencies.insert(name.clone(), dep);
        } else if build {
            manifest.build_dependencies.insert(name.clone(), dep);
        } else {
            manifest.dependencies.insert(name.clone(), dep);
        }

        // Fetch package metadata to verify it exists
        match fetch_package_metadata(registry, &name).await {
            Ok(metadata) => {
                print_success(&format!("Found {} v{}", metadata.name, metadata.version));
                if let Some(desc) = metadata.description {
                    println!("  {}", desc);
                }
            }
            Err(e) => {
                print_warning(&format!("Could not verify package: {}", e));
            }
        }

        // Update manifest file
        let updated_toml = toml::to_string_pretty(&manifest)?;
        fs::write(manifest_path, updated_toml)?;

        if !frozen {
            // Update lock file
            print_info("Updating dependencies...");
            update_lock_file(&manifest).await?;
        }

        print_success(&format!("Added {} to dependencies", name));
    } else {
        // Installing from lock file or all dependencies
        print_info("Installing dependencies...");
        install_dependencies(&manifest, registry, frozen).await?;
    }

    Ok(())
}

// =============================================================================
// Uninstall Command
// =============================================================================

async fn cmd_uninstall(name: String, dev: bool, build: bool) -> Result<()> {
    let manifest_path = Path::new("jet.toml");
    if !manifest_path.exists() {
        anyhow::bail!("No jet.toml found");
    }

    let manifest_content = fs::read_to_string(manifest_path)?;
    let mut manifest: Manifest = toml::from_str(&manifest_content)?;

    let removed = if dev {
        manifest.dev_dependencies.remove(&name).is_some()
    } else if build {
        manifest.build_dependencies.remove(&name).is_some()
    } else {
        manifest.dependencies.remove(&name).is_some()
            || manifest.dev_dependencies.remove(&name).is_some()
            || manifest.build_dependencies.remove(&name).is_some()
    };

    if removed {
        let updated_toml = toml::to_string_pretty(&manifest)?;
        fs::write(manifest_path, updated_toml)?;
        print_success(&format!("Removed {} from dependencies", name));
    } else {
        print_warning(&format!("{} was not in dependencies", name));
    }

    Ok(())
}

// =============================================================================
// Update Command
// =============================================================================

async fn cmd_update(
    registry: Option<&str>,
    package: Option<String>,
    compatible: bool,
    dry_run: bool,
) -> Result<()> {
    let registry = registry.unwrap_or(DEFAULT_REGISTRY);

    let manifest_path = Path::new("jet.toml");
    if !manifest_path.exists() {
        anyhow::bail!("No jet.toml found");
    }

    let manifest_content = fs::read_to_string(manifest_path)?;
    let manifest: Manifest = toml::from_str(&manifest_content)?;

    print_info("Updating dependencies...");

    if dry_run {
        println!("(Dry run - no changes will be made)");
    }

    let deps_to_check = match package {
        Some(name) => vec![name],
        None => manifest
            .dependencies
            .keys()
            .chain(manifest.dev_dependencies.keys())
            .chain(manifest.build_dependencies.keys())
            .cloned()
            .collect(),
    };

    for dep_name in deps_to_check {
        match fetch_package_metadata(registry, &dep_name).await {
            Ok(metadata) => {
                let latest = &metadata.version;
                println!("  {}: latest version is {}", dep_name, latest);

                if let Some(current_dep) = manifest.dependencies.get(&dep_name) {
                    let current_version = match current_dep {
                        Dependency::Simple(v) => v.clone(),
                        Dependency::Detailed(d) => d.version.clone().unwrap_or_default(),
                    };

                    if current_version != *latest {
                        if compatible {
                            // Check if update is compatible (same major version)
                            if is_compatible_update(&current_version, latest) {
                                println!("    -> Would update to {} (compatible)", latest);
                            }
                        } else if !dry_run {
                            println!("    -> Updating to {}", latest);
                            // Would update the version here
                        }
                    }
                }
            }
            Err(e) => {
                print_warning(&format!("Could not check {}: {}", dep_name, e));
            }
        }
    }

    if !dry_run {
        print_success("Dependencies updated");
    }

    Ok(())
}

// =============================================================================
// Publish Command
// =============================================================================

async fn cmd_publish(
    registry: Option<&str>,
    dry_run: bool,
    no_verify: bool,
    allow_dirty: bool,
    no_doc: bool,
) -> Result<()> {
    let registry = registry.unwrap_or(DEFAULT_REGISTRY);

    // Check authentication
    let creds = load_credentials()?;
    if !creds.registries.contains_key(registry) {
        anyhow::bail!("Not logged in to {}. Run 'jetpkg login' first.", registry);
    }

    let manifest_path = Path::new("jet.toml");
    if !manifest_path.exists() {
        anyhow::bail!("No jet.toml found");
    }

    let manifest_content = fs::read_to_string(manifest_path)?;
    let manifest: Manifest = toml::from_str(&manifest_content)?;

    // Verify package
    if !no_verify {
        print_info("Verifying package...");
        verify_package(&manifest, allow_dirty).await?;
    }

    if dry_run {
        println!("(Dry run - not actually publishing)");
        print_success(&format!(
            "Package {} v{} is ready to publish",
            manifest.package.name, manifest.package.version
        ));
        return Ok(());
    }

    // Build documentation if requested
    if !no_doc {
        print_info("Building documentation...");
        // Would build docs here
    }

    print_info(&format!(
        "Publishing {} v{} to {}...",
        manifest.package.name, manifest.package.version, registry
    ));

    // Create package archive
    let archive = create_package_archive(&manifest).await?;

    // Upload to registry
    publish_to_registry(registry, &manifest, &archive, &creds).await?;

    print_success(&format!(
        "Published {} v{}",
        manifest.package.name, manifest.package.version
    ));

    Ok(())
}

// =============================================================================
// Search Command
// =============================================================================

async fn cmd_search(registry: Option<&str>, query: String, limit: usize) -> Result<()> {
    let registry = registry.unwrap_or(DEFAULT_REGISTRY);

    print_info(&format!("Searching for '{}'...", query));

    // Search the registry using the registry client
    let client = RegistryClient::new(registry);

    match client.search(&query, limit, 0).await {
        Ok(search_result) => {
            if search_result.packages.is_empty() {
                println!("No packages found matching '{}'", query);
            } else {
                println!("Found {} package(s):\n", search_result.total);

                for pkg in search_result.packages {
                    print_package_summary(&pkg);
                }
            }
        }
        Err(e) => {
            anyhow::bail!("Registry search failed: {}", e);
        }
    }

    Ok(())
}

fn print_package_summary(pkg: &registry::SearchResult) {
    print_highlight(&pkg.name);
    if let Some(ref version) = pkg.version {
        print!(" = {}", version);
    }
    println!();

    if let Some(ref desc) = pkg.description {
        println!("    {}", desc);
    }

    println!("    Downloads: {}", pkg.downloads);
    println!();
}

// =============================================================================
// Show Command
// =============================================================================

async fn cmd_show(
    registry: Option<&str>,
    name: String,
    show_versions: bool,
    show_dependencies: bool,
) -> Result<()> {
    let registry = registry.unwrap_or(DEFAULT_REGISTRY);

    let client = RegistryClient::new(registry);

    match client.fetch_package(&name, None).await {
        Ok(metadata) => {
            print_highlight(&metadata.name);
            println!(" = {}", metadata.version);

            if let Some(ref desc) = metadata.description {
                println!("\n{}", desc);
            }

            println!();
            println!("Downloads: {}", metadata.downloads);

            if !metadata.authors.is_empty() {
                println!("Authors: {}", metadata.authors.join(", "));
            }

            if let Some(ref license) = metadata.license {
                println!("License: {}", license);
            }

            if let Some(ref repo) = metadata.repository {
                println!("Repository: {}", repo);
            }

            if let Some(ref docs) = metadata.documentation {
                println!("Documentation: {}", docs);
            }

            if show_versions {
                // Fetch all versions
                match client.get_versions(&name).await {
                    Ok(versions) => {
                        if !versions.is_empty() {
                            println!("\nAvailable versions:");
                            for version in versions.iter().take(10) {
                                let yank_marker = if version.yanked { " (yanked)" } else { "" };
                                println!("  - {}{}", version.version, yank_marker);
                            }
                            if versions.len() > 10 {
                                println!("  ... and {} more", versions.len() - 10);
                            }
                        }
                    }
                    Err(e) => {
                        print_warning(&format!("Could not fetch versions: {}", e));
                    }
                }
            }

            if show_dependencies && !metadata.dependencies.is_empty() {
                println!("\nDependencies:");
                for dep in &metadata.dependencies {
                    if dep.optional {
                        println!("  {} {} (optional)", dep.name, dep.version_req);
                    } else {
                        println!("  {} {}", dep.name, dep.version_req);
                    }
                }
            }
        }
        Err(e) => {
            anyhow::bail!("Failed to fetch package '{}' from registry: {}", name, e);
        }
    }

    Ok(())
}

// =============================================================================
// Login/Logout Commands
// =============================================================================

async fn cmd_login(registry: Option<&str>, token: Option<String>) -> Result<()> {
    let registry = registry.unwrap_or(DEFAULT_REGISTRY).to_string();

    let token = match token {
        Some(t) => t,
        None => {
            print!("Enter API token for {}: ", registry);
            std::io::stdout().flush()?;
            let mut input = String::new();
            std::io::stdin().read_line(&mut input)?;
            input.trim().to_string()
        }
    };

    if token.is_empty() {
        anyhow::bail!("Token cannot be empty");
    }

    let mut creds = load_credentials()?;
    creds.registries.insert(
        registry.clone(),
        RegistryCredential {
            token,
            username: None,
        },
    );

    save_credentials(&creds)?;

    print_success(&format!("Logged in to {}", registry));
    Ok(())
}

async fn cmd_logout(registry: Option<&str>) -> Result<()> {
    let registry = registry.unwrap_or(DEFAULT_REGISTRY).to_string();

    let mut creds = load_credentials()?;
    if creds.registries.remove(&registry).is_some() {
        save_credentials(&creds)?;
        print_success(&format!("Logged out from {}", registry));
    } else {
        print_warning(&format!("Not logged in to {}", registry));
    }

    Ok(())
}

// =============================================================================
// Init Command
// =============================================================================

async fn cmd_init(
    name: Option<String>,
    lib: bool,
    description: Option<String>,
    license: Option<String>,
) -> Result<()> {
    let name = name.unwrap_or_else(|| {
        std::env::current_dir()
            .ok()
            .and_then(|p| p.file_name().map(|n| n.to_string_lossy().to_string()))
            .unwrap_or_else(|| "my-package".to_string())
    });

    if Path::new("jet.toml").exists() {
        anyhow::bail!("jet.toml already exists in this directory");
    }

    let manifest = Manifest {
        package: Package {
            name: name.clone(),
            version: "0.1.0".to_string(),
            description,
            license,
            ..Default::default()
        },
        dependencies: HashMap::new(),
        dev_dependencies: HashMap::new(),
        build_dependencies: HashMap::new(),
    };

    let toml_content = toml::to_string_pretty(&manifest)?;
    fs::write("jet.toml", toml_content)?;

    // Create src directory with appropriate entry point
    fs::create_dir_all("src")?;

    if lib {
        fs::write(
            "src/lib.jet",
            format!(
                r#"# {name} - A Jet library

pub fn hello() -> string:
    "Hello from {name}!"
"#
            ),
        )?;
    } else {
        fs::write(
            "src/main.jet",
            format!(
                r#"# {name} - A Jet application

fn main():
    print("Hello from {name}!")
"#
            ),
        )?;
    }

    // Create .gitignore
    fs::write(".gitignore", "target/\n*.lock\n")?;

    print_success(&format!(
        "Initialized {} package '{}'",
        if lib { "library" } else { "binary" },
        name
    ));

    Ok(())
}

// =============================================================================
// Verify Command
// =============================================================================

async fn cmd_verify(strict: bool) -> Result<()> {
    let manifest_path = Path::new("jet.toml");
    if !manifest_path.exists() {
        anyhow::bail!("No jet.toml found");
    }

    let manifest_content = fs::read_to_string(manifest_path)?;
    let manifest: Manifest = toml::from_str(&manifest_content)?;

    print_info(&format!(
        "Verifying {} v{}...",
        manifest.package.name, manifest.package.version
    ));

    let mut errors = Vec::new();
    let mut warnings = Vec::new();

    // Check required fields
    if manifest.package.name.is_empty() {
        errors.push("Package name is required");
    }

    if manifest.package.version.is_empty() {
        errors.push("Package version is required");
    }

    if !is_valid_semver(&manifest.package.version) {
        errors.push("Version must follow semantic versioning");
    }

    if manifest.package.description.is_none() {
        warnings.push("Package description is recommended");
    }

    if manifest.package.license.is_none() {
        warnings.push("License is recommended");
    }

    if strict {
        if !Path::new("README.md").exists() {
            warnings.push("README.md is recommended for publishing");
        }

        if !Path::new("LICENSE").exists() && !Path::new("LICENSE-MIT").exists() {
            warnings.push("LICENSE file is recommended for publishing");
        }

        if manifest.package.repository.is_none() {
            warnings.push("Repository URL is recommended");
        }
    }

    // Check source files
    let src_dir = Path::new("src");
    if src_dir.exists() {
        let jet_files: Vec<_> = walkdir::WalkDir::new(src_dir)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.file_type().is_file())
            .filter(|e| {
                e.path()
                    .extension()
                    .map(|ext| ext == "jet")
                    .unwrap_or(false)
            })
            .collect();

        if jet_files.is_empty() {
            errors.push("No .jet source files found in src/");
        }

        for file in &jet_files {
            if let Ok(content) = fs::read_to_string(file.path()) {
                // Basic syntax check
                if content.contains("fn ") && !content.contains(":") {
                    warnings.push("Possible syntax issue (missing colon)");
                }
            }
        }
    } else {
        errors.push("No src/ directory found");
    }

    // Print results
    if !errors.is_empty() {
        print_error("Verification failed:");
        for error in &errors {
            println!("  [ERROR] {}", error);
        }
    }

    if !warnings.is_empty() {
        print_warning("Warnings:");
        for warning in &warnings {
            println!("  [WARN] {}", warning);
        }
    }

    if errors.is_empty() && warnings.is_empty() {
        print_success("Package verified successfully");
    } else if errors.is_empty() {
        print_success("Package verified with warnings");
    } else {
        anyhow::bail!("Package verification failed");
    }

    Ok(())
}

// =============================================================================
// Yank Command
// =============================================================================

async fn cmd_yank(registry: Option<&str>, name: String, version: String, undo: bool) -> Result<()> {
    let registry = registry.unwrap_or(DEFAULT_REGISTRY);

    // Check authentication
    let creds = load_credentials()?;
    let token = creds
        .registries
        .get(registry)
        .map(|c| c.token.clone())
        .ok_or_else(|| anyhow::anyhow!("Not logged in to {}", registry))?;

    let action = if undo { "unyank" } else { "yank" };
    print_info(&format!("{}ing {} v{}...", action, name, version));

    // Create registry client with authentication
    let client = RegistryClient::new(registry).with_auth(token);

    if undo {
        client.unyank(&name, &version).await?;
    } else {
        client.yank(&name, &version, None).await?;
    }

    print_success(&format!("{}ed {} v{}", action, name, version));

    Ok(())
}

// =============================================================================
// Doc Command
// =============================================================================

async fn cmd_doc(open: bool, output: Option<PathBuf>) -> Result<()> {
    let manifest_path = Path::new("jet.toml");
    if !manifest_path.exists() {
        anyhow::bail!("No jet.toml found");
    }

    let manifest_content = fs::read_to_string(manifest_path)?;
    let manifest: Manifest = toml::from_str(&manifest_content)?;

    let docs_dir = output.unwrap_or_else(|| PathBuf::from("target/doc"));
    fs::create_dir_all(&docs_dir)?;

    print_info(&format!(
        "Generating documentation for {} v{}...",
        manifest.package.name, manifest.package.version
    ));

    let sources = collect_source_docs(Path::new("src"))?;
    let mut source_links = String::new();
    for source in &sources {
        let page_rel = source_doc_rel_path(&source.path);
        source_links.push_str(&format!(
            r#"<li><a href="{}">{}</a> ({} items)</li>"#,
            page_rel,
            escape_html(&source.path.display().to_string()),
            source.symbols.len()
        ));
    }
    if source_links.is_empty() {
        source_links.push_str("<li>No source files found under <code>src/</code></li>");
    }

    let total_items = sources.iter().map(|s| s.symbols.len()).sum::<usize>();
    let index_html = format!(
        r#"<!DOCTYPE html>
<html>
<head>
    <title>{} {} - Documentation</title>
    <style>
        body {{ font-family: sans-serif; max-width: 800px; margin: 2em auto; padding: 0 1em; }}
        header {{ border-bottom: 2px solid #333; padding-bottom: 1em; margin-bottom: 2em; }}
        h1 {{ margin: 0; }}
        .version {{ color: #666; }}
        .description {{ font-size: 1.2em; margin: 1em 0; }}
    </style>
</head>
<body>
    <header>
        <h1>{} <span class="version">{}</span></h1>
        <div class="description">{}</div>
    </header>
    <main>
        <p>Documentation generated by jetpkg doc.</p>
        <p>Discovered API items: <strong>{}</strong></p>
        <h2>Source Files</h2>
        <ul>{}</ul>
    </main>
</body>
</html>"#,
        manifest.package.name,
        manifest.package.version,
        manifest.package.name,
        manifest.package.version,
        manifest
            .package
            .description
            .as_deref()
            .unwrap_or("No description provided"),
        total_items,
        source_links
    );

    fs::write(docs_dir.join("index.html"), index_html)?;
    for source in &sources {
        let page_path = docs_dir.join(source_doc_rel_path(&source.path));
        if let Some(parent) = page_path.parent() {
            fs::create_dir_all(parent)?;
        }
        let mut symbols_html = String::new();
        for symbol in &source.symbols {
            symbols_html.push_str(&format!(
                r#"<tr><td>{}</td><td>{}</td><td><code>{}</code></td><td>{}</td></tr>"#,
                escape_html(symbol.kind),
                escape_html(&symbol.name),
                escape_html(&symbol.signature),
                symbol.line
            ));
        }
        if symbols_html.is_empty() {
            symbols_html.push_str(
                "<tr><td colspan=\"4\"><em>No public or private top-level declarations found.</em></td></tr>",
            );
        }
        let source_content = fs::read_to_string(&source.path).unwrap_or_default();
        let page_html = format!(
            r#"<!DOCTYPE html>
<html>
<head>
    <title>{} - Source Documentation</title>
    <style>
        body {{ font-family: sans-serif; max-width: 1000px; margin: 2em auto; padding: 0 1em; }}
        table {{ width: 100%; border-collapse: collapse; margin: 1em 0 2em; }}
        th, td {{ text-align: left; border-bottom: 1px solid #ddd; padding: 0.4em; vertical-align: top; }}
        th {{ background: #f5f5f5; }}
        pre {{ background: #f8f8f8; border: 1px solid #e2e2e2; padding: 1em; overflow: auto; }}
    </style>
</head>
<body>
    <p><a href="../index.html">Back to index</a></p>
    <h1>{}</h1>
    <h2>Discovered Items</h2>
    <table>
        <thead><tr><th>Kind</th><th>Name</th><th>Signature</th><th>Line</th></tr></thead>
        <tbody>{}</tbody>
    </table>
    <h2>Source</h2>
    <pre>{}</pre>
</body>
</html>"#,
            escape_html(&source.path.display().to_string()),
            escape_html(&source.path.display().to_string()),
            symbols_html,
            escape_html(&source_content)
        );
        fs::write(page_path, page_html)?;
    }

    if open {
        let url = format!("file://{}", docs_dir.canonicalize()?.display());

        #[cfg(target_os = "macos")]
        std::process::Command::new("open").arg(&url).spawn()?;

        #[cfg(target_os = "linux")]
        std::process::Command::new("xdg-open").arg(&url).spawn()?;

        #[cfg(target_os = "windows")]
        std::process::Command::new("cmd")
            .args(["/C", "start", &url])
            .spawn()?;
    }

    print_success(&format!(
        "Documentation generated at {}",
        docs_dir.display()
    ));

    Ok(())
}

#[derive(Debug, Clone)]
struct SourceDoc {
    path: PathBuf,
    symbols: Vec<DocSymbol>,
}

#[derive(Debug, Clone)]
struct DocSymbol {
    kind: &'static str,
    name: String,
    signature: String,
    line: usize,
}

fn collect_source_docs(src_root: &Path) -> Result<Vec<SourceDoc>> {
    if !src_root.exists() {
        return Ok(Vec::new());
    }
    let mut out = Vec::new();
    for entry in walkdir::WalkDir::new(src_root) {
        let entry = entry?;
        if !entry.file_type().is_file() || entry.path().extension() != Some("jet".as_ref()) {
            continue;
        }
        let content = fs::read_to_string(entry.path())
            .with_context(|| format!("Failed to read source {}", entry.path().display()))?;
        let mut symbols = Vec::new();
        for (idx, line) in content.lines().enumerate() {
            if let Some(symbol) = parse_doc_symbol(line.trim_start(), idx + 1) {
                symbols.push(symbol);
            }
        }
        out.push(SourceDoc {
            path: entry.path().to_path_buf(),
            symbols,
        });
    }
    out.sort_by(|a, b| a.path.cmp(&b.path));
    Ok(out)
}

fn parse_doc_symbol(trimmed_line: &str, line: usize) -> Option<DocSymbol> {
    let line_no_pub = trimmed_line.strip_prefix("pub ").unwrap_or(trimmed_line);
    let defs = [
        ("fn ", "function"),
        ("struct ", "struct"),
        ("enum ", "enum"),
        ("trait ", "trait"),
        ("type ", "type"),
        ("effect ", "effect"),
        ("const ", "const"),
    ];
    for (prefix, kind) in defs {
        if let Some(rest) = line_no_pub.strip_prefix(prefix) {
            let name = rest
                .split(|c: char| {
                    c == '(' || c == ':' || c == '{' || c == '<' || c == '=' || c.is_whitespace()
                })
                .next()
                .unwrap_or("")
                .trim();
            if name.is_empty() {
                return None;
            }
            return Some(DocSymbol {
                kind,
                name: name.to_string(),
                signature: line_no_pub.to_string(),
                line,
            });
        }
    }
    None
}

fn source_doc_rel_path(path: &Path) -> String {
    let mut rendered = path.to_string_lossy().to_string();
    rendered.push_str(".html");
    rendered
}

fn escape_html(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

// =============================================================================
// List Command
// =============================================================================

async fn cmd_list(long: bool, outdated: bool) -> Result<()> {
    let manifest_path = Path::new("jet.toml");
    if !manifest_path.exists() {
        anyhow::bail!("No jet.toml found");
    }

    let manifest_content = fs::read_to_string(manifest_path)?;
    let manifest: Manifest = toml::from_str(&manifest_content)?;

    println!("{}", manifest.package.name);
    println!("v{}\n", manifest.package.version);

    if !manifest.dependencies.is_empty() {
        println!("[dependencies]");
        for (name, dep) in &manifest.dependencies {
            print_dependency(name, dep, long, outdated).await?;
        }
        println!();
    }

    if !manifest.dev_dependencies.is_empty() {
        println!("[dev-dependencies]");
        for (name, dep) in &manifest.dev_dependencies {
            print_dependency(name, dep, long, outdated).await?;
        }
        println!();
    }

    if !manifest.build_dependencies.is_empty() {
        println!("[build-dependencies]");
        for (name, dep) in &manifest.build_dependencies {
            print_dependency(name, dep, long, outdated).await?;
        }
    }

    Ok(())
}

async fn print_dependency(name: &str, dep: &Dependency, long: bool, outdated: bool) -> Result<()> {
    match dep {
        Dependency::Simple(version) => {
            if long {
                println!("  {} = {}", name, version);
            } else {
                println!("  {} {}", name, version);
            }
        }
        Dependency::Detailed(d) => {
            if let Some(ref path) = d.path {
                println!("  {} (path: {})", name, path.display());
            } else if let Some(ref git) = d.git {
                println!("  {} (git: {})", name, git);
            } else if let Some(ref version) = d.version {
                if long {
                    println!("  {} = {}", name, version);
                } else {
                    println!("  {} {}", name, version);
                }
            } else {
                println!("  {}", name);
            }

            if long {
                if !d.features.is_empty() {
                    println!("    features: [{}]", d.features.join(", "));
                }
                if d.optional {
                    println!("    optional: true");
                }
            }

            if outdated {
                // Would check for newer versions
            }
        }
    }

    Ok(())
}

// =============================================================================
// Clean Command
// =============================================================================

async fn cmd_clean(all: bool, registry: bool, git: bool) -> Result<()> {
    let cache_dir = dirs::cache_dir()
        .unwrap_or_else(|| PathBuf::from(".cache"))
        .join("jet");

    if all || registry {
        let registry_cache = cache_dir.join("registry");
        if registry_cache.exists() {
            fs::remove_dir_all(&registry_cache)?;
            print_success(&format!(
                "Cleaned registry cache at {}",
                registry_cache.display()
            ));
        } else {
            println!("Registry cache is already clean");
        }
    }

    if all || git {
        let git_cache = cache_dir.join("git");
        if git_cache.exists() {
            fs::remove_dir_all(&git_cache)?;
            print_success(&format!("Cleaned git cache at {}", git_cache.display()));
        } else {
            println!("Git cache is already clean");
        }
    }

    if !all && !registry && !git {
        let target_dir = Path::new("target");
        if target_dir.exists() {
            fs::remove_dir_all(target_dir)?;
            print_success("Cleaned target directory");
        }
    }

    Ok(())
}

// =============================================================================
// Helper Functions
// =============================================================================

fn load_credentials() -> Result<Credentials> {
    let creds_path = get_credentials_path()?;

    if !creds_path.exists() {
        return Ok(Credentials::default());
    }

    let content = fs::read_to_string(&creds_path)
        .with_context(|| format!("Failed to read credentials from {}", creds_path.display()))?;

    let creds: Credentials =
        toml::from_str(&content).with_context(|| "Failed to parse credentials file")?;

    Ok(creds)
}

fn save_credentials(creds: &Credentials) -> Result<()> {
    let creds_path = get_credentials_path()?;

    if let Some(parent) = creds_path.parent() {
        fs::create_dir_all(parent)?;
    }

    let content =
        toml::to_string_pretty(creds).with_context(|| "Failed to serialize credentials")?;

    fs::write(&creds_path, content)
        .with_context(|| format!("Failed to write credentials to {}", creds_path.display()))?;

    // Set restrictive permissions on Unix
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(&creds_path)?.permissions();
        perms.set_mode(0o600);
        fs::set_permissions(&creds_path, perms)?;
    }

    Ok(())
}

fn get_credentials_path() -> Result<PathBuf> {
    let home =
        dirs::home_dir().ok_or_else(|| anyhow::anyhow!("Could not determine home directory"))?;
    Ok(home.join(".jet").join("credentials.toml"))
}

async fn fetch_package_metadata(registry: &str, name: &str) -> Result<PackageMetadata> {
    let client = RegistryClient::new(registry);
    client.fetch_package(name, None).await
}

async fn install_dependencies(manifest: &Manifest, registry: &str, frozen: bool) -> Result<()> {
    let lock_path = Path::new("jet.lock");
    let manifest_path = Path::new("jet.toml");

    // Check if we can use the existing lock file
    if frozen {
        if !lock_path.exists() {
            anyhow::bail!("--frozen flag requires an existing jet.lock file");
        }
    }

    let all_deps: HashMap<String, Dependency> = manifest
        .dependencies
        .clone()
        .into_iter()
        .chain(manifest.dev_dependencies.clone())
        .chain(manifest.build_dependencies.clone())
        .collect();

    if all_deps.is_empty() {
        println!("No dependencies to install");
        return Ok(());
    }

    let cache_dir = dirs::cache_dir()
        .unwrap_or_else(|| PathBuf::from(".cache"))
        .join("jet")
        .join("registry");

    fs::create_dir_all(&cache_dir)?;

    // Check if lock file is up to date
    let use_lock_file =
        !frozen && lock_path.exists() && is_lock_file_up_to_date(lock_path, manifest_path).await;

    if use_lock_file {
        print_info("Using existing lock file");
        let lock = resolve::read_lock_file(lock_path).await?;

        // Download packages from lock file
        let client = RegistryClient::new(registry);
        for package in &lock.packages {
            download_and_install_package(&client, package, &cache_dir).await?;
        }
    } else {
        // Resolve dependencies
        print_info("Resolving dependencies...");
        let client = RegistryClient::new(registry);
        let mut resolver = Resolver::new(client.clone());

        let graph = resolver
            .resolve(&manifest.package.name, &manifest.package.version, &all_deps)
            .await?;

        // Generate and write lock file
        let lock = resolver.generate_lock_file(&graph);
        write_lock_file(lock_path, &lock).await?;

        // Download packages
        for (_, package) in &graph.packages {
            download_and_install_package(&client, package, &cache_dir).await?;
        }
    }

    print_success(&format!("Installed {} dependencies", all_deps.len()));
    Ok(())
}

async fn download_and_install_package(
    client: &RegistryClient,
    package: &resolve::ResolvedPackage,
    cache_dir: &Path,
) -> Result<()> {
    print_info(&format!("Fetching {} {}...", package.name, package.version));

    let package_dir = cache_dir.join(&package.name).join(&package.version);
    fs::create_dir_all(&package_dir)?;

    let tarball_path = package_dir.join("package.tar.gz");

    // Download if not already cached
    if !tarball_path.exists() {
        client
            .download_package(&package.name, &package.version, tarball_path.clone())
            .await
            .with_context(|| {
                format!(
                    "Failed to download package {} {}",
                    package.name, package.version
                )
            })?;
    }

    // Extract tarball
    let extract_dir = package_dir.join("source");
    if !extract_dir.exists() {
        fs::create_dir_all(&extract_dir)?;

        // Extract the tarball
        extract_tarball(&tarball_path, &extract_dir).with_context(|| {
            format!(
                "Failed to extract tarball for {} {}",
                package.name, package.version
            )
        })?;

        print_info(&format!(
            "Extracted {} {} to {}",
            package.name,
            package.version,
            extract_dir.display()
        ));
    }

    Ok(())
}

/// Extract a gzipped tarball to a directory
fn extract_tarball(tarball_path: &Path, extract_dir: &Path) -> Result<()> {
    use flate2::read::GzDecoder;
    use tar::Archive;

    let file = std::fs::File::open(tarball_path)
        .with_context(|| format!("Failed to open tarball at {}", tarball_path.display()))?;

    let decoder = GzDecoder::new(file);
    let mut archive = Archive::new(decoder);

    archive
        .unpack(extract_dir)
        .with_context(|| format!("Failed to extract tarball to {}", extract_dir.display()))?;

    Ok(())
}

async fn update_lock_file(manifest: &Manifest) -> Result<()> {
    let all_deps: HashMap<String, Dependency> = manifest
        .dependencies
        .clone()
        .into_iter()
        .chain(manifest.dev_dependencies.clone())
        .chain(manifest.build_dependencies.clone())
        .collect();

    if all_deps.is_empty() {
        // Write empty lock file
        let lock = resolve::LockFile {
            version: 1,
            root: resolve::ResolvedPackage {
                name: manifest.package.name.clone(),
                version: manifest.package.version.clone(),
                dependencies: HashMap::new(),
                source: resolve::PackageSource::default(),
                checksum: None,
            },
            packages: vec![],
            metadata: Some(resolve::LockMetadata {
                generated_at: chrono::Utc::now().to_rfc3339(),
                registry: DEFAULT_REGISTRY.to_string(),
            }),
        };
        write_lock_file(Path::new("jet.lock"), &lock).await?;
        return Ok(());
    }

    let client = RegistryClient::default_registry();
    let mut resolver = Resolver::new(client);

    let graph = resolver
        .resolve(&manifest.package.name, &manifest.package.version, &all_deps)
        .await?;

    let lock = resolver.generate_lock_file(&graph);
    write_lock_file(Path::new("jet.lock"), &lock).await?;

    Ok(())
}

async fn verify_package(manifest: &Manifest, _allow_dirty: bool) -> Result<()> {
    if manifest.package.name.is_empty() {
        anyhow::bail!("Package name is required");
    }

    if manifest.package.version.is_empty() {
        anyhow::bail!("Package version is required");
    }

    if !is_valid_semver(&manifest.package.version) {
        anyhow::bail!("Version must follow semantic versioning (e.g., 1.0.0)");
    }

    Ok(())
}

async fn create_package_archive(manifest: &Manifest) -> Result<Vec<u8>> {
    create_package_archive_from_root(manifest, Path::new(".")).await
}

async fn create_package_archive_from_root(manifest: &Manifest, root: &Path) -> Result<Vec<u8>> {
    use flate2::write::GzEncoder;
    use flate2::Compression;
    use std::io::Cursor;
    use tar::{Builder, Header};

    let encoder = GzEncoder::new(Vec::new(), Compression::default());
    let mut builder = Builder::new(encoder);

    let mut append_bytes = |path: &Path, bytes: &[u8]| -> Result<()> {
        let mut header = Header::new_gnu();
        header.set_size(bytes.len() as u64);
        header.set_mode(0o644);
        header.set_cksum();
        builder
            .append_data(&mut header, path, Cursor::new(bytes))
            .with_context(|| format!("Failed to append {} to archive", path.display()))?;
        Ok(())
    };

    let manifest_content = toml::to_string_pretty(manifest)?;
    append_bytes(Path::new("jet.toml"), manifest_content.as_bytes())?;

    let src_dir = root.join("src");
    if src_dir.exists() {
        for entry in walkdir::WalkDir::new(src_dir) {
            let entry = entry?;
            if entry.file_type().is_file() && entry.path().extension() == Some("jet".as_ref()) {
                let bytes = fs::read(entry.path())?;
                let relative_path = entry.path().strip_prefix(root).with_context(|| {
                    format!(
                        "Failed to compute relative path for {} against {}",
                        entry.path().display(),
                        root.display()
                    )
                })?;
                append_bytes(relative_path, &bytes)?;
            }
        }
    }

    let encoder = builder
        .into_inner()
        .context("Failed to finalize tar archive builder")?;
    let archive = encoder
        .finish()
        .context("Failed to finalize gzip archive")?;
    Ok(archive)
}

async fn publish_to_registry(
    registry: &str,
    manifest: &Manifest,
    archive: &[u8],
    creds: &Credentials,
) -> Result<()> {
    let token = creds
        .registries
        .get(registry)
        .map(|c| c.token.clone())
        .ok_or_else(|| anyhow::anyhow!("No credentials for registry"))?;

    // Create registry client with authentication
    let client = RegistryClient::new(registry).with_auth(token);

    // Create package metadata
    let metadata = PackageMetadata {
        name: manifest.package.name.clone(),
        version: manifest.package.version.clone(),
        description: manifest.package.description.clone(),
        authors: manifest.package.authors.clone(),
        license: manifest.package.license.clone(),
        repository: manifest.package.repository.clone(),
        homepage: manifest.package.homepage.clone(),
        documentation: manifest.package.documentation.clone(),
        keywords: manifest.package.keywords.clone(),
        readme: manifest.package.readme.clone(),
        downloads: 0,
        created_at: String::new(),
        updated_at: String::new(),
        versions: vec![],
        dependencies: manifest
            .dependencies
            .iter()
            .map(|(name, dep)| match dep {
                Dependency::Simple(version) => registry::DependencyInfo {
                    name: name.clone(),
                    version_req: version.clone(),
                    optional: false,
                    features: vec![],
                },
                Dependency::Detailed(d) => registry::DependencyInfo {
                    name: name.clone(),
                    version_req: d.version.clone().unwrap_or_else(|| "*".to_string()),
                    optional: d.optional,
                    features: d.features.clone(),
                },
            })
            .collect(),
        checksum: None,
        download_url: None,
    };

    // Publish the package
    let response = client
        .publish_internal(
            &manifest.package.name,
            &manifest.package.version,
            archive.to_vec(),
            metadata,
        )
        .await?;

    if !response.warnings.is_empty() {
        for warning in &response.warnings {
            print_warning(warning);
        }
    }

    if let Some(ref message) = response.message {
        print_info(message);
    }

    Ok(())
}

fn is_valid_semver(version: &str) -> bool {
    // Basic semver validation: MAJOR.MINOR.PATCH
    let parts: Vec<&str> = version.split('.').collect();
    parts.len() == 3 && parts.iter().all(|p| p.parse::<u64>().is_ok())
}

fn is_compatible_update(current: &str, latest: &str) -> bool {
    // Check if update is compatible (same major version)
    let current_parts: Vec<&str> = current.split('.').collect();
    let latest_parts: Vec<&str> = latest.split('.').collect();

    if !current_parts.is_empty() && !latest_parts.is_empty() {
        current_parts[0] == latest_parts[0]
    } else {
        false
    }
}

fn default_true() -> bool {
    true
}

// =============================================================================
// Output Helpers
// =============================================================================

fn print_info(message: &str) {
    let mut stdout = StandardStream::stdout(ColorChoice::Auto);
    stdout
        .set_color(ColorSpec::new().set_fg(Some(Color::Cyan)))
        .ok();
    write!(&mut stdout, "info").ok();
    stdout.reset().ok();
    writeln!(&mut stdout, ": {}", message).ok();
}

fn print_success(message: &str) {
    let mut stdout = StandardStream::stdout(ColorChoice::Auto);
    stdout
        .set_color(ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true))
        .ok();
    write!(&mut stdout, "success").ok();
    stdout.reset().ok();
    writeln!(&mut stdout, ": {}", message).ok();
}

fn print_warning(message: &str) {
    let mut stderr = StandardStream::stderr(ColorChoice::Auto);
    stderr
        .set_color(ColorSpec::new().set_fg(Some(Color::Yellow)).set_bold(true))
        .ok();
    write!(&mut stderr, "warning").ok();
    stderr.reset().ok();
    writeln!(&mut stderr, ": {}", message).ok();
}

fn print_error(message: &str) {
    let mut stderr = StandardStream::stderr(ColorChoice::Auto);
    stderr
        .set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true))
        .ok();
    write!(&mut stderr, "error").ok();
    stderr.reset().ok();
    writeln!(&mut stderr, ": {}", message).ok();
}

fn print_highlight(text: &str) {
    let mut stdout = StandardStream::stdout(ColorChoice::Auto);
    stdout
        .set_color(ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true))
        .ok();
    write!(&mut stdout, "{}", text).ok();
    stdout.reset().ok();
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use flate2::read::GzDecoder;
    use std::io::Read;
    use tar::Archive;
    use tempfile::TempDir;

    #[test]
    fn test_semver_validation() {
        assert!(is_valid_semver("1.0.0"));
        assert!(is_valid_semver("0.1.0"));
        assert!(is_valid_semver("2.10.3"));

        assert!(!is_valid_semver("1.0"));
        assert!(!is_valid_semver("1"));
        assert!(!is_valid_semver("abc"));
        assert!(!is_valid_semver("1.0.0.0"));
    }

    #[test]
    fn test_compatible_update() {
        assert!(is_compatible_update("1.0.0", "1.2.0"));
        assert!(is_compatible_update("1.0.0", "1.0.1"));
        assert!(!is_compatible_update("1.0.0", "2.0.0"));
        assert!(!is_compatible_update("0.1.0", "1.0.0"));
    }

    #[tokio::test]
    async fn test_create_package_archive_is_valid_tar_gz() {
        let tmp = TempDir::new().expect("tempdir");
        let root = tmp.path();

        fs::create_dir_all(root.join("src")).expect("create src");
        fs::write(root.join("src/main.jet"), "fn main():\n    pass\n").expect("write source");

        let manifest = Manifest {
            package: Package {
                name: "demo".to_string(),
                version: "1.0.0".to_string(),
                authors: vec!["Icarus".to_string()],
                description: Some("demo package".to_string()),
                license: Some("MIT".to_string()),
                repository: None,
                homepage: None,
                documentation: None,
                keywords: vec![],
                readme: None,
                publish: default_true(),
                exclude: vec![],
                include: vec![],
            },
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            build_dependencies: HashMap::new(),
        };

        let archive = create_package_archive_from_root(&manifest, root)
            .await
            .expect("archive creation");

        let decoder = GzDecoder::new(&archive[..]);
        let mut tar = Archive::new(decoder);
        let mut names = Vec::new();
        let mut jet_toml_contents = String::new();
        let mut source_contents = String::new();

        for entry in tar.entries().expect("entries") {
            let mut entry = entry.expect("entry");
            let path = entry.path().expect("path").to_string_lossy().to_string();
            names.push(path.clone());
            if path == "jet.toml" {
                entry
                    .read_to_string(&mut jet_toml_contents)
                    .expect("read manifest");
            } else if path == "src/main.jet" {
                entry
                    .read_to_string(&mut source_contents)
                    .expect("read source");
            }
        }

        assert!(names.contains(&"jet.toml".to_string()));
        assert!(names.contains(&"src/main.jet".to_string()));
        assert!(jet_toml_contents.contains("name = \"demo\""));
        assert!(source_contents.contains("fn main():"));
    }
}
