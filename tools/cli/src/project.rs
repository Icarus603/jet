//! Project structure detection and management
//!
//! This module provides functionality for:
//! - Detecting project structure (binary, library, workspace)
//! - Managing source files
//! - Finding entry points
//! - Workspace member discovery

#![allow(dead_code)]

use anyhow::{Context, Result};
use std::path::{Path, PathBuf};

use crate::manifest::Manifest;

/// Type of Jet project
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProjectType {
    /// Binary executable project
    Binary,
    /// Library project
    Library,
    /// Workspace root (contains multiple packages)
    #[allow(dead_code)]
    Workspace,
}

/// Information about a Jet project
#[derive(Debug, Clone)]
pub struct Project {
    /// Project root directory
    #[allow(dead_code)]
    pub root: PathBuf,
    /// Project manifest
    #[allow(dead_code)]
    pub manifest: Manifest,
    /// Type of project
    pub project_type: ProjectType,
    /// Source directory
    pub src_dir: PathBuf,
    /// Entry point file (main.jet or lib.jet)
    pub entry_point: Option<PathBuf>,
    /// All source files
    pub source_files: Vec<SourceFile>,
    /// Test files
    pub test_files: Vec<PathBuf>,
    /// Example files
    pub example_files: Vec<PathBuf>,
    /// Benchmark files
    pub bench_files: Vec<PathBuf>,
    /// Build script (build.jet)
    pub build_script: Option<PathBuf>,
}

/// Information about a source file
#[derive(Debug, Clone)]
pub struct SourceFile {
    /// Absolute path to the file
    pub path: PathBuf,
    /// Path relative to src_dir
    pub relative_path: PathBuf,
    /// Module name derived from path
    pub module_name: String,
    /// Whether this is the main entry point
    pub is_main: bool,
    /// Whether this is the library entry point
    pub is_lib: bool,
    /// Whether this is a test file
    pub is_test: bool,
    /// Whether this is an example
    pub is_example: bool,
    /// Whether this is a benchmark
    pub is_bench: bool,
}

/// Workspace information
#[derive(Debug, Clone)]
pub struct Workspace {
    /// Workspace root directory
    #[allow(dead_code)]
    pub root: PathBuf,
    /// Workspace manifest
    #[allow(dead_code)]
    pub manifest: Manifest,
    /// Workspace members
    pub members: Vec<Project>,
    /// Member paths (relative to root)
    #[allow(dead_code)]
    pub member_paths: Vec<PathBuf>,
}

impl Project {
    /// Discover a project from a directory
    pub fn discover(dir: &Path) -> Result<Self> {
        let manifest_path = dir.join("jet.toml");
        if !manifest_path.exists() {
            anyhow::bail!(
                "No jet.toml found in {}. Use 'jet new' to create a new project.",
                dir.display()
            );
        }

        let manifest = Manifest::from_file(&manifest_path)
            .with_context(|| format!("Failed to read manifest at {}", manifest_path.display()))?;

        Self::from_manifest(dir, manifest)
    }

    /// Create a project from a manifest
    pub fn from_manifest(root: &Path, manifest: Manifest) -> Result<Self> {
        let src_dir = root.join("src");
        let project_type = Self::detect_project_type(&src_dir)?;

        let mut project = Project {
            root: root.to_path_buf(),
            manifest,
            project_type,
            src_dir: src_dir.clone(),
            entry_point: None,
            source_files: vec![],
            test_files: vec![],
            example_files: vec![],
            bench_files: vec![],
            build_script: Self::find_build_script(root),
        };

        // Discover source files
        project.discover_source_files()?;

        // Find entry point
        project.entry_point = project.find_entry_point();

        // Discover tests, examples, and benchmarks
        project.discover_tests()?;
        project.discover_examples()?;
        project.discover_benchmarks()?;

        Ok(project)
    }

    /// Detect the project type based on source files
    fn detect_project_type(src_dir: &Path) -> Result<ProjectType> {
        if !src_dir.exists() {
            return Ok(ProjectType::Binary); // Default to binary
        }

        let main_jet = src_dir.join("main.jet");
        let lib_jet = src_dir.join("lib.jet");

        if main_jet.exists() && lib_jet.exists() {
            // Both exist - this is a binary with a library
            Ok(ProjectType::Binary)
        } else if lib_jet.exists() {
            Ok(ProjectType::Library)
        } else if main_jet.exists() {
            Ok(ProjectType::Binary)
        } else {
            // Look for any .jet files
            let has_jet_files = std::fs::read_dir(src_dir)?.filter_map(|e| e.ok()).any(|e| {
                e.path()
                    .extension()
                    .map(|ext| ext == "jet")
                    .unwrap_or(false)
            });

            if has_jet_files {
                Ok(ProjectType::Binary)
            } else {
                anyhow::bail!("No .jet source files found in src/")
            }
        }
    }

    /// Find the build script if it exists
    fn find_build_script(root: &Path) -> Option<PathBuf> {
        let build_jet = root.join("build.jet");
        if build_jet.exists() {
            Some(build_jet)
        } else {
            None
        }
    }

    /// Discover all source files in the project
    fn discover_source_files(&mut self) -> Result<()> {
        if !self.src_dir.exists() {
            return Ok(());
        }

        self.source_files = Self::collect_jet_files(&self.src_dir, &self.src_dir)?;

        // Mark entry points
        for file in &mut self.source_files {
            let file_name = file.path.file_stem().and_then(|s| s.to_str()).unwrap_or("");

            file.is_main = file_name == "main";
            file.is_lib = file_name == "lib";
        }

        Ok(())
    }

    /// Collect all .jet files recursively
    fn collect_jet_files(dir: &Path, src_dir: &Path) -> Result<Vec<SourceFile>> {
        let mut files = Vec::new();

        if !dir.exists() {
            return Ok(files);
        }

        for entry in walkdir::WalkDir::new(dir) {
            let entry = entry?;
            if entry.file_type().is_file() {
                if let Some(ext) = entry.path().extension() {
                    if ext == "jet" {
                        let path = entry.path().to_path_buf();
                        let relative_path =
                            path.strip_prefix(src_dir).unwrap_or(&path).to_path_buf();

                        let module_name = Self::path_to_module_name(&relative_path);

                        files.push(SourceFile {
                            path,
                            relative_path,
                            module_name,
                            is_main: false,
                            is_lib: false,
                            is_test: false,
                            is_example: false,
                            is_bench: false,
                        });
                    }
                }
            }
        }

        Ok(files)
    }

    /// Convert a file path to a module name
    fn path_to_module_name(path: &Path) -> String {
        let mut components: Vec<String> = Vec::new();

        for component in path.components() {
            if let Some(s) = component.as_os_str().to_str() {
                if s.ends_with(".jet") {
                    components.push(s.trim_end_matches(".jet").to_string());
                } else {
                    components.push(s.to_string());
                }
            }
        }

        components.join("::")
    }

    /// Find the main entry point
    fn find_entry_point(&self) -> Option<PathBuf> {
        match self.project_type {
            ProjectType::Binary => {
                let main = self.src_dir.join("main.jet");
                if main.exists() {
                    Some(main)
                } else {
                    // Find any file with a main function
                    self.source_files.first().map(|f| f.path.clone())
                }
            }
            ProjectType::Library => {
                let lib = self.src_dir.join("lib.jet");
                if lib.exists() {
                    Some(lib)
                } else {
                    self.source_files.first().map(|f| f.path.clone())
                }
            }
            ProjectType::Workspace => None,
        }
    }

    /// Discover test files
    fn discover_tests(&mut self) -> Result<()> {
        let tests_dir = self.root.join("tests");
        if tests_dir.exists() {
            self.test_files = Self::collect_jet_files(&tests_dir, &tests_dir)?
                .into_iter()
                .map(|f| f.path)
                .collect();
        }
        Ok(())
    }

    /// Discover example files
    fn discover_examples(&mut self) -> Result<()> {
        let examples_dir = self.root.join("examples");
        if examples_dir.exists() {
            self.example_files = Self::collect_jet_files(&examples_dir, &examples_dir)?
                .into_iter()
                .map(|f| f.path)
                .collect();
        }
        Ok(())
    }

    /// Discover benchmark files
    fn discover_benchmarks(&mut self) -> Result<()> {
        let benches_dir = self.root.join("benches");
        if benches_dir.exists() {
            self.bench_files = Self::collect_jet_files(&benches_dir, &benches_dir)?
                .into_iter()
                .map(|f| f.path)
                .collect();
        }
        Ok(())
    }

    /// Get all source files (excluding tests, examples, benches)
    pub fn lib_source_files(&self) -> Vec<&SourceFile> {
        self.source_files
            .iter()
            .filter(|f| !f.is_test && !f.is_example && !f.is_bench)
            .collect()
    }

    /// Get the package name
    pub fn name(&self) -> &str {
        &self.manifest.package.name
    }

    /// Get the package version
    pub fn version(&self) -> &str {
        &self.manifest.package.version
    }

    /// Get the target directory
    pub fn target_dir(&self) -> PathBuf {
        self.root.join(self.manifest.target_dir())
    }

    /// Get the output directory for a profile
    pub fn output_dir(&self, profile: &str) -> PathBuf {
        self.target_dir().join(profile)
    }

    /// Get the path to the compiled executable
    pub fn executable_path(&self, profile: &str) -> PathBuf {
        let exe_name = if cfg!(windows) {
            format!("{}.exe", self.name())
        } else {
            self.name().to_string()
        };

        self.output_dir(profile).join(exe_name)
    }

    /// Check if this project has tests
    pub fn has_tests(&self) -> bool {
        !self.test_files.is_empty()
    }

    /// Check if this project has examples
    #[allow(dead_code)]
    pub fn has_examples(&self) -> bool {
        !self.example_files.is_empty()
    }

    /// Check if this project has benchmarks
    #[allow(dead_code)]
    pub fn has_benchmarks(&self) -> bool {
        !self.bench_files.is_empty()
    }

    /// Get all files that need to be compiled for the library
    #[allow(dead_code)]
    pub fn lib_files(&self) -> Vec<PathBuf> {
        self.lib_source_files()
            .into_iter()
            .map(|f| f.path.clone())
            .collect()
    }

    /// Get all files that need to be compiled for a binary
    #[allow(dead_code)]
    pub fn bin_files(&self) -> Vec<PathBuf> {
        self.source_files.iter().map(|f| f.path.clone()).collect()
    }
}

impl Workspace {
    /// Discover a workspace from a directory
    pub fn discover(dir: &Path) -> Result<Self> {
        let manifest_path = dir.join("jet.toml");
        if !manifest_path.exists() {
            anyhow::bail!("No jet.toml found in {}", dir.display());
        }

        let manifest = Manifest::from_file(&manifest_path)?;

        if !manifest.is_workspace() {
            anyhow::bail!("{} is not a workspace root", dir.display());
        }

        Self::from_manifest(dir, manifest)
    }

    /// Create a workspace from a manifest
    pub fn from_manifest(root: &Path, manifest: Manifest) -> Result<Self> {
        let workspace_config = manifest
            .workspace
            .as_ref()
            .ok_or_else(|| anyhow::anyhow!("Not a workspace"))?;

        let mut members = Vec::new();
        let mut member_paths = Vec::new();

        // Discover workspace members
        for pattern in &workspace_config.members {
            let paths = glob::glob(&root.join(pattern).to_string_lossy())?;

            for entry in paths {
                let path = entry?;
                if path.is_dir() && path.join("jet.toml").exists() {
                    match Project::discover(&path) {
                        Ok(project) => {
                            member_paths
                                .push(path.strip_prefix(root).unwrap_or(&path).to_path_buf());
                            members.push(project);
                        }
                        Err(e) => {
                            eprintln!(
                                "Warning: Failed to load workspace member {}: {}",
                                path.display(),
                                e
                            );
                        }
                    }
                }
            }
        }

        Ok(Workspace {
            root: root.to_path_buf(),
            manifest,
            members,
            member_paths,
        })
    }

    /// Get a member by name
    #[allow(dead_code)]
    pub fn get_member(&self, name: &str) -> Option<&Project> {
        self.members.iter().find(|p| p.name() == name)
    }

    /// Get all member names
    #[allow(dead_code)]
    pub fn member_names(&self) -> Vec<&str> {
        self.members.iter().map(|p| p.name()).collect()
    }

    /// Check if this is a virtual workspace
    pub fn is_virtual(&self) -> bool {
        self.manifest.package.name.is_empty()
    }
}

/// Find the project or workspace containing the given path
pub fn find_project(path: &Path) -> Result<ProjectOrWorkspace> {
    let manifest_dir = if path.is_file() {
        path.parent().unwrap_or(path)
    } else {
        path
    };

    // First try to find a project
    if let Some(root) = crate::manifest::find_project_root(manifest_dir) {
        let manifest = Manifest::from_file(&root.join("jet.toml"))?;

        if manifest.is_workspace() {
            // This is a workspace
            let workspace = Workspace::from_manifest(&root, manifest)?;
            Ok(ProjectOrWorkspace::Workspace(workspace))
        } else {
            // This is a single project
            let project = Project::from_manifest(&root, manifest)?;
            Ok(ProjectOrWorkspace::Project(project))
        }
    } else {
        anyhow::bail!(
            "Could not find jet.toml in {} or any parent directory",
            path.display()
        )
    }
}

/// Either a project or a workspace
#[derive(Debug)]
pub enum ProjectOrWorkspace {
    Project(Project),
    Workspace(Workspace),
}

impl ProjectOrWorkspace {
    /// Get the root directory
    pub fn root(&self) -> &Path {
        match self {
            ProjectOrWorkspace::Project(p) => &p.root,
            ProjectOrWorkspace::Workspace(w) => &w.root,
        }
    }

    /// Get the manifest
    pub fn manifest(&self) -> &Manifest {
        match self {
            ProjectOrWorkspace::Project(p) => &p.manifest,
            ProjectOrWorkspace::Workspace(w) => &w.manifest,
        }
    }

    /// Check if this is a workspace
    pub fn is_workspace(&self) -> bool {
        matches!(self, ProjectOrWorkspace::Workspace(_))
    }

    /// Get as project if it is one
    pub fn as_project(&self) -> Option<&Project> {
        match self {
            ProjectOrWorkspace::Project(p) => Some(p),
            _ => None,
        }
    }

    /// Get as workspace if it is one
    pub fn as_workspace(&self) -> Option<&Workspace> {
        match self {
            ProjectOrWorkspace::Workspace(w) => Some(w),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn create_test_project(dir: &TempDir, name: &str, is_lib: bool) -> PathBuf {
        let project_dir = dir.path().join(name);
        let src_dir = project_dir.join("src");
        std::fs::create_dir_all(&src_dir).unwrap();

        // Create jet.toml
        let toml = format!(
            r#"[package]
name = "{}"
version = "1.0.0"
edition = "2024"
"#,
            name
        );
        std::fs::write(project_dir.join("jet.toml"), toml).unwrap();

        // Create entry point
        if is_lib {
            std::fs::write(
                src_dir.join("lib.jet"),
                "pub fn hello() -> string:\n    \"Hello\"\n",
            )
            .unwrap();
        } else {
            std::fs::write(
                src_dir.join("main.jet"),
                "fn main():\n    print(\"Hello\")\n",
            )
            .unwrap();
        }

        project_dir
    }

    #[test]
    fn test_detect_binary_project() {
        let temp_dir = TempDir::new().unwrap();
        let project_dir = create_test_project(&temp_dir, "test-bin", false);

        let project = Project::discover(&project_dir).unwrap();
        assert_eq!(project.project_type, ProjectType::Binary);
        assert!(project.entry_point.is_some());
    }

    #[test]
    fn test_detect_library_project() {
        let temp_dir = TempDir::new().unwrap();
        let project_dir = create_test_project(&temp_dir, "test-lib", true);

        let project = Project::discover(&project_dir).unwrap();
        assert_eq!(project.project_type, ProjectType::Library);
    }

    #[test]
    fn test_discover_tests() {
        let temp_dir = TempDir::new().unwrap();
        let project_dir = create_test_project(&temp_dir, "test-project", false);

        // Create tests directory
        let tests_dir = project_dir.join("tests");
        std::fs::create_dir(&tests_dir).unwrap();
        std::fs::write(tests_dir.join("test_basic.jet"), "# Test file\n").unwrap();

        let project = Project::discover(&project_dir).unwrap();
        assert!(project.has_tests());
        assert_eq!(project.test_files.len(), 1);
    }

    #[test]
    fn test_path_to_module_name() {
        let path = Path::new("foo/bar/baz.jet");
        assert_eq!(Project::path_to_module_name(path), "foo::bar::baz");

        let path = Path::new("main.jet");
        assert_eq!(Project::path_to_module_name(path), "main");
    }

    #[test]
    fn test_executable_path() {
        let temp_dir = TempDir::new().unwrap();
        let project_dir = create_test_project(&temp_dir, "my-app", false);

        let project = Project::discover(&project_dir).unwrap();
        let exe_path = project.executable_path("debug");

        assert!(exe_path.to_string_lossy().contains("my-app"));
        assert!(exe_path.parent().unwrap().ends_with("debug"));
    }
}
