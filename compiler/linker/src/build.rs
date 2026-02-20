//! Build system for Jet
//!
//! This module provides:
//! - BuildCommand implementation
//! - Dependency resolution
//! - Compilation unit management
//! - Parallel compilation support

use std::path::{Path, PathBuf};
use std::sync::Arc;

use tokio::sync::{Mutex, Semaphore};

use crate::cache::{Cache, CompileFlags};
use crate::error::{BuildError, BuildResult};
use crate::incremental::{IncrementalState, Module, ModuleId};
use crate::linker::LinkerBuilder;
use crate::platform::TargetConfig;

use jet_codegen::CodeGen;
use jet_lower::lower_module;
use jet_resolve::Resolver;

/// Build target type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuildTarget {
    /// Build a binary executable
    Binary,
    /// Build a library
    Library,
    /// Build all targets
    All,
}

/// Build profile (debug or release)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuildProfile {
    /// Debug build with full debug info
    Debug,
    /// Release build with optimizations
    Release,
}

impl BuildProfile {
    /// Get the output directory name for this profile
    pub fn output_dir(&self) -> &'static str {
        match self {
            BuildProfile::Debug => "debug",
            BuildProfile::Release => "release",
        }
    }

    /// Get the compile flags for this profile
    pub fn compile_flags(&self) -> CompileFlags {
        match self {
            BuildProfile::Debug => CompileFlags::debug(),
            BuildProfile::Release => CompileFlags::release(),
        }
    }
}

/// Configuration for a build
#[derive(Debug, Clone)]
pub struct BuildConfig {
    /// Target type
    pub target: BuildTarget,
    /// Build profile
    pub profile: BuildProfile,
    /// Target triple (None for native)
    pub target_triple: Option<String>,
    /// Number of parallel jobs (None for auto)
    pub jobs: Option<usize>,
    /// Features to enable
    pub features: Vec<String>,
    /// Additional library paths
    pub lib_paths: Vec<PathBuf>,
    /// Additional libraries to link
    pub libraries: Vec<String>,
    /// Whether to use incremental compilation
    pub incremental: bool,
    /// Output directory
    pub output_dir: PathBuf,
    /// Project root directory
    pub project_root: PathBuf,
}

impl BuildConfig {
    /// Create a new build configuration
    pub fn new(project_root: impl AsRef<Path>) -> Self {
        BuildConfig {
            target: BuildTarget::Binary,
            profile: BuildProfile::Debug,
            target_triple: None,
            jobs: None,
            features: vec![],
            lib_paths: vec![],
            libraries: vec![],
            incremental: true,
            output_dir: PathBuf::from("target"),
            project_root: project_root.as_ref().to_path_buf(),
        }
    }

    /// Set the build target
    pub fn with_target(mut self, target: BuildTarget) -> Self {
        self.target = target;
        self
    }

    /// Set the build profile
    pub fn with_profile(mut self, profile: BuildProfile) -> Self {
        self.profile = profile;
        self
    }

    /// Set the target triple
    pub fn with_target_triple(mut self, triple: impl Into<String>) -> Self {
        self.target_triple = Some(triple.into());
        self
    }

    /// Set the number of parallel jobs
    pub fn with_jobs(mut self, jobs: usize) -> Self {
        self.jobs = Some(jobs);
        self
    }

    /// Enable a feature
    pub fn with_feature(mut self, feature: impl Into<String>) -> Self {
        self.features.push(feature.into());
        self
    }

    /// Add a library path
    pub fn with_lib_path(mut self, path: impl AsRef<Path>) -> Self {
        self.lib_paths.push(path.as_ref().to_path_buf());
        self
    }

    /// Add a library
    pub fn with_library(mut self, lib: impl Into<String>) -> Self {
        self.libraries.push(lib.into());
        self
    }

    /// Disable incremental compilation
    pub fn without_incremental(mut self) -> Self {
        self.incremental = false;
        self
    }

    /// Set the output directory
    pub fn with_output_dir(mut self, path: impl AsRef<Path>) -> Self {
        self.output_dir = path.as_ref().to_path_buf();
        self
    }

    /// Get the full output directory for the current profile
    pub fn profile_output_dir(&self) -> PathBuf {
        self.output_dir.join(self.profile.output_dir())
    }

    /// Get the target configuration
    pub fn target_config(&self) -> BuildResult<TargetConfig> {
        match &self.target_triple {
            Some(triple) => TargetConfig::from_triple(triple)
                .map_err(|e| BuildError::ConfigError(e.to_string())),
            None => Ok(TargetConfig::native()),
        }
    }
}

/// A compilation unit (single source file)
#[derive(Debug, Clone)]
pub struct CompilationUnit {
    /// Module ID
    pub id: ModuleId,
    /// Module name
    pub name: String,
    /// Path to the source file
    pub source_path: PathBuf,
    /// Path to the output object file
    pub object_path: PathBuf,
    /// Dependencies (other units this unit depends on)
    pub dependencies: Vec<ModuleId>,
    /// Whether this unit needs recompilation
    pub needs_rebuild: bool,
}

/// Result of a compilation
#[derive(Debug, Clone)]
pub struct CompilationResult {
    /// Module ID
    pub id: ModuleId,
    /// Path to the generated object file
    pub object_path: PathBuf,
    /// Any diagnostics produced
    pub diagnostics: Vec<String>,
    /// Whether compilation succeeded
    pub success: bool,
}

/// The build system
pub struct BuildSystem {
    /// Build configuration
    config: BuildConfig,
    /// Target configuration
    target: TargetConfig,
    /// Incremental state
    incremental: Option<IncrementalState>,
    /// Compilation units
    units: Vec<CompilationUnit>,
    /// Semaphore for limiting parallel compilation
    semaphore: Arc<Semaphore>,
}

impl BuildSystem {
    /// Create a new build system
    pub async fn new(config: BuildConfig) -> BuildResult<Self> {
        let target = config.target_config()?;

        // Initialize incremental compilation if enabled
        let incremental = if config.incremental {
            let cache = Cache::default_cache().map_err(|e| {
                BuildError::ConfigError(format!("Failed to initialize cache: {}", e))
            })?;
            let compiler_version = env!("CARGO_PKG_VERSION").to_string();
            Some(IncrementalState::new(
                cache,
                target.clone(),
                compiler_version,
            ))
        } else {
            None
        };

        // Determine number of parallel jobs
        let jobs = config.jobs.unwrap_or_else(num_cpus::get);
        let semaphore = Arc::new(Semaphore::new(jobs));

        Ok(BuildSystem {
            config,
            target,
            incremental,
            units: vec![],
            semaphore,
        })
    }

    /// Add source files to the build
    pub fn add_sources(&mut self, sources: &[PathBuf]) -> BuildResult<()> {
        let flags = self.config.profile.compile_flags();

        for (idx, source) in sources.iter().enumerate() {
            let id = ModuleId::new(idx as u32);
            let name = source
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("unknown")
                .to_string();

            // Determine object file path
            let obj_name = format!("{}{}", name, self.target.platform.obj_extension());
            let object_path = self.config.profile_output_dir().join(obj_name);

            // Check if rebuild is needed
            let needs_rebuild = if let Some(ref mut inc) = self.incremental {
                // Add to dependency graph
                let module = Module::from_file(id, name.clone(), source).map_err(|e| {
                    BuildError::SourceError {
                        path: source.clone(),
                        message: e.to_string(),
                    }
                })?;

                let needs_rebuild =
                    inc.needs_rebuild(&module, &flags)
                        .map_err(|e| BuildError::SourceError {
                            path: source.clone(),
                            message: e.to_string(),
                        })?;

                inc.graph_mut().add_module(module);
                needs_rebuild
            } else {
                true // Always rebuild without incremental
            };

            self.units.push(CompilationUnit {
                id,
                name,
                source_path: source.clone(),
                object_path,
                dependencies: vec![],
                needs_rebuild,
            });
        }

        Ok(())
    }

    /// Resolve dependencies between compilation units
    pub fn resolve_dependencies(&mut self) -> BuildResult<()> {
        // This is a simplified dependency resolution
        // In a real implementation, this would parse imports from source files
        // and build a proper dependency graph

        // For now, just add all previous units as dependencies
        for i in 1..self.units.len() {
            let prev_id = self.units[i - 1].id;
            self.units[i].dependencies.push(prev_id);

            if let Some(ref mut inc) = self.incremental {
                inc.graph_mut().add_dependency(self.units[i].id, prev_id);
            }
        }

        Ok(())
    }

    /// Compile all units
    pub async fn compile(&mut self) -> BuildResult<Vec<CompilationResult>> {
        // Ensure output directory exists
        std::fs::create_dir_all(self.config.profile_output_dir())?;

        // Sort units by dependencies (topological order)
        let sorted_ids = if let Some(ref inc) = self.incremental {
            inc.graph().topological_sort()
        } else {
            self.units.iter().map(|u| u.id).collect()
        };

        // Compile units in parallel
        let results = Arc::new(Mutex::new(Vec::new()));
        let mut handles = vec![];

        for id in sorted_ids {
            let unit = self.units.iter().find(|u| u.id == id).cloned();
            if let Some(unit) = unit {
                if !unit.needs_rebuild {
                    // Use cached result
                    let result = CompilationResult {
                        id: unit.id,
                        object_path: unit.object_path.clone(),
                        diagnostics: vec!["Using cached artifact".to_string()],
                        success: true,
                    };
                    results.lock().await.push(result);
                    continue;
                }

                let semaphore = self.semaphore.clone();
                let results = results.clone();
                let target = self.target.clone();
                let config = self.config.clone();

                let handle = tokio::spawn(async move {
                    let _permit = semaphore.acquire().await.unwrap();
                    let result = compile_unit(&unit, &target, &config).await;
                    results.lock().await.push(result);
                });

                handles.push(handle);
            }
        }

        // Wait for all compilations to complete
        for handle in handles {
            handle
                .await
                .map_err(|e| BuildError::ParallelError(e.to_string()))?;
        }

        // Extract results
        let results = Arc::try_unwrap(results)
            .map_err(|_| BuildError::ParallelError("Failed to unwrap results".to_string()))?
            .into_inner();

        // Check for failures
        let failures: Vec<_> = results.iter().filter(|r| !r.success).collect();
        if !failures.is_empty() {
            let diagnostics: Vec<String> = failures
                .iter()
                .flat_map(|f| f.diagnostics.clone())
                .collect();

            return Err(BuildError::CompilationFailed {
                name: "build".to_string(),
                diagnostics,
            });
        }

        Ok(results)
    }

    /// Link the compiled objects into an executable
    pub async fn link(&self, results: &[CompilationResult]) -> BuildResult<PathBuf> {
        let object_files: Vec<PathBuf> = results.iter().map(|r| r.object_path.clone()).collect();

        // Determine output name from project
        let output_name = self
            .config
            .project_root
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or("main")
            .to_string();

        // Create linker
        let linker = match self.config.profile {
            BuildProfile::Debug => LinkerBuilder::debug(self.target.clone(), &output_name)
                .with_output_dir(self.config.profile_output_dir()),
            BuildProfile::Release => LinkerBuilder::release(self.target.clone(), &output_name)
                .with_output_dir(self.config.profile_output_dir()),
        };

        let mut linker = linker.build();

        // Add object files
        for obj in &object_files {
            linker.add_object(obj);
        }

        // Add library paths
        for path in &self.config.lib_paths {
            linker.config_mut().lib_paths.push(path.clone());
        }

        // Add libraries
        for lib in &self.config.libraries {
            linker.config_mut().libraries.push(lib.clone());
        }

        // Link
        let output_path = linker.link().await.map_err(BuildError::LinkError)?;

        Ok(output_path)
    }

    /// Get the build configuration
    pub fn config(&self) -> &BuildConfig {
        &self.config
    }

    /// Get the target configuration
    pub fn target(&self) -> &TargetConfig {
        &self.target
    }

    /// Get the compilation units
    pub fn units(&self) -> &[CompilationUnit] {
        &self.units
    }
}

/// Compile a single unit
async fn compile_unit(
    unit: &CompilationUnit,
    _target: &TargetConfig,
    _config: &BuildConfig,
) -> CompilationResult {
    use inkwell::context::Context;
    use jet_codegen::object::generate_object_file;

    // Read the source file
    let source = match std::fs::read_to_string(&unit.source_path) {
        Ok(s) => s,
        Err(e) => {
            return CompilationResult {
                id: unit.id,
                object_path: unit.object_path.clone(),
                diagnostics: vec![format!("Failed to read source file: {}", e)],
                success: false,
            };
        }
    };

    // Step 1: Lex the source
    let tokens = jet_lexer::tokenize(&source);

    // Step 2: Parse into AST
    let ast = match jet_parser::Parser::new(tokens).parse_module() {
        Ok(a) => a,
        Err(e) => {
            return CompilationResult {
                id: unit.id,
                object_path: unit.object_path.clone(),
                diagnostics: vec![format!("Parsing error: {:?}", e)],
                success: false,
            };
        }
    };

    // Step 3: Resolve symbols
    let mut resolver = Resolver::new(jet_resolve::ModuleId::root());
    let resolve_result = resolver.resolve_module(&ast);
    if resolve_result.is_err() {
        return CompilationResult {
            id: unit.id,
            object_path: unit.object_path.clone(),
            diagnostics: vec![format!("Resolution errors: {:?}", resolve_result.errors)],
            success: false,
        };
    }

    // Step 4: Type check
    let tast = match jet_typeck::type_check_module(&ast) {
        Ok(t) => t,
        Err(e) => {
            return CompilationResult {
                id: unit.id,
                object_path: unit.object_path.clone(),
                diagnostics: e.iter().map(|d| format!("{:?}", d)).collect(),
                success: false,
            };
        }
    };

    // Step 5: Lower to IR (takes TAST with type context)
    // Create a temporary type context for lowering
    let tcx = jet_typeck::TypeContext::new();
    let ir = lower_module(&tast, &tcx, &unit.name);

    // Step 6: Generate object code using LLVM
    let context = Context::create();
    let mut codegen = CodeGen::new(&context, &unit.name);

    if let Err(e) = jet_codegen::compile_module(&mut codegen, &ir) {
        return CompilationResult {
            id: unit.id,
            object_path: unit.object_path.clone(),
            diagnostics: vec![format!("Code generation error: {:?}", e)],
            success: false,
        };
    }

    // Generate object file
    // Note: ObjectConfig expects a 'static str for target_triple, so we use None
    // and let LLVM use the default native target
    let obj_config = jet_codegen::ObjectConfig {
        target_triple: None,
        opt_level: inkwell::OptimizationLevel::Default,
        ..Default::default()
    };

    if let Err(e) = generate_object_file(&codegen, obj_config, &unit.object_path) {
        return CompilationResult {
            id: unit.id,
            object_path: unit.object_path.clone(),
            diagnostics: vec![format!("Object file generation error: {:?}", e)],
            success: false,
        };
    }

    CompilationResult {
        id: unit.id,
        object_path: unit.object_path.clone(),
        diagnostics: vec![format!("Compiled {}", unit.name)],
        success: true,
    }
}

/// Build command that orchestrates the entire build process
pub struct BuildCommand {
    config: BuildConfig,
}

impl BuildCommand {
    /// Create a new build command
    pub fn new(config: BuildConfig) -> Self {
        BuildCommand { config }
    }

    /// Execute the build
    pub async fn execute(&self) -> BuildResult<PathBuf> {
        // Find source files
        let src_dir = self.config.project_root.join("src");
        let sources = find_source_files(&src_dir)?;

        if sources.is_empty() {
            return Err(BuildError::SourceError {
                path: src_dir,
                message: "No .jet source files found".to_string(),
            });
        }

        // Initialize build system
        let mut build_system = BuildSystem::new(self.config.clone()).await?;

        // Add sources
        build_system.add_sources(&sources)?;

        // Resolve dependencies
        build_system.resolve_dependencies()?;

        // Compile
        let results = build_system.compile().await?;

        // Link
        let output_path = build_system.link(&results).await?;

        Ok(output_path)
    }
}

/// Find all .jet source files in a directory
fn find_source_files(dir: &Path) -> BuildResult<Vec<PathBuf>> {
    let mut files = Vec::new();

    if !dir.exists() {
        return Ok(files);
    }

    for entry in walkdir::WalkDir::new(dir) {
        let entry = entry.map_err(|e| BuildError::Io(e.to_string()))?;
        if entry.file_type().is_file() {
            if let Some(ext) = entry.path().extension() {
                if ext == "jet" {
                    files.push(entry.path().to_path_buf());
                }
            }
        }
    }

    Ok(files)
}

/// Quick build function for simple use cases
pub async fn build_project(
    project_root: impl AsRef<Path>,
    profile: BuildProfile,
) -> BuildResult<PathBuf> {
    let config = BuildConfig::new(project_root).with_profile(profile);

    let cmd = BuildCommand::new(config);
    cmd.execute().await
}

/// Get the compiler version
pub fn compiler_version() -> &'static str {
    env!("CARGO_PKG_VERSION")
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn create_test_project(dir: &TempDir) -> PathBuf {
        let project_dir = dir.path().join("test_project");
        let src_dir = project_dir.join("src");
        std::fs::create_dir_all(&src_dir).unwrap();

        // Create main.jet
        std::fs::write(
            src_dir.join("main.jet"),
            "fn main():\n    print(\"Hello, Jet!\")\n",
        )
        .unwrap();

        // Create lib.jet
        std::fs::write(
            src_dir.join("lib.jet"),
            "pub fn add(a: int, b: int) -> int:\n    a + b\n",
        )
        .unwrap();

        project_dir
    }

    #[test]
    fn test_build_config() {
        let config = BuildConfig::new("/tmp/project")
            .with_profile(BuildProfile::Release)
            .with_target(BuildTarget::Binary)
            .with_jobs(4);

        assert_eq!(config.profile, BuildProfile::Release);
        assert_eq!(config.target, BuildTarget::Binary);
        assert_eq!(config.jobs, Some(4));
    }

    #[test]
    fn test_build_profile_output_dir() {
        assert_eq!(BuildProfile::Debug.output_dir(), "debug");
        assert_eq!(BuildProfile::Release.output_dir(), "release");
    }

    #[test]
    fn test_find_source_files() {
        let temp_dir = TempDir::new().unwrap();
        let project_dir = create_test_project(&temp_dir);

        let sources = find_source_files(&project_dir.join("src")).unwrap();
        assert_eq!(sources.len(), 2);
    }

    #[test]
    fn test_compilation_unit() {
        let unit = CompilationUnit {
            id: ModuleId::new(0),
            name: "main".to_string(),
            source_path: PathBuf::from("src/main.jet"),
            object_path: PathBuf::from("target/debug/main.o"),
            dependencies: vec![],
            needs_rebuild: true,
        };

        assert_eq!(unit.name, "main");
        assert!(unit.needs_rebuild);
    }

    #[tokio::test]
    async fn test_build_system_new() {
        let config = BuildConfig::new("/tmp/project");
        let result = BuildSystem::new(config).await;
        assert!(result.is_ok());
    }
}
