//! Compilation pipeline for the Jet programming language
//!
//! This module orchestrates the full compilation pipeline:
//! Source → Lex → Parse → Resolve → Type Check → Effect Check → Lower → Optimize → Codegen → Link

#![allow(dead_code)]

use anyhow::{Context, Result};
use jet_diagnostics::Diagnostic;
use jet_lexer::{Lexer, SpannedToken};
use jet_linker::BuildProfile;
use jet_parser::ast::Module;
use jet_resolve::Resolver;
use jet_typeck::TypeCheckSession;
use std::io::Write;
use std::path::{Path, PathBuf};
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

use crate::project::{Project, SourceFile};

/// A single compilation unit (source file)
#[derive(Debug, Clone)]
pub struct CompilationUnit {
    /// Source file information
    pub source_file: SourceFile,
    /// Source code content
    pub source: String,
    /// Lexed tokens
    pub tokens: Option<Vec<SpannedToken>>,
    /// Parsed AST
    pub ast: Option<Module>,
    /// Resolved AST (with symbol information)
    pub resolved: Option<Module>,
    /// Type-checked AST
    pub typed: Option<jet_typeck::TypedModule>,
    /// Lowered IR
    pub ir: Option<jet_ir::Module>,
    /// Compilation errors
    pub errors: Vec<Diagnostic>,
    /// Whether this unit needs recompilation
    pub needs_rebuild: bool,
}

/// Compilation pipeline configuration
#[derive(Debug, Clone)]
pub struct PipelineConfig {
    /// Build profile
    pub profile: BuildProfile,
    /// Target triple (None for native)
    pub target_triple: Option<String>,
    /// Features to enable
    pub features: Vec<String>,
    /// Whether to emit LLVM IR
    pub emit_llvm_ir: bool,
    /// Whether to emit assembly
    pub emit_asm: bool,
    /// Whether to emit object files only
    pub emit_obj: bool,
    /// Whether to skip linking
    pub no_link: bool,
    /// Number of parallel jobs
    pub jobs: usize,
    /// Whether to be verbose
    pub verbose: bool,
    /// Output directory override
    pub out_dir: Option<PathBuf>,
}

impl Default for PipelineConfig {
    fn default() -> Self {
        Self {
            profile: BuildProfile::Debug,
            target_triple: None,
            features: vec![],
            emit_llvm_ir: false,
            emit_asm: false,
            emit_obj: false,
            no_link: false,
            jobs: num_cpus::get(),
            verbose: false,
            out_dir: None,
        }
    }
}

/// The compilation pipeline
pub struct CompilationPipeline {
    /// Pipeline configuration
    config: PipelineConfig,
    /// Project being compiled
    project: Project,
    /// Compilation units
    units: Vec<CompilationUnit>,
    /// Type check session (shared across units)
    type_session: TypeCheckSession,
    /// Resolver (shared across units)
    resolver: Resolver,
    /// Statistics
    stats: CompilationStats,
}

/// Compilation statistics
#[derive(Debug, Default)]
pub struct CompilationStats {
    #[allow(dead_code)]
    pub files_compiled: usize,
    #[allow(dead_code)]
    pub files_cached: usize,
    pub lex_time_ms: u64,
    pub parse_time_ms: u64,
    pub resolve_time_ms: u64,
    pub typecheck_time_ms: u64,
    pub lower_time_ms: u64,
    pub codegen_time_ms: u64,
    pub link_time_ms: u64,
}

/// Result of a compilation
#[derive(Debug)]
pub struct CompilationResult {
    /// Whether compilation succeeded
    pub success: bool,
    /// Path to the output binary (if linking)
    pub output_path: Option<PathBuf>,
    /// Generated object files
    pub object_files: Vec<PathBuf>,
    /// Generated LLVM IR files
    #[allow(dead_code)]
    pub llvm_ir_files: Vec<PathBuf>,
    /// Generated assembly files
    #[allow(dead_code)]
    pub asm_files: Vec<PathBuf>,
    /// All diagnostics from compilation
    pub diagnostics: Vec<Diagnostic>,
    /// Compilation statistics
    pub stats: CompilationStats,
}

impl CompilationPipeline {
    /// Create a new compilation pipeline
    pub fn new(project: Project, config: PipelineConfig) -> Self {
        Self {
            config,
            project,
            units: vec![],
            type_session: TypeCheckSession::new(),
            resolver: Resolver::new(jet_resolve::ModuleId::root()),
            stats: CompilationStats::default(),
        }
    }

    /// Initialize compilation units from source files
    pub fn initialize(&mut self) -> Result<()> {
        let source_files = self.project.lib_source_files();

        for source_file in source_files {
            let source = std::fs::read_to_string(&source_file.path)
                .with_context(|| format!("Failed to read {}", source_file.path.display()))?;

            self.units.push(CompilationUnit {
                source_file: source_file.clone(),
                source,
                tokens: None,
                ast: None,
                resolved: None,
                typed: None,
                ir: None,
                errors: vec![],
                needs_rebuild: true,
            });
        }

        Ok(())
    }

    /// Run the full compilation pipeline
    pub async fn compile(&mut self) -> Result<CompilationResult> {
        let start_time = std::time::Instant::now();
        let mut result = CompilationResult {
            success: false,
            output_path: None,
            object_files: vec![],
            llvm_ir_files: vec![],
            asm_files: vec![],
            diagnostics: vec![],
            stats: CompilationStats::default(),
        };

        // Phase 1: Lexing
        self.print_phase("Lexing");
        if let Err(e) = self.run_lexing_phase().await {
            self.print_error("Lexing failed", &e);
            result.diagnostics.extend(self.collect_diagnostics());
            return Ok(result);
        }

        // Phase 2: Parsing
        self.print_phase("Parsing");
        if let Err(e) = self.run_parsing_phase().await {
            self.print_error("Parsing failed", &e);
            result.diagnostics.extend(self.collect_diagnostics());
            return Ok(result);
        }

        // Phase 3: Name Resolution
        self.print_phase("Resolving");
        if let Err(e) = self.run_resolution_phase().await {
            self.print_error("Name resolution failed", &e);
            result.diagnostics.extend(self.collect_diagnostics());
            return Ok(result);
        }

        // Phase 4: Type Checking
        self.print_phase("Type checking");
        if let Err(e) = self.run_typecheck_phase().await {
            self.print_error("Type checking failed", &e);
            result.diagnostics.extend(self.collect_diagnostics());
            return Ok(result);
        }

        // Phase 5: Lowering to IR
        self.print_phase("Lowering");
        if let Err(e) = self.run_lowering_phase().await {
            self.print_error("Lowering failed", &e);
            result.diagnostics.extend(self.collect_diagnostics());
            return Ok(result);
        }

        // Phase 6: Code Generation
        self.print_phase("Generating code");
        match self.run_codegen_phase().await {
            Ok(objects) => {
                result.object_files = objects;
            }
            Err(e) => {
                self.print_error("Code generation failed", &e);
                result.diagnostics.extend(self.collect_diagnostics());
                return Ok(result);
            }
        }

        // Phase 7: Linking (unless disabled)
        if !self.config.no_link {
            self.print_phase("Linking");
            match self.run_linking_phase(&result.object_files).await {
                Ok(output_path) => {
                    result.output_path = Some(output_path);
                }
                Err(e) => {
                    self.print_error("Linking failed", &e);
                    result.diagnostics.extend(self.collect_diagnostics());
                    return Ok(result);
                }
            }
        }

        let elapsed = start_time.elapsed();
        result.success = true;
        result.stats = std::mem::take(&mut self.stats);

        self.print_success(&result, elapsed);

        Ok(result)
    }

    /// Run lexing phase
    async fn run_lexing_phase(&mut self) -> Result<()> {
        let start = std::time::Instant::now();

        for unit in &mut self.units {
            if !unit.needs_rebuild {
                continue;
            }

            let mut lexer = Lexer::new(&unit.source);
            let tokens = lexer.tokenize();

            // Check for lexer errors
            for token in &tokens {
                if let jet_lexer::Token::Error(ref msg) = token.token {
                    unit.errors.push(
                        Diagnostic::error(
                            format!("Lex error: {}", msg),
                            jet_diagnostics::Span::new(token.span.start, token.span.end),
                        )
                        .with_error_code(jet_diagnostics::ErrorCode::UnrecognizedToken),
                    );
                }
            }

            unit.tokens = Some(tokens);
        }

        self.stats.lex_time_ms = start.elapsed().as_millis() as u64;
        Ok(())
    }

    /// Run parsing phase
    async fn run_parsing_phase(&mut self) -> Result<()> {
        let start = std::time::Instant::now();

        for unit in &mut self.units {
            if !unit.needs_rebuild {
                continue;
            }

            if let Some(ref tokens) = unit.tokens {
                let mut parser = jet_parser::Parser::new(tokens.clone());

                match parser.parse_module() {
                    Ok(ast) => {
                        unit.ast = Some(ast);
                    }
                    Err(e) => {
                        unit.errors.push(
                            Diagnostic::error(
                                format!("Parse error: {:?}", e),
                                jet_diagnostics::Span::new(0, 0),
                            )
                            .with_error_code(jet_diagnostics::ErrorCode::InvalidSyntax),
                        );
                    }
                }
            }
        }

        // Check for errors
        let has_errors = self.units.iter().any(|u| !u.errors.is_empty());
        if has_errors {
            anyhow::bail!("Parsing failed with errors");
        }

        self.stats.parse_time_ms = start.elapsed().as_millis() as u64;
        Ok(())
    }

    /// Run name resolution phase
    async fn run_resolution_phase(&mut self) -> Result<()> {
        let start = std::time::Instant::now();

        // First pass: collect module bindings and items from all units
        // This ensures all top-level definitions are available for cross-module imports
        self.pre_populate_module_bindings();

        // Collect items from all modules first (without resolving imports/references)
        for unit in &mut self.units {
            if !unit.needs_rebuild {
                continue;
            }

            if let Some(ref ast) = unit.ast {
                match self.resolver.collect_module_items_only(ast) {
                    jet_resolve::ResolutionResult { success: true, .. } => {}
                    jet_resolve::ResolutionResult { errors, .. } => {
                        unit.errors.extend(errors.to_diagnostics());
                    }
                }
            }
        }

        // Second pass: resolve imports and references in each module
        // Now all module items are available in the symbol table
        for unit in &mut self.units {
            if !unit.needs_rebuild {
                continue;
            }

            if let Some(ref mut ast) = unit.ast {
                match self.resolver.resolve_module_imports_and_refs(ast) {
                    jet_resolve::ResolutionResult { success: true, .. } => {
                        unit.resolved = Some(ast.clone());
                    }
                    jet_resolve::ResolutionResult { errors, .. } => {
                        unit.errors.extend(errors.to_diagnostics());
                    }
                }
            }
        }

        // Check for errors
        let has_errors = self.units.iter().any(|u| !u.errors.is_empty());
        if has_errors {
            anyhow::bail!("Name resolution failed with errors");
        }

        self.stats.resolve_time_ms = start.elapsed().as_millis() as u64;
        Ok(())
    }

    /// Pre-populate the resolver's symbol table with module bindings for all source files
    ///
    /// This allows imports to find modules defined in other source files
    fn pre_populate_module_bindings(&mut self) {
        use jet_resolve::{Binding, BindingKind};

        for unit in &self.units {
            let module_name = &unit.source_file.module_name;

            // Skip the main entry point - it's not importable as a module
            if unit.source_file.is_main {
                continue;
            }

            // Skip if already defined
            if self
                .resolver
                .symbol_table
                .lookup_module(module_name)
                .is_some()
            {
                continue;
            }

            // Create a module binding for this source file
            let def_id = self.resolver.next_def_id();
            let binding = Binding::new(
                module_name.clone(),
                jet_lexer::Span::new(0, 0),
                BindingKind::Module,
                def_id,
                self.resolver.current_module,
            )
            .with_public(true);

            self.resolver.symbol_table.insert_module_binding(binding);
        }
    }

    /// Run type checking phase
    async fn run_typecheck_phase(&mut self) -> Result<()> {
        let start = std::time::Instant::now();

        for unit in &mut self.units {
            if !unit.needs_rebuild {
                continue;
            }

            if let Some(ref ast) = unit.resolved {
                match self.type_session.check_module(ast) {
                    Ok(typed) => {
                        unit.typed = Some(typed);
                    }
                    Err(()) => {
                        // Errors are in the session
                        for diag in self.type_session.diagnostics() {
                            unit.errors.push(diag.clone());
                        }
                    }
                }
            }
        }

        // Check for errors
        let has_errors = self.units.iter().any(|u| !u.errors.is_empty());
        if has_errors {
            anyhow::bail!("Type checking failed with errors");
        }

        self.stats.typecheck_time_ms = start.elapsed().as_millis() as u64;
        Ok(())
    }

    /// Run lowering phase (AST to IR)
    async fn run_lowering_phase(&mut self) -> Result<()> {
        let start = std::time::Instant::now();

        for unit in &mut self.units {
            if !unit.needs_rebuild {
                continue;
            }

            if let Some(ref typed) = unit.typed {
                // Lower the typed AST to IR, passing the type context for type information
                let ir = jet_lower::lower_module(
                    typed,
                    self.type_session.type_context(),
                    &unit.source_file.module_name,
                );
                unit.ir = Some(ir);
            }
        }

        self.stats.lower_time_ms = start.elapsed().as_millis() as u64;
        Ok(())
    }

    /// Run code generation phase
    async fn run_codegen_phase(&mut self) -> Result<Vec<PathBuf>> {
        let start = std::time::Instant::now();
        let mut object_files = Vec::new();

        // Create output directory
        let out_dir = self
            .config
            .out_dir
            .clone()
            .unwrap_or_else(|| self.project.output_dir(self.config.profile.output_dir()));
        std::fs::create_dir_all(&out_dir)?;

        for unit in &mut self.units {
            if !unit.needs_rebuild {
                continue;
            }

            if let Some(ref ir) = unit.ir {
                // Generate object file
                let obj_name = format!("{}.o", unit.source_file.module_name.replace("::", "_"));
                let obj_path = out_dir.join(&obj_name);

                // Compile the IR module to an object file
                match compile_ir_module(ir, &obj_path).await {
                    Ok(_) => {
                        object_files.push(obj_path);
                    }
                    Err(e) => {
                        anyhow::bail!(
                            "Code generation failed for {}: {}",
                            unit.source_file.module_name,
                            e
                        );
                    }
                }

                // Emit LLVM IR if requested
                if self.config.emit_llvm_ir {
                    let ir_path = out_dir.join(format!(
                        "{}.ll",
                        unit.source_file.module_name.replace("::", "_")
                    ));
                    if let Ok(llvm_ir) = jet_codegen::compile_to_llvm_ir(ir) {
                        std::fs::write(&ir_path, llvm_ir)?;
                    }
                }
            }
        }

        self.stats.codegen_time_ms = start.elapsed().as_millis() as u64;
        Ok(object_files)
    }

    /// Run linking phase
    async fn run_linking_phase(&mut self, object_files: &[PathBuf]) -> Result<PathBuf> {
        let start = std::time::Instant::now();

        let out_dir = self
            .config
            .out_dir
            .clone()
            .unwrap_or_else(|| self.project.output_dir(self.config.profile.output_dir()));

        // Ensure output directory exists
        std::fs::create_dir_all(&out_dir)?;

        // Create linker configuration
        let target = jet_linker::TargetConfig::native();
        let linker_config =
            jet_linker::LinkerConfig::new(target.clone(), self.project.name().to_string())
                .with_output_dir(&out_dir);

        // Create linker and add object files
        let mut linker = jet_linker::Linker::new(linker_config);
        for obj in object_files {
            linker.add_object(obj);
        }

        // Runtime library is mandatory for executable builds.
        let runtime_lib = jet_linker::find_runtime_lib(target.platform).map_err(|e| {
            anyhow::anyhow!(
                "Jet runtime library is required for linking but was not found: {}",
                e
            )
        })?;
        linker.config_mut().runtime_lib_path = Some(runtime_lib);

        // Run the linker
        match linker.link().await {
            Ok(output_path) => {
                self.stats.link_time_ms = start.elapsed().as_millis() as u64;
                Ok(output_path)
            }
            Err(e) => {
                anyhow::bail!("Linking failed: {}", e);
            }
        }
    }

    /// Collect all diagnostics from all units
    fn collect_diagnostics(&self) -> Vec<Diagnostic> {
        self.units.iter().flat_map(|u| u.errors.clone()).collect()
    }

    /// Print compilation phase
    fn print_phase(&self, phase: &str) {
        if self.config.verbose {
            let mut stdout = StandardStream::stdout(ColorChoice::Auto);
            stdout
                .set_color(ColorSpec::new().set_fg(Some(Color::Cyan)).set_bold(true))
                .ok();
            write!(&mut stdout, "   {} ", phase).ok();
            stdout.reset().ok();
            writeln!(&mut stdout).ok();
        }
    }

    /// Print error message
    fn print_error(&self, message: &str, error: &anyhow::Error) {
        let mut stderr = StandardStream::stderr(ColorChoice::Auto);
        stderr
            .set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true))
            .ok();
        write!(&mut stderr, "error").ok();
        stderr.reset().ok();
        writeln!(&mut stderr, ": {}: {}", message, error).ok();
    }

    /// Print success message
    fn print_success(&self, result: &CompilationResult, elapsed: std::time::Duration) {
        let mut stdout = StandardStream::stdout(ColorChoice::Auto);

        stdout
            .set_color(ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true))
            .ok();
        write!(&mut stdout, "    Finished").ok();
        stdout.reset().ok();

        let profile_name = match self.config.profile {
            BuildProfile::Debug => "dev",
            BuildProfile::Release => "release",
        };

        writeln!(
            &mut stdout,
            " {} [{}] in {:.2}s",
            profile_name,
            self.project.name(),
            elapsed.as_secs_f64()
        )
        .ok();

        if let Some(ref path) = result.output_path {
            stdout
                .set_color(ColorSpec::new().set_fg(Some(Color::Cyan)))
                .ok();
            write!(&mut stdout, "     Running").ok();
            stdout.reset().ok();
            writeln!(&mut stdout, " `{}`", path.display()).ok();
        }
    }
}

/// Quick compile function for simple use cases
pub async fn compile_project(
    project: &Project,
    config: PipelineConfig,
) -> Result<CompilationResult> {
    let mut pipeline = CompilationPipeline::new(project.clone(), config);
    pipeline.initialize()?;
    pipeline.compile().await
}

/// Type check only (no code generation)
pub async fn check_project(project: &Project, verbose: bool) -> Result<Vec<Diagnostic>> {
    let config = PipelineConfig {
        verbose,
        ..Default::default()
    };

    let mut pipeline = CompilationPipeline::new(project.clone(), config);
    pipeline.initialize()?;

    // Run only up to type checking
    pipeline.run_lexing_phase().await?;
    pipeline.run_parsing_phase().await?;
    pipeline.run_resolution_phase().await?;
    pipeline.run_typecheck_phase().await?;

    Ok(pipeline.collect_diagnostics())
}

/// Compile an IR module to an object file
async fn compile_ir_module(ir: &jet_ir::Module, output_path: &Path) -> Result<()> {
    // Use jet_codegen to compile the module
    jet_codegen::compile_to_object_file(ir, output_path)
        .map_err(|e| anyhow::anyhow!("LLVM compilation failed: {:?}", e))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn create_test_project(dir: &TempDir) -> Project {
        let project_dir = dir.path().join("test_project");
        let src_dir = project_dir.join("src");
        std::fs::create_dir_all(&src_dir).unwrap();

        // Create jet.toml
        std::fs::write(
            project_dir.join("jet.toml"),
            r#"[package]
name = "test-project"
version = "1.0.0"
edition = "2024"
"#,
        )
        .unwrap();

        // Create main.jet
        std::fs::write(
            src_dir.join("main.jet"),
            "fn main():\n    print(\"Hello, Jet!\")\n",
        )
        .unwrap();

        Project::discover(&project_dir).unwrap()
    }

    #[test]
    fn test_pipeline_config_default() {
        let config = PipelineConfig::default();
        assert!(matches!(config.profile, BuildProfile::Debug));
        assert!(!config.emit_llvm_ir);
        assert!(!config.no_link);
    }

    #[tokio::test]
    async fn test_compilation_pipeline_new() {
        let temp_dir = TempDir::new().unwrap();
        let project = create_test_project(&temp_dir);

        let config = PipelineConfig::default();
        let mut pipeline = CompilationPipeline::new(project, config);

        assert!(pipeline.initialize().is_ok());
    }
}
