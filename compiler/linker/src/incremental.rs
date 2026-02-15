//! Incremental compilation support
//!
//! This module provides:
//! - Source file hashing
//! - Dependency graph tracking
//! - Cache invalidation
//! - Artifact storage

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use crate::cache::{Cache, CacheKey, CompileFlags, DependencyHash};
use crate::error::{LinkerError, LinkerResult};
use crate::platform::TargetConfig;

/// A module identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(pub u32);

impl ModuleId {
    /// Create a new module ID
    pub fn new(id: u32) -> Self {
        ModuleId(id)
    }
}

/// Information about a source module
#[derive(Debug, Clone)]
pub struct Module {
    /// Unique identifier
    pub id: ModuleId,
    /// Name of the module
    pub name: String,
    /// Path to the source file
    pub source_path: PathBuf,
    /// Hash of the source content
    pub source_hash: String,
    /// Last modification time
    pub modified: SystemTime,
    /// Dependencies (other modules this module imports)
    pub dependencies: Vec<ModuleId>,
}

impl Module {
    /// Create a new module from a source file
    pub fn from_file(id: ModuleId, name: String, path: &Path) -> LinkerResult<Self> {
        let content = fs::read_to_string(path)
            .map_err(|e| LinkerError::Io(format!("Failed to read {}: {}", path.display(), e)))?;

        let metadata = fs::metadata(path)?;
        let modified = metadata.modified().unwrap_or(SystemTime::UNIX_EPOCH);

        Ok(Module {
            id,
            name,
            source_path: path.to_path_buf(),
            source_hash: hash_content(&content),
            modified,
            dependencies: vec![],
        })
    }

    /// Check if the module has changed on disk
    pub fn has_changed(&self) -> LinkerResult<bool> {
        let current = Self::from_file(self.id, self.name.clone(), &self.source_path)?;
        Ok(current.source_hash != self.source_hash)
    }

    /// Add a dependency
    pub fn add_dependency(&mut self, dep: ModuleId) {
        if !self.dependencies.contains(&dep) {
            self.dependencies.push(dep);
        }
    }
}

/// Dependency graph for tracking module relationships
#[derive(Debug, Clone, Default)]
pub struct DependencyGraph {
    /// Module ID -> Module info
    modules: HashMap<ModuleId, Module>,
    /// File path -> Module ID
    path_to_id: HashMap<PathBuf, ModuleId>,
    /// Module ID -> dependents (modules that depend on this one)
    dependents: HashMap<ModuleId, Vec<ModuleId>>,
    /// Next module ID
    next_id: u32,
}

impl DependencyGraph {
    /// Create a new empty dependency graph
    pub fn new() -> Self {
        DependencyGraph::default()
    }

    /// Add a module to the graph
    pub fn add_module(&mut self, module: Module) -> ModuleId {
        let id = module.id;
        self.path_to_id.insert(module.source_path.clone(), id);
        self.modules.insert(id, module);
        id
    }

    /// Create a new module from a file and add it to the graph
    pub fn add_module_from_file(&mut self, name: String, path: &Path) -> LinkerResult<ModuleId> {
        let id = ModuleId::new(self.next_id);
        self.next_id += 1;

        let module = Module::from_file(id, name, path)?;
        self.add_module(module);
        Ok(id)
    }

    /// Get a module by ID
    pub fn get(&self, id: ModuleId) -> Option<&Module> {
        self.modules.get(&id)
    }

    /// Get a module by path
    pub fn get_by_path(&self, path: &Path) -> Option<&Module> {
        self.path_to_id
            .get(path)
            .and_then(|id| self.modules.get(id))
    }

    /// Get a mutable reference to a module
    pub fn get_mut(&mut self, id: ModuleId) -> Option<&mut Module> {
        self.modules.get_mut(&id)
    }

    /// Add a dependency edge
    pub fn add_dependency(&mut self, from: ModuleId, to: ModuleId) {
        if let Some(module) = self.modules.get_mut(&from) {
            module.add_dependency(to);
        }

        self.dependents.entry(to).or_default().push(from);
    }

    /// Get all modules that depend on the given module
    pub fn get_dependents(&self, id: ModuleId) -> &[ModuleId] {
        self.dependents
            .get(&id)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }

    /// Get all transitive dependents (recursive)
    pub fn get_transitive_dependents(&self, id: ModuleId) -> Vec<ModuleId> {
        let mut result = Vec::new();
        let mut visited = HashSet::new();
        let mut stack = vec![id];

        while let Some(current) = stack.pop() {
            if visited.insert(current) {
                for &dep in self.get_dependents(current) {
                    if !visited.contains(&dep) {
                        result.push(dep);
                        stack.push(dep);
                    }
                }
            }
        }

        result
    }

    /// Get all modules in topological order (dependencies first)
    pub fn topological_sort(&self) -> Vec<ModuleId> {
        let mut result = Vec::new();
        let mut visited = HashSet::new();
        let mut temp_mark = HashSet::new();

        for id in self.modules.keys().copied() {
            if !visited.contains(&id) {
                self.visit(id, &mut visited, &mut temp_mark, &mut result);
            }
        }

        result
    }

    fn visit(
        &self,
        id: ModuleId,
        visited: &mut HashSet<ModuleId>,
        temp_mark: &mut HashSet<ModuleId>,
        result: &mut Vec<ModuleId>,
    ) {
        if temp_mark.contains(&id) {
            // Cycle detected - skip for now
            return;
        }

        if visited.contains(&id) {
            return;
        }

        temp_mark.insert(id);

        if let Some(module) = self.modules.get(&id) {
            for &dep in &module.dependencies {
                self.visit(dep, visited, temp_mark, result);
            }
        }

        temp_mark.remove(&id);
        visited.insert(id);
        result.push(id);
    }

    /// Find all modules that need recompilation
    pub fn find_dirty_modules(&self, _cache: &Cache) -> Vec<ModuleId> {
        let mut dirty = HashSet::new();

        for (id, module) in &self.modules {
            // Check if module has changed
            if let Ok(changed) = module.has_changed() {
                if changed {
                    dirty.insert(*id);
                    // Also mark all dependents as dirty
                    for dep in self.get_transitive_dependents(*id) {
                        dirty.insert(dep);
                    }
                }
            }
        }

        dirty.into_iter().collect()
    }

    /// Get all modules
    pub fn all_modules(&self) -> impl Iterator<Item = &Module> {
        self.modules.values()
    }

    /// Get the number of modules
    pub fn len(&self) -> usize {
        self.modules.len()
    }

    /// Check if the graph is empty
    pub fn is_empty(&self) -> bool {
        self.modules.is_empty()
    }
}

/// Incremental compilation state
pub struct IncrementalState {
    /// Dependency graph
    graph: DependencyGraph,
    /// Cache for storing artifacts
    cache: Cache,
    /// Target configuration
    target: TargetConfig,
    /// Compiler version
    compiler_version: String,
}

impl IncrementalState {
    /// Create a new incremental compilation state
    pub fn new(cache: Cache, target: TargetConfig, compiler_version: String) -> Self {
        IncrementalState {
            graph: DependencyGraph::new(),
            cache,
            target,
            compiler_version,
        }
    }

    /// Get the dependency graph
    pub fn graph(&self) -> &DependencyGraph {
        &self.graph
    }

    /// Get a mutable reference to the dependency graph
    pub fn graph_mut(&mut self) -> &mut DependencyGraph {
        &mut self.graph
    }

    /// Get the cache
    pub fn cache(&self) -> &Cache {
        &self.cache
    }

    /// Get a mutable reference to the cache
    pub fn cache_mut(&mut self) -> &mut Cache {
        &mut self.cache
    }

    /// Check if a module needs recompilation
    pub fn needs_rebuild(&mut self, module: &Module, flags: &CompileFlags) -> LinkerResult<bool> {
        // Build dependency hashes
        let dep_hashes: Vec<DependencyHash> = module
            .dependencies
            .iter()
            .filter_map(|dep_id| {
                self.graph.get(*dep_id).map(|dep| {
                    DependencyHash::from_string(dep.source_path.clone(), &dep.source_hash)
                })
            })
            .collect();

        // Compute cache key
        let cache_key = CacheKey::for_file(
            &module.source_path,
            &self.compiler_version,
            flags,
            &dep_hashes,
            &self.target,
        )?;

        // Check if in cache
        if !self.cache.contains(&cache_key) {
            return Ok(true);
        }

        // Check if module has changed
        if module.has_changed()? {
            return Ok(true);
        }

        // Check if any dependencies have changed
        for dep_id in &module.dependencies {
            if let Some(dep) = self.graph.get(*dep_id) {
                if dep.has_changed()? {
                    return Ok(true);
                }
            }
        }

        Ok(false)
    }

    /// Get the cache key for a module
    pub fn cache_key(&self, module: &Module, flags: &CompileFlags) -> LinkerResult<CacheKey> {
        let dep_hashes: Vec<DependencyHash> = module
            .dependencies
            .iter()
            .filter_map(|dep_id| {
                self.graph.get(*dep_id).map(|dep| {
                    DependencyHash::from_string(dep.source_path.clone(), &dep.source_hash)
                })
            })
            .collect();

        CacheKey::for_file(
            &module.source_path,
            &self.compiler_version,
            flags,
            &dep_hashes,
            &self.target,
        )
    }

    /// Save the incremental state to disk
    pub fn save(&self, path: &Path) -> LinkerResult<()> {
        let state = SerializableState {
            modules: self
                .graph
                .all_modules()
                .map(|m| SerializableModule {
                    id: m.id.0,
                    name: m.name.clone(),
                    source_path: m.source_path.clone(),
                    source_hash: m.source_hash.clone(),
                    dependencies: m.dependencies.iter().map(|d| d.0).collect(),
                })
                .collect(),
            target: self.target.triple.clone(),
            compiler_version: self.compiler_version.clone(),
        };

        let json = serde_json::to_string_pretty(&state)
            .map_err(|e| LinkerError::CacheError(format!("Failed to serialize state: {}", e)))?;

        fs::write(path, json)?;
        Ok(())
    }

    /// Load the incremental state from disk
    pub fn load(path: &Path, cache: Cache, target: TargetConfig) -> LinkerResult<Self> {
        let json = fs::read_to_string(path)?;
        let state: SerializableState = serde_json::from_str(&json)
            .map_err(|e| LinkerError::CacheError(format!("Failed to deserialize state: {}", e)))?;

        let mut graph = DependencyGraph::new();

        for module in state.modules {
            graph.add_module(Module {
                id: ModuleId::new(module.id),
                name: module.name,
                source_path: module.source_path,
                source_hash: module.source_hash,
                modified: SystemTime::UNIX_EPOCH,
                dependencies: module.dependencies.into_iter().map(ModuleId::new).collect(),
            });
        }

        Ok(IncrementalState {
            graph,
            cache,
            target,
            compiler_version: state.compiler_version,
        })
    }
}

/// Serializable module information
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct SerializableModule {
    id: u32,
    name: String,
    source_path: PathBuf,
    source_hash: String,
    dependencies: Vec<u32>,
}

/// Serializable state
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct SerializableState {
    modules: Vec<SerializableModule>,
    target: String,
    compiler_version: String,
}

/// Hash content using a simple hash function
fn hash_content(content: &str) -> String {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let mut hasher = DefaultHasher::new();
    content.hash(&mut hasher);
    format!("{:016x}", hasher.finish())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn create_test_module(dir: &TempDir, name: &str, content: &str) -> PathBuf {
        let path = dir.path().join(format!("{}.jet", name));
        fs::write(&path, content).unwrap();
        path
    }

    #[test]
    fn test_module_from_file() {
        let temp_dir = TempDir::new().unwrap();
        let path = create_test_module(&temp_dir, "test", "fn main(): pass");

        let module = Module::from_file(ModuleId::new(0), "test".to_string(), &path).unwrap();

        assert_eq!(module.name, "test");
        assert_eq!(module.source_path, path);
        assert!(!module.source_hash.is_empty());
    }

    #[test]
    fn test_module_has_changed() {
        let temp_dir = TempDir::new().unwrap();
        let path = create_test_module(&temp_dir, "test", "fn main(): pass");

        let module = Module::from_file(ModuleId::new(0), "test".to_string(), &path).unwrap();

        // Should not have changed immediately
        assert!(!module.has_changed().unwrap());

        // Modify the file
        fs::write(&path, "fn main(): print(\"hello\")").unwrap();

        // Should detect change
        assert!(module.has_changed().unwrap());
    }

    #[test]
    fn test_dependency_graph() {
        let mut graph = DependencyGraph::new();

        let temp_dir = TempDir::new().unwrap();
        let path1 = create_test_module(&temp_dir, "mod1", "fn foo(): pass");
        let path2 = create_test_module(&temp_dir, "mod2", "use mod1");

        let id1 = graph
            .add_module_from_file("mod1".to_string(), &path1)
            .unwrap();
        let id2 = graph
            .add_module_from_file("mod2".to_string(), &path2)
            .unwrap();

        graph.add_dependency(id2, id1);

        assert_eq!(graph.get(id1).unwrap().dependencies.len(), 0);
        assert_eq!(graph.get(id2).unwrap().dependencies.len(), 1);
        assert_eq!(graph.get_dependents(id1), &[id2]);
    }

    #[test]
    fn test_transitive_dependents() {
        let mut graph = DependencyGraph::new();

        let temp_dir = TempDir::new().unwrap();
        let path1 = create_test_module(&temp_dir, "mod1", "");
        let path2 = create_test_module(&temp_dir, "mod2", "");
        let path3 = create_test_module(&temp_dir, "mod3", "");

        let id1 = graph
            .add_module_from_file("mod1".to_string(), &path1)
            .unwrap();
        let id2 = graph
            .add_module_from_file("mod2".to_string(), &path2)
            .unwrap();
        let id3 = graph
            .add_module_from_file("mod3".to_string(), &path3)
            .unwrap();

        // id1 -> id2 -> id3
        graph.add_dependency(id2, id1);
        graph.add_dependency(id3, id2);

        let dependents = graph.get_transitive_dependents(id1);
        assert!(dependents.contains(&id2));
        assert!(dependents.contains(&id3));
    }

    #[test]
    fn test_topological_sort() {
        let mut graph = DependencyGraph::new();

        let temp_dir = TempDir::new().unwrap();
        let path1 = create_test_module(&temp_dir, "mod1", "");
        let path2 = create_test_module(&temp_dir, "mod2", "");
        let path3 = create_test_module(&temp_dir, "mod3", "");

        let id1 = graph
            .add_module_from_file("mod1".to_string(), &path1)
            .unwrap();
        let id2 = graph
            .add_module_from_file("mod2".to_string(), &path2)
            .unwrap();
        let id3 = graph
            .add_module_from_file("mod3".to_string(), &path3)
            .unwrap();

        // id3 depends on id2, which depends on id1
        graph.add_dependency(id2, id1);
        graph.add_dependency(id3, id2);

        let sorted = graph.topological_sort();

        // id1 should come before id2, which should come before id3
        let pos1 = sorted.iter().position(|&id| id == id1).unwrap();
        let pos2 = sorted.iter().position(|&id| id == id2).unwrap();
        let pos3 = sorted.iter().position(|&id| id == id3).unwrap();

        assert!(pos1 < pos2);
        assert!(pos2 < pos3);
    }

    #[test]
    fn test_incremental_state_save_load() {
        let temp_dir = TempDir::new().unwrap();
        let cache_dir = temp_dir.path().join("cache");
        let state_file = temp_dir.path().join("state.json");

        let cache = Cache::new(cache_dir).unwrap();
        let target = TargetConfig::native();

        {
            let mut state = IncrementalState::new(cache, target.clone(), "1.0.0".to_string());

            let path = create_test_module(&temp_dir, "test", "fn main(): pass");
            state
                .graph_mut()
                .add_module_from_file("test".to_string(), &path)
                .unwrap();

            state.save(&state_file).unwrap();
        }

        // Load the state
        let cache = Cache::new(temp_dir.path().join("cache2")).unwrap();
        let loaded = IncrementalState::load(&state_file, cache, target).unwrap();

        assert_eq!(loaded.graph().len(), 1);
        assert_eq!(loaded.compiler_version, "1.0.0");
    }
}
