//! Lowering context for AST to IR conversion.
//!
//! This module provides the `LoweringContext` struct which maintains the state
//! during the lowering process from the typed AST to Jet IR.

use jet_ir::{BasicBlock, BlockId, Function, Instruction, Module, Terminator, Ty, ValueId};
use std::collections::HashMap;

/// A scope in the variable stack.
#[derive(Debug, Clone)]
pub struct Scope {
    /// Maps variable names to their value IDs.
    variables: HashMap<String, ValueId>,
    /// Maps variable names to their types.
    types: HashMap<String, Ty>,
    /// Maps variable names to their mutability.
    mutable: HashMap<String, bool>,
}

impl Scope {
    /// Creates a new empty scope.
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            types: HashMap::new(),
            mutable: HashMap::new(),
        }
    }

    /// Inserts a variable into the scope.
    pub fn insert(&mut self, name: String, value: ValueId, ty: Ty, is_mut: bool) {
        self.variables.insert(name.clone(), value);
        self.types.insert(name.clone(), ty);
        self.mutable.insert(name, is_mut);
    }

    /// Gets the value ID for a variable.
    pub fn get(&self, name: &str) -> Option<ValueId> {
        self.variables.get(name).copied()
    }

    /// Gets the type for a variable.
    pub fn get_type(&self, name: &str) -> Option<&Ty> {
        self.types.get(name)
    }

    /// Checks if a variable is mutable.
    pub fn is_mutable(&self, name: &str) -> Option<bool> {
        self.mutable.get(name).copied()
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

/// Information about a struct type for field access.
#[derive(Debug, Clone)]
pub struct StructInfo {
    /// The name of the struct.
    pub name: String,
    /// Field names in order.
    pub field_names: Vec<String>,
    /// Field types in order.
    pub field_types: Vec<Ty>,
}

impl StructInfo {
    /// Gets the index of a field by name.
    pub fn get_field_index(&self, field_name: &str) -> Option<usize> {
        self.field_names.iter().position(|name| name == field_name)
    }

    /// Gets the type of a field by name.
    pub fn get_field_type(&self, field_name: &str) -> Option<&Ty> {
        self.get_field_index(field_name)
            .map(|idx| &self.field_types[idx])
    }
}

/// Context for lowering AST to IR.
///
/// This struct maintains all the state needed during the lowering process,
/// including the current function, block, value generation, and variable scopes.
#[derive(Debug)]
pub struct LoweringContext {
    /// The module being built.
    pub module: Module,
    /// The ID of the current function being lowered.
    current_function: Option<usize>,
    /// The ID of the current basic block being built.
    current_block: Option<BlockId>,
    /// Counter for generating unique value IDs.
    value_counter: u32,
    /// Counter for generating unique block IDs.
    block_counter: u32,
    /// Stack of scopes for variable lookup.
    scope_stack: Vec<Scope>,
    /// Handler context for effect operations.
    handler_context: Option<HandlerContext>,
    /// Break/continue target stack for loops.
    loop_targets: Vec<LoopTarget>,
    /// Struct type definitions for field access.
    struct_types: HashMap<String, StructInfo>,
}

/// Information about a loop for break/continue targets.
#[derive(Debug, Clone)]
pub struct LoopTarget {
    /// The label of the loop (if any).
    pub label: Option<String>,
    /// Block to break to.
    pub break_block: BlockId,
    /// Block to continue to.
    pub continue_block: BlockId,
    /// Optional value to return on break.
    pub break_value: Option<ValueId>,
}

/// Context for effect handlers.
#[derive(Debug, Clone)]
pub struct HandlerContext {
    /// The resume block for continuing after an effect.
    pub resume_block: BlockId,
    /// Maps operation names to their handler blocks.
    pub handlers: HashMap<String, BlockId>,
}

impl LoweringContext {
    /// Creates a new lowering context for the given module name.
    pub fn new(module_name: impl Into<String>) -> Self {
        Self {
            module: Module::new(module_name),
            current_function: None,
            current_block: None,
            value_counter: 0,
            block_counter: 0,
            scope_stack: Vec::new(),
            handler_context: None,
            loop_targets: Vec::new(),
            struct_types: HashMap::new(),
        }
    }

    /// Generates a new unique value ID.
    pub fn new_value(&mut self) -> ValueId {
        let id = ValueId::new(self.value_counter);
        self.value_counter += 1;
        id
    }

    /// Returns the next value ID without incrementing the counter.
    pub fn next_value_id(&self) -> ValueId {
        ValueId::new(self.value_counter)
    }

    /// Generates a new unique block ID.
    pub fn new_block_id(&mut self) -> BlockId {
        let id = BlockId::new(self.block_counter);
        self.block_counter += 1;
        id
    }

    /// Creates a new basic block and adds it to the current function.
    pub fn create_block(&mut self, name: impl Into<String>) -> BlockId {
        let id = self.new_block_id();
        let block = BasicBlock::with_name(id, name);

        if let Some(func_idx) = self.current_function {
            self.module.functions[func_idx].add_block(block);
        }

        id
    }

    /// Sets the current block for instruction emission.
    pub fn set_current_block(&mut self, block_id: BlockId) {
        self.current_block = Some(block_id);
    }

    /// Gets the current block ID.
    pub fn current_block(&self) -> Option<BlockId> {
        self.current_block
    }

    /// Gets a mutable reference to the current basic block.
    pub fn get_current_block_mut(&mut self) -> Option<&mut BasicBlock> {
        let block_id = self.current_block?;
        let func_idx = self.current_function?;
        self.module.functions[func_idx]
            .blocks
            .iter_mut()
            .find(|b| b.id == block_id)
    }

    /// Gets a reference to the current basic block.
    pub fn get_current_block(&self) -> Option<&BasicBlock> {
        let block_id = self.current_block?;
        let func_idx = self.current_function?;
        self.module.functions[func_idx]
            .blocks
            .iter()
            .find(|b| b.id == block_id)
    }

    /// Emits an instruction into the current block.
    pub fn emit(&mut self, instruction: Instruction) {
        if let Some(block) = self.get_current_block_mut() {
            block.add_instruction(instruction);
        }
    }

    /// Sets the terminator for the current block.
    pub fn terminate(&mut self, terminator: Terminator) {
        if let Some(block) = self.get_current_block_mut() {
            block.set_terminator(terminator);
        }
    }

    /// Starts lowering a new function.
    pub fn start_function(&mut self, index: usize) {
        self.current_function = Some(index);
        self.value_counter = 0;
        self.block_counter = 0;
        self.scope_stack.clear();
        self.enter_scope();
    }

    /// Ends the current function lowering.
    pub fn end_function(&mut self) {
        self.exit_scope();
        self.current_function = None;
        self.current_block = None;
    }

    /// Enters a new scope.
    pub fn enter_scope(&mut self) {
        self.scope_stack.push(Scope::new());
    }

    /// Exits the current scope.
    pub fn exit_scope(&mut self) {
        self.scope_stack.pop();
    }

    /// Looks up a variable in the scope stack.
    pub fn lookup_variable(&self, name: &str) -> Option<ValueId> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }

    /// Looks up a variable's type in the scope stack.
    pub fn lookup_variable_type(&self, name: &str) -> Option<&Ty> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(ty) = scope.get_type(name) {
                return Some(ty);
            }
        }
        None
    }

    /// Checks if a variable is mutable.
    pub fn is_variable_mutable(&self, name: &str) -> Option<bool> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(is_mut) = scope.is_mutable(name) {
                return Some(is_mut);
            }
        }
        None
    }

    /// Binds a variable in the current scope.
    pub fn bind_variable(&mut self, name: String, value: ValueId, ty: Ty, is_mut: bool) {
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.insert(name, value, ty, is_mut);
        }
    }

    /// Pushes a handler context for effect operations.
    pub fn push_handler_context(&mut self, context: HandlerContext) -> Option<HandlerContext> {
        let old = self.handler_context.clone();
        self.handler_context = Some(context);
        old
    }

    /// Pops the handler context.
    pub fn pop_handler_context(&mut self, old: Option<HandlerContext>) {
        self.handler_context = old;
    }

    /// Gets the current handler context.
    pub fn handler_context(&self) -> Option<&HandlerContext> {
        self.handler_context.as_ref()
    }

    /// Pushes a loop target for break/continue.
    pub fn push_loop_target(&mut self, target: LoopTarget) {
        self.loop_targets.push(target);
    }

    /// Pops a loop target.
    pub fn pop_loop_target(&mut self) -> Option<LoopTarget> {
        self.loop_targets.pop()
    }

    /// Finds a loop target by label (or the innermost one if no label).
    pub fn find_loop_target(&self, label: Option<&str>) -> Option<&LoopTarget> {
        if let Some(lbl) = label {
            self.loop_targets
                .iter()
                .rev()
                .find(|t| t.label.as_deref() == Some(lbl))
        } else {
            self.loop_targets.last()
        }
    }

    /// Gets a mutable reference to the current function.
    pub fn current_function_mut(&mut self) -> Option<&mut Function> {
        self.current_function
            .map(|idx| &mut self.module.functions[idx])
    }

    /// Gets a reference to the current function.
    pub fn current_function(&self) -> Option<&Function> {
        self.current_function.map(|idx| &self.module.functions[idx])
    }

    /// Resets the value and block counters for a new function.
    pub fn reset_counters(&mut self) {
        self.value_counter = 0;
        self.block_counter = 0;
    }

    /// Registers a struct type definition.
    pub fn register_struct(&mut self, info: StructInfo) {
        self.struct_types.insert(info.name.clone(), info);
    }

    /// Looks up a struct type by name.
    pub fn lookup_struct(&self, name: &str) -> Option<&StructInfo> {
        self.struct_types.get(name)
    }

    /// Gets the field index for a struct field.
    pub fn get_field_index(&self, struct_name: &str, field_name: &str) -> Option<usize> {
        self.lookup_struct(struct_name)
            .and_then(|s| s.get_field_index(field_name))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scope_operations() {
        let mut scope = Scope::new();
        scope.insert("x".to_string(), ValueId::new(0), Ty::I32, false);

        assert_eq!(scope.get("x"), Some(ValueId::new(0)));
        assert_eq!(scope.get_type("x"), Some(&Ty::I32));
        assert_eq!(scope.is_mutable("x"), Some(false));
        assert!(scope.get("y").is_none());
    }

    #[test]
    fn test_lowering_context_value_generation() {
        let mut ctx = LoweringContext::new("test");

        let v1 = ctx.new_value();
        let v2 = ctx.new_value();

        assert_eq!(v1, ValueId::new(0));
        assert_eq!(v2, ValueId::new(1));
    }

    #[test]
    fn test_lowering_context_block_creation() {
        let mut ctx = LoweringContext::new("test");

        // Add a function first
        let func = Function::new("test_func", vec![], Ty::Void);
        ctx.module.add_function(func);
        ctx.start_function(0);

        let block_id = ctx.create_block("entry");
        assert_eq!(block_id, BlockId::new(0));

        ctx.set_current_block(block_id);
        assert_eq!(ctx.current_block(), Some(block_id));
    }

    #[test]
    fn test_variable_lookup() {
        let mut ctx = LoweringContext::new("test");

        ctx.enter_scope();
        ctx.bind_variable("x".to_string(), ValueId::new(0), Ty::I32, false);

        assert_eq!(ctx.lookup_variable("x"), Some(ValueId::new(0)));
        assert_eq!(ctx.lookup_variable_type("x"), Some(&Ty::I32));

        ctx.enter_scope();
        ctx.bind_variable("y".to_string(), ValueId::new(1), Ty::I64, true);

        // Should find x in outer scope
        assert_eq!(ctx.lookup_variable("x"), Some(ValueId::new(0)));
        // Should find y in inner scope
        assert_eq!(ctx.lookup_variable("y"), Some(ValueId::new(1)));

        ctx.exit_scope();
        // After exiting, y should not be found
        assert_eq!(ctx.lookup_variable("y"), None);
        // x should still be found
        assert_eq!(ctx.lookup_variable("x"), Some(ValueId::new(0)));
    }

    #[test]
    fn test_loop_targets() {
        let mut ctx = LoweringContext::new("test");

        let target1 = LoopTarget {
            label: Some("outer".to_string()),
            break_block: BlockId::new(0),
            continue_block: BlockId::new(1),
            break_value: None,
        };

        let target2 = LoopTarget {
            label: None,
            break_block: BlockId::new(2),
            continue_block: BlockId::new(3),
            break_value: None,
        };

        ctx.push_loop_target(target1);
        ctx.push_loop_target(target2);

        // Should find innermost loop without label
        let found = ctx.find_loop_target(None);
        assert_eq!(found.unwrap().break_block, BlockId::new(2));

        // Should find outer loop by label
        let found = ctx.find_loop_target(Some("outer"));
        assert_eq!(found.unwrap().break_block, BlockId::new(0));
    }
}
