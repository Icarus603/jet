//! Collection logic for Immix GC
//!
//! This module implements the mark-sweep collection algorithm with evacuation support.

use crate::allocator::Allocator;
use crate::block::{Block, BlockList, BlockState, LINES_PER_BLOCK};
use crate::header::ObjectHeader;
use std::collections::VecDeque;
use std::ptr::NonNull;

/// Collection statistics
#[derive(Debug, Clone, Default)]
pub struct CollectionStats {
    /// Number of objects marked
    pub objects_marked: usize,
    /// Number of objects evacuated
    pub objects_evacuated: usize,
    /// Bytes reclaimed
    pub bytes_reclaimed: usize,
    /// Time spent in mark phase (microseconds)
    pub mark_time_us: u64,
    /// Time spent in sweep phase (microseconds)
    pub sweep_time_us: u64,
}

/// Mark stack for tracing objects
pub struct MarkStack {
    /// Stack of objects to process
    stack: VecDeque<*mut ObjectHeader>,
}

impl MarkStack {
    /// Create a new mark stack
    pub fn new() -> Self {
        MarkStack {
            stack: VecDeque::new(),
        }
    }

    /// Push an object onto the stack
    pub fn push(&mut self, object: *mut ObjectHeader) {
        self.stack.push_back(object);
    }

    /// Pop an object from the stack
    pub fn pop(&mut self) -> Option<*mut ObjectHeader> {
        self.stack.pop_front()
    }

    /// Check if stack is empty
    pub fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    /// Get stack size
    pub fn len(&self) -> usize {
        self.stack.len()
    }

    /// Clear the stack
    pub fn clear(&mut self) {
        self.stack.clear();
    }
}

impl Default for MarkStack {
    fn default() -> Self {
        Self::new()
    }
}

/// Collector state
pub struct Collector {
    /// Mark stack for tracing
    mark_stack: MarkStack,
    /// Collection statistics
    stats: CollectionStats,
    /// Evacuation candidates
    evacuation_candidates: Vec<NonNull<Block>>,
}

impl Collector {
    /// Create a new collector
    pub fn new() -> Self {
        Collector {
            mark_stack: MarkStack::new(),
            stats: CollectionStats::default(),
            evacuation_candidates: Vec::new(),
        }
    }

    /// Perform nursery collection
    ///
    /// # Arguments
    /// * `nursery` - Nursery allocator containing blocks to collect
    /// * `mature` - Mature space allocator for promoted objects
    /// * `roots` - Root pointers to start tracing from
    pub fn collect_nursery(
        &mut self,
        nursery: &mut Allocator,
        mature: &mut Allocator,
        roots: &[*mut ObjectHeader],
    ) -> CollectionStats {
        let _start_time = std::time::Instant::now();
        self.stats = CollectionStats::default();

        // Take all nursery blocks
        let mut nursery_blocks = nursery.take_all_blocks();

        // Mark phase: trace from roots
        let mark_start = std::time::Instant::now();
        self.mark_from_roots(roots, &mut nursery_blocks);
        self.stats.mark_time_us = mark_start.elapsed().as_micros() as u64;

        // Sweep phase: reclaim unmarked objects, promote survivors
        let sweep_start = std::time::Instant::now();
        self.sweep_nursery(&mut nursery_blocks, mature);
        self.stats.sweep_time_us = sweep_start.elapsed().as_micros() as u64;

        // Return empty blocks to nursery
        nursery.add_blocks(nursery_blocks);

        self.stats.clone()
    }

    /// Perform full collection
    ///
    /// # Arguments
    /// * `nursery` - Nursery allocator
    /// * `mature` - Mature space allocator
    /// * `roots` - Root pointers
    pub fn collect_full(
        &mut self,
        nursery: &mut Allocator,
        mature: &mut Allocator,
        roots: &[*mut ObjectHeader],
    ) -> CollectionStats {
        let _start_time = std::time::Instant::now();
        self.stats = CollectionStats::default();

        // Take all blocks from both spaces
        let mut all_blocks = nursery.take_all_blocks();
        let mature_blocks = mature.take_all_blocks();

        // Combine blocks
        for block in mature_blocks.iter() {
            all_blocks.push(block);
        }

        // Mark phase
        let mark_start = std::time::Instant::now();
        self.mark_from_roots(roots, &mut all_blocks);
        self.stats.mark_time_us = mark_start.elapsed().as_micros() as u64;

        // Sweep phase with evacuation
        let sweep_start = std::time::Instant::now();
        self.sweep_with_evacuation(&mut all_blocks, mature);
        self.stats.sweep_time_us = sweep_start.elapsed().as_micros() as u64;

        // Distribute blocks back
        self.distribute_blocks(all_blocks, nursery, mature);

        self.stats.clone()
    }

    /// Mark objects starting from roots
    fn mark_from_roots(&mut self, roots: &[*mut ObjectHeader], blocks: &mut BlockList) {
        // Clear all marks first
        for block in blocks.iter_mut() {
            unsafe {
                block.as_ptr().as_mut().unwrap().clear_marks();
            }
        }

        // Initialize mark stack with roots
        for &root in roots {
            if !root.is_null() {
                unsafe {
                    self.mark_object(root, blocks);
                }
            }
        }

        // Process mark stack
        while let Some(object) = self.mark_stack.pop() {
            unsafe {
                self.process_object(object, blocks);
            }
        }
    }

    /// Mark a single object
    unsafe fn mark_object(&mut self, object: *mut ObjectHeader, blocks: &mut BlockList) {
        let header = &*object;

        // Try to mark (returns false if already marked)
        if !header.try_mark() {
            return;
        }

        self.stats.objects_marked += 1;

        // Find containing block and mark lines
        if let Some(block) = self.find_block_for(object, blocks) {
            let block_ref = block.as_ptr().as_mut().unwrap();

            // Calculate line range for this object
            let obj_start = object as *const u8;
            let obj_end = obj_start.add(ObjectHeader::SIZE + header.object_size());

            let start_line = block_ref.line_index(obj_start);
            let end_line = block_ref.line_index(obj_end.sub(1));

            // Mark all lines containing this object
            for i in start_line..=end_line.min(LINES_PER_BLOCK - 1) {
                block_ref.mark_line(i);
            }
        }

        // Add to mark stack for tracing
        self.mark_stack.push(object);
    }

    /// Process an object during marking (trace references)
    unsafe fn process_object(&mut self, object: *mut ObjectHeader, blocks: &mut BlockList) {
        let header = &*object;

        // Get object payload
        let payload = header.payload_ptr();
        let size = header.object_size();

        // Scan payload for pointers
        // This is a conservative scan - we check every aligned word
        let words = size / std::mem::size_of::<usize>();

        for i in 0..words {
            let word_ptr = payload.add(i * std::mem::size_of::<usize>()) as *const usize;
            let word = *word_ptr;

            // Check if this looks like a heap pointer
            if let Some(referenced) = self.is_heap_pointer(word as *const u8, blocks) {
                self.mark_object(referenced, blocks);
            }
        }
    }

    /// Check if a pointer points into the heap
    unsafe fn is_heap_pointer(
        &self,
        ptr: *const u8,
        blocks: &BlockList,
    ) -> Option<*mut ObjectHeader> {
        // Quick check: must be non-null and reasonably aligned
        if ptr.is_null() || (ptr as usize) < 0x1000 {
            return None;
        }

        // Find which block (if any) contains this pointer
        for block in blocks.iter() {
            let block_ref = block.as_ptr().as_ref().unwrap();
            let block_start = block_ref.memory;
            let block_end = block_start.add(crate::block::BLOCK_SIZE);

            if ptr >= block_start && ptr < block_end {
                // Pointer is within this block
                // Find the object header
                let line_idx = block_ref.line_index(ptr);

                // Scan backwards to find object start
                for i in (0..=line_idx).rev() {
                    if block_ref.has_object_start(i) {
                        return block_ref.get_object_at_line(i);
                    }
                }
            }
        }

        None
    }

    /// Find the block containing an object
    fn find_block_for(
        &self,
        object: *mut ObjectHeader,
        blocks: &BlockList,
    ) -> Option<NonNull<Block>> {
        let obj_ptr = object as *const u8;

        for block in blocks.iter() {
            unsafe {
                let block_ref = block.as_ptr().as_ref().unwrap();
                let block_start = block_ref.memory;
                let block_end = block_start.add(crate::block::BLOCK_SIZE);

                if obj_ptr >= block_start && obj_ptr < block_end {
                    return Some(block);
                }
            }
        }

        None
    }

    /// Sweep nursery: promote survivors, reclaim dead objects
    fn sweep_nursery(&mut self, blocks: &mut BlockList, mature: &mut Allocator) {
        let mut survivors = BlockList::new();

        while let Some(block) = blocks.pop() {
            unsafe {
                let block_ref = block.as_ptr().as_mut().unwrap();

                // Check each object in the block
                for i in 0..LINES_PER_BLOCK {
                    if block_ref.has_object_start(i) {
                        if let Some(obj) = block_ref.get_object_at_line(i) {
                            let header = &*obj;

                            if header.is_marked() {
                                // Promote to mature space
                                self.promote_object(obj, mature);
                            }
                        }
                    }
                }

                // Reset block for reuse
                block_ref.reset();
                survivors.push(block);
            }
        }

        // Return survivors to nursery
        while let Some(block) = survivors.pop() {
            blocks.push(block);
        }
    }

    /// Promote an object to mature space
    unsafe fn promote_object(&mut self, object: *mut ObjectHeader, mature: &mut Allocator) {
        let header = &*object;
        let size = header.total_size();
        let align = header.align as usize;

        // Allocate in mature space
        if let Some(new_ptr) = mature.allocate(size, align) {
            // Copy object
            std::ptr::copy_nonoverlapping(object as *const u8, new_ptr, size);

            // Update header flags
            let new_header = &mut *(new_ptr as *mut ObjectHeader);
            new_header.set_flag(crate::header::GcFlags::Nursery, false);
            new_header.set_flag(crate::header::GcFlags::Mature, true);

            // Clear mark for next cycle
            new_header.clear_mark();

            self.stats.objects_evacuated += 1;
        }
    }

    /// Sweep with evacuation for mature space
    fn sweep_with_evacuation(&mut self, blocks: &mut BlockList, mature: &mut Allocator) {
        // First pass: identify evacuation candidates
        self.identify_evacuation_candidates(blocks);

        // Second pass: sweep and evacuate
        let mut kept_blocks = BlockList::new();

        while let Some(block) = blocks.pop() {
            unsafe {
                let block_ref = block.as_ptr().as_mut().unwrap();

                if self.should_evacuate_block(block) {
                    // Evacuate live objects
                    for i in 0..LINES_PER_BLOCK {
                        if block_ref.has_object_start(i) {
                            if let Some(obj) = block_ref.get_object_at_line(i) {
                                let header = &*obj;

                                if header.is_marked() {
                                    self.evacuate_object(obj, mature);
                                }
                            }
                        }
                    }

                    // Reset and keep block
                    block_ref.reset();
                    kept_blocks.push(block);
                } else {
                    // Normal sweep
                    let reclaimed = block_ref.sweep();
                    self.stats.bytes_reclaimed += reclaimed;

                    if block_ref.state != BlockState::Empty {
                        kept_blocks.push(block);
                    }
                }
            }
        }

        // Return kept blocks
        while let Some(block) = kept_blocks.pop() {
            blocks.push(block);
        }

        self.evacuation_candidates.clear();
    }

    /// Identify blocks that should be evacuated
    fn identify_evacuation_candidates(&mut self, blocks: &BlockList) {
        self.evacuation_candidates.clear();

        for block in blocks.iter() {
            unsafe {
                let block_ref = block.as_ptr().as_ref().unwrap();

                if block_ref.should_evacuate() {
                    self.evacuation_candidates.push(block);
                }
            }
        }
    }

    /// Check if a block should be evacuated
    fn should_evacuate_block(&self, block: NonNull<Block>) -> bool {
        self.evacuation_candidates.contains(&block)
    }

    /// Evacuate an object to a new location
    unsafe fn evacuate_object(&mut self, object: *mut ObjectHeader, mature: &mut Allocator) {
        let header = &*object;
        let size = header.total_size();
        let align = header.align as usize;

        // Allocate in mature space
        if let Some(new_ptr) = mature.allocate(size, align) {
            // Copy object
            std::ptr::copy_nonoverlapping(object as *const u8, new_ptr, size);

            // Update forwarding pointer (stored in header word)
            let new_header = &mut *(new_ptr as *mut ObjectHeader);
            new_header.clear_mark();

            self.stats.objects_evacuated += 1;
        }
    }

    /// Distribute blocks back to nursery and mature spaces
    fn distribute_blocks(
        &self,
        mut blocks: BlockList,
        nursery: &mut Allocator,
        mature: &mut Allocator,
    ) {
        let mut nursery_blocks = BlockList::new();
        let mut mature_blocks = BlockList::new();

        while let Some(block) = blocks.pop() {
            unsafe {
                let block_ref = block.as_ptr().as_mut().unwrap();

                if block_ref.is_nursery {
                    nursery_blocks.push(block);
                } else {
                    mature_blocks.push(block);
                }
            }
        }

        nursery.add_blocks(nursery_blocks);
        mature.add_blocks(mature_blocks);
    }

    /// Get collection statistics
    pub fn stats(&self) -> &CollectionStats {
        &self.stats
    }

    /// Reset collector state
    pub fn reset(&mut self) {
        self.mark_stack.clear();
        self.stats = CollectionStats::default();
        self.evacuation_candidates.clear();
    }
}

impl Default for Collector {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mark_stack() {
        let mut stack = MarkStack::new();
        assert!(stack.is_empty());

        let dummy = std::ptr::null_mut();
        stack.push(dummy);
        assert_eq!(stack.len(), 1);

        assert_eq!(stack.pop(), Some(dummy));
        assert!(stack.is_empty());
    }

    #[test]
    fn test_collector_creation() {
        let collector = Collector::new();
        assert!(collector.mark_stack.is_empty());
        assert_eq!(collector.stats.objects_marked, 0);
    }

    #[test]
    fn test_collection_stats() {
        let stats = CollectionStats {
            objects_marked: 10,
            objects_evacuated: 5,
            bytes_reclaimed: 1024,
            mark_time_us: 100,
            sweep_time_us: 50,
        };

        assert_eq!(stats.objects_marked, 10);
        assert_eq!(stats.objects_evacuated, 5);
        assert_eq!(stats.bytes_reclaimed, 1024);
    }
}
