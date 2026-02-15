//! Allocation logic for Immix GC
//!
//! This module provides fast bump allocation and fallback allocation strategies.

use crate::block::{Block, BlockList, BlockState, BLOCK_SIZE};
use crate::header::{align_up, ObjectHeader};
use std::ptr::NonNull;

/// Large object threshold (larger objects go to LOS)
pub const LARGE_OBJECT_THRESHOLD: usize = BLOCK_SIZE / 2; // 16KB

/// Minimum nursery size before triggering collection
pub const DEFAULT_NURSERY_THRESHOLD: usize = 256 * 1024; // 256KB

/// Allocator state
pub struct Allocator {
    /// Current block for bump allocation
    current_block: Option<NonNull<Block>>,
    /// List of blocks with available space
    recyclable_blocks: BlockList,
    /// Total bytes allocated
    allocated_bytes: usize,
    /// Collection threshold
    threshold: usize,
}

impl Allocator {
    /// Create a new allocator
    pub fn new(threshold: usize) -> Self {
        Allocator {
            current_block: None,
            recyclable_blocks: BlockList::new(),
            allocated_bytes: 0,
            threshold,
        }
    }

    /// Check if collection is needed
    pub fn needs_collection(&self) -> bool {
        self.allocated_bytes >= self.threshold
    }

    /// Get total allocated bytes
    pub fn allocated_bytes(&self) -> usize {
        self.allocated_bytes
    }

    /// Set collection threshold
    pub fn set_threshold(&mut self, threshold: usize) {
        self.threshold = threshold;
    }

    /// Allocate memory with given size and alignment
    ///
    /// This is the main allocation entry point. It tries fast bump allocation
    /// first, then falls back to hole punching or new block allocation.
    pub fn allocate(&mut self, size: usize, align: usize) -> Option<*mut u8> {
        // Try fast path first
        if let Some(ptr) = self.try_allocate_fast(size, align) {
            self.allocated_bytes += align_up(size, align.max(8));
            return Some(ptr);
        }

        // Try slow path
        if let Some(ptr) = self.try_allocate_slow(size, align) {
            self.allocated_bytes += align_up(size, align.max(8));
            return Some(ptr);
        }

        None
    }

    /// Fast allocation path - bump pointer in current block
    fn try_allocate_fast(&mut self, size: usize, align: usize) -> Option<*mut u8> {
        if let Some(block) = self.current_block {
            unsafe {
                let block_ref = block.as_ptr().as_mut().unwrap();
                if let Some(ptr) = block_ref.allocate(size, align) {
                    return Some(ptr);
                }
            }
        }
        None
    }

    /// Slow allocation path - find new block or use hole punching
    fn try_allocate_slow(&mut self, size: usize, align: usize) -> Option<*mut u8> {
        // Move current block to recyclable if it has space
        if let Some(block) = self.current_block.take() {
            unsafe {
                let block_ref = block.as_ptr().as_mut().unwrap();
                if block_ref.free_lines > 0 {
                    block_ref.update_state();
                    self.recyclable_blocks.push(block);
                }
            }
        }

        // Try to find a recyclable block
        if let Some(block) = self.recyclable_blocks.pop() {
            unsafe {
                let block_ref = block.as_ptr().as_mut().unwrap();

                // Try bump allocation first
                if let Some(ptr) = block_ref.allocate(size, align) {
                    self.current_block = Some(block);
                    return Some(ptr);
                }

                // Try hole punching
                if let Some(ptr) = block_ref.allocate_from_hole(size, align) {
                    self.current_block = Some(block);
                    return Some(ptr);
                }

                // Block is full, keep it out of recyclable
            }
        }

        // Allocate a new block
        let new_block = Block::new()?;
        self.current_block = Some(new_block);

        // Try allocation again
        self.try_allocate_fast(size, align)
    }

    /// Get the current block
    pub fn current_block(&self) -> Option<NonNull<Block>> {
        self.current_block
    }

    /// Move all blocks to a block list (for collection)
    pub fn take_all_blocks(&mut self) -> BlockList {
        let mut all_blocks = BlockList::new();

        // Take current block
        if let Some(block) = self.current_block.take() {
            all_blocks.push(block);
        }

        // Take recyclable blocks
        while let Some(block) = self.recyclable_blocks.pop() {
            all_blocks.push(block);
        }

        self.allocated_bytes = 0;
        all_blocks
    }

    /// Add blocks back after collection
    pub fn add_blocks(&mut self, blocks: BlockList) {
        for block in blocks.iter() {
            unsafe {
                let block_ref = block.as_ptr().as_mut().unwrap();
                block_ref.update_state();

                match block_ref.state {
                    BlockState::Empty => {
                        // Can be reused as current block
                        if self.current_block.is_none() {
                            self.current_block = Some(block);
                        } else {
                            self.recyclable_blocks.push(block);
                        }
                    }
                    BlockState::Recyclable => {
                        self.recyclable_blocks.push(block);
                    }
                    BlockState::Full | BlockState::Evacuate => {
                        // Keep full blocks separate or handle evacuation
                    }
                }
            }
        }
    }

    /// Reset the allocator (clear all blocks)
    pub fn reset(&mut self) {
        self.current_block = None;
        self.recyclable_blocks = BlockList::new();
        self.allocated_bytes = 0;
    }
}

/// Large object space for objects > 16KB
pub struct LargeObjectSpace {
    /// List of large objects
    objects: Vec<LargeObject>,
    /// Total bytes allocated
    total_bytes: usize,
}

struct LargeObject {
    /// Pointer to the object (header + payload)
    ptr: *mut u8,
    /// Total size including header
    size: usize,
    /// Mark bit for collection
    marked: bool,
}

// LargeObjectSpace is Send but not Sync
unsafe impl Send for LargeObjectSpace {}

impl LargeObjectSpace {
    /// Create a new large object space
    pub fn new() -> Self {
        LargeObjectSpace {
            objects: Vec::new(),
            total_bytes: 0,
        }
    }

    /// Allocate a large object
    ///
    /// # Safety
    /// This allocates raw memory
    pub unsafe fn allocate(&mut self, size: usize, align: usize) -> Option<*mut u8> {
        let total_size = ObjectHeader::SIZE + size;
        let layout = std::alloc::Layout::from_size_align(total_size, align.max(8)).ok()?;
        let ptr = std::alloc::alloc(layout);

        if ptr.is_null() {
            return None;
        }

        self.objects.push(LargeObject {
            ptr,
            size: total_size,
            marked: false,
        });

        self.total_bytes += total_size;

        // Return pointer to payload (after header)
        Some(ptr.add(ObjectHeader::SIZE))
    }

    /// Find the object containing a pointer
    pub fn find_object(&self, ptr: *const u8) -> Option<*mut u8> {
        for obj in &self.objects {
            let start = obj.ptr;
            let end = unsafe { obj.ptr.add(obj.size) };

            if ptr >= start && ptr < end {
                return Some(obj.ptr);
            }
        }
        None
    }

    /// Mark a large object
    pub fn mark_object(&mut self, ptr: *mut u8) -> bool {
        for obj in &mut self.objects {
            if obj.ptr == ptr {
                if obj.marked {
                    return false;
                }
                obj.marked = true;
                return true;
            }
        }
        false
    }

    /// Sweep unmarked large objects
    pub fn sweep(&mut self) -> usize {
        let mut reclaimed = 0;

        self.objects.retain(|obj| {
            if obj.marked {
                true // Keep marked objects
            } else {
                // Free unmarked object
                unsafe {
                    let layout = std::alloc::Layout::from_size_align(obj.size, 8).unwrap();
                    std::alloc::dealloc(obj.ptr, layout);
                }
                reclaimed += obj.size;
                false
            }
        });

        self.total_bytes -= reclaimed;
        reclaimed
    }

    /// Clear all marks
    pub fn clear_marks(&mut self) {
        for obj in &mut self.objects {
            obj.marked = false;
        }
    }

    /// Get total bytes allocated
    pub fn total_bytes(&self) -> usize {
        self.total_bytes
    }

    /// Get number of objects
    pub fn len(&self) -> usize {
        self.objects.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.objects.is_empty()
    }

    /// Iterate over objects
    pub fn iter(&self) -> impl Iterator<Item = *mut u8> + '_ {
        self.objects.iter().map(|obj| obj.ptr)
    }
}

impl Default for LargeObjectSpace {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for LargeObjectSpace {
    fn drop(&mut self) {
        for obj in &self.objects {
            unsafe {
                let layout = std::alloc::Layout::from_size_align(obj.size, 8).unwrap();
                std::alloc::dealloc(obj.ptr, layout);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_allocator_creation() {
        let allocator = Allocator::new(DEFAULT_NURSERY_THRESHOLD);
        assert!(!allocator.needs_collection());
        assert_eq!(allocator.allocated_bytes(), 0);
    }

    #[test]
    fn test_allocator_basic() {
        let mut allocator = Allocator::new(DEFAULT_NURSERY_THRESHOLD);

        let ptr = allocator.allocate(100, 8).expect("Allocation failed");
        assert!(!ptr.is_null());

        let ptr2 = allocator.allocate(200, 8).expect("Allocation failed");
        assert!(!ptr2.is_null());
        assert_ne!(ptr, ptr2);
    }

    #[test]
    fn test_allocator_threshold() {
        let mut allocator = Allocator::new(256); // Small threshold for testing

        // Allocate enough to trigger collection need
        for _ in 0..10 {
            let _ = allocator.allocate(100, 8);
        }

        assert!(allocator.needs_collection());
    }

    #[test]
    fn test_large_object_space() {
        let mut los = LargeObjectSpace::new();
        assert!(los.is_empty());

        unsafe {
            let ptr = los.allocate(1000, 8).expect("Allocation failed");
            assert!(!ptr.is_null());
            assert_eq!(los.len(), 1);
            assert!(los.total_bytes() > 0);
        }
    }

    #[test]
    fn test_large_object_mark_sweep() {
        let mut los = LargeObjectSpace::new();

        unsafe {
            let ptr = los.allocate(1000, 8).expect("Allocation failed");
            let header_ptr = ptr.sub(ObjectHeader::SIZE);

            // Mark the object
            assert!(los.mark_object(header_ptr));
            assert!(!los.mark_object(header_ptr)); // Already marked

            // Sweep should not reclaim marked object
            let reclaimed = los.sweep();
            assert_eq!(reclaimed, 0);
            assert_eq!(los.len(), 1);

            // Clear marks and sweep again
            los.clear_marks();
            let reclaimed = los.sweep();
            assert!(reclaimed > 0);
            assert!(los.is_empty());
        }
    }
}
