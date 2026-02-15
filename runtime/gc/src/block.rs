//! Block management for Immix GC
//!
//! Immix organizes memory into 32KB blocks, each divided into 128-byte lines.
//! This module provides block allocation, line marking, and state management.

use crate::header::{align_up, ObjectHeader};
use std::alloc::{alloc_zeroed, Layout};
use std::ptr::NonNull;

/// Size of an Immix block (32KB)
pub const BLOCK_SIZE: usize = 32 * 1024;

/// Size of a line within a block (128 bytes)
pub const LINE_SIZE: usize = 128;

/// Number of lines per block
pub const LINES_PER_BLOCK: usize = BLOCK_SIZE / LINE_SIZE; // 256

/// Size of object start bitmap (1 bit per line = 32 bytes)
pub const BITMAP_SIZE: usize = LINES_PER_BLOCK / 8; // 32

/// Minimum allocation size (to avoid tiny allocations)
pub const MIN_ALLOC_SIZE: usize = 8;

/// Block state during GC
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum BlockState {
    /// Block is empty and ready for allocation
    Empty = 0,
    /// Block has some free lines available
    Recyclable = 1,
    /// Block is full (no free lines)
    Full = 2,
    /// Block is fragmented and should be evacuated
    Evacuate = 3,
}

/// State of a single line within a block
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum LineState {
    /// Line is free for allocation
    Free = 0,
    /// Line contains live object(s)
    Live = 1,
    /// Line is conserved (don't allocate here)
    Conserved = 2,
}

/// Metadata for a single line
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineMetadata {
    /// State of the line
    pub state: LineState,
    /// Mark bit for collection
    pub mark: bool,
    /// Number of live objects starting in this line (saturates at 255)
    pub object_count: u8,
}

impl Default for LineMetadata {
    fn default() -> Self {
        LineMetadata {
            state: LineState::Free,
            mark: false,
            object_count: 0,
        }
    }
}

impl LineMetadata {
    /// Create new free line metadata
    pub const fn new() -> Self {
        LineMetadata {
            state: LineState::Free,
            mark: false,
            object_count: 0,
        }
    }

    /// Check if line is available for allocation
    pub fn is_available(&self) -> bool {
        matches!(self.state, LineState::Free)
    }

    /// Reset line metadata for new cycle
    pub fn reset(&mut self) {
        self.state = LineState::Free;
        self.mark = false;
        self.object_count = 0;
    }
}

/// An Immix memory block (32KB)
///
/// Blocks are page-aligned (4KB) and contain:
/// - Line metadata (256 bytes)
/// - Object start bitmap (32 bytes)
/// - Bump pointer for allocation
/// - State tracking
/// - The actual memory (allocated separately)
#[repr(C, align(4096))]
pub struct Block {
    /// Line metadata (1 entry per line)
    pub line_metadata: [LineMetadata; LINES_PER_BLOCK],
    /// Object start bitmap (1 bit per line)
    pub object_start_bitmap: [u8; BITMAP_SIZE],
    /// Current bump pointer (offset from start of block memory)
    pub cursor: usize,
    /// Number of free lines
    pub free_lines: u16,
    /// Block state
    pub state: BlockState,
    /// Whether this block is in the nursery
    pub is_nursery: bool,
    /// The actual memory (32KB, allocated separately)
    pub memory: *mut u8,
    /// Link to next block in list
    pub next: Option<NonNull<Block>>,
}

// Block is Send but not Sync (owned by single thread during allocation)
unsafe impl Send for Block {}

impl Block {
    /// Allocate a new block
    ///
    /// # Safety
    /// This allocates raw memory using the system allocator
    pub fn new() -> Option<NonNull<Self>> {
        // Allocate the block structure
        let block_layout = Layout::new::<Self>();
        let block_ptr = unsafe { alloc_zeroed(block_layout) };
        if block_ptr.is_null() {
            return None;
        }

        // Allocate the 32KB memory region (page-aligned)
        let memory_layout = Layout::from_size_align(BLOCK_SIZE, 4096).ok()?;
        let memory_ptr = unsafe { alloc_zeroed(memory_layout) };
        if memory_ptr.is_null() {
            return None;
        }

        unsafe {
            let block = block_ptr as *mut Block;
            (*block).memory = memory_ptr;
            (*block).cursor = 0;
            (*block).free_lines = LINES_PER_BLOCK as u16;
            (*block).state = BlockState::Empty;
            (*block).is_nursery = false;
            (*block).next = None;

            // Line metadata and bitmap are already zeroed

            Some(NonNull::new_unchecked(block))
        }
    }

    /// Get the block containing the given pointer
    ///
    /// # Safety
    /// The pointer must be within a valid block
    pub unsafe fn containing(ptr: *const u8) -> Option<NonNull<Block>> {
        // Align down to 4KB boundary to find block header
        let addr = ptr as usize;
        let block_addr = addr & !(4096 - 1);
        Some(NonNull::new_unchecked(block_addr as *mut Block))
    }

    /// Get the line index for a pointer within this block
    pub fn line_index(&self, ptr: *const u8) -> usize {
        let offset = ptr as usize - self.memory as usize;
        offset / LINE_SIZE
    }

    /// Get the start address of a line
    pub fn line_start(&self, line_index: usize) -> *mut u8 {
        debug_assert!(line_index < LINES_PER_BLOCK);
        unsafe { self.memory.add(line_index * LINE_SIZE) }
    }

    /// Check if a line has an object start
    pub fn has_object_start(&self, line_index: usize) -> bool {
        let byte_index = line_index / 8;
        let bit_index = line_index % 8;
        (self.object_start_bitmap[byte_index] >> bit_index) & 1 != 0
    }

    /// Set the object start bit for a line
    pub fn set_object_start(&mut self, line_index: usize) {
        let byte_index = line_index / 8;
        let bit_index = line_index % 8;
        self.object_start_bitmap[byte_index] |= 1 << bit_index;
    }

    /// Clear the object start bit for a line
    pub fn clear_object_start(&mut self, line_index: usize) {
        let byte_index = line_index / 8;
        let bit_index = line_index % 8;
        self.object_start_bitmap[byte_index] &= !(1 << bit_index);
    }

    /// Try to allocate from this block using bump pointer
    ///
    /// Returns the pointer to the allocated memory, or None if there's not enough space.
    /// This performs bump pointer allocation at the current cursor position.
    pub fn allocate(&mut self, size: usize, align: usize) -> Option<*mut u8> {
        let align = align.max(MIN_ALLOC_SIZE);
        let aligned_size = align_up(size, align);
        let start_offset = align_up(self.cursor, align);

        if aligned_size == 0 {
            let ptr = unsafe { self.memory.add(start_offset) };
            return Some(ptr);
        }

        // Check if we have enough space
        let new_cursor = start_offset + aligned_size;
        if new_cursor > BLOCK_SIZE {
            return None;
        }

        // Calculate start and end lines
        let start_line = start_offset / LINE_SIZE;
        let end_line = (new_cursor - 1) / LINE_SIZE;

        // For bump pointer allocation, we only need to check if we would cross
        // into lines that are already allocated (not free and not by us).
        // Since we're bumping sequentially, any already-marked lines in our range
        // must have been marked by us in a previous allocation.
        // We only need to check if lines beyond our current line are available.
        let current_line = self.cursor / LINE_SIZE;
        for i in start_line..=end_line {
            if i == current_line {
                continue;
            }
            if self.line_metadata[i].state != LineState::Free {
                // Would cross into already-allocated lines
                return None;
            }
        }

        // Perform allocation
        let ptr = unsafe { self.memory.add(start_offset) };
        self.cursor = new_cursor;

        // Mark lines as live (only newly used lines)
        let mut newly_used_lines = 0;
        for i in start_line..=end_line {
            if self.line_metadata[i].state == LineState::Free {
                self.line_metadata[i].state = LineState::Live;
                newly_used_lines += 1;
            }
        }

        // Mark object start
        self.set_object_start(start_line);
        self.line_metadata[start_line].object_count += 1;

        // Update free line count
        self.free_lines -= newly_used_lines as u16;

        // Update block state
        self.update_state();

        Some(ptr)
    }

    /// Try to allocate from recyclable lines (hole punching)
    ///
    /// This searches for contiguous free lines within the block
    pub fn allocate_from_hole(&mut self, size: usize, align: usize) -> Option<*mut u8> {
        let align = align.max(MIN_ALLOC_SIZE);
        let aligned_size = align_up(size, align);
        let lines_needed = aligned_size.div_ceil(LINE_SIZE);

        // Search for contiguous free lines
        let mut start_line = 0;
        let mut consecutive_free = 0;

        for i in 0..LINES_PER_BLOCK {
            if self.line_metadata[i].is_available() {
                if consecutive_free == 0 {
                    start_line = i;
                }
                consecutive_free += 1;

                if consecutive_free >= lines_needed {
                    // Found enough contiguous lines
                    let offset = start_line * LINE_SIZE;
                    if !offset.is_multiple_of(align) {
                        continue;
                    }
                    let ptr = unsafe { self.memory.add(offset) };

                    // Mark lines as live
                    for j in start_line..(start_line + lines_needed) {
                        self.line_metadata[j].state = LineState::Live;
                    }

                    // Mark object start
                    self.set_object_start(start_line);
                    self.line_metadata[start_line].object_count += 1;

                    // Update free line count
                    self.free_lines -= lines_needed as u16;
                    self.update_state();

                    return Some(ptr);
                }
            } else {
                consecutive_free = 0;
            }
        }

        None
    }

    /// Update block state based on free line count
    pub fn update_state(&mut self) {
        self.state = if self.free_lines == 0 {
            BlockState::Full
        } else if self.free_lines == LINES_PER_BLOCK as u16 {
            BlockState::Empty
        } else {
            BlockState::Recyclable
        };
    }

    /// Mark a line as used during collection
    pub fn mark_line(&mut self, line_index: usize) {
        debug_assert!(line_index < LINES_PER_BLOCK);
        self.line_metadata[line_index].mark = true;
    }

    /// Check if a line is marked
    pub fn is_line_marked(&self, line_index: usize) -> bool {
        self.line_metadata[line_index].mark
    }

    /// Reset all mark bits (for new collection cycle)
    pub fn clear_marks(&mut self) {
        for meta in &mut self.line_metadata {
            meta.mark = false;
        }
    }

    /// Sweep the block after marking
    ///
    /// Returns the number of bytes reclaimed
    pub fn sweep(&mut self) -> usize {
        let mut reclaimed = 0;
        let mut new_free_lines = 0;

        for i in 0..LINES_PER_BLOCK {
            match self.line_metadata[i].state {
                LineState::Live if !self.line_metadata[i].mark => {
                    // Unmarked live line - reclaim it
                    self.line_metadata[i].reset();
                    self.clear_object_start(i);
                    reclaimed += LINE_SIZE;
                    new_free_lines += 1;
                }
                LineState::Live => {
                    // Keep marked live lines, but clear mark for next cycle
                    self.line_metadata[i].mark = false;
                }
                _ => {
                    if self.line_metadata[i].is_available() {
                        new_free_lines += 1;
                    }
                }
            }
        }

        self.free_lines = new_free_lines;

        // Reset cursor if block is now empty
        if self.free_lines == LINES_PER_BLOCK as u16 {
            self.cursor = 0;
            self.state = BlockState::Empty;
        } else {
            self.update_state();
        }

        reclaimed
    }

    /// Calculate fragmentation ratio (0.0 to 1.0)
    pub fn fragmentation(&self) -> f32 {
        if self.free_lines == 0 {
            return 0.0;
        }

        let free = self.free_lines as f32;
        let total = LINES_PER_BLOCK as f32;
        1.0 - (free / total)
    }

    /// Check if block should be evacuated (high fragmentation)
    pub fn should_evacuate(&self) -> bool {
        self.state == BlockState::Evacuate
            || (self.state == BlockState::Recyclable && self.fragmentation() > 0.7)
    }

    /// Get the object header at the start of a line (if any)
    /// # Safety
    /// The line_index must be valid and the block must be properly initialized.
    pub unsafe fn get_object_at_line(&self, line_index: usize) -> Option<*mut ObjectHeader> {
        if !self.has_object_start(line_index) {
            return None;
        }

        let ptr = self.line_start(line_index);
        Some(ObjectHeader::from_ptr(ptr))
    }

    /// Iterate over all objects in the block
    pub fn iter_objects(&self) -> BlockObjectIterator<'_> {
        BlockObjectIterator {
            block: self,
            current_line: 0,
        }
    }

    /// Reset the block for reuse
    pub fn reset(&mut self) {
        self.cursor = 0;
        self.free_lines = LINES_PER_BLOCK as u16;
        self.state = BlockState::Empty;

        for meta in &mut self.line_metadata {
            meta.reset();
        }

        for byte in &mut self.object_start_bitmap {
            *byte = 0;
        }
    }
}

impl Drop for Block {
    fn drop(&mut self) {
        // Free the memory region
        if !self.memory.is_null() {
            let layout = Layout::from_size_align(BLOCK_SIZE, 4096).unwrap();
            unsafe {
                std::alloc::dealloc(self.memory, layout);
            }
        }
    }
}

/// Iterator over objects in a block
pub struct BlockObjectIterator<'a> {
    block: &'a Block,
    current_line: usize,
}

impl<'a> Iterator for BlockObjectIterator<'a> {
    type Item = *mut ObjectHeader;

    fn next(&mut self) -> Option<Self::Item> {
        while self.current_line < LINES_PER_BLOCK {
            let line = self.current_line;
            self.current_line += 1;

            if self.block.has_object_start(line) {
                return unsafe { self.block.get_object_at_line(line) };
            }
        }
        None
    }
}

/// A list of blocks
pub struct BlockList {
    head: Option<NonNull<Block>>,
    count: usize,
}

impl BlockList {
    /// Create a new empty block list
    pub fn new() -> Self {
        BlockList {
            head: None,
            count: 0,
        }
    }

    /// Add a block to the front of the list
    pub fn push(&mut self, block: NonNull<Block>) {
        unsafe {
            block.as_ptr().as_mut().unwrap().next = self.head;
        }
        self.head = Some(block);
        self.count += 1;
    }

    /// Remove and return the first block
    pub fn pop(&mut self) -> Option<NonNull<Block>> {
        self.head.inspect(|block| {
            unsafe {
                self.head = block.as_ref().next;
            }
            self.count -= 1;
        })
    }

    /// Get the number of blocks
    pub fn len(&self) -> usize {
        self.count
    }

    /// Check if list is empty
    pub fn is_empty(&self) -> bool {
        self.count == 0
    }

    /// Iterate over blocks
    pub fn iter(&self) -> BlockListIterator {
        BlockListIterator { current: self.head }
    }

    /// Iterate over blocks mutably
    pub fn iter_mut(&mut self) -> BlockListIteratorMut {
        BlockListIteratorMut { current: self.head }
    }

    /// Find a recyclable block with enough space
    pub fn find_recyclable(&mut self, size: usize, align: usize) -> Option<NonNull<Block>> {
        let mut current = self.head;

        while let Some(block_ptr) = current {
            unsafe {
                let block = block_ptr.as_ptr().as_mut().unwrap();

                // Try bump allocation first
                if block.allocate(size, align).is_some() {
                    return Some(block_ptr);
                }

                // Try hole punching
                if block.state == BlockState::Recyclable
                    && block.allocate_from_hole(size, align).is_some()
                {
                    return Some(block_ptr);
                }

                current = block.next;
            }
        }

        None
    }

    /// Get total bytes allocated across all blocks
    pub fn total_allocated(&self) -> usize {
        self.iter()
            .map(|block| {
                let free = unsafe { block.as_ref().free_lines as usize };
                (LINES_PER_BLOCK - free) * LINE_SIZE
            })
            .sum()
    }
}

impl Default for BlockList {
    fn default() -> Self {
        Self::new()
    }
}

/// Iterator over blocks in a list
pub struct BlockListIterator {
    current: Option<NonNull<Block>>,
}

impl Iterator for BlockListIterator {
    type Item = NonNull<Block>;

    fn next(&mut self) -> Option<Self::Item> {
        self.current.inspect(|block| unsafe {
            self.current = block.as_ref().next;
        })
    }
}

/// Mutable iterator over blocks in a list
pub struct BlockListIteratorMut {
    current: Option<NonNull<Block>>,
}

impl Iterator for BlockListIteratorMut {
    type Item = NonNull<Block>;

    fn next(&mut self) -> Option<Self::Item> {
        self.current.inspect(|block| unsafe {
            self.current = block.as_ref().next;
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_block_constants() {
        assert_eq!(BLOCK_SIZE, 32 * 1024);
        assert_eq!(LINE_SIZE, 128);
        assert_eq!(LINES_PER_BLOCK, 256);
    }

    #[test]
    fn test_block_allocation() {
        let block = Block::new().expect("Failed to allocate block");
        let block_ref = unsafe { block.as_ptr().as_ref().unwrap() };

        assert_eq!(block_ref.cursor, 0);
        assert_eq!(block_ref.free_lines, LINES_PER_BLOCK as u16);
        assert_eq!(block_ref.state, BlockState::Empty);
    }

    #[test]
    fn test_allocate_from_block() {
        let block = Block::new().expect("Failed to allocate block");
        let block_ref = unsafe { block.as_ptr().as_mut().unwrap() };

        // Allocate 100 bytes
        let ptr1 = block_ref.allocate(100, 8).expect("Allocation failed");
        assert!(!ptr1.is_null());

        // Allocate another 100 bytes
        let ptr2 = block_ref.allocate(100, 8).expect("Allocation failed");
        assert!(!ptr2.is_null());
        assert_ne!(ptr1, ptr2);
    }

    #[test]
    fn test_line_marking() {
        let block = Block::new().expect("Failed to allocate block");
        let block_ref = unsafe { block.as_ptr().as_mut().unwrap() };

        block_ref.mark_line(5);
        assert!(block_ref.is_line_marked(5));
        assert!(!block_ref.is_line_marked(4));
    }

    #[test]
    fn test_object_start_bitmap() {
        let block = Block::new().expect("Failed to allocate block");
        let block_ref = unsafe { block.as_ptr().as_mut().unwrap() };

        assert!(!block_ref.has_object_start(10));
        block_ref.set_object_start(10);
        assert!(block_ref.has_object_start(10));
        block_ref.clear_object_start(10);
        assert!(!block_ref.has_object_start(10));
    }

    #[test]
    fn test_block_list() {
        let mut list = BlockList::new();
        assert!(list.is_empty());

        let block = Block::new().expect("Failed to allocate block");
        list.push(block);
        assert_eq!(list.len(), 1);

        let popped = list.pop();
        assert!(popped.is_some());
        assert!(list.is_empty());
    }

    #[test]
    fn test_block_sweep() {
        let block = Block::new().expect("Failed to allocate block");
        let block_ref = unsafe { block.as_ptr().as_mut().unwrap() };

        // Allocate some memory
        let _ptr = block_ref.allocate(100, 8).expect("Allocation failed");
        let initial_free = block_ref.free_lines;

        // Mark the line
        block_ref.mark_line(0);

        // Sweep (should not reclaim marked line)
        let reclaimed = block_ref.sweep();
        assert_eq!(reclaimed, 0);
        assert_eq!(block_ref.free_lines, initial_free);
    }
}
