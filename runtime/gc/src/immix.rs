//! Immix Garbage Collector implementation
//!
//! Immix is a mark-region garbage collector that organizes memory into 32KB blocks,
//! each divided into 128-byte lines. It uses bump pointer allocation within lines
//! and supports evacuation of fragmented blocks.
//!
//! # Key Features
//!
//! - **Block-based organization**: 32KB blocks with 128-byte lines
//! - **Bump pointer allocation**: Fast allocation within contiguous free lines
//! - **Hole punching**: Allocation into fragmented blocks
//! - **Nursery collection**: Fast minor collections for short-lived objects
//! - **Full collection**: Mark-sweep with evacuation for mature objects
//!
//! # Architecture
//!
//! ```text
//! ImmixHeap
//!   ├── Nursery (new objects)
//!   │   └── BlockList
//!   ├── Mature (surviving objects)
//!   │   └── BlockList
//!   └── Large Object Space (objects > 16KB)
//! ```

use crate::allocator::{
    Allocator, LargeObjectSpace, DEFAULT_NURSERY_THRESHOLD, LARGE_OBJECT_THRESHOLD,
};
use crate::block::{Block, BlockList, BLOCK_SIZE};
use crate::collect::{CollectionStats, Collector};
use crate::header::{align_up, init_header, ObjectHeader, TypeId};
use std::ptr::NonNull;

/// The main Immix heap
///
/// This is the primary interface for garbage collection in the Jet runtime.
/// It manages nursery and mature spaces, handles allocation, and triggers
/// collections when necessary.
pub struct ImmixHeap {
    /// Nursery for new objects
    nursery: Allocator,
    /// Mature space for long-lived objects
    mature: Allocator,
    /// Large object space
    los: LargeObjectSpace,
    /// Current block for bump allocation
    current_block: Option<NonNull<Block>>,
    /// Collection trigger threshold for nursery
    _nursery_size_threshold: usize,
    /// The garbage collector
    collector: Collector,
    /// Total bytes allocated since last collection
    total_allocated: usize,
    /// Collection count
    collection_count: usize,
}

/// Configuration for Immix heap
#[derive(Debug, Clone)]
pub struct ImmixConfig {
    /// Nursery collection threshold
    pub nursery_threshold: usize,
    /// Mature collection threshold
    pub mature_threshold: usize,
    /// Large object threshold
    pub large_object_threshold: usize,
    /// Enable evacuation
    pub enable_evacuation: bool,
    /// Evacuation fragmentation threshold (0.0 - 1.0)
    pub evacuation_threshold: f32,
}

impl Default for ImmixConfig {
    fn default() -> Self {
        ImmixConfig {
            nursery_threshold: DEFAULT_NURSERY_THRESHOLD,
            mature_threshold: DEFAULT_NURSERY_THRESHOLD * 4,
            large_object_threshold: LARGE_OBJECT_THRESHOLD,
            enable_evacuation: true,
            evacuation_threshold: 0.7,
        }
    }
}

/// Allocation result
#[derive(Debug)]
pub struct Allocation {
    /// Pointer to allocated memory (payload, after header)
    pub ptr: *mut u8,
    /// Pointer to object header
    pub header: *mut ObjectHeader,
    /// Total size including header
    pub total_size: usize,
}

impl ImmixHeap {
    /// Create a new Immix heap with default configuration
    pub fn new() -> Self {
        Self::with_config(ImmixConfig::default())
    }

    /// Create a new Immix heap with custom configuration
    pub fn with_config(config: ImmixConfig) -> Self {
        ImmixHeap {
            nursery: Allocator::new(config.nursery_threshold),
            mature: Allocator::new(config.mature_threshold),
            los: LargeObjectSpace::new(),
            current_block: None,
            _nursery_size_threshold: config.nursery_threshold,
            collector: Collector::new(),
            total_allocated: 0,
            collection_count: 0,
        }
    }

    /// Allocate an object
    ///
    /// # Arguments
    /// * `size` - Size of object payload (excluding header)
    /// * `align` - Alignment requirement
    /// * `ty` - Type ID for the object
    ///
    /// # Returns
    /// Pointer to the allocated object payload (after header), or null on failure
    pub fn allocate_object(&mut self, size: usize, align: usize, ty: TypeId) -> *mut u8 {
        let total_size = ObjectHeader::SIZE + size;
        let align = align.max(ObjectHeader::ALIGN);

        // Check if this is a large object
        if total_size >= LARGE_OBJECT_THRESHOLD {
            return self.allocate_large_object(size, align, ty);
        }

        // Try to allocate from nursery
        let ptr = self.allocate_from_nursery(total_size, align);

        if ptr.is_null() {
            // Allocation failed - try triggering collection
            self.collect_nursery(&[]);

            // Try again
            let ptr = self.allocate_from_nursery(total_size, align);
            if ptr.is_null() {
                return std::ptr::null_mut();
            }

            // Initialize header
            unsafe {
                init_header(ptr, ty, size);
                let header = &mut *(ptr as *mut ObjectHeader);
                header.set_flag(crate::header::GcFlags::Nursery, true);
            }

            return unsafe { ptr.add(ObjectHeader::SIZE) };
        }

        // Initialize header
        unsafe {
            init_header(ptr, ty, size);
            let header = &mut *(ptr as *mut ObjectHeader);
            header.set_flag(crate::header::GcFlags::Nursery, true);
        }

        self.total_allocated += total_size;

        unsafe { ptr.add(ObjectHeader::SIZE) }
    }

    /// Allocate raw memory (without header initialization)
    ///
    /// # Arguments
    /// * `size` - Size to allocate
    /// * `align` - Alignment requirement
    ///
    /// # Returns
    /// Pointer to allocated memory, or null on failure
    pub fn allocate(&mut self, size: usize, align: usize) -> *mut u8 {
        let align = align.max(8);

        // Check if this is a large object
        if size >= LARGE_OBJECT_THRESHOLD {
            unsafe {
                return self
                    .los
                    .allocate(size, align)
                    .unwrap_or(std::ptr::null_mut());
            }
        }

        // Try nursery first
        if let Some(ptr) = self.nursery.allocate(size, align) {
            self.total_allocated += align_up(size, align);
            return ptr;
        }

        // Try to get a new block
        if let Some(block) = Block::new() {
            self.nursery.add_blocks({
                let mut list = BlockList::new();
                list.push(block);
                list
            });

            if let Some(ptr) = self.nursery.allocate(size, align) {
                self.total_allocated += align_up(size, align);
                return ptr;
            }
        }

        std::ptr::null_mut()
    }

    /// Allocate from nursery space
    fn allocate_from_nursery(&mut self, size: usize, align: usize) -> *mut u8 {
        self.nursery
            .allocate(size, align)
            .unwrap_or(std::ptr::null_mut())
    }

    /// Allocate a large object
    fn allocate_large_object(&mut self, size: usize, align: usize, ty: TypeId) -> *mut u8 {
        unsafe {
            if let Some(ptr) = self.los.allocate(size, align) {
                let header_ptr = ptr.sub(ObjectHeader::SIZE);
                init_header(header_ptr, ty, size);

                let header = &mut *(header_ptr as *mut ObjectHeader);
                header.set_flag(crate::header::GcFlags::Large, true);

                ptr
            } else {
                std::ptr::null_mut()
            }
        }
    }

    /// Check if collection is needed
    pub fn needs_collection(&self) -> bool {
        self.nursery.needs_collection()
    }

    /// Perform nursery collection
    ///
    /// # Arguments
    /// * `roots` - Root pointers to start tracing from
    pub fn collect_nursery(&mut self, roots: &[*mut ObjectHeader]) -> CollectionStats {
        let stats = self
            .collector
            .collect_nursery(&mut self.nursery, &mut self.mature, roots);

        self.collection_count += 1;
        self.total_allocated = self.nursery.allocated_bytes() + self.mature.allocated_bytes();

        stats
    }

    /// Perform full collection
    ///
    /// # Arguments
    /// * `roots` - Root pointers to start tracing from
    pub fn collect_full(&mut self, roots: &[*mut ObjectHeader]) -> CollectionStats {
        let stats = self
            .collector
            .collect_full(&mut self.nursery, &mut self.mature, roots);

        // Also sweep large object space
        let _los_reclaimed = self.los.sweep();

        self.collection_count += 1;
        self.total_allocated =
            self.nursery.allocated_bytes() + self.mature.allocated_bytes() + self.los.total_bytes();

        stats
    }

    /// Force a collection (chooses nursery or full based on pressure)
    pub fn collect(&mut self, roots: &[*mut ObjectHeader]) -> CollectionStats {
        if self.nursery.needs_collection() {
            self.collect_nursery(roots)
        } else if self.mature.needs_collection() {
            self.collect_full(roots)
        } else {
            // Minor collection by default
            self.collect_nursery(roots)
        }
    }

    /// Get total bytes allocated
    pub fn total_allocated(&self) -> usize {
        self.total_allocated
    }

    /// Get collection count
    pub fn collection_count(&self) -> usize {
        self.collection_count
    }

    /// Get nursery bytes
    pub fn nursery_bytes(&self) -> usize {
        self.nursery.allocated_bytes()
    }

    /// Get mature space bytes
    pub fn mature_bytes(&self) -> usize {
        self.mature.allocated_bytes()
    }

    /// Get large object space bytes
    pub fn los_bytes(&self) -> usize {
        self.los.total_bytes()
    }

    /// Find the block containing a pointer
    pub fn find_block(&self, ptr: *const u8) -> Option<NonNull<Block>> {
        // Check nursery blocks
        for block in self.nursery.current_block().into_iter() {
            unsafe {
                let block_ref = block.as_ptr().as_ref().unwrap();
                let start = block_ref.memory;
                let end = start.add(BLOCK_SIZE);
                if ptr >= start && ptr < end {
                    return Some(block);
                }
            }
        }

        // Check mature blocks
        for block in self.mature.current_block().into_iter() {
            unsafe {
                let block_ref = block.as_ptr().as_ref().unwrap();
                let start = block_ref.memory;
                let end = start.add(BLOCK_SIZE);
                if ptr >= start && ptr < end {
                    return Some(block);
                }
            }
        }

        None
    }

    /// Check if a pointer is managed by this heap
    pub fn contains(&self, ptr: *const u8) -> bool {
        self.find_block(ptr).is_some() || self.los.find_object(ptr).is_some()
    }

    /// Get object header from payload pointer
    ///
    /// # Safety
    /// The pointer must be a valid object payload
    pub unsafe fn header_from_payload(&self, ptr: *mut u8) -> *mut ObjectHeader {
        ObjectHeader::from_payload(ptr)
    }

    /// Reset the heap (clear all allocations)
    ///
    /// # Safety
    /// This invalidates all existing pointers
    pub unsafe fn reset(&mut self) {
        self.nursery.reset();
        self.mature.reset();
        self.los = LargeObjectSpace::new();
        self.current_block = None;
        self.collector.reset();
        self.total_allocated = 0;
    }

    /// Get heap statistics
    pub fn stats(&self) -> HeapStats {
        HeapStats {
            nursery_bytes: self.nursery_bytes(),
            mature_bytes: self.mature_bytes(),
            los_bytes: self.los_bytes(),
            total_bytes: self.total_allocated(),
            collection_count: self.collection_count(),
        }
    }
}

impl Default for ImmixHeap {
    fn default() -> Self {
        Self::new()
    }
}

/// Heap statistics
#[derive(Debug, Clone, Copy)]
pub struct HeapStats {
    /// Bytes in nursery
    pub nursery_bytes: usize,
    /// Bytes in mature space
    pub mature_bytes: usize,
    /// Bytes in large object space
    pub los_bytes: usize,
    /// Total bytes allocated
    pub total_bytes: usize,
    /// Number of collections performed
    pub collection_count: usize,
}

/// Thread-local allocation buffer
///
/// This provides fast allocation by caching a block in thread-local storage
pub struct ThreadLocalBuffer {
    /// Cached block for allocation
    block: Option<NonNull<Block>>,
    /// Remaining bytes in block
    remaining: usize,
    /// Pointer to next allocation
    cursor: *mut u8,
}

impl ThreadLocalBuffer {
    /// Create a new thread-local buffer
    pub fn new() -> Self {
        ThreadLocalBuffer {
            block: None,
            remaining: 0,
            cursor: std::ptr::null_mut(),
        }
    }

    /// Try to allocate from this buffer
    pub fn allocate(&mut self, size: usize, align: usize) -> Option<*mut u8> {
        let aligned_size = align_up(size, align.max(8));

        if aligned_size > self.remaining {
            return None;
        }

        let ptr = self.cursor;
        self.cursor = unsafe { self.cursor.add(aligned_size) };
        self.remaining -= aligned_size;

        Some(ptr)
    }

    /// Refill the buffer with a new block
    pub fn refill(&mut self, block: NonNull<Block>) {
        unsafe {
            let block_ref = block.as_ptr().as_ref().unwrap();
            self.block = Some(block);
            self.cursor = block_ref.memory.add(block_ref.cursor);
            self.remaining = BLOCK_SIZE - block_ref.cursor;
        }
    }

    /// Flush changes back to the block
    pub fn flush(&mut self) {
        if let Some(block) = self.block {
            unsafe {
                let block_ref = block.as_ptr().as_mut().unwrap();
                block_ref.cursor = BLOCK_SIZE - self.remaining;
            }
        }
    }
}

impl Default for ThreadLocalBuffer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_heap_creation() {
        let heap = ImmixHeap::new();
        assert_eq!(heap.total_allocated(), 0);
        assert_eq!(heap.collection_count(), 0);
    }

    #[test]
    fn test_allocation() {
        let mut heap = ImmixHeap::new();
        let ptr = heap.allocate(100, 8);
        assert!(!ptr.is_null());
    }

    #[test]
    fn test_allocate_object() {
        let mut heap = ImmixHeap::new();
        let ptr = heap.allocate_object(100, 8, TypeId::new(1));
        assert!(!ptr.is_null());

        // Verify header
        unsafe {
            let header = heap.header_from_payload(ptr);
            assert_eq!((*header).type_id().raw(), 1);
            assert_eq!((*header).object_size(), 100);
        }
    }

    #[test]
    fn test_multiple_allocations() {
        let mut heap = ImmixHeap::new();

        let mut ptrs = Vec::new();
        for i in 0..100 {
            let ptr = heap.allocate_object(64, 8, TypeId::new(i as u32));
            assert!(!ptr.is_null());
            ptrs.push(ptr);
        }

        // Verify all pointers are unique
        let unique: std::collections::HashSet<_> = ptrs.iter().map(|p| *p as usize).collect();
        assert_eq!(unique.len(), ptrs.len());
    }

    #[test]
    fn test_large_object_allocation() {
        let mut heap = ImmixHeap::new();

        // Allocate something larger than LARGE_OBJECT_THRESHOLD
        let large_size = LARGE_OBJECT_THRESHOLD + 1024;
        let ptr = heap.allocate_object(large_size, 8, TypeId::new(1));
        assert!(!ptr.is_null());

        unsafe {
            let header = heap.header_from_payload(ptr);
            assert!((*header).is_large());
        }
    }

    #[test]
    fn test_collection_stats() {
        let stats = HeapStats {
            nursery_bytes: 1024,
            mature_bytes: 2048,
            los_bytes: 4096,
            total_bytes: 7168,
            collection_count: 5,
        };

        assert_eq!(stats.nursery_bytes, 1024);
        assert_eq!(stats.total_bytes, 7168);
    }

    #[test]
    fn test_thread_local_buffer() {
        let mut buffer = ThreadLocalBuffer::new();
        assert_eq!(buffer.remaining, 0);

        // Can't allocate from empty buffer
        assert!(buffer.allocate(100, 8).is_none());
    }

    #[test]
    fn test_heap_stats() {
        let mut heap = ImmixHeap::new();

        // Allocate some objects
        for _ in 0..10 {
            let _ = heap.allocate_object(100, 8, TypeId::new(1));
        }

        let stats = heap.stats();
        assert!(stats.total_bytes > 0);
        assert_eq!(stats.collection_count, 0);
    }

    #[test]
    fn test_config_defaults() {
        let config = ImmixConfig::default();
        assert_eq!(config.nursery_threshold, DEFAULT_NURSERY_THRESHOLD);
        assert!(config.enable_evacuation);
        assert!(config.evacuation_threshold > 0.0);
    }
}
