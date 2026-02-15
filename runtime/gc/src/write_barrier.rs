//! Write Barriers for Generational Garbage Collection
//!
//! Write barriers are required for generational GC to track references
//! from mature objects to nursery objects. This enables the GC to find
//! all reachable nursery objects during minor collections without scanning
//! the entire mature space.

use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Size of a remembered set buffer (4KB)
pub const REMEMBERED_SET_BUFFER_SIZE: usize = 4 * 1024;

/// Maximum number of entries per buffer
pub const ENTRIES_PER_BUFFER: usize =
    REMEMBERED_SET_BUFFER_SIZE / std::mem::size_of::<RememberedEntry>();

/// A single entry in the remembered set
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RememberedEntry {
    /// Pointer to the object (mature space)
    pub object: *mut u8,
    /// Pointer to the field being written
    pub field: *mut *mut u8,
}

impl RememberedEntry {
    /// Create a new remembered entry
    pub const fn new(object: *mut u8, field: *mut *mut u8) -> Self {
        RememberedEntry { object, field }
    }

    /// Check if this entry is null/empty
    pub fn is_null(&self) -> bool {
        self.object.is_null()
    }
}

impl Default for RememberedEntry {
    fn default() -> Self {
        RememberedEntry {
            object: std::ptr::null_mut(),
            field: std::ptr::null_mut(),
        }
    }
}

/// A buffer of remembered set entries
pub struct RememberedSetBuffer {
    /// Entries in the buffer (fixed-size array)
    entries: Box<[RememberedEntry; ENTRIES_PER_BUFFER]>,
    /// Current position for next entry
    cursor: AtomicUsize,
    /// Next buffer in the list (for GC processing)
    next: Option<NonNull<RememberedSetBuffer>>,
}

impl RememberedSetBuffer {
    /// Create a new empty buffer
    pub fn new() -> Self {
        let entries = Box::new([RememberedEntry::default(); ENTRIES_PER_BUFFER]);
        RememberedSetBuffer {
            entries,
            cursor: AtomicUsize::new(0),
            next: None,
        }
    }

    /// Try to add an entry to this buffer
    pub fn try_add(&self, object: *mut u8, field: *mut *mut u8) -> bool {
        let index = self.cursor.fetch_add(1, Ordering::Relaxed);
        if index >= ENTRIES_PER_BUFFER {
            self.cursor.fetch_sub(1, Ordering::Relaxed);
            return false;
        }
        unsafe {
            let entry_ptr = self.entries.as_ptr().add(index) as *mut RememberedEntry;
            (*entry_ptr) = RememberedEntry::new(object, field);
        }
        true
    }

    /// Get the number of entries in the buffer
    pub fn len(&self) -> usize {
        self.cursor.load(Ordering::Relaxed).min(ENTRIES_PER_BUFFER)
    }

    /// Check if the buffer is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Check if the buffer is full
    pub fn is_full(&self) -> bool {
        self.cursor.load(Ordering::Relaxed) >= ENTRIES_PER_BUFFER
    }

    /// Iterate over entries in the buffer
    pub fn iter(&self) -> impl Iterator<Item = &RememberedEntry> {
        let len = self.len();
        self.entries[..len].iter()
    }

    /// Clear the buffer for reuse
    pub fn clear(&mut self) {
        self.cursor.store(0, Ordering::Relaxed);
        self.next = None;
    }

    /// Get the next buffer in the chain
    pub fn next(&self) -> Option<NonNull<RememberedSetBuffer>> {
        self.next
    }

    /// Set the next buffer in the chain
    pub fn set_next(&mut self, next: Option<NonNull<RememberedSetBuffer>>) {
        self.next = next;
    }
}

impl Default for RememberedSetBuffer {
    fn default() -> Self {
        Self::new()
    }
}

unsafe impl Send for RememberedSetBuffer {}
unsafe impl Sync for RememberedSetBuffer {}

/// Per-thread remembered set
pub struct ThreadRememberedSet {
    /// Current buffer being filled
    current: Option<Box<RememberedSetBuffer>>,
    /// Full buffers waiting to be processed
    full_buffers: Vec<RememberedSetBuffer>,
    /// Total number of entries added
    total_entries: usize,
}

impl ThreadRememberedSet {
    /// Create a new per-thread remembered set
    pub fn new() -> Self {
        ThreadRememberedSet {
            current: Some(Box::new(RememberedSetBuffer::new())),
            full_buffers: Vec::new(),
            total_entries: 0,
        }
    }

    /// Add an entry to the remembered set
    pub fn add(&mut self, object: *mut u8, field: *mut *mut u8) {
        if let Some(ref current) = self.current {
            if current.try_add(object, field) {
                self.total_entries += 1;
                return;
            }
        }
        if let Some(full) = self.current.take() {
            self.full_buffers.push(*full);
        }
        let new_buffer = Box::new(RememberedSetBuffer::new());
        let _ = new_buffer.try_add(object, field);
        self.current = Some(new_buffer);
        self.total_entries += 1;
    }

    /// Get all full buffers (for GC processing)
    pub fn take_full_buffers(&mut self) -> Vec<RememberedSetBuffer> {
        std::mem::take(&mut self.full_buffers)
    }

    /// Get the current buffer (for GC processing)
    pub fn take_current_buffer(&mut self) -> Option<Box<RememberedSetBuffer>> {
        self.current.take()
    }

    /// Set a new current buffer
    pub fn set_current_buffer(&mut self, buffer: Box<RememberedSetBuffer>) {
        self.current = Some(buffer);
    }

    /// Get total number of entries
    pub fn total_entries(&self) -> usize {
        self.total_entries
    }

    /// Clear all buffers
    pub fn clear(&mut self) {
        self.current = Some(Box::new(RememberedSetBuffer::new()));
        self.full_buffers.clear();
        self.total_entries = 0;
    }
}

impl Default for ThreadRememberedSet {
    fn default() -> Self {
        Self::new()
    }
}

/// Global remembered set
pub struct GlobalRememberedSet {
    /// All buffers collected from threads
    buffers: Vec<RememberedSetBuffer>,
    /// Total number of entries
    total_entries: usize,
}

impl GlobalRememberedSet {
    /// Create a new global remembered set
    pub fn new() -> Self {
        GlobalRememberedSet {
            buffers: Vec::new(),
            total_entries: 0,
        }
    }

    /// Add a buffer from a thread
    pub fn add_buffer(&mut self, buffer: RememberedSetBuffer) {
        self.total_entries += buffer.len();
        self.buffers.push(buffer);
    }

    /// Iterate over all entries
    pub fn iter(&self) -> impl Iterator<Item = &RememberedEntry> {
        self.buffers.iter().flat_map(|b| b.iter())
    }

    /// Get total number of entries
    pub fn total_entries(&self) -> usize {
        self.total_entries
    }

    /// Clear all buffers
    pub fn clear(&mut self) {
        self.buffers.clear();
        self.total_entries = 0;
    }

    /// Process all entries with a callback
    pub fn process_entries<F>(&self, mut callback: F)
    where
        F: FnMut(*mut u8, *mut *mut u8),
    {
        for buffer in &self.buffers {
            for entry in buffer.iter() {
                if !entry.is_null() {
                    callback(entry.object, entry.field);
                }
            }
        }
    }
}

impl Default for GlobalRememberedSet {
    fn default() -> Self {
        Self::new()
    }
}

/// Write barrier state for a thread
pub struct WriteBarrierState {
    /// Per-thread remembered set
    pub remembered_set: ThreadRememberedSet,
    /// Whether write barriers are enabled
    pub enabled: bool,
    /// Number of barriers executed
    pub barrier_count: u64,
}

impl WriteBarrierState {
    /// Create a new write barrier state
    pub fn new() -> Self {
        WriteBarrierState {
            remembered_set: ThreadRememberedSet::new(),
            enabled: true,
            barrier_count: 0,
        }
    }

    /// Enable write barriers
    pub fn enable(&mut self) {
        self.enabled = true;
    }

    /// Disable write barriers
    pub fn disable(&mut self) {
        self.enabled = false;
    }

    /// Check if write barriers are enabled
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }
}

impl Default for WriteBarrierState {
    fn default() -> Self {
        Self::new()
    }
}

/// Fast path write barrier check
#[inline(always)]
pub fn needs_write_barrier(
    _object: *const u8,
    new_value: *const u8,
    object_is_nursery: bool,
) -> bool {
    if object_is_nursery {
        return false;
    }
    if new_value.is_null() {
        return false;
    }
    let addr = new_value as usize;
    addr != 0
}

/// Slow path write barrier
#[inline(never)]
pub fn write_barrier_slow(state: &mut WriteBarrierState, object: *mut u8, field: *mut *mut u8) {
    if state.enabled {
        state.remembered_set.add(object, field);
        state.barrier_count += 1;
    }
}

/// Full write barrier
///
/// # Safety
/// - `object` must point to a valid object in the GC heap
/// - `field` must be a valid pointer to a field within `object`
/// - `new_value` must be a valid pointer or null
#[inline]
pub unsafe fn write_barrier(
    state: &mut WriteBarrierState,
    object: *mut u8,
    field: *mut *mut u8,
    new_value: *mut u8,
    object_is_nursery: bool,
) {
    if needs_write_barrier(object, new_value, object_is_nursery) {
        write_barrier_slow(state, object, field);
    }
    *field = new_value;
}

/// Array write barrier
///
/// # Safety
/// The `array` pointer must be valid and `index` must be within bounds
#[inline]
pub unsafe fn array_write_barrier(
    state: &mut WriteBarrierState,
    array: *mut u8,
    index: usize,
    element: *mut u8,
    array_is_nursery: bool,
) {
    let element_ptr = array.add(index * std::mem::size_of::<*mut u8>()) as *mut *mut u8;
    if needs_write_barrier(array, element, array_is_nursery) {
        write_barrier_slow(state, array, element_ptr);
    }
    *element_ptr = element;
}

/// Bulk write barrier for array copies
///
/// # Safety
/// The `array` pointer must be valid and the range `start_index..start_index+count` must be within bounds
pub unsafe fn bulk_write_barrier(
    state: &mut WriteBarrierState,
    array: *mut u8,
    start_index: usize,
    count: usize,
    elements: &[*mut u8],
    array_is_nursery: bool,
) {
    if array_is_nursery {
        let base = array.add(start_index * std::mem::size_of::<*mut u8>()) as *mut *mut u8;
        for (i, &elem) in elements.iter().enumerate().take(count) {
            *base.add(i) = elem;
        }
        return;
    }
    for (i, &elem) in elements.iter().enumerate().take(count) {
        let element_ptr =
            array.add((start_index + i) * std::mem::size_of::<*mut u8>()) as *mut *mut u8;
        if !elem.is_null() {
            write_barrier_slow(state, array, element_ptr);
        }
        *element_ptr = elem;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_remembered_entry() {
        let entry = RememberedEntry::new(0x1000 as *mut u8, 0x2000 as *mut *mut u8);
        assert!(!entry.is_null());
        assert_eq!(entry.object, 0x1000 as *mut u8);
        assert_eq!(entry.field, 0x2000 as *mut *mut u8);
    }

    #[test]
    fn test_remembered_set_buffer() {
        let buffer = RememberedSetBuffer::new();
        assert!(buffer.is_empty());
        assert!(!buffer.is_full());
        let mut count = 0;
        while buffer.try_add(0x1000 as *mut u8, 0x2000 as *mut *mut u8) {
            count += 1;
        }
        assert!(count > 0);
        assert_eq!(buffer.len(), count);
        assert!(buffer.is_full());
    }

    #[test]
    fn test_thread_remembered_set() {
        let mut set = ThreadRememberedSet::new();
        assert_eq!(set.total_entries(), 0);
        set.add(0x1000 as *mut u8, 0x2000 as *mut *mut u8);
        assert_eq!(set.total_entries(), 1);
        for i in 0..(ENTRIES_PER_BUFFER + 10) {
            set.add((0x1000 + i) as *mut u8, (0x2000 + i) as *mut *mut u8);
        }
        assert!(set.total_entries() > ENTRIES_PER_BUFFER);
        assert!(!set.full_buffers.is_empty());
    }

    #[test]
    fn test_global_remembered_set() {
        let mut global = GlobalRememberedSet::new();
        let buffer = RememberedSetBuffer::new();
        buffer.try_add(0x1000 as *mut u8, 0x2000 as *mut *mut u8);
        buffer.try_add(0x3000 as *mut u8, 0x4000 as *mut *mut u8);
        global.add_buffer(buffer);
        assert_eq!(global.total_entries(), 2);
        let mut count = 0;
        global.process_entries(|_, _| count += 1);
        assert_eq!(count, 2);
    }

    #[test]
    fn test_write_barrier_state() {
        let mut state = WriteBarrierState::new();
        assert!(state.is_enabled());
        state.disable();
        assert!(!state.is_enabled());
        state.enable();
        assert!(state.is_enabled());
    }

    #[test]
    fn test_needs_write_barrier() {
        assert!(!needs_write_barrier(
            0x1000 as *mut u8,
            0x2000 as *mut u8,
            true
        ));
        assert!(!needs_write_barrier(
            0x1000 as *mut u8,
            std::ptr::null_mut(),
            false
        ));
    }
}
