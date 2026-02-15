//! Object header for Immix GC
//!
//! The object header contains metadata for garbage collection including
//! type information, mark bits, and object size.

use std::sync::atomic::{AtomicUsize, Ordering};

/// Type identifier for Jet objects
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub u32);

impl TypeId {
    /// Create a new type ID
    pub const fn new(id: u32) -> Self {
        TypeId(id)
    }

    /// Get the raw ID value
    pub fn raw(&self) -> u32 {
        self.0
    }
}

/// Object header layout:
/// Bits 0-31: Type ID (32 bits)
/// Bits 32-62: Size (31 bits, max ~2GB)
/// Bit 63: Mark bit
const TYPE_ID_MASK: usize = 0xFFFFFFFF;
const SIZE_SHIFT: usize = 32;
const SIZE_MASK: usize = 0x7FFFFFFF;
const MARK_BIT: usize = 1 << 63;

/// Object header for all GC-managed objects
///
/// The header is designed to be compact while providing all necessary
/// information for garbage collection and type safety.
#[repr(C)]
#[derive(Debug)]
pub struct ObjectHeader {
    /// Packed word containing type ID, size, and mark bit
    pub word: AtomicUsize,
    /// Size of object payload (excluding header)
    pub size: u32,
    /// Flags for GC and object state
    pub flags: u16,
    /// Alignment requirement
    pub align: u16,
}

/// GC flags for objects
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum GcFlags {
    /// Object is in the nursery
    Nursery = 0x01,
    /// Object has been promoted to mature space
    Mature = 0x02,
    /// Object has finalizer
    HasFinalizer = 0x04,
    /// Object is pinned (cannot move)
    Pinned = 0x08,
    /// Object is large (in LOS)
    Large = 0x10,
}

impl ObjectHeader {
    /// Size of the object header in bytes
    pub const SIZE: usize = std::mem::size_of::<Self>();

    /// Alignment of the object header
    pub const ALIGN: usize = std::mem::align_of::<Self>();

    /// Create a new object header
    ///
    /// # Arguments
    /// * `ty` - The type ID of the object
    /// * `size` - Size of the object payload (excluding header)
    pub fn new(ty: TypeId, size: usize) -> Self {
        assert!(size <= SIZE_MASK, "Object size too large");

        let word = (size << SIZE_SHIFT) | (ty.0 as usize);

        ObjectHeader {
            word: AtomicUsize::new(word),
            size: size as u32,
            flags: 0,
            align: 8, // Default 8-byte alignment
        }
    }

    /// Create a new object header with specific alignment
    pub fn with_align(ty: TypeId, size: usize, align: u16) -> Self {
        assert!(size <= SIZE_MASK, "Object size too large");
        assert!(align.is_power_of_two(), "Alignment must be power of two");

        let word = (size << SIZE_SHIFT) | (ty.0 as usize);

        ObjectHeader {
            word: AtomicUsize::new(word),
            size: size as u32,
            flags: 0,
            align,
        }
    }

    /// Get the type ID
    pub fn type_id(&self) -> TypeId {
        let word = self.word.load(Ordering::Relaxed);
        TypeId((word & TYPE_ID_MASK) as u32)
    }

    /// Set the type ID
    pub fn set_type_id(&self, ty: TypeId) {
        let mut word = self.word.load(Ordering::Relaxed);
        word = (word & !TYPE_ID_MASK) | (ty.0 as usize);
        self.word.store(word, Ordering::Relaxed);
    }

    /// Get the object size (payload only)
    pub fn object_size(&self) -> usize {
        self.size as usize
    }

    /// Get the total size including header
    pub fn total_size(&self) -> usize {
        Self::SIZE + self.size as usize
    }

    /// Check if the object is marked
    pub fn is_marked(&self) -> bool {
        let word = self.word.load(Ordering::Relaxed);
        (word & MARK_BIT) != 0
    }

    /// Set the mark bit
    pub fn set_marked(&self, marked: bool) {
        let mut word = self.word.load(Ordering::Relaxed);
        if marked {
            word |= MARK_BIT;
        } else {
            word &= !MARK_BIT;
        }
        self.word.store(word, Ordering::Relaxed);
    }

    /// Atomically check and set marked (for concurrent marking)
    pub fn try_mark(&self) -> bool {
        let word = self.word.load(Ordering::Relaxed);
        if (word & MARK_BIT) != 0 {
            return false; // Already marked
        }
        self.word
            .compare_exchange(word, word | MARK_BIT, Ordering::Relaxed, Ordering::Relaxed)
            .is_ok()
    }

    /// Check if a specific flag is set
    pub fn has_flag(&self, flag: GcFlags) -> bool {
        (self.flags & (flag as u16)) != 0
    }

    /// Set a flag
    pub fn set_flag(&mut self, flag: GcFlags, value: bool) {
        if value {
            self.flags |= flag as u16;
        } else {
            self.flags &= !(flag as u16);
        }
    }

    /// Check if object is in nursery
    pub fn is_nursery(&self) -> bool {
        self.has_flag(GcFlags::Nursery)
    }

    /// Check if object is in mature space
    pub fn is_mature(&self) -> bool {
        self.has_flag(GcFlags::Mature)
    }

    /// Check if object is pinned
    pub fn is_pinned(&self) -> bool {
        self.has_flag(GcFlags::Pinned)
    }

    /// Check if object is large
    pub fn is_large(&self) -> bool {
        self.has_flag(GcFlags::Large)
    }

    /// Clear the mark bit (for collection reset)
    pub fn clear_mark(&self) {
        self.set_marked(false);
    }

    /// Get a pointer to the object payload (after header)
    pub fn payload_ptr(&self) -> *mut u8 {
        unsafe { (self as *const Self as *mut u8).add(Self::SIZE) }
    }

    /// Get the object header from a payload pointer
    ///
    /// # Safety
    /// The pointer must point to a valid object payload
    pub unsafe fn from_payload(ptr: *mut u8) -> *mut Self {
        ptr.sub(Self::SIZE) as *mut Self
    }

    /// Get the object header from an object pointer
    ///
    /// # Safety
    /// The pointer must point to a valid object header
    pub unsafe fn from_ptr(ptr: *mut u8) -> *mut Self {
        ptr as *mut Self
    }
}

/// Initialize an object header at the given memory location
///
/// # Safety
/// The pointer must be valid for writing an ObjectHeader
pub unsafe fn init_header(ptr: *mut u8, ty: TypeId, size: usize) -> *mut ObjectHeader {
    let header = ptr as *mut ObjectHeader;
    header.write(ObjectHeader::new(ty, size));
    header
}

/// Align a size up to the given alignment
pub const fn align_up(size: usize, align: usize) -> usize {
    (size + align - 1) & !(align - 1)
}

/// Align a pointer up to the given alignment
pub fn align_ptr_up(ptr: *mut u8, align: usize) -> *mut u8 {
    let addr = ptr as usize;
    let aligned = (addr + align - 1) & !(align - 1);
    aligned as *mut u8
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_header_creation() {
        let header = ObjectHeader::new(TypeId::new(42), 100);
        assert_eq!(header.type_id().raw(), 42);
        assert_eq!(header.object_size(), 100);
        assert!(!header.is_marked());
    }

    #[test]
    fn test_mark_bit() {
        let header = ObjectHeader::new(TypeId::new(1), 50);
        assert!(!header.is_marked());

        header.set_marked(true);
        assert!(header.is_marked());

        header.set_marked(false);
        assert!(!header.is_marked());
    }

    #[test]
    fn test_try_mark() {
        let header = ObjectHeader::new(TypeId::new(1), 50);
        assert!(header.try_mark());
        assert!(!header.try_mark()); // Already marked
        assert!(header.is_marked());
    }

    #[test]
    fn test_flags() {
        let mut header = ObjectHeader::new(TypeId::new(1), 50);
        assert!(!header.is_nursery());

        header.set_flag(GcFlags::Nursery, true);
        assert!(header.is_nursery());

        header.set_flag(GcFlags::Mature, true);
        assert!(header.is_nursery());
        assert!(header.is_mature());
    }

    #[test]
    fn test_align_up() {
        assert_eq!(align_up(0, 8), 0);
        assert_eq!(align_up(1, 8), 8);
        assert_eq!(align_up(7, 8), 8);
        assert_eq!(align_up(8, 8), 8);
        assert_eq!(align_up(9, 8), 16);
    }

    #[test]
    fn test_total_size() {
        let header = ObjectHeader::new(TypeId::new(1), 100);
        assert_eq!(header.total_size(), ObjectHeader::SIZE + 100);
    }
}
