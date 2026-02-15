//! Map - Hash map implementation for Jet
//!
//! This module provides a hash map with Robin Hood hashing for efficient
//! lookups, insertion, and deletion.

use std::alloc::{alloc, dealloc, Layout};
use std::marker::PhantomData;
use std::ptr;

/// A bucket entry in the hash map.
///
/// Uses Robin Hood hashing with backward shift deletion.
#[derive(Clone)]
struct Bucket<K, V> {
    key: K,
    value: V,
    hash: u64,
    /// Distance from ideal position (for Robin Hood hashing)
    distance: u8,
}

/// A hash map implementation.
///
/// Map stores key-value pairs and provides efficient lookup based on keys.
/// It uses Robin Hood hashing with linear probing for collision handling.
pub struct Map<K, V> {
    /// Pointer to the bucket array
    buckets: *mut Option<Bucket<K, V>>,
    /// Number of elements currently in the map
    len: usize,
    /// Total capacity of the bucket array
    capacity: usize,
    /// Load factor threshold for resizing
    max_load_factor: f64,
    /// Phantom data for the types
    _marker: PhantomData<(K, V)>,
}

/// Iterator over map keys
pub struct Keys<'a, K, V> {
    map: &'a Map<K, V>,
    index: usize,
}

/// Iterator over map values
pub struct Values<'a, K, V> {
    map: &'a Map<K, V>,
    index: usize,
}

/// Iterator over map entries (key-value pairs)
pub struct Entries<'a, K, V> {
    map: &'a Map<K, V>,
    index: usize,
}

impl<K, V> Map<K, V>
where
    K: Eq,
{
    /// Creates an empty map.
    ///
    /// The map will have an initial capacity of 16 buckets.
    pub fn new() -> Self {
        Map::with_capacity(16)
    }

    /// Creates an empty map with the specified initial capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        let capacity = capacity.max(4);
        let layout =
            Layout::array::<Option<Bucket<K, V>>>(capacity).expect("Layout computation failed");

        let buckets = unsafe { alloc(layout) as *mut Option<Bucket<K, V>> };
        if buckets.is_null() {
            panic!("Allocation failed");
        }

        // Initialize all buckets to None
        for i in 0..capacity {
            unsafe {
                ptr::write(buckets.add(i), None);
            }
        }

        Map {
            buckets,
            len: 0,
            capacity,
            max_load_factor: 0.75,
            _marker: PhantomData,
        }
    }

    /// Returns the number of elements in the map.
    pub fn len(&self) -> usize {
        self.len
    }

    /// Returns true if the map contains no elements.
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Returns the number of buckets in the map.
    pub fn capacity(&self) -> usize {
        self.capacity
    }

    /// Inserts a key-value pair into the map.
    ///
    /// If the map already had this key present, the value is updated and the
    /// old value is returned. Otherwise, None is returned.
    pub fn insert(&mut self, key: K, value: V) -> Option<V>
    where
        K: Hash,
    {
        if self.should_grow() {
            self.grow();
        }

        let hash = key.hash();
        self.insert_with_hash(key, value, hash)
    }

    /// Gets a reference to the value associated with the key.
    ///
    /// Returns None if the key is not present in the map.
    pub fn get(&self, key: &K) -> Option<&V>
    where
        K: Hash,
    {
        if self.is_empty() {
            return None;
        }

        let hash = key.hash();
        let index = self.bucket_index(hash);

        let mut idx = index;
        let mut distance: u8 = 0;

        loop {
            unsafe {
                match &*self.buckets.add(idx) {
                    None => return None,
                    Some(bucket) => {
                        if bucket.hash == hash && bucket.key == *key {
                            return Some(&bucket.value);
                        }
                        // Robin Hood: if we find a bucket with less distance, our key isn't here
                        if bucket.distance < distance {
                            return None;
                        }
                    }
                }
            }

            idx = (idx + 1) % self.capacity;
            distance = distance.saturating_add(1);

            // Prevent infinite loop
            if idx == index {
                return None;
            }
        }
    }

    /// Gets a mutable reference to the value associated with the key.
    ///
    /// Returns None if the key is not present in the map.
    pub fn get_mut(&mut self, key: &K) -> Option<&mut V>
    where
        K: Hash,
    {
        if self.is_empty() {
            return None;
        }

        let hash = key.hash();
        let index = self.bucket_index(hash);

        let mut idx = index;
        let mut distance: u8 = 0;

        loop {
            unsafe {
                match &mut *self.buckets.add(idx) {
                    None => return None,
                    Some(bucket) => {
                        if bucket.hash == hash && bucket.key == *key {
                            return Some(&mut bucket.value);
                        }
                        if bucket.distance < distance {
                            return None;
                        }
                    }
                }
            }

            idx = (idx + 1) % self.capacity;
            distance = distance.saturating_add(1);

            if idx == index {
                return None;
            }
        }
    }

    /// Returns true if the map contains the specified key.
    pub fn contains_key(&self, key: &K) -> bool
    where
        K: Hash,
    {
        self.get(key).is_some()
    }

    /// Removes a key from the map, returning the value if it existed.
    pub fn remove(&mut self, key: &K) -> Option<V>
    where
        K: Hash,
    {
        if self.is_empty() {
            return None;
        }

        let hash = key.hash();
        let index = self.bucket_index(hash);

        let mut idx = index;
        let mut distance: u8 = 0;

        // Find the bucket
        let found_idx = loop {
            unsafe {
                match &*self.buckets.add(idx) {
                    None => return None,
                    Some(bucket) => {
                        if bucket.hash == hash && bucket.key == *key {
                            break idx;
                        }
                        if bucket.distance < distance {
                            return None;
                        }
                    }
                }
            }

            idx = (idx + 1) % self.capacity;
            distance = distance.saturating_add(1);

            if idx == index {
                return None;
            }
        };

        // Remove the bucket and shift subsequent buckets back (backward shift deletion)
        unsafe {
            let value = (*self.buckets.add(found_idx)).take().map(|b| b.value)?;
            self.len -= 1;

            // Backward shift: shift buckets back to fill the gap
            let mut current = found_idx;
            let mut next = (current + 1) % self.capacity;

            while let Some(bucket) = &*self.buckets.add(next) {
                if bucket.distance == 0 {
                    break;
                }

                let bucket = (*self.buckets.add(next)).take().unwrap();
                *self.buckets.add(current) = Some(Bucket {
                    distance: bucket.distance - 1,
                    ..bucket
                });
                current = next;
                next = (next + 1) % self.capacity;
            }

            Some(value)
        }
    }

    /// Clears the map, removing all key-value pairs.
    pub fn clear(&mut self) {
        for i in 0..self.capacity {
            unsafe {
                drop(ptr::read(self.buckets.add(i)));
                ptr::write(self.buckets.add(i), None);
            }
        }
        self.len = 0;
    }

    /// Returns an iterator over the keys of the map.
    pub fn keys(&self) -> Keys<'_, K, V> {
        Keys {
            map: self,
            index: 0,
        }
    }

    /// Returns an iterator over the values of the map.
    pub fn values(&self) -> Values<'_, K, V> {
        Values {
            map: self,
            index: 0,
        }
    }

    /// Returns an iterator over the key-value pairs of the map.
    pub fn entries(&self) -> Entries<'_, K, V> {
        Entries {
            map: self,
            index: 0,
        }
    }

    /// Returns a copy of the value associated with the key, or a default.
    pub fn get_or(&self, key: &K, default: V) -> V
    where
        K: Hash,
        V: Clone,
    {
        self.get(key).cloned().unwrap_or(default)
    }

    /// Returns a copy of the value or computes it from a closure.
    pub fn get_or_else<F>(&self, key: &K, f: F) -> V
    where
        K: Hash,
        V: Clone,
        F: FnOnce() -> V,
    {
        self.get(key).cloned().unwrap_or_else(f)
    }

    /// Inserts a key-value pair only if the key is not already present.
    pub fn insert_if_absent(&mut self, key: K, value: V) -> Option<V>
    where
        K: Hash + Clone,
        V: Clone,
    {
        if let Some(existing) = self.get(&key) {
            Some(existing.clone())
        } else {
            self.insert(key, value);
            None
        }
    }

    /// Updates the value associated with the key using a function.
    pub fn update<F>(&mut self, key: K, f: F)
    where
        K: Hash + Clone,
        F: FnOnce(Option<&V>) -> V,
    {
        let new_value = f(self.get(&key));
        self.insert(key, new_value);
    }

    /// Retains only the elements specified by the predicate.
    pub fn retain<F>(&mut self, mut f: F)
    where
        K: Clone + Hash,
        F: FnMut(&K, &V) -> bool,
    {
        let keys_to_remove: Vec<K> = self
            .entries()
            .filter(|(k, v)| !f(k, v))
            .map(|(k, _)| k.clone())
            .collect();

        for key in keys_to_remove {
            self.remove(&key);
        }
    }

    /// Maps values while keeping the same keys.
    pub fn map_values<F, U>(&self, mut f: F) -> Map<K, U>
    where
        K: Clone + Hash,
        F: FnMut(&V) -> U,
    {
        let mut result = Map::with_capacity(self.capacity);
        for (key, value) in self.entries() {
            result.insert(key.clone(), f(value));
        }
        result
    }

    /// Filters entries by a predicate.
    pub fn filter<F>(&self, f: F) -> Map<K, V>
    where
        K: Clone + Hash,
        V: Clone,
        F: Fn(&K, &V) -> bool,
    {
        let mut result = Map::new();
        for (key, value) in self.entries() {
            if f(key, value) {
                result.insert(key.clone(), value.clone());
            }
        }
        result
    }

    /// Merges another map into this one.
    pub fn extend(&mut self, other: &Map<K, V>)
    where
        K: Clone + Hash,
        V: Clone,
    {
        for (key, value) in other.entries() {
            self.insert(key.clone(), value.clone());
        }
    }

    // Private methods

    fn should_grow(&self) -> bool {
        (self.len as f64) / (self.capacity as f64) > self.max_load_factor
    }

    fn bucket_index(&self, hash: u64) -> usize {
        (hash % self.capacity as u64) as usize
    }

    fn insert_with_hash(&mut self, key: K, value: V, hash: u64) -> Option<V>
    where
        K: Eq,
    {
        let index = self.bucket_index(hash);

        let mut idx = index;
        let mut distance: u8 = 0;
        let mut key_to_insert = key;
        let mut value_to_insert = value;
        let mut hash_to_insert = hash;

        loop {
            unsafe {
                match &mut *self.buckets.add(idx) {
                    None => {
                        // Empty slot - insert here
                        *self.buckets.add(idx) = Some(Bucket {
                            key: key_to_insert,
                            value: value_to_insert,
                            hash: hash_to_insert,
                            distance,
                        });
                        self.len += 1;
                        return None;
                    }
                    Some(bucket) => {
                        // Check if key already exists
                        if bucket.hash == hash_to_insert && bucket.key == key_to_insert {
                            // Update existing value
                            let old_value = ptr::replace(&mut bucket.value, value_to_insert);
                            return Some(old_value);
                        }

                        // Robin Hood: steal from the rich (closer to ideal position)
                        if bucket.distance < distance {
                            // Swap with current bucket
                            let old_bucket = ptr::replace(
                                self.buckets.add(idx),
                                Some(Bucket {
                                    key: key_to_insert,
                                    value: value_to_insert,
                                    hash: hash_to_insert,
                                    distance,
                                }),
                            );
                            let old_bucket = old_bucket.unwrap();
                            key_to_insert = old_bucket.key;
                            value_to_insert = old_bucket.value;
                            hash_to_insert = old_bucket.hash;
                            distance = bucket.distance + 1;
                        }
                    }
                }
            }

            idx = (idx + 1) % self.capacity;
            distance = distance.saturating_add(1);

            if idx == index {
                // This shouldn't happen if we grow properly
                panic!("Hash map is full");
            }
        }
    }

    fn grow(&mut self) {
        let old_capacity = self.capacity;
        let new_capacity = old_capacity * 2;

        let old_buckets = self.buckets;
        let old_layout =
            Layout::array::<Option<Bucket<K, V>>>(old_capacity).expect("Layout computation failed");

        let new_layout =
            Layout::array::<Option<Bucket<K, V>>>(new_capacity).expect("Layout computation failed");

        let new_buckets = unsafe { alloc(new_layout) as *mut Option<Bucket<K, V>> };
        if new_buckets.is_null() {
            panic!("Allocation failed");
        }

        // Initialize new buckets
        for i in 0..new_capacity {
            unsafe {
                ptr::write(new_buckets.add(i), None);
            }
        }

        // Update capacity and buckets temporarily for reinsertion
        let old_len = self.len;
        self.capacity = new_capacity;
        self.buckets = new_buckets;
        self.len = 0;

        // Reinsert all existing buckets
        for i in 0..old_capacity {
            unsafe {
                if let Some(bucket) = ptr::read(old_buckets.add(i)) {
                    self.insert_with_hash(bucket.key, bucket.value, bucket.hash);
                }
            }
        }

        self.len = old_len;

        // Free old bucket array
        unsafe {
            dealloc(old_buckets as *mut u8, old_layout);
        }
    }
}

impl<K, V> Default for Map<K, V>
where
    K: Eq,
{
    fn default() -> Self {
        Map::new()
    }
}

impl<K, V> Drop for Map<K, V> {
    fn drop(&mut self) {
        if self.capacity > 0 {
            // Clear all elements - drop each bucket
            for i in 0..self.capacity {
                unsafe {
                    drop(ptr::read(self.buckets.add(i)));
                }
            }
            self.len = 0;
            let layout = Layout::array::<Option<Bucket<K, V>>>(self.capacity)
                .expect("Layout computation failed");
            unsafe {
                dealloc(self.buckets as *mut u8, layout);
            }
        }
    }
}

impl<K, V> Clone for Map<K, V>
where
    K: Clone + Eq + Hash,
    V: Clone,
{
    fn clone(&self) -> Self {
        let mut new_map = Map::with_capacity(self.capacity);
        for (key, value) in self.entries() {
            new_map.insert(key.clone(), value.clone());
        }
        new_map
    }
}

impl<'a, K, V> Iterator for Keys<'a, K, V> {
    type Item = &'a K;

    fn next(&mut self) -> Option<Self::Item> {
        while self.index < self.map.capacity {
            unsafe {
                let bucket = &*self.map.buckets.add(self.index);
                self.index += 1;
                if let Some(b) = bucket {
                    return Some(&b.key);
                }
            }
        }
        None
    }
}

impl<'a, K, V> Iterator for Values<'a, K, V> {
    type Item = &'a V;

    fn next(&mut self) -> Option<Self::Item> {
        while self.index < self.map.capacity {
            unsafe {
                let bucket = &*self.map.buckets.add(self.index);
                self.index += 1;
                if let Some(b) = bucket {
                    return Some(&b.value);
                }
            }
        }
        None
    }
}

impl<'a, K, V> Iterator for Entries<'a, K, V> {
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        while self.index < self.map.capacity {
            unsafe {
                let bucket = &*self.map.buckets.add(self.index);
                self.index += 1;
                if let Some(b) = bucket {
                    return Some((&b.key, &b.value));
                }
            }
        }
        None
    }
}

/// Hash trait for map keys
pub trait Hash {
    fn hash(&self) -> u64;
}

// Implement Hash for common types
impl Hash for i32 {
    fn hash(&self) -> u64 {
        *self as u64
    }
}

impl Hash for i64 {
    fn hash(&self) -> u64 {
        *self as u64
    }
}

impl Hash for u32 {
    fn hash(&self) -> u64 {
        *self as u64
    }
}

impl Hash for u64 {
    fn hash(&self) -> u64 {
        *self
    }
}

impl Hash for usize {
    fn hash(&self) -> u64 {
        *self as u64
    }
}

impl Hash for &str {
    fn hash(&self) -> u64 {
        // FNV-1a hash
        let mut hash: u64 = 0xcbf29ce484222325;
        for byte in self.bytes() {
            hash ^= byte as u64;
            hash = hash.wrapping_mul(0x100000001b3);
        }
        hash
    }
}

impl Hash for String {
    fn hash(&self) -> u64 {
        self.as_str().hash()
    }
}

// C ABI exports

/// Creates a new empty map
#[no_mangle]
pub extern "C" fn jet_map_new() -> *mut Map<i64, i64> {
    Box::into_raw(Box::new(Map::new()))
}

/// Frees a map
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_map_free`'s FFI contract.
pub unsafe extern "C" fn jet_map_free(map: *mut Map<i64, i64>) {
    if !map.is_null() {
        unsafe { drop(Box::from_raw(map)) };
    }
}

/// Returns the length of a map
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_map_len`'s FFI contract.
pub unsafe extern "C" fn jet_map_len(map: *const Map<i64, i64>) -> usize {
    unsafe { (*map).len() }
}

/// Returns true if the map is empty
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_map_is_empty`'s FFI contract.
pub unsafe extern "C" fn jet_map_is_empty(map: *const Map<i64, i64>) -> bool {
    unsafe { (*map).is_empty() }
}

/// Clears a map
#[no_mangle]
/// # Safety
/// The caller must pass a valid pointer according to `jet_map_clear`'s FFI contract.
pub unsafe extern "C" fn jet_map_clear(map: *mut Map<i64, i64>) {
    unsafe { (*map).clear() }
}
