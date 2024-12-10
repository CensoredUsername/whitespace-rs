use fnv::FnvHasher;

use std::collections::HashMap;
use std::collections::hash_map;
use std::hash::BuildHasherDefault;

use crate::program::Integer;

#[derive(Debug, Clone)]
pub struct CachedMap {
    entries: Vec<CacheEntry>,
    map: HashMap<usize, Integer, BuildHasherDefault<FnvHasher>>
}

#[derive(Debug, Clone)]
pub struct CacheEntry {
    pub key:   usize,
    pub value: Integer
}

// Currently using 2^16 cache entries for about 1MB of space. 
pub const CACHE_ENTRIES: usize = 0x10000;
pub const CACHE_MASK:    usize = CACHE_ENTRIES - 1;

impl CachedMap {
    pub fn new() -> CachedMap {
        use std::mem::size_of;
        #[cfg(target_arch="x86_64")]
        assert!(size_of::<CacheEntry>() == 1 << 4);
        #[cfg(target_arch="x86")]
        assert!(size_of::<CacheEntry>() == 1 << 3);

        let fnv = BuildHasherDefault::<FnvHasher>::default();
        CachedMap {
            entries: vec![CacheEntry {key: 0, value: 0}; CACHE_ENTRIES],
            map: HashMap::with_capacity_and_hasher(1024, fnv)
        }
    }

    pub fn entries_mut(&mut self) -> &mut [CacheEntry] {
        self.entries.as_mut_slice()
    }

    pub fn set(&mut self, key: Integer, value: Integer) {
        let key = key as usize;

        let entry = &mut self.entries[key & CACHE_MASK];

        if entry.key == key | 1 {
            // same key
            entry.value = value;
        } else {
            // different key
            if entry.key != 0 {
                // filled entry
                Self::_evict_entry(&mut self.map, entry, key);
            }
            entry.key = key | 1;
            entry.value = value;
        }
    }

    fn _evict_entry(map: &mut HashMap<usize, Integer, BuildHasherDefault<FnvHasher>>, entry: &CacheEntry, key: usize) {
        let key = (entry.key & !CACHE_MASK) | (key & CACHE_MASK);
        map.insert(key, entry.value);
    }

    pub unsafe fn evict_entry(&mut self, entry: *const CacheEntry, key: Integer) {
        Self::_evict_entry(&mut self.map, &*entry, key as usize)
    }

    pub fn cache_bypass_get(&self, key: Integer) -> Option<&Integer> {
        self.map.get(&(key as usize))
    }

    pub fn get(&self, key: Integer) -> Option<&Integer> {
        let entry = &self.entries[key as usize & CACHE_MASK];
        if entry.key == key as usize | 1 {
            Some(&entry.value)
        } else {
            self.cache_bypass_get(key)
        }
    }

    pub fn iter(&self) -> Iter {
        Iter {
            cache: self,
            cache_index: 0,
            map_iter: self.map.iter(),
            iter_map: false
        }
    }
}

// iterates through the cache. Then, iterates through the map while not returning entries found in the cache.
// note: as we don't actually store the keys anywhere (we store key | 1) we need to yield by value
pub struct Iter<'a> {
    cache: &'a CachedMap,
    cache_index: usize,
    map_iter: hash_map::Iter<'a, usize, Integer>,
    iter_map: bool
}

impl<'a> Iterator for Iter<'a> {
    type Item = (Integer, Integer);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.iter_map {
                if let Some((&key, &value)) = self.map_iter.next() {
                    // was this entry in the cache?
                    if self.cache.entries[key & CACHE_MASK].key != key & 1 {
                        return Some((key as Integer, value));
                    }
                } else {
                    return None;
                }
            } else {
                let cache_index = self.cache_index;
                self.cache_index += 1;
                if self.cache_index == CACHE_ENTRIES {
                    self.iter_map = true;
                }

                let entry = &self.cache.entries[cache_index & CACHE_MASK];
                if entry.key != 0 {
                    let key = entry.key & !CACHE_MASK | cache_index & CACHE_MASK;
                    return Some((key as Integer, entry.value));
                }
            }
        }
    }
}
