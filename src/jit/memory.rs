use std::slice;
use std::fmt;
use std::ops::{Deref, DerefMut};

/**
 * A wrapper around a block of memory which can have it's writeability/executeability toggled.
 */

pub struct Memory {
    buffer: *mut u8,
    len: usize
}

impl Memory {
    pub fn offset<'a>(&'a self, i: usize) -> *mut u8 {
        if i >= self.len {
            panic!("Memory offset out of range");
        }
        unsafe { self.buffer.offset(i as isize) }
    }
}

impl Deref for Memory {
    type Target = [u8];

    fn deref(&self) -> &[u8] {
        unsafe { slice::from_raw_parts(self.buffer, self.len) }
    }
}

impl DerefMut for Memory {
    fn deref_mut(&mut self) -> &mut [u8] {
        unsafe { slice::from_raw_parts_mut(self.buffer, self.len) }
    }
}

impl fmt::Debug for Memory {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        (self as &[u8]).fmt(f)
    }
}

#[cfg(target_family = "windows")]
mod windows {
    use super::Memory;

    use std::ptr;
    use std::os::raw::c_void;

    extern crate winapi;
    extern crate kernel32;
    use self::kernel32::{VirtualAlloc, VirtualProtect, VirtualFree};

    impl Memory {
        pub fn new(len: usize) -> Memory {

            let inner = unsafe { VirtualAlloc(
                ptr::null_mut(),
                len as u64, 
                winapi::MEM_COMMIT | winapi::MEM_RESERVE,
                winapi::PAGE_READWRITE
            ) };
            if inner.is_null() {
                panic!("Failed to allocate memory");
            }
            Memory {
                buffer: inner as *mut u8,
                len: len
            }
        }

        pub fn make_executable(&mut self) {
            let mut a: u32 = 0;
            if 0 == unsafe { VirtualProtect(
                self.buffer as *mut c_void,
                self.len as u64,
                winapi::PAGE_EXECUTE_READ,
                &mut a as *mut _
            ) } {
                panic!("Failed to protect memory");
            }
        }

        pub fn make_editable(&mut self) {
            let mut a: u32 = 0;
            if 0 == unsafe { VirtualProtect(
                self.buffer as *mut c_void,
                self.len as u64,
                winapi::PAGE_READWRITE,
                &mut a as *mut _
            ) } {
                panic!("Failed to make memory editable");
            }
        }
    }

    impl Drop for Memory {
        fn drop(&mut self) {
            if 0 == unsafe { VirtualFree(self.buffer as *mut c_void, 0, winapi::MEM_RELEASE) } {
                panic!("Failed to free memory");
            }
        }
    }
}

#[cfg(target_family = "unix")]
mod unix {
    use super::Memory;

    use std::ptr;
    use std::os::raw::c_void;

    extern crate libc;

    impl Memory {
        pub fn new(len: usize) -> Memory {
            let inner = unsafe { libc::mmap64(
                ptr::null(),
                len,
                libc::PROT_READ | libc::PROT_WRITE, libc::MAP_ANONYMOUS,
                -1,
                0
            ) };
            if inner == libc::MAP_FAILED {
                panic!("Failed to allocate memory");
            }
            Memory {
                buffer: inner as *mut u8,
                len: len
            }
        }

        pub fn make_executable(&mut self) {
            if -1 == unsafe { libc::mprotect(
                self.buffer as *mut c_void,
                self.len,
                libc::PROT_READ | libc::PROT_EXEC
            ) } {
                panic!("Failed to protect memory");
            }
        }

        pub fn make_editable(&mut self) {
            if -1 == unsafe { libc::mprotect(
                self.buffer as *mut c_void,
                self.len,
                libc::PROT_READ | libc::PROT_WRITE
            ) } {
                panic!("Failed to make memory editable");
            }
        }
    }

    impl Drop for Memory {
        fn drop(&mut self) {
            if -1 == unsafe { libc::munmap(self.buffer as *mut c_void, self.len) } {
                panic!("Failed to free memory");
            }
        }
    }
}
