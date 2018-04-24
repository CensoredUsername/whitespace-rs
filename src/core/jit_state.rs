use std::io::{BufRead, Write};
use std::ptr;
use std::mem;

use super::cached_map::{CachedMap, CacheEntry, Iter};
use super::{State, SmallIntState};
use super::bigint_state::BigIntState;
use ::program::{Integer, BigInteger};
use ::{Options, UNCHECKED_HEAP, IGNORE_OVERFLOW};
use ::{WsError, WsErrorKind};


#[cfg(target_arch = "x86_64")]
macro_rules! abi {
    ($($t:tt)*) => {unsafe extern "win64" $($t)*}
}
#[cfg(target_arch = "x86")]
macro_rules! abi {
    ($($t:tt)*) => {unsafe extern "cdecl" $($t)*}
}
#[cfg(not(any(target_arch = "x86", target_arch = "x86_64")))]
macro_rules! abi {
    ($($t:tt)*) => {unsafe extern  $($t)*}
}
#[cfg(target_arch = "x86_64")]
macro_rules! pub_abi {
    ($($t:tt)*) => {pub unsafe extern "win64" $($t)*}
}
#[cfg(target_arch = "x86")]
macro_rules! pub_abi {
    ($($t:tt)*) => {pub unsafe extern "cdecl" $($t)*}
}
#[cfg(not(any(target_arch = "x86", target_arch = "x86_64")))]
macro_rules! pub_abi {
    ($($t:tt)*) => {pub unsafe extern  $($t)*}
}

pub struct JitState<'a> {
    options: Options,
    command_index: usize,
    callstack: Vec<RetLoc>,
    heap: CachedMap,
    pub heap_cache: *mut CacheEntry,
    stack: Vec<Integer>,
    pub stack_change: isize,
    input: &'a mut (BufRead + 'a),
    output: &'a mut (Write + 'a)
}

#[derive(Debug, Clone, Copy)]
struct RetLoc(usize, *const u8);

impl<'a, 'b> State<'a> for JitState<'b> {
    type Var = Integer;
    type HeapIterator = Iter<'a>;

    fn options(&self) -> Options {
        self.options
    }

    fn index(&mut self) -> &mut usize {
        &mut self.command_index
    }

    fn stack(&mut self) -> &mut Vec<Integer> {
        &mut self.stack
    }

    fn set(&mut self, key: Integer, value: Integer) {
        self.heap.set(key, value);
    }

    fn get(&self, key: &Integer) -> Option<&Integer> {
        self.heap.get(*key)
    }

    fn iter_heap(&'a self) -> Iter<'a> {
        self.heap.iter()
    }

    fn call(&mut self, retloc: usize) {
        self.callstack.push(RetLoc(retloc, ptr::null()));
    }
    fn ret(&mut self) -> Option<usize> {
        self.callstack.pop().map(|RetLoc(retloc, _)| retloc)
    }

    fn input(&mut self) -> &mut BufRead {
        self.input
    }

    fn output(&mut self) -> &mut Write {
        self.output
    }

    fn input_num(&mut self) -> Result<(), WsError> {
        let s = self.read_num()?;
        let s = s.trim();
        match s.parse::<Integer>() {
            Ok(value) => {
                let key = self.stack().pop().unwrap();
                self.set(key, value);
                Ok(())
            },
            Err(e) => match s.parse::<BigInteger>() {
                Ok(i) => if self.options.contains(IGNORE_OVERFLOW) {
                    Err(WsError::wrap(e, WsErrorKind::RuntimeParseError, "Parsed number is outside arithmetic range"))
                } else {
                    *self.index() += 1;
                    Err(WsError::new(WsErrorKind::InumOverflow(self.stack().pop().unwrap(), i), "Overflow while parsing number"))
                },
                Err(e) => Err(WsError::wrap(e, WsErrorKind::RuntimeParseError, "Expected a number to parse"))
            }
        }
    }
}

impl<'a, 'b> SmallIntState<'a> for JitState<'b> {
    fn into_bigintstate(&'a mut self) -> BigIntState<'a> {
        BigIntState::from_components(
            self.options,
            self.command_index,
            self.stack.iter().cloned().map(|i| i.into()).collect(),
            self.iter_heap().map(|(k, v)| (k.into(), v.into())).collect(),
            self.callstack.iter().map(|loc| loc.0).collect(),
            self.input,
            self.output
        )
    }
}

impl<'a> JitState<'a> {
    #![allow(dead_code)]

    pub fn new(options: Options, input: &'a mut (BufRead + 'a), output: &'a mut (Write + 'a)) -> JitState<'a> {
        let mut heap = CachedMap::new();
        let ptr  = heap.entries_mut().as_mut_ptr();
        JitState {
            options: options,
            command_index: 0,
            callstack: Vec::with_capacity(1024),
            heap: heap,
            heap_cache: ptr,
            stack: Vec::with_capacity(1024),
            stack_change: 0,
            input: input,
            output: output
        }
    }

    pub unsafe fn run_block(&mut self, ptr: *const u8) {
        let f: abi!(fn(*mut JitState) -> usize) = mem::transmute(ptr);
        self.command_index = f(self as *mut _);
        let len = (self.stack.len() as isize + self.stack_change) as usize;
        self.stack.set_len(len);
        self.stack_change = 0;
    }

    pub_abi!(fn cache_bypass_get(state: *mut JitState, stack: *mut Integer) -> u8 {
        if let Some(&value) = (*state).heap.cache_bypass_get(*stack) {
            *stack = value;
            0
        } else if (*state).options().contains(UNCHECKED_HEAP) {
            *stack = 0;
            0
        } else {
            1
        }
    });

    pub_abi!(fn get(state: *mut JitState, stack: *mut Integer) -> u8 {
        if let Some(&value) = (*state).heap.get(*stack) {
            *stack = value;
            0
        } else {
            1
        }
    });

    pub_abi!(fn set(state: *mut JitState, stack: *mut Integer) {
        (*state).heap.set(*stack.offset(-1), *stack);
    });

    pub_abi!(fn cache_evict(state: *mut JitState, stack: *mut Integer, entry: *mut CacheEntry, key: Integer) -> *mut CacheEntry {
        (*state).heap.evict_entry(entry, key);
        (*entry).key = key as usize | 1;
        (*entry).value = *stack;
        entry
    });

    pub_abi!(fn print_num(state: *mut JitState, stack: *mut Integer) -> u8 {
        (*state).write_num(*stack).is_err() as u8
    });

    pub_abi!(fn input_char(state: *mut JitState, stack: *mut Integer) -> u8 {
        if let Ok(c) = (*state).read_char() {
            (*state).set(*stack, c);
            0
        } else {
            1
        }
    });

    pub_abi!(fn print_char(state: *mut JitState, stack: *mut Integer) -> u8 {
        (*state).write_char(*stack).is_err() as u8
    });

    pub_abi!(fn call(state: *mut JitState, _stack: *mut Integer, index: usize, retptr: *const u8) {
        (*state).callstack.push(RetLoc(index, retptr));
    });

    pub_abi!(fn ret(state: *mut JitState, _stack: *mut Integer, fail_index: usize, ret_index: *mut usize) -> *const u8 {
        if let Some(RetLoc(index, block_ptr)) = (*state).callstack.pop() {
            if block_ptr.is_null() {
                *ret_index = index;
                ptr::null()
            } else {
                block_ptr
            }
        } else {
            *ret_index = fail_index;
            ptr::null()
        }
    });

    pub_abi!(fn get_stack(state: *mut JitState, min_stack: usize, max_stack: usize, stack_start: *mut *mut Integer) -> *mut Integer {
        let state = &mut *state;

        // fix the length of the stack and zero stack_change (only relevant when chained into as otherwise stack_change will be 0)
        let len = (state.stack.len() as isize + state.stack_change) as usize;
        state.stack.set_len(len);
        state.stack_change = 0;

        // ensure that the stack is at least min_stack items large
        if len < min_stack {
            return ptr::null_mut();
        }
        // ensure we will be able to push max_stack items
        if len + max_stack <= state.stack.capacity() {
            let start = state.stack.as_mut_ptr();
            *stack_start = start;
            start.offset(len as isize)
        } else {
            Self::reserve(state, mem::uninitialized(), max_stack, stack_start)
        }
    });

    #[cfg(target_arch = "x86_64")]
    #[cold]
    #[inline(never)]
    pub unsafe extern "win64" fn reserve(state: &mut JitState, _: usize, max_stack: usize, stack_start: *mut *mut Integer) -> *mut Integer {
        state.stack.reserve(max_stack);
        let start = state.stack.as_mut_ptr();
        *stack_start = start;
        start.offset(state.stack.len() as isize)
    }

    #[cfg(target_arch = "x86")]
    #[cold]
    #[inline(never)]
    pub unsafe extern "cdecl" fn reserve(state: &mut JitState, _: usize, max_stack: usize, stack_start: *mut *mut Integer) -> *mut Integer {
        state.stack.reserve(max_stack);
        let start = state.stack.as_mut_ptr();
        *stack_start = start;
        start.offset(state.stack.len() as isize)
    }

    #[cfg(not(any(target_arch = "x86", target_arch = "x86_64")))]
    pub unsafe extern fn reserve(state: &mut JitState, _: usize, max_stack: usize, stack_start: *mut *mut Integer) -> *mut Integer {
        state.stack.reserve(max_stack);
        let start = state.stack.as_mut_ptr();
        *stack_start = start;
        start.offset(state.stack.len() as isize)
    }
}
