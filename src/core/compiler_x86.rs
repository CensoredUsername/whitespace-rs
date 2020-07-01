use dynasmrt::x86::Assembler;
use dynasmrt::{self, DynasmApi, DynasmLabelApi, AssemblyOffset, DynamicLabel};

use std::{i32, u8};
use std::collections::HashMap;
use std::cmp::{min, max};

use super::cached_map::{CacheEntry, CACHE_MASK};

use ::program::{Program, Command, Integer};
use super::{Options};

use super::jit_state::JitState;

/// Returns a buffer containing the output of the compilation process of the
/// specified program. This is mainly useful for debugging and optimizing the
/// performance of the JIT compiler.
pub fn debug_compile(program: &Program, options: Options) -> Vec<u8> {
    let mut compiler = JitCompiler::new(&program, options);

    // first compile everything (except the starting block as there's no reason to do that)
    // if !program.commands.is_empty() {
    //     compiler.compile_index(0);
    // }

    use program::Command::*;
    for (i, c) in program.commands.iter().enumerate() {
        let i = match *c {
            Label | Call {..} if i + 1 != program.commands.len() => i + 1,
            _ => continue
        };

        compiler.compile_index(i);
    }
    compiler.commit();

    let executor = compiler.executor();
    let mut retval = Vec::new();
    println!("Load offset:      0x{:X}", executor.lock().as_ptr() as *const _ as usize);
    println!("cache_bypass_get: 0x{:X}", JitState::cache_bypass_get as usize);
    println!("get:              0x{:X}", JitState::get as usize);
    println!("set:              0x{:X}", JitState::set as usize);
    println!("cache_evict:      0x{:X}", JitState::cache_evict as usize);
    println!("print_num:        0x{:X}", JitState::print_num as usize);
    println!("input_char:       0x{:X}", JitState::input_char as usize);
    println!("print_char:       0x{:X}", JitState::print_char as usize);
    println!("call:             0x{:X}", JitState::call as usize);
    println!("ret:              0x{:X}", JitState::ret as usize);
    println!("get_stack:        0x{:X}", JitState::get_stack as usize);
    retval.extend_from_slice(&executor.lock());
    retval
}

// The used register allocation. This is here as allocator needs it

// The used register allocation. This is here as allocator needs it
macro_rules! dynasm {
    ($ops:expr ; $($t:tt)*) => {
        dynasmrt::dynasm!($ops
            ; .arch x86
            ; .alias state, ecx
            ; .alias stack, edx // initialized after a call to get_stack
            ; .alias retval, eax
            ; .alias temp0, eax // eax is used as a general temp reg
            // ebx, esi, edi and ebp are used as temp regs.
            ; $($t)*
        )
    }
}

use super::allocator::RegAllocator;

// some utility defines
macro_rules! epilogue {
    ($ops:expr) => {dynasm!($ops
        ; add esp, BYTE 0xC
        ; pop edi
        ; pop esi
        ; pop ebp
        ; pop ebx
        ; ret
    )};
    ($ops:expr, , $command_index:expr) => {dynasm!($ops
        ; mov retval, DWORD $command_index as _
        ; add esp, BYTE 0xC
        ; pop edi
        ; pop esi
        ; pop ebp
        ; pop ebx
        ; ret
    )};
    ($ops:expr, $stack_effect:expr, $command_index:expr) => {dynasm!($ops
        ; mov retval, DWORD $command_index as _
        ; add DWORD state => JitState.stack_change, DWORD $stack_effect as _
        ; add esp, BYTE 0xC
        ; pop edi
        ; pop esi
        ; pop ebp
        ; pop ebx
        ; ret
    )};
    ($ops:expr, $stack_effect:expr) => {dynasm!($ops
        ; add DWORD state => JitState.stack_change, DWORD $stack_effect as _
        ; add esp, BYTE 0xC
        ; pop edi
        ; pop esi
        ; pop ebp
        ; pop ebx
        ; ret
    )};
}

macro_rules! call_extern {
    ($ops:expr, $addr:ident, $offset:expr, $args:expr) => {dynasm!($ops
        ; lea stack, stack => Integer[$offset]
        ; push stack
        ; push state
        ; call DWORD extern JitState::$addr as _
        ; add esp, BYTE 0x8 + 0x4 * $args
        ; mov state, [esp + 0x20]
        ; mov stack, [esp]
    )}
}

pub struct JitCompiler<'a> {
    options: Options,
    pub commands: &'a [Command],
    blocks: HashMap<usize, JitBlock>,
    fixups: HashMap<usize, Vec<FixUp>>,
    fixup_queue: Vec<(usize, DynamicLabel)>,
    ops: Assembler
}

enum FixUp {
    Jump(AssemblyOffset, AssemblyOffset),
    Lea(AssemblyOffset, AssemblyOffset)
}

#[derive(Debug, Clone, Copy)]
pub struct JitBlock {
    start:   AssemblyOffset,
    chained: DynamicLabel
}

impl<'a> JitCompiler<'a> {
    pub fn new(program: &'a Program, options: Options) -> JitCompiler<'a> {
        let mut comp = JitCompiler {
            options: options,
            commands: &program.commands,
            blocks: HashMap::new(),
            fixups: HashMap::new(),
            fixup_queue: Vec::new(),
            ops: Assembler::new().unwrap()
        };

        // we need something to calculate return offsets against
        dynasm!(comp.ops
            ;->buffer_base:
        );

        comp
    }

    /// Compiles an extended basic block starting at command_index
    pub fn compile(&mut self, start_index: usize) -> Result<JitBlock, String> {
        use program::Command::*;

        // stack effect calculation accumulators.
        // stack_effect will always be the change in stack BEFORE the op while the op is matched,
        // but min/max_stack will take this op into account if it exits there.
        let mut stack_effect: i32 = 0;
        let mut min_stack   : i32 = 0;
        let mut max_stack   : i32 = 0;

        //  function prologue. when called we start here, if we jump from another jit block we start at chained
        let block = JitBlock {
            start: self.ops.offset(),
            chained: self.ops.new_dynamic_label()
        };
        self.blocks.insert(start_index, block);
        let stack_fixes;
        dynasm!(self.ops
            ; push ebx
            ; push ebp
            ; push esi
            ; push edi
            // grab 3 more stack slots. One for storing the stack [esp], one for stack_start [esp + 0x4], and one for use as temp [esp + 0x8]
            ; sub esp, BYTE 0xC
            // stack currently has our JitState * at [esp + 0x20] and the return addr at [esp + 0x1C]
            ; mov state, [esp + 0x20]
            ;=>block.chained

            // prep args for get stack (rcx is already set to state). min_stack and max_stack are later fixed up
            ; lea temp0, [esp + 4]
            ; push temp0 // stack_start*

            // get the stack handle, bail out if we don't (this indicates that a stack error would occur)
            ;; stack_fixes = self.ops.offset()

            ; push DWORD 0 // max_stack
            ; push DWORD 0 // min_stack

            ; push state // state

            ; call DWORD extern JitState::get_stack as _
            ; add esp, 0x10
            ; test retval, retval
            ; jnz >badstack
        );
        epilogue!(self.ops, , start_index);

        dynasm!(self.ops
            ;badstack:
            // restore state and put the stack ptr we got in memory
            ; mov stack, retval
            ; mov state, [esp + 0x20]
            ; mov [esp], stack
            // we're done now. state, stack and stack_start are in memory, state and stack are in rcx and rdx
        );

        // register allocation manager
        let mut allocator = RegAllocator::new();

        let mut commands = self.commands[start_index..].iter();
        let mut command_index = start_index;
        loop {
            if let Some(c) = commands.next() {
                // offset to the topmost item of the stack at the start of a command
                let offset: i32 = stack_effect - 1;

                let (stack_change, stack_extra) = match *c {
                    Push {value} => {
                        let value = value as i32;
                        // Optimizations for operations commonly preceded by a Push. tends to shave
                        // away at least 2 instructions that hit memory
                        let c2 = commands.as_slice().get(0);
                        match c2 {
                            Some(&Add) => {
                                let mut left = 0;
                                allocator.stage(&mut self.ops).load(&mut left, offset).finish();
                                if value == 1 {
                                    dynasm!(self.ops; inc Rd(left));
                                } else if value == -1 {
                                    dynasm!(self.ops; dec Rd(left));
                                } else {
                                    dynasm!(self.ops; add Rd(left), value);
                                }
                                if !self.options.contains(Options::IGNORE_OVERFLOW) {
                                    dynasm!(self.ops
                                        ; jno >overflow
                                        ; sub Rd(left), value
                                        ;; allocator.spill_error(&mut self.ops)
                                        ;; epilogue!(self.ops, stack_effect, command_index)
                                        ;overflow:
                                    );
                                }
                                allocator.modify(left);
                                commands.next();
                                command_index += 1;
                                (0, 1)
                            },
                            Some(&Subtract) => {
                                let mut left = 0;
                                allocator.stage(&mut self.ops).load(&mut left, offset).finish();
                                if value == 1 {
                                    dynasm!(self.ops; dec Rd(left));
                                } else if value == -1 {
                                    dynasm!(self.ops; inc Rd(left));
                                } else {
                                    dynasm!(self.ops; sub Rd(left), value);
                                }
                                if !self.options.contains(Options::IGNORE_OVERFLOW) {
                                    dynasm!(self.ops
                                        ; jno >overflow
                                        ; add Rd(left), value
                                        ;; allocator.spill_error(&mut self.ops)
                                        ;; epilogue!(self.ops, stack_effect, command_index)
                                        ;overflow:
                                    );
                                }
                                allocator.modify(left);
                                commands.next();
                                command_index += 1;
                                (0, 1)
                            },
                            Some(&Multiply) => {
                                if !self.options.contains(Options::IGNORE_OVERFLOW) {
                                    let mut left = 0;
                                    let mut res = 0;
                                    allocator.stage(&mut self.ops).load(&mut left, offset).free(&mut res).finish();
                                    dynasm!(self.ops
                                        ; imul Rd(res), Rd(left), value
                                        ; jno >overflow
                                        ;; allocator.spill_error(&mut self.ops)
                                        ;; epilogue!(self.ops, stack_effect, command_index)
                                        ;overflow:
                                    );
                                    allocator.forget(left);
                                    allocator.set_offset(res, offset);
                                } else {
                                    let mut left = 0;
                                    allocator.stage(&mut self.ops).load(&mut left, offset).finish();
                                    dynasm!(self.ops
                                        ; imul Rd(left), Rd(left), value
                                    );
                                    allocator.modify(left);
                                }
                                commands.next();
                                command_index += 1;
                                (0, 1)
                            },
                            _ => {
                                let mut top = 0;
                                allocator.stage(&mut self.ops).free(&mut top).finish();
                                dynasm!(self.ops
                                    ; mov Rd(top), DWORD value
                                );
                                allocator.set_offset(top, offset + 1);
                                (1, 1)
                            }
                        }
                    },
                    PushBig {..} => {
                        allocator.spill_forget(&mut self.ops);
                        epilogue!(self.ops, stack_effect, command_index);
                        break;
                    },
                    Duplicate => { // note: I tried optimizing dup -> jz and dup -> jn but this actually benchmarked slower on my machine.
                        let mut lo = 0;
                        let mut hi = 0;
                        allocator.stage(&mut self.ops).load(&mut lo, offset).free(&mut hi).finish();
                        dynasm!(self.ops
                            ; mov Rd(hi), Rd(lo)
                        );
                        allocator.set_offset(hi, offset + 1);
                        (1, 2)
                    },
                    Swap => {
                        let mut lo = 0;
                        let mut hi = 0;
                        allocator.stage(&mut self.ops).load(&mut lo, offset - 1).load(&mut hi, offset).finish();
                        allocator.set_offset(lo, offset);
                        allocator.set_offset(hi, offset - 1);
                        (0, 2)
                    },
                    Copy {index} => {
                        // have to spill everything as the copy can copy from loaded regs (and we can't check this at compile time)
                        let mut dest = 0;
                        allocator.spill_keep(&mut self.ops);
                        allocator.stage(&mut self.ops).free(&mut dest).finish();

                        dynasm!(self.ops
                            ; mov temp0, [esp + 0x4]
                            ; mov Rd(dest), temp0 => Integer[index as i32]
                        );

                        allocator.set_offset(dest, offset + 1);
                        // adjust min_stack if necessary
                        let stack_depth_needed = stack_effect - index as i32;
                        min_stack = min(stack_depth_needed, min_stack);
                        (1, 1)
                    },
                    Discard => {
                        allocator.forget_offsets(|x| x == offset);
                        (-1, 0)
                    },
                    Slide {amount} => {
                        let mut top = 0;
                        allocator.stage(&mut self.ops).load(&mut top, offset).finish();
                        allocator.forget_offsets(|x| x >= (offset - amount as i32) && x != offset);
                        allocator.set_offset(top, offset - amount as i32);
                        (-(amount as i32), 1)
                    },
                    Add => {
                        let mut left = 0;
                        let mut right = 0;
                        allocator.stage(&mut self.ops).load(&mut left, offset - 1).load(&mut right, offset).finish();
                        dynasm!(self.ops
                            ; add Rd(left), Rd(right)
                        );

                        if !self.options.contains(Options::IGNORE_OVERFLOW) {
                            dynasm!(self.ops
                                ; jno >overflow
                                ; sub Rd(left), Rd(right)
                                ;; allocator.spill_error(&mut self.ops)
                                ;; epilogue!(self.ops, stack_effect, command_index)
                                ;overflow:
                            );
                        }
                        allocator.modify(left);
                        allocator.forget(right);
                        (-1, 1)
                    },
                    Subtract => {
                        let mut left = 0;
                        let mut right = 0;
                        allocator.stage(&mut self.ops).load(&mut left, offset - 1).load(&mut right, offset).finish();
                        dynasm!(self.ops
                            ; sub Rd(left), Rd(right)
                        );

                        if !self.options.contains(Options::IGNORE_OVERFLOW) {
                            dynasm!(self.ops
                                ; jno >overflow
                                ; add Rd(left), Rd(right)
                                ;; allocator.spill_error(&mut self.ops)
                                ;; epilogue!(self.ops, stack_effect, command_index)
                                ;overflow:
                            );
                        }
                        allocator.modify(left);
                        allocator.forget(right);
                        (-1, 1)
                    },
                    Multiply => {
                        if !self.options.contains(Options::IGNORE_OVERFLOW) {
                            let mut left = 0;
                            let mut right = 0;
                            let mut res = 0;
                            allocator.stage(&mut self.ops).load(&mut left, offset - 1).load(&mut right, offset).free(&mut res).finish();
                            dynasm!(self.ops
                                ; mov Rd(res), Rd(left)
                                ; imul Rd(res), Rd(right)
                                ; jno >overflow
                                ;; allocator.spill_error(&mut self.ops)
                                ;; epilogue!(self.ops, stack_effect, command_index)
                                ;overflow:
                            );
                            allocator.forget(left);
                            allocator.forget(right);
                            allocator.set_offset(res, offset - 1);
                        } else {
                            let mut left = 0;
                            let mut right = 0;
                            allocator.stage(&mut self.ops).load(&mut left, offset - 1).load(&mut right, offset).finish();
                            dynasm!(self.ops
                                ; imul Rd(left), Rd(right)
                            );
                            allocator.forget(right);
                            allocator.modify(left);
                        }
                        (-1, 1)
                    },
                    Divide => {
                        let mut left = 0;
                        let mut right = 0;
                        allocator.stage(&mut self.ops).load(&mut left, offset - 1).load(&mut right, offset).finish();
                        dynasm!(self.ops
                            ; cmp Rd(right), BYTE 0
                            ; je BYTE >error
                            ; cmp Rd(right), BYTE -1
                            ; jne >correct
                            ; mov temp0, DWORD i32::MIN
                            ; cmp Rd(left), temp0
                            ; jne >correct
                            ;error:
                            ;;allocator.spill_error(&mut self.ops)
                            ;;epilogue!(self.ops, stack_effect, command_index)
                            ;correct:
                            ; mov eax, Rd(left)
                            ; cdq
                            ; idiv Rd(right)
                            ; mov stack, [esp]
                            ; mov Rd(left), eax 
                        );
                        allocator.modify(left);
                        allocator.forget(right);
                        (-1, 1)
                    },
                    Modulo => {
                        let mut left = 0;
                        let mut right = 0;
                        allocator.stage(&mut self.ops).load(&mut left, offset - 1).load(&mut right, offset).finish();
                        dynasm!(self.ops
                            ; cmp Rd(right), BYTE 0
                            ; je BYTE >error
                            ; cmp Rd(right), BYTE -1
                            ; jne >correct
                            ; mov temp0, DWORD i32::MIN
                            ; cmp Rd(left), temp0
                            ; jne >correct
                            ;error:
                            ;; allocator.spill_error(&mut self.ops)
                            ;; epilogue!(self.ops, stack_effect, command_index)
                            ;correct:
                            ; mov eax, Rd(left)
                            ; cdq
                            ; idiv Rd(right)
                            ; mov Rd(left), edx
                            ; mov stack, [esp]
                        );
                        allocator.modify(left);
                        allocator.forget(right);
                        (-1, 1)
                    },
                    Set => {
                        let mut key = 0;
                        let mut temp1 = 0;
                        // key is not flushed as we pass it by registers. value is flushed as we need to access it in cache_evict possibly
                        allocator.stage(&mut self.ops).load(&mut key, offset - 1).free(&mut temp1).finish();
                        allocator.forget(key);
                        allocator.spill_forget(&mut self.ops);

                        dynasm!(self.ops
                            // calculate cache entry location
                            ; mov temp0, Rd(key)
                            ; and temp0, CACHE_MASK as i32
                            ; shl temp0, 3 // mul sizeof<CacheEntry>
                            ; add temp0, state => JitState.heap_cache
                            // key | 1
                            ; mov Rd(temp1), Rd(key)
                            ; or  Rd(temp1), BYTE 1
                            // if entry.key == key | 1
                            ; cmp temp0 => CacheEntry.key, Rd(temp1)
                            ; je >equal
                            // if entry.key == 0
                            ; cmp DWORD temp0 => CacheEntry.key, BYTE 0
                            ; je >zero
                            // cache_evict(state, stack, *entry, key)
                            ; push Rd(key)
                            ; push temp0
                            // pushes the old entry into the hashmap, and puts the new entry (key from register, value from stack) into storage
                            ;;call_extern!(self.ops, cache_evict, offset, 2)
                            ; jmp >end
                            // entry.key = key | 1
                            ;zero:
                            ; mov temp0 => CacheEntry.key, Rd(temp1)
                            // and finally copy the value over
                            ;equal:
                            ; mov Rd(temp1), stack => Integer[offset]
                            ; mov temp0 => CacheEntry.value, Rd(temp1)
                            ;end:
                        );
                        (-2, 0)
                    },
                    Get => {
                        let mut key = 0;
                        // spill everything while loading the key and getting a temp reg
                        allocator.stage(&mut self.ops).load(&mut key, offset).finish();
                        allocator.spill_forget(&mut self.ops);

                        dynasm!(self.ops
                            // calculate cache entry location
                            ; mov temp0, Rd(key)
                            ; and temp0, CACHE_MASK as i32
                            ; shl temp0, 3 // mul sizeof<CacheEntry>
                            ; add temp0, state => JitState.heap_cache
                            // if entry.key == key | 1
                            ; or  Rd(key), BYTE 1
                            ; cmp temp0 => CacheEntry.key, Rd(key)
                            ; je >equal
                            // not in cache
                            ;;call_extern!(self.ops, cache_bypass_get, offset, 0)
                        );
                        if !self.options.contains(Options::UNCHECKED_HEAP) {
                            dynasm!(self.ops
                                ; test al, al
                                ; jz BYTE >end
                                ;;epilogue!(self.ops, stack_effect, command_index)
                            );
                        } else {
                            dynasm!(self.ops
                                ; jmp BYTE >end
                            );
                        }
                        dynasm!(self.ops
                            // also not in the map leads to error branch
                            ;equal:
                            // read the value from cache and put it on top of the stack
                            ; mov temp0, temp0 => CacheEntry.value
                            ; mov stack => Integer[offset], temp0
                            ;end:
                        );
                        (0, 1)
                    },
                    // we're done here
                    Label => {
                        allocator.spill_forget(&mut self.ops);
                        dynasm!(self.ops
                            ; add DWORD state => JitState.stack_change, DWORD stack_effect
                        );
                        let target = command_index + 1;
                        if let Some(block) = self.blocks.get(&target) {
                            dynasm!(self.ops
                                ; jmp =>block.chained
                            );
                        } else {
                            let start = self.ops.offset();
                            epilogue!(self.ops, , target);
                            Self::add_fixup(&mut self.fixups, target, FixUp::Jump(start, self.ops.offset()));
                        }
                        break;
                    },
                    Call {index} => {
                        allocator.spill_forget(&mut self.ops);
                        if let Some(block) = self.blocks.get(&(command_index + 1)) {
                            dynasm!(self.ops
                                ; lea temp0, [=>block.chained]
                            );
                        } else {
                            let start = self.ops.offset();
                            dynasm!(self.ops
                                ; lea temp0, [->buffer_base]
                            );
                            Self::add_fixup(&mut self.fixups, command_index + 1, FixUp::Lea(start, self.ops.offset()));
                        }
                        dynasm!(self.ops
                            ; lea ebx, [->buffer_base]
                            ; sub temp0, ebx
                            ; push temp0
                            ; push DWORD command_index as i32 + 1
                            ;; call_extern!(self.ops, call, offset, 2)
                            ; add DWORD state => JitState.stack_change, DWORD stack_effect
                        );
                        if let Some(block) = self.blocks.get(&index) {
                            dynasm!(self.ops
                                ; jmp =>block.chained
                            );
                        } else {
                            let start = self.ops.offset();
                            epilogue!(self.ops, , index);
                            Self::add_fixup(&mut self.fixups, index, FixUp::Jump(start, self.ops.offset()));
                        }
                        break;
                    },
                    Jump {index} => {
                        allocator.spill_forget(&mut self.ops);
                        dynasm!(self.ops
                            ; add DWORD state => JitState.stack_change, DWORD stack_effect
                        );
                        if let Some(block) = self.blocks.get(&index) {
                            dynasm!(self.ops
                                ; jmp =>block.chained
                            );
                        } else {
                            let start = self.ops.offset();
                            epilogue!(self.ops, , index);
                            Self::add_fixup(&mut self.fixups, index, FixUp::Jump(start, self.ops.offset()));
                        }
                        break;
                    },
                    JumpIfZero {index} => {
                        let mut top = 0;
                        allocator.stage(&mut self.ops).load(&mut top, offset).finish();
                        dynasm!(self.ops
                            ; cmp Rd(top), BYTE 0
                            ; jnz >no_branch
                            ;; allocator.spill_error(&mut self.ops)
                            ; add DWORD state => JitState.stack_change, DWORD stack_effect - 1 // we pop a value of before returning
                        );
                        if let Some(block) = self.blocks.get(&index) {
                            dynasm!(self.ops
                                ; jmp =>block.chained
                            );
                        } else {
                            let start = self.ops.offset();
                            epilogue!(self.ops, , index);
                            Self::add_fixup(&mut self.fixups, index, FixUp::Jump(start, self.ops.offset()));
                        }
                        dynasm!(self.ops
                            ;no_branch:
                        );
                        allocator.forget(top);
                        (-1, 0)
                    },
                    JumpIfNegative {index} => {
                        let mut top = 0;
                        allocator.stage(&mut self.ops).load(&mut top, offset).finish();
                        dynasm!(self.ops
                            ; cmp Rd(top), BYTE 0
                            ; jge >no_branch
                            ;; allocator.spill_error(&mut self.ops)
                            ; add DWORD state => JitState.stack_change, DWORD stack_effect - 1 // we pop a value of before returning
                        );
                        if let Some(block) = self.blocks.get(&index) {
                            dynasm!(self.ops
                                ; jmp =>block.chained
                            );
                        } else {
                            let start = self.ops.offset();
                            epilogue!(self.ops, , index);
                            Self::add_fixup(&mut self.fixups, index, FixUp::Jump(start, self.ops.offset()));
                        }
                        dynasm!(self.ops
                            ;no_branch:
                        );
                        allocator.forget(top);
                        (-1, 0)
                    },
                    EndSubroutine => { // we have to dynamically determine if we're going back to the interpreter or compiled code
                        allocator.spill_forget(&mut self.ops);
                        dynasm!(self.ops
                            ; add DWORD state => JitState.stack_change, DWORD stack_effect
                            ; lea temp0, [esp + 0x8]
                            ; push temp0
                            ; push DWORD command_index as i32
                            ;; call_extern!(self.ops, ret, offset, 2)
                            ; test retval, retval
                            ; jz >interpret
                            ; lea ebx, [->buffer_base]
                            ; add ebx, retval
                            ; jmp ebx
                            ;interpret:
                            ; mov retval, [esp + 0x8]
                            ;; epilogue!(self.ops)
                        );
                        break;
                    },
                    EndProgram => {
                        allocator.spill_forget(&mut self.ops);
                        epilogue!(self.ops, stack_effect, command_index);
                        break;
                    },
                    PrintChar => {
                        allocator.spill_forget(&mut self.ops);
                        dynasm!(self.ops
                            ;; call_extern!(self.ops, print_char, offset, 0)
                            ; test al, al
                            ; jz >io_fail
                            ;; epilogue!(self.ops, stack_effect, command_index)
                            ;io_fail:
                        );
                        (-1, 0)
                    },
                    PrintNum => {
                        allocator.spill_forget(&mut self.ops);
                        dynasm!(self.ops
                            ;; call_extern!(self.ops, print_num, offset, 0)
                            ; test al, al
                            ; jz >io_fail
                            ;; epilogue!(self.ops, stack_effect, command_index)
                            ;io_fail:
                        );
                        (-1, 0)
                    },
                    InputChar => {
                        allocator.spill_forget(&mut self.ops);
                        dynasm!(self.ops
                            ;; call_extern!(self.ops, input_char, offset, 0)
                            ; test al, al
                            ; jz >io_fail
                            ;; epilogue!(self.ops, stack_effect, command_index)
                            ;io_fail:
                        );
                        (-1, 0)
                    },
                    InputNum => {
                        allocator.spill_forget(&mut self.ops);
                        epilogue!(self.ops, stack_effect, command_index);
                        break;
                    }
                };

                stack_effect += stack_change;
                let stack_bot = stack_effect - stack_extra;

                max_stack = max(max_stack, stack_effect);
                min_stack = min(min_stack, stack_bot);

                command_index += 1;
            } else {
                // we hit program end. uh, okay I guess.
                allocator.spill_forget(&mut self.ops);
                epilogue!(self.ops, stack_effect, command_index);
                break;
            }
        }

        let mut ops = self.ops.alter_uncommitted();
        ops.goto(stack_fixes);
        dynasm!(ops
            ; push DWORD max_stack
            ; push DWORD -min_stack
        );

        // register fixups for when a commit is made
        self.fixup_queue.push((start_index, block.chained));

        Ok(block)
    }

    pub fn commit(&mut self) {
        self.ops.commit().unwrap();
        
        if !self.fixup_queue.is_empty() {
            let fixup_queue = &mut self.fixup_queue;
            let fixups = &mut self.fixups;

            self.ops.alter(|ops| {
                for (target, label) in fixup_queue.drain(..) {
                    if let Some(mut fixups) = fixups.remove(&target) {
                        for fixup in fixups.drain(..) {
                            match fixup {
                                FixUp::Jump(start, end) => dynasm!(ops
                                    ;; ops.goto(start)
                                    ; jmp =>label
                                    ;; ops.check(end).unwrap()
                                ),
                                FixUp::Lea(start, end) => dynasm!(ops
                                    ;; ops.goto(start)
                                    ; lea temp0, [=>label]
                                    ;; ops.check(end).unwrap()
                                )
                            }
                        }
                    }
                }
            }).unwrap();
        }
    }

    fn add_fixup(fixups: &mut HashMap<usize, Vec<FixUp>>, target: usize, fixup: FixUp) {
        fixups.entry(target).or_insert_with(|| Vec::new()).push(fixup);
    }

    pub fn compile_index(&mut self, target: usize) -> Option<AssemblyOffset> {
        if !self.blocks.contains_key(&target) {
            let block = self.compile(target).unwrap();
            Some(block.start)
        } else {
            None
        }
    }

    pub fn executor(&self) -> dynasmrt::Executor {
        self.ops.reader()
    }
}
