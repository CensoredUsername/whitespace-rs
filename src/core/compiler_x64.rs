use dynasmrt::x64::Assembler;
use dynasmrt::{self, DynasmApi, DynasmLabelApi, AssemblyOffset, DynamicLabel};

use std::{i32, i64, u8};
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
    retval.extend_from_slice(&executor.lock());
    retval
}

// The used register allocation. This is here as allocator needs it
macro_rules! dynasm {
    ($ops:expr ; $($t:tt)*) => {
        dynasmrt::dynasm!($ops
            ; .arch x64
            ; .alias state, rcx
            ; .alias stack, rdx // initialized after a call to get_stack
            ; .alias retval, rax
            ; .alias temp0, rax // rax is used as a general temp reg
            // r8, r9, r10 and r11 are used as temp regs
            ; $($t)*
        )
    }
}

use super::allocator::RegAllocator;

// some utility defines
macro_rules! epilogue {
    ($ops:expr) => {dynasm!($ops
        ; add rsp, BYTE 0x28
        ; ret
    )};
    ($ops:expr, , $command_index:expr) => {dynasm!($ops
        ; mov retval, DWORD $command_index as _
        ; add rsp, BYTE 0x28
        ; ret
    )};
    ($ops:expr, $stack_effect:expr, $command_index:expr) => {dynasm!($ops
        ; mov retval, DWORD $command_index as _
        ; add QWORD state => JitState.stack_change, DWORD $stack_effect as _
        ; add rsp, BYTE 0x28
        ; ret
    )};
    ($ops:expr, $stack_effect:expr) => {dynasm!($ops
        ; add QWORD state => JitState.stack_change, DWORD $stack_effect as _
        ; add rsp, BYTE 0x28
        ; ret
    )};
}

macro_rules! call_extern {
    ($ops:expr, $addr:ident, $offset:expr) => {dynasm!($ops
        ; lea stack, stack => Integer[$offset]
        ; call QWORD [->$addr]
        ; mov state, [rsp + 0x30]
        ; mov stack, [rsp + 0x38]
    )}
}

#[cfg(target_arch = "x86_64")]
pub struct JitCompiler<'a> {
    options: Options,
    pub commands: &'a [Command],
    blocks: HashMap<usize, JitBlock>,
    fixups: HashMap<usize, Vec<FixUp>>,
    fixup_queue: Vec<(usize, DynamicLabel)>,
    ops: Assembler
}

#[derive(Debug)]
enum FixUp {
    Jump(AssemblyOffset, AssemblyOffset),
    Lea(AssemblyOffset, AssemblyOffset)
}

#[derive(Debug, Clone, Copy)]
pub struct JitBlock {
    start:   AssemblyOffset,
    chained: DynamicLabel
}

#[cfg(target_arch = "x86_64")]
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

        // create the import section
        dynasm!(comp.ops
            ;->buffer_base:
            ;->cache_bypass_get:
            ; .qword JitState::cache_bypass_get as _
            ;->cache_evict:
            ; .qword JitState::cache_evict as _
            ;->print_num:
            ; .qword JitState::print_num as _
            ;->print_char:
            ; .qword JitState::print_char as _
            ;->input_char:
            ; .qword JitState::input_char as _
            ;->call:
            ; .qword JitState::call as _
            ;->ret:
            ; .qword JitState::ret as _
            ;->get_stack:
            ; .qword JitState::get_stack as _
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
            ; sub rsp, BYTE 0x28
            ; mov [rsp + 0x30], state // rcx
            ;=>block.chained

            // get the stack handle, bail out if we don't (this indicates that a stack error would occur)
            ;; stack_fixes = self.ops.offset()

            // prep args for get stack (rcx is already set to state). min_stack and max_stack are later fixed up
            ; mov rdx, DWORD 0
            ; mov r8, DWORD 0
            ; lea r9, [rsp + 0x40] // this is where stack_start will be stored
            ; call QWORD [->get_stack]
            ; test retval, retval
            ; jnz >badstack
        );
        epilogue!(self.ops, , start_index);

        dynasm!(self.ops
            ;badstack:
            // restore state and put the stack ptr we got in memory
            ; mov stack, retval
            ; mov state, [rsp + 0x30]
            ; mov [rsp + 0x38], stack
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
                    Push {value} => if value > i32::MAX as Integer || value < i32::MIN as Integer {
                        let mut top = 0;
                        allocator.stage(&mut self.ops).free(&mut top).finish();
                        dynasm!(self.ops
                            ; mov Rq(top), QWORD value as i64
                        );
                        allocator.set_offset(top, offset + 1);
                        (1i32, 1)
                    } else {
                        let value = value as i32;
                        // Optimizations for operations commonly preceded by a Push. tends to shave
                        // away at least 2 instructions that hit memory
                        let c2 = commands.as_slice().get(0);
                        match c2 {
                            Some(&Add) => {
                                let mut left = 0;
                                allocator.stage(&mut self.ops).load(&mut left, offset).finish();
                                if value == 1 {
                                    dynasm!(self.ops; inc Rq(left));
                                } else if value == -1 {
                                    dynasm!(self.ops; dec Rq(left));
                                } else {
                                    dynasm!(self.ops; add Rq(left), value);
                                }
                                if !self.options.contains(Options::IGNORE_OVERFLOW) {
                                    dynasm!(self.ops
                                        ; jno >overflow
                                        ; sub Rq(left), value
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
                                    dynasm!(self.ops; dec Rq(left));
                                } else if value == -1 {
                                    dynasm!(self.ops; inc Rq(left));
                                } else {
                                    dynasm!(self.ops; sub Rq(left), value);
                                }
                                if !self.options.contains(Options::IGNORE_OVERFLOW) {
                                    dynasm!(self.ops
                                        ; jno >overflow
                                        ; add Rq(left), value
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
                                        ; imul Rq(res), Rq(left), value
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
                                        ; imul Rq(left), Rq(left), value
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
                                    ; mov Rq(top), DWORD value
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
                            ; mov Rq(hi), Rq(lo)
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
                            ; mov temp0, [rsp + 0x40]
                            ; mov Rq(dest), temp0 => Integer[index as i32]
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
                            ; add Rq(left), Rq(right)
                        );

                        if !self.options.contains(Options::IGNORE_OVERFLOW) {
                            dynasm!(self.ops
                                ; jno >overflow
                                ; sub Rq(left), Rq(right)
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
                            ; sub Rq(left), Rq(right)
                        );

                        if !self.options.contains(Options::IGNORE_OVERFLOW) {
                            dynasm!(self.ops
                                ; jno >overflow
                                ; add Rq(left), Rq(right)
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
                                ; mov Rq(res), Rq(left)
                                ; imul Rq(res), Rq(right)
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
                                ; imul Rq(left), Rq(right)
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
                            ; cmp Rq(right), BYTE 0
                            ; je BYTE >error
                            ; cmp Rq(right), BYTE -1
                            ; jne >correct
                            ; mov temp0, QWORD i64::MIN
                            ; cmp Rq(left), temp0
                            ; jne >correct
                            ;error:
                            ;;allocator.spill_error(&mut self.ops)
                            ;;epilogue!(self.ops, stack_effect, command_index)
                            ;correct:
                            ; mov rax, Rq(left)
                            ; cqo
                            ; idiv Rq(right)
                            ; mov stack, [rsp + 0x38]
                            ; mov Rq(left), rax 
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
                            ; cmp Rq(right), BYTE 0
                            ; je BYTE >error
                            ; cmp Rq(right), BYTE -1
                            ; jne >correct
                            ; mov temp0, QWORD i64::MIN
                            ; cmp Rq(left), temp0
                            ; jne >correct
                            ;error:
                            ;; allocator.spill_error(&mut self.ops)
                            ;; epilogue!(self.ops, stack_effect, command_index)
                            ;correct:
                            ; mov rax, Rq(left)
                            ; cqo
                            ; idiv Rq(right)
                            ; mov Rq(left), rdx
                            ; mov stack, [rsp + 0x38]
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
                            ; mov temp0, Rq(key)
                            ; and temp0, CACHE_MASK as i32
                            ; shl temp0, 4 // mul sizeof<CacheEntry>
                            ; add temp0, state => JitState.heap_cache
                            // key | 1
                            ; mov Rq(temp1), Rq(key)
                            ; or  Rq(temp1), BYTE 1
                            // if entry.key == key | 1
                            ; cmp temp0 => CacheEntry.key, Rq(temp1)
                            ; je >equal
                            // if entry.key == 0
                            ; cmp QWORD temp0 => CacheEntry.key, BYTE 0
                            ; je >zero
                            // cache_evict(state, stack, *entry, key)
                            ; mov r9, Rq(key)
                            ; mov r8, temp0
                            // pushes the old entry into the hashmap, and puts the new entry (key from register, value from stack) into storage
                            ;;call_extern!(self.ops, cache_evict, offset)
                            ; jmp >end
                            // entry.key = key | 1
                            ;zero:
                            ; mov temp0 => CacheEntry.key, Rq(temp1)
                            // and finally copy the value over
                            ;equal:
                            ; mov Rq(temp1), stack => Integer[offset]
                            ; mov temp0 => CacheEntry.value, Rq(temp1)
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
                            ; mov temp0, Rq(key)
                            ; and temp0, CACHE_MASK as i32
                            ; shl temp0, 4 // mul sizeof<CacheEntry>
                            ; add temp0, state => JitState.heap_cache
                            // if entry.key == key | 1
                            ; or  Rq(key), BYTE 1
                            ; cmp temp0 => CacheEntry.key, Rq(key)
                            ; je >equal
                            // not in cache
                            ;;call_extern!(self.ops, cache_bypass_get, offset)
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
                            ; add QWORD state => JitState.stack_change, DWORD stack_effect
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
                                ; lea r9, [=>block.chained]
                            );
                        } else {
                            let start = self.ops.offset();
                            dynasm!(self.ops
                                ; lea r9, [->buffer_base]
                            );
                            Self::add_fixup(&mut self.fixups, command_index + 1, FixUp::Lea(start, self.ops.offset()));
                        }
                        dynasm!(self.ops
                            ; lea temp0, [->buffer_base]
                            ; sub r9, temp0
                            ; mov r8, command_index as i32 + 1
                            ;; call_extern!(self.ops, call, offset)
                            ; add QWORD state => JitState.stack_change, DWORD stack_effect
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
                            ; add QWORD state => JitState.stack_change, DWORD stack_effect
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
                            ; cmp Rq(top), BYTE 0
                            ; jnz >no_branch
                            ;; allocator.spill_error(&mut self.ops)
                            ; add QWORD state => JitState.stack_change, DWORD stack_effect - 1 // we pop a value of before returning
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
                            ; cmp Rq(top), BYTE 0
                            ; jge >no_branch
                            ;; allocator.spill_error(&mut self.ops)
                            ; add QWORD state => JitState.stack_change, DWORD stack_effect - 1 // we pop a value of before returning
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
                            ; add QWORD state => JitState.stack_change, DWORD stack_effect
                            ; mov r8, command_index as i32
                            ; lea r9, [rsp + 0x48]
                            ;; call_extern!(self.ops, ret, offset)
                            ; test retval, retval
                            ; jz >interpret
                            ; lea r9, [->buffer_base]
                            ; add r9, retval
                            ; jmp r9
                            ;interpret:
                            ; mov retval, [rsp + 0x48]
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
                            ;; call_extern!(self.ops, print_char, offset)
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
                            ;; call_extern!(self.ops, print_num, offset)
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
                            ;; call_extern!(self.ops, input_char, offset)
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
            ; mov rdx, -min_stack
            ; mov r8, max_stack
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
                                    ; lea r9, [=>label]
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
