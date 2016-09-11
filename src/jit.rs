use dynasmrt::{self, DynasmApi, DynasmLabelApi, AssemblyOffset, DynamicLabel};
use crossbeam;

use std::mem;
use std::{i32, i64, u8};
use std::collections::HashMap;
use std::cmp::{min, max};
use std::ptr;
use std::io::{self, BufRead, Read, Write};
use std::sync::mpsc;
use std::borrow::Cow;
use std::fs::File;
use std::path::Path;

use interpreter::{Command, Integer};

pub struct JitState<'a> {
    callstack: Vec<usize>,
    heap: HashMap<Integer, Integer>,
    stack: Vec<Integer>,
    stack_change: isize,
    input: Box<BufRead + 'a>,
    output: Box<Write + 'a>,
}

impl<'a> JitState<'a> {
    pub fn new(input: Box<BufRead + 'a>, output: Box<Write + 'a>) -> JitState<'a> {
        JitState {
            callstack: Vec::new(),
            heap: HashMap::new(),
            stack: Vec::new(),
            stack_change: 0,
            input: input,
            output: output
        }
    }

    unsafe extern "win64" fn get(state: *mut JitState, stack: *mut Integer) -> u8 {
        if let Some(&val) = (*state).heap.get(&*stack) {
            *stack = val;
            0
        } else {
            1
        }
    }

    unsafe extern "win64" fn set(state: *mut JitState, stack: *mut Integer) {
        (*state).heap.insert(*stack.offset(-1), *stack);
    }

    unsafe extern "win64" fn input_num(state: *mut JitState, stack: *mut Integer) -> u8 {
        let mut value = String::new();
        if (*state).input.read_line(&mut value).is_err() {
            return 1;
        }
        if let Ok(value) = value.trim().parse() {
            (*state).heap.insert(*stack, value);
            0
        } else {
            1
        }
    }

    unsafe extern "win64" fn print_num(state: *mut JitState, stack: *mut Integer) -> u8 {
        let value = *stack;
        (*state).output.write_all(value.to_string().as_bytes()).is_err() as u8
    }

    unsafe extern "win64" fn input_char(state: *mut JitState, stack: *mut Integer) -> u8 {
        let mut buf = [0u8];
        if (*state).input.read_exact(&mut buf).is_err() {
            return 1;
        }
        (*state).heap.insert(*stack, buf[0] as Integer);
        0
    }

    unsafe extern "win64" fn print_char(state: *mut JitState, stack: *mut Integer) -> u8 {
        let value = *stack;
        if value > u8::MAX as isize {
            return 1;
        }
        (*state).output.write_all(&[value as u8]).is_err() as u8
    }

    unsafe extern "win64" fn call(state: *mut JitState, _stack: *mut Integer, index: usize) {
        (*state).callstack.push(index)
    }

    unsafe extern "win64" fn ret(state: *mut JitState, _stack: *mut Integer, index: usize) -> usize {
        (*state).callstack.pop().unwrap_or(index)
    }

    unsafe extern "win64" fn get_stack(state: *mut JitState, min_stack: u64, max_stack: u64, stack_start: *mut *mut Integer) -> *mut Integer {
        let state = &mut *state;
        // fix the length of the stack and zero stack_change (only relevant when chained into as otherwise stack_change will be 0)
        let len = (state.stack.len() as isize + state.stack_change) as usize;
        state.stack.set_len(len);
        state.stack_change = 0;

        // ensure that the stack is at least min_stack items large
        if len < (min_stack as usize) {
            return ptr::null_mut();
        }
        // ensure we will be able to push max_stack items
        state.stack.reserve(max_stack as usize);

        // return stack_top by value and stack_start by reference
        let start = state.stack.as_mut_ptr();
        *stack_start = start;
        start.offset(len as isize)
    }
}

struct JitCompiler<'a> {
    commands: &'a [Command],
    blocks: HashMap<usize, JitBlock>,
    fixups: HashMap<usize, Vec<(AssemblyOffset, AssemblyOffset)>>,
    fixup_queue: Vec<(usize, DynamicLabel)>,
    ops: dynasmrt::Assembler
}

#[derive(Debug, Clone, Copy)]
struct JitBlock {
    start:   AssemblyOffset,
    chained: DynamicLabel
}

impl<'a> JitCompiler<'a> {
    pub fn new(commands: &'a [Command]) -> JitCompiler<'a> {
        JitCompiler {
            commands: commands,
            blocks: HashMap::new(),
            fixups: HashMap::new(),
            fixup_queue: Vec::new(),
            ops: dynasmrt::Assembler::new()
        }
    }

    /// Compiles an extended basic block starting at command_index
    fn compile(&mut self, start_index: usize) -> Result<JitBlock, String> {
        use interpreter::Command::*;

        // utility defs:
        dynasm!(ops
            ; .alias state, rcx
            ; .alias stack, rdx // initialized after a call to get_stack
            ; .alias stack_start, r8 // not restored after a call since only copy needs it.
            ; .alias retval, rax
            ; .alias temp0, rax
            ; .alias temp1, r9
            ; .alias temp2, r10
            ; .alias temp3, r11
        );

        macro_rules! epilogue {
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
            ($ops:expr, $addr:expr, $offset:expr) => {dynasm!($ops
                ; lea stack, [stack + $offset]
                ; mov temp0, QWORD $addr as _
                ; call temp0
                ; mov state, [rsp + 0x30]
                ; mov stack, [rsp + 0x38]
            )}
        }

        // stack effect calculation accumulators.
        // stack_effect will always be the change in stack BEFORE the op while the op is matched,
        // but min/max_stack will take this op into account if it exits there.
        let mut stack_effect: i32 = 0;
        let mut min_stack   : i32 = 0;
        let mut max_stack   : i32 = 0;

        // to prevent large amounts of memory traffic to the top of the stack, we try to keep the topmost live value
        // in the temp0 register. 
        /* let mut top_in_reg = false;

        macro_rules! spill_top_reg {
            ($offset:expr) => {if top_in_reg {
                dynasm!(ops
                    ; mov stack => Integer[$offset], temp0
                );
            }};
        }

        macro_rules! get_top_reg {
            ($offset:expr) => {if !top_in_reg {
                dynasm!(ops
                    ; mov temp0, stack => Integer[$offset]
                );
            }};
        }*/

        //  function prologue. when called we start here, if we jump from another jit block we start at chained
        let block = JitBlock {
            start: self.ops.offset(),
            chained: self.ops.new_dynamic_label()
        };
        dynasm!(self.ops
            ; sub rsp, BYTE 0x28
            ; mov [rsp + 0x30], state // rcx
            ;=>block.chained
        );

        self.blocks.insert(start_index, block);

        // get the stack handle, bail out if we don't (this indicates that a stack error would occur)
        let stack_fixes = self.ops.offset();
        dynasm!(self.ops
            // prep args for get stack (rcx is already set to state). min_stack and max_stack are later fixed up
            ; mov rdx, 0
            ; mov r8, 0
            ; lea r9, [rsp + 0x40] // this is where stack_start will be stored
            ; mov temp0, QWORD JitState::get_stack as _
            ; call temp0
            ; test retval, retval
            ; jnz >badstack
        );
        epilogue!(self.ops, 0, start_index);

        dynasm!(self.ops
            ;badstack:
            // restore state and put the stack ptr we got in memory
            ; mov stack, retval
            ; mov state, [rsp + 0x30]
            ; mov [rsp + 0x38], stack
            // we're done now. state, stack and stack_start are in memory, state and stack are in rcx and rdx
        );

        let mut commands = self.commands[start_index..].iter();
        let mut command_index = start_index;
        loop {
            if let Some(c) = commands.next() {
                // offset to the topmost item of the stack at the start of a command
                let offset: i32 = stack_effect - 1;

                let (stack_change, stack_extra) = match *c {
                    Push {value} => if value > i32::MAX as Integer || value < i32::MIN as Integer {
                        dynasm!(self.ops
                            ; mov temp0, QWORD value as i64
                            ; mov stack => Integer[offset + 1], temp0
                        );
                        (1i32, 1)
                    } else {
                        let value = value as i32;
                        // Optimizations for operations commonly preceded by a Push. tends to shave
                        // away at least 2 instructions that hit memory
                        let c2 = &commands.as_slice()[0];
                        match *c2 {
                            Duplicate => {
                                dynasm!(self.ops
                                    ; mov QWORD stack => Integer[offset + 1], DWORD value
                                    ; mov QWORD stack => Integer[offset + 2], DWORD value
                                );
                                commands.next();
                                command_index += 1;
                                (2, 2)
                            },
                            Swap => {
                                dynasm!(self.ops
                                    ; mov temp0, stack => Integer[offset]
                                    ; mov stack => Integer[offset + 1], temp0
                                    ; mov QWORD stack => Integer[offset], DWORD value
                                );
                                commands.next();
                                command_index += 1;
                                (1, 2)
                            },
                            Add => {
                                dynasm!(self.ops
                                    ; mov temp0, stack => Integer[offset]
                                    ; add temp0, value
                                    ; jno >overflow
                                );
                                epilogue!(self.ops, stack_effect, command_index);
                                dynasm!(self.ops
                                    ;overflow:
                                    ; mov stack => Integer[offset], temp0 
                                );
                                commands.next();
                                command_index += 1;
                                (0, 1)
                            },
                            Subtract => {
                                dynasm!(self.ops
                                    ; mov temp0, stack => Integer[offset]
                                    ; sub temp0, value
                                    ; jno >overflow
                                );
                                epilogue!(self.ops, stack_effect, command_index);
                                dynasm!(self.ops
                                    ;overflow:
                                    ; mov stack => Integer[offset], temp0 
                                );
                                commands.next();
                                command_index += 1;
                                (0, 1)
                            },
                            Multiply => {
                                dynasm!(self.ops
                                    ; imul temp0, stack => Integer[offset], value
                                    ; jno >overflow
                                );
                                epilogue!(self.ops, stack_effect, command_index);
                                dynasm!(self.ops
                                    ;overflow:
                                    ; mov QWORD stack => Integer[offset], value
                                );
                                commands.next();
                                command_index += 1;
                                (0, 1)
                            },
                            _ => {
                                dynasm!(self.ops
                                    ; mov QWORD stack => Integer[offset + 1], DWORD value
                                );
                                (1, 1)
                            }
                        }
                    },
                    Duplicate => {
                        dynasm!(self.ops
                            ; mov temp0, stack => Integer[offset]
                            ; mov stack => Integer[offset + 1], temp0
                        );
                        (1, 2)
                    },
                    Swap => {
                        dynasm!(self.ops
                            ; mov temp0, stack => Integer[offset]
                            ; mov temp1, stack => Integer[offset - 1]
                            ; mov stack => Integer[offset - 1], temp0
                            ; mov stack => Integer[offset],     temp1
                        );
                        (0, 2)
                    },
                    Copy {index} => {
                        dynasm!(self.ops
                            ; mov stack_start, [rsp + 0x40]
                            ; mov temp0, stack_start => Integer[index as i32]
                            ; mov stack => Integer[offset + 1], temp0
                        );
                        // adjust min_stack if necessary
                        let stack_depth_needed = stack_effect - index as i32;
                        min_stack = min(stack_depth_needed, min_stack);
                        (1, 1)
                    },
                    Discard => (-1, 0),
                    Slide {amount} => {
                        dynasm!(self.ops
                            ; mov temp0, stack => Integer[offset]
                            ; mov stack => Integer[offset - amount as i32], temp0
                        );
                        (-(amount as i32), 1)
                    },
                    Add => {
                        dynasm!(self.ops
                            ; mov temp0, stack => Integer[offset]
                            ; add temp0, stack => Integer[offset - 1]
                            ; jno >overflow
                        );
                        epilogue!(self.ops, stack_effect, command_index);
                        dynasm!(self.ops
                            ;overflow:
                            ; mov stack => Integer[offset - 1], temp0
                        );
                        (-1, 1)
                    },
                    Subtract => {
                        dynasm!(self.ops
                            ; mov temp0, stack => Integer[offset]
                            ; add temp0, stack => Integer[offset - 1]
                            ; jno >overflow
                        );
                        epilogue!(self.ops, stack_effect, command_index);
                        dynasm!(self.ops
                            ;overflow:
                            ; mov stack => Integer[offset - 1], temp0
                        );
                        (-1, 1)
                    },
                    Multiply => {
                        dynasm!(self.ops
                            ; mov temp0, stack => Integer[offset]
                            ; imul temp0, stack => Integer[offset - 1]
                            ; jno >overflow
                        );
                        epilogue!(self.ops, stack_effect, command_index);
                        dynasm!(self.ops
                            ;overflow:
                            ; mov stack => Integer[offset - 1], temp0
                        );
                        (-1, 1)
                    },
                    Divide => {
                        dynasm!(self.ops
                            ; cmp QWORD stack => Integer[offset - 1], BYTE 0
                            ; jnz >div0
                        );
                        epilogue!(self.ops, stack_effect, command_index);
                        dynasm!(self.ops
                            ;div0:
                            ; mov rax, stack => Integer[offset - 1]
                            ; mov temp1, stack
                            ; cqo
                            ; idiv QWORD temp1 => Integer[offset]
                            ; mov temp1 => Integer[offset - 1], rax
                            ; mov stack, temp1
                        );
                        (-1, 1)
                    },
                    Modulo => {
                        dynasm!(self.ops
                            ; cmp QWORD stack => Integer[offset - 1], BYTE 0
                            ; jnz >div0
                        );
                        epilogue!(self.ops, stack_effect, command_index);
                        dynasm!(self.ops
                            ;div0:
                            ; mov rax, stack => Integer[offset - 1]
                            ; mov temp1, stack
                            ; cqo
                            ; idiv QWORD temp1 => Integer[offset]
                            ; mov temp1 => Integer[offset - 1], rdx
                            ; mov stack, temp1
                        );
                        (-1, 1)
                    },
                    Set => {
                        call_extern!(self.ops, JitState::set, offset);
                        (-2, 0)
                    },
                    Get => {
                        call_extern!(self.ops, JitState::get, offset);
                        dynasm!(self.ops
                            ; test al, al
                            ; jz >key_not_found
                        );
                        epilogue!(self.ops, stack_effect, command_index);
                        dynasm!(self.ops
                            ;key_not_found:
                        );
                        (0, 1)
                    },
                    // we're done here
                    Label => {
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
                            Self::add_fixup(&mut self.fixups, target, start, self.ops.offset());
                        }
                        break;
                    },
                    Call {index} => {
                        dynasm!(self.ops
                            ; mov r8, command_index as i32 + 1
                        );
                        call_extern!(self.ops, JitState::call, offset);
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
                            Self::add_fixup(&mut self.fixups, index, start, self.ops.offset());
                        }
                        break;
                    },
                    Jump {index} => {
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
                            Self::add_fixup(&mut self.fixups, index, start, self.ops.offset());
                        }
                        break;
                    },
                    JumpIfZero {index} => {
                        dynasm!(self.ops
                            ; cmp QWORD stack => Integer[offset], BYTE 0
                            ; jnz >no_branch
                            ; add QWORD state => JitState.stack_change, DWORD stack_effect - 1 // we pop a value of before returning
                        );
                        if let Some(block) = self.blocks.get(&index) {
                            dynasm!(self.ops
                                ; jmp =>block.chained
                            );
                        } else {
                            let start = self.ops.offset();
                            epilogue!(self.ops, , index);
                            Self::add_fixup(&mut self.fixups, index, start, self.ops.offset());
                        }
                        dynasm!(self.ops
                            ;no_branch:
                        );
                        (-1, 0)
                    },
                    JumpIfNegative {index} => {
                        dynasm!(self.ops
                            ; cmp QWORD stack => Integer[offset], BYTE 0
                            ; jge >no_branch
                            ; add QWORD state => JitState.stack_change, DWORD stack_effect - 1 // we pop a value of before returning
                        );
                        if let Some(block) = self.blocks.get(&index) {
                            dynasm!(self.ops
                                ; jmp =>block.chained
                            );
                        } else {
                            let start = self.ops.offset();
                            epilogue!(self.ops, , index);
                            Self::add_fixup(&mut self.fixups, index, start, self.ops.offset());
                        }
                        dynasm!(self.ops
                            ;no_branch:
                        );
                        (-1, 0)
                    },
                    EndSubroutine => { // we would always eat the virtual dispatch here so we just eat it.
                        dynasm!(self.ops
                            ; mov r8, command_index as i32
                        );
                        call_extern!(self.ops, JitState::ret, offset);
                        epilogue!(self.ops, stack_effect);
                        break;
                    },
                    EndProgram => {
                        epilogue!(self.ops, stack_effect, command_index);
                        break;
                    },
                    PrintChar => {
                        call_extern!(self.ops, JitState::print_char, offset);
                        dynasm!(self.ops
                            ; test al, al
                            ; jz >io_fail
                        );
                        epilogue!(self.ops, stack_effect, command_index);
                        dynasm!(self.ops
                            ;io_fail:
                        );
                        (-1, 0)
                    },
                    PrintNum => {
                        call_extern!(self.ops, JitState::print_num, offset);
                        dynasm!(self.ops
                            ; test al, al
                            ; jz >io_fail
                        );
                        epilogue!(self.ops, stack_effect, command_index);
                        dynasm!(self.ops
                            ;io_fail:
                        );
                        (-1, 0)
                    },
                    InputChar => {
                        call_extern!(self.ops, JitState::input_char, offset);
                        dynasm!(self.ops
                            ; test al, al
                            ; jz >io_fail
                        );
                        epilogue!(self.ops, stack_effect, command_index);
                        dynasm!(self.ops
                            ;io_fail:
                        );
                        (0, 1)
                    },
                    InputNum => {
                        call_extern!(self.ops, JitState::input_num, offset);
                        dynasm!(self.ops
                            ; test al, al
                            ; jz >io_fail
                        );
                        epilogue!(self.ops, stack_effect, command_index);
                        dynasm!(self.ops
                            ;io_fail:
                        );
                        (0, 1)
                    }
                };

                stack_effect += stack_change;
                let stack_bot = stack_effect - stack_extra;

                max_stack = max(max_stack, stack_effect);
                min_stack = min(min_stack, stack_bot);

                command_index += 1;
            } else {
                // we hit program end. uh, okay I guess.
                epilogue!(self.ops, stack_effect, command_index);
                break;
            }
        }

        self.ops.alter_uncommitted(|ops| {
            ops.goto(stack_fixes);
            dynasm!(ops
                ; mov rdx, -min_stack
                ; mov r8, max_stack
            );
        });

        // register fixups for when a commit is made
        self.fixup_queue.push((start_index, block.chained));

        Ok(block)
    }

    pub fn commit(&mut self) {
        self.ops.commit();
        
        if !self.fixup_queue.is_empty() {
            let fixup_queue = &mut self.fixup_queue;
            let fixups = &mut self.fixups;

            self.ops.alter(|ops| {
                for (target, label) in fixup_queue.drain(..) {
                    if let Some(mut fixups) = fixups.remove(&target) {
                        for (start, end) in fixups.drain(..) {
                            ops.goto(start);
                            dynasm!(ops
                                ; jmp =>label
                            );
                            ops.check(end);
                        }
                    }
                }
            });
        }
    }

    fn add_fixup(fixups: &mut HashMap<usize, Vec<(AssemblyOffset, AssemblyOffset)>>, target: usize, start: AssemblyOffset, end: AssemblyOffset) {
        fixups.entry(target).or_insert_with(|| Vec::new()).push((start, end));
    }

    fn compile_index(&mut self, target: usize) -> Option<AssemblyOffset> {
        if !self.blocks.contains_key(&target) {
            let block = self.compile(target).unwrap();
            Some(block.start)
        } else {
            None
        }
    }

    fn executor(&self) -> dynasmrt::Executor {
        self.ops.reader()
    }

    unsafe fn run_block(ptr: *const u8, state: &mut JitState) -> usize {
        let f: unsafe extern "win64" fn(*mut JitState) -> usize = mem::transmute(ptr);
        let cont = f(state as *mut _);
        let len = (state.stack.len() as isize + state.stack_change) as usize;
        state.stack.set_len(len);
        state.stack_change = 0;
        cont
    }
}

pub struct JitInterpreter<'a> {
    state: JitState<'a>,
    commands: &'a [Command],
    compiler: JitCompiler<'a>,
    jit_handles: Vec<Option<AssemblyOffset>>
}

impl<'a> JitInterpreter<'a> {
    pub fn new(commands: &'a [Command], input: Box<BufRead + 'a>, output: Box<Write + 'a>) -> JitInterpreter<'a> {
        JitInterpreter {
            state: JitState::new(input, output),
            commands: commands,
            compiler: JitCompiler::new(commands),
            jit_handles: vec![None; commands.len()]
        }
    }

    pub fn precompile(&mut self) {
        use interpreter::Command::*;
        if !self.commands.is_empty() {
            self.jit_handles[0] = self.compiler.compile_index(0);

            for (i, c) in self.commands.iter().enumerate() {
                let i = match *c {
                    Label | Call {..} if i + 1 != self.commands.len() => i + 1,
                    _ => continue
                };
                if let Some(start) = self.compiler.compile_index(i) {
                    self.jit_handles[i] = Some(start);
                }
            }
        }
        self.compiler.commit();
    }

    pub fn interpret(&mut self) -> Result<(), Cow<'static, str>> {
        let mut command_index = 0;
        let mut retval = Some("Hit end of program".into());

        while let Some(_) = self.commands.get(command_index) {
            match Self::interpret_block(&mut self.state, &self.commands, command_index) {
                Ok(new_index) => command_index = new_index,
                Err(msg) => {
                    retval = msg;
                    break;
                }
            }
        }

        if let Some(err) = retval {
            Err(err)
        } else {
            Ok(())
        }
    }

    pub fn simple_jit(&mut self) -> Result<(), Cow<'static, str>> {
        let mut command_index = 0;
        let mut retval = Some("Hit end of program".into());
        let executor = self.compiler.executor();
        let lock = executor.lock();

        while let Some(&offset) = self.jit_handles.get(command_index) {
            // can we jit?
            if let Some(offset) = offset {
                let new_index = unsafe {
                    JitCompiler::run_block(lock.ptr(offset), &mut self.state)
                };
                // not a bail-out
                if new_index != command_index {
                    command_index = new_index;
                    continue;
                }
            }

            // fallback interpreting
            match Self::interpret_block(&mut self.state, &self.commands, command_index) {
                Ok(new_index) => command_index = new_index,
                Err(msg) => {
                    retval = msg;
                    break;
                }
            }
        }

        if let Some(err) = retval {
            Err(err)
        } else {
            Ok(())
        }
    }

    pub fn synchronous_jit(&mut self) -> Result<(), Cow<'static, str>> {
        let executor = self.compiler.executor();
        let mut lock = executor.lock();

        let mut retval = Some("Hit end of program".into());

        let mut command_index = 0;
        while let Some(&offset) = self.jit_handles.get(command_index) {

            // can we jit?
            if let Some(offset) = offset {
                let new_index = unsafe {
                    let lock = executor.lock();
                    JitCompiler::run_block(lock.ptr(offset), &mut self.state)
                };
                // not a bail-out
                if new_index != command_index {
                    command_index = new_index;
                    continue;
                }
            } else if command_index != 0 {

                if let Some(start) = self.compiler.compile_index(command_index) {
                    drop(lock);
                    self.compiler.commit();
                    lock = executor.lock();

                    self.jit_handles[command_index] = Some(start);
                    let new_index = unsafe {
                        JitCompiler::run_block(lock.ptr(start), &mut self.state)
                    };
                    // not a bail-out
                    if new_index != command_index {
                        command_index = new_index;
                        continue;
                    }
                }
            }

            // fallback interpreting
            match Self::interpret_block(&mut self.state, &self.commands, command_index) {
                Ok(new_index) => command_index = new_index,
                Err(msg) => {
                    retval = msg;
                    break;
                }
            }
        }

        if let Some(err) = retval {
            Err(err)
        } else {
            Ok(())
        }
    }

    pub fn threaded_jit(&mut self) -> Result<(), Cow<'static, str>> {
        use interpreter::Command::*;
        let executor = self.compiler.executor();

        let (jit_finished_send, jit_finished_receive) = mpsc::channel();

        // this thread compiles our code in the background.
        let compiler = &mut self.compiler;
        let compile_thread = crossbeam::scope(|scope| scope.spawn(move || {
            for (i, c) in compiler.commands.iter().enumerate() {
                let i = match *c {
                    Label | Call {..} if i + 1 != compiler.commands.len() => i + 1,
                    _ => continue
                };
                if let Some(start) = compiler.compile_index(i) {
                    compiler.commit();
                    if jit_finished_send.send((i, start)).is_err() {
                        break;
                    }
                }
            }
        }));

        let mut retval = Some("Hit end of program".into());

        let mut command_index = 0;
        while let Some(&offset) = self.jit_handles.get(command_index) {
            // can we jit?
            if let Some(offset) = offset {
                let new_index = unsafe {
                    let lock = executor.lock();
                    JitCompiler::run_block(lock.ptr(offset), &mut self.state)
                };
                // not a bail-out
                if new_index != command_index {
                    command_index = new_index;
                    continue;
                }
            }

            // hot loop optimization: only check for new chunks when we fall back to interpreting.
            while let Ok((index, offset)) = jit_finished_receive.try_recv() {
                self.jit_handles[index] = Some(offset);
            }

            // fallback interpreting
            match Self::interpret_block(&mut self.state, &self.commands, command_index) {
                Ok(new_index) => command_index = new_index,
                Err(msg) => {
                    retval = msg;
                    break;
                }
            }
        }

        // drop the channel so the compilation thread will terminate soon
        drop(jit_finished_receive);

        drop(compile_thread);

        if let Some(err) = retval {
            Err(err)
        } else {
            Ok(())
        }
    }

    fn interpret_block(state: &mut JitState, commands: &[Command], mut command_index: usize) -> Result<usize, Option<Cow<'static, str>>> {
        use interpreter::Command::*;
        // interpret until we hit something that can cause a flow control convergence (jump, call, ret)
        while let Some(c) = commands.get(command_index) {
            let stack = &mut state.stack;
            let len = stack.len();

            match *c {
                Push {value} => stack.push(value),
                Duplicate => if let Some(&value) = stack.last() {
                    stack.push(value);
                } else {
                    return Err(Some("Tried to duplicate but stack is empty".into()));
                },
                Copy {index} => if let Some(&value) = stack.get(index) {
                    stack.push(value);
                } else {
                    return Err(Some("Tried to copy from outside the stack".into()));
                },
                Swap => if len > 1 {
                    stack.swap(len - 1, len - 2);
                } else {
                    return Err(Some("Cannot swap with empty stack".into()));
                },
                Discard => if let None = stack.pop() {
                    return Err(Some("Cannot pop from empty stack".into()));
                },
                Slide {amount} => if len > amount {
                    let top = stack[len - 1];
                    stack.truncate(len - amount);
                    stack[len - amount - 1] = top;
                } else {
                    return Err(Some("Cannot discard more elements than items exist on stack".into()));
                },
                Add => if len > 1 {
                    if let Some(val) = stack[len - 2].checked_add(stack[len - 1]) {
                        stack[len - 2] = val;
                    } else {
                        return Err(Some("Overflow during addition".into()));
                    }
                    stack.pop();
                } else {
                    return Err(Some("Not enough items on stack to add".into()));
                },
                Subtract => if len > 1 {
                    if let Some(val) = stack[len - 2].checked_sub(stack[len - 1]) {
                        stack[len - 2] = val;
                    } else {
                        return Err(Some("Overflow during addition".into()));
                    }
                    stack.pop();
                } else {
                    return Err(Some("Not enough items on stack to subtract".into()));
                },
                Multiply => if len > 1 {
                    if let Some(val) = stack[len - 2].checked_mul(stack[len - 1]) {
                        stack[len - 2] = val;
                    } else {
                        return Err(Some("Overflow during addition".into()));
                    }
                    stack.pop();
                } else {
                    return Err(Some("Not enough items on stack to multiply".into()));
                },
                Divide => if len > 1 {
                    if let Some(val) = stack[len - 2].checked_div(stack[len - 1]) {
                        stack[len - 2] = val;
                    } else {
                        return Err(Some("Divide by zero".into()));
                    }
                    stack.pop();
                } else {
                    return Err(Some("Not enough items on stack to divide".into()));
                },
                Modulo => if len > 1 {
                    if let Some(val) = stack[len - 2].checked_rem(stack[len - 1]) {
                        stack[len - 2] = val;
                    } else {
                        return Err(Some("Divide by zero".into()));
                    }
                    stack.pop();
                } else {
                    return Err(Some("Not enough items on stack to modulo".into()));
                },
                Set => if len > 1 {
                    state.heap.insert(stack[len - 2], stack[len - 1]);
                    stack.pop();
                    stack.pop();
                } else {
                    return Err(Some("Not enough items on stack to set value".into()));
                },
                Get => if let Some(last) = stack.last_mut() {
                    if let Some(&value) = state.heap.get(last) {
                        *last = value;
                    } else {
                        return Err(Some("Key does not exist on the heap".into()));
                    }
                } else {
                    return Err(Some("not enough items on stack to get value".into()));
                },
                Label => {
                    command_index += 1;
                    break;
                },
                Call {index} => {
                    state.callstack.push(command_index + 1);
                    command_index = index;
                    break;
                },
                Jump {index} => {
                    command_index = index;
                    break;
                },
                JumpIfZero {index} => match stack.pop() {
                    Some(0) => {
                        command_index = index;
                        break;
                    },
                    None => {
                        return Err(Some("Not enough items on stack to test if zero".into()));
                    },
                    _ => ()
                },
                JumpIfNegative {index} => match stack.pop() {
                    Some(x) if x < 0 => {
                        command_index = index;
                        break;
                    },
                    None => {
                        return Err(Some("Not enough items on stack to test if negative".into()));
                    }
                    _ => (),
                },
                EndSubroutine => if let Some(index) = state.callstack.pop() {
                    command_index = index;
                    break;
                } else {
                    return Err(Some("Not enough items on callstack to return".into()));
                },
                EndProgram => return Err(None),
                PrintChar => if let Some(c) = stack.pop() {
                    let c: [u8; 1] = [c as u8];
                    state.output.write_all(&c).expect("Could not write to output");
                } else {
                    return Err(Some("Not enough items on stack to print".into()));
                },
                PrintNum               => if let Some(c) = stack.pop() {
                    state.output.write_all(&c.to_string().as_bytes()).expect("Could not write to output");
                } else {
                    return Err(Some("Not enough items on stack to print".into()));
                },
                InputChar              => if len > 0 {
                    state.output.flush().expect("Could not flush output");
                    let mut s = [0; 1];
                    state.input.read_exact(&mut s).expect("Could not read a character from input");
                    state.heap.insert(stack[len - 1], s[0] as Integer);
                } else {
                    return Err(Some("Not enough items on stack to input character".into()));
                },
                InputNum               => if len > 0 { // does not match JitState impl. fix plz
                    state.output.flush().expect("Could not flush output");
                    let mut s = String::new();
                    state.input.read_line(&mut s).expect("Could not read a line from input");
                    state.heap.insert(stack[len - 1], s.trim().parse().expect("Expected a number to be entered"));
                } else {
                    return Err(Some("Not enough items on stack to input number".into()));
                }
            };
            command_index += 1;
        }
        Ok(command_index)
    }

    pub fn dump<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        let mut f = try!(File::create(path));
        let executor = self.compiler.executor();
        let buf = executor.lock();
        f.write_all(&buf)
    }
}
