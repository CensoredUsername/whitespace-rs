use dynasmrt::{self, DynasmApi, DynasmLabelApi, AssemblyOffset, DynamicLabel};
use crossbeam;
use fnv::FnvHasher;

use std::mem;
use std::{i32, i64, u8};
use std::collections::HashMap;
use std::cmp::{min, max};
use std::ptr;
use std::io::{self, BufRead, Write};
use std::sync::mpsc;
use std::borrow::Cow;
use std::fs::File;
use std::path::Path;
use std::hash::BuildHasherDefault;

use program::{Program, Command, Integer};

// The used register allocation. This is here as allocator needs it
dynasm!(ops
    ; .alias state, rcx
    ; .alias stack, rdx // initialized after a call to get_stack
    ; .alias retval, rax
    ; .alias temp0, rax // rax is used as a general temp reg
    // r8, r9, r10 and r11 are used as temp regs
);

mod allocator;
use self::allocator::RegAllocator;

pub struct JitInterpreter<'a> {
    state: JitState<'a>,
    program: &'a Program<'a>,
    compiler: JitCompiler<'a>,
    jit_handles: Vec<Option<AssemblyOffset>>
}

impl<'a> JitInterpreter<'a> {
    pub fn new(program: &'a Program, input: &'a mut (BufRead + 'a), output: &'a mut (Write + 'a)) -> JitInterpreter<'a> {
        JitInterpreter {
            state: JitState::new(input, output),
            program: program,
            compiler: JitCompiler::new(program),
            jit_handles: vec![None; program.commands.len()]
        }
    }

    pub fn precompile(&mut self) {
        use program::Command::*;
        if !self.program.commands.is_empty() {

            for (i, c) in self.program.commands.iter().enumerate() {
                let i = match *c {
                    Label | Call {..} if i + 1 != self.program.commands.len() => i + 1,
                    _ => continue
                };
                if let Some(start) = self.compiler.compile_index(i) {
                    self.jit_handles[i] = Some(start);
                }
            }
        }
        self.compiler.commit();
    }

    pub fn interpret(&mut self) -> Result<(), (usize, Cow<'static, str>)> {
        let mut command_index = 0;
        let mut retval = "Hit end of program".into();

        while let Some(_) = self.program.commands.get(command_index) {
            if let Err(msg) = self.state.interpret_block(&self.program.commands, &mut command_index) {
                retval = msg;
                break;
            }
        }

        if retval == "" {
            Ok(())
        } else {
            Err((command_index, retval))
        }
    }

    pub fn simple_jit(&mut self) -> Result<(), (usize, Cow<'static, str>)> {
        let mut command_index = 0;
        let mut retval = "Hit end of program".into();
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
            if let Err(msg) = self.state.interpret_block(&self.program.commands, &mut command_index) {
                retval = msg;
                break;
            }
        }

        if retval == "" {
            Ok(())
        } else {
            Err((command_index, retval))
        }
    }

    pub fn synchronous_jit(&mut self) -> Result<(), (usize, Cow<'static, str>)> {
        let executor = self.compiler.executor();
        let mut lock = executor.lock();

        let mut retval = "Hit end of program".into();

        let mut command_index = 0;
        // avoid compiling the first part
        match self.state.interpret_block(&self.program.commands, &mut command_index) {
            Ok(())         => (),
            Err(msg)       => return if msg == "" {
                Ok(())
            } else {
                Err((command_index, retval))
            }
        }

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
            } else if let Some(start) = self.compiler.compile_index(command_index) {
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

            // fallback interpreting
            if let Err(msg) = self.state.interpret_block(&self.program.commands, &mut command_index) {
                retval = msg;
                break;
            }
        }

        if retval == "" {
            Ok(())
        } else {
            Err((command_index, retval))
        }
    }

    pub fn threaded_jit(&mut self) -> Result<(), (usize, Cow<'static, str>)> {
        use program::Command::*;
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

        let mut retval = "Hit end of program".into();

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
            if let Err(msg) = self.state.interpret_block(&self.program.commands, &mut command_index) {
                retval = msg;
                break;
            }
        }

        // drop the channel so the compilation thread will terminate soon
        drop(jit_finished_receive);

        drop(compile_thread);

        if retval == "" {
            Ok(())
        } else {
            Err((command_index, retval))
        }
    }

    pub fn dump<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        let mut f = try!(File::create(path));
        let executor = self.compiler.executor();
        let buf = executor.lock();
        f.write_all(&buf)
    }
}

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
    ($ops:expr, $addr:expr, $offset:expr) => {dynasm!($ops
        ; lea stack, stack => Integer[$offset]
        ; mov temp0, QWORD $addr as _
        ; call temp0
        ; mov state, [rsp + 0x30]
        ; mov stack, [rsp + 0x38]
    )}
}

struct JitCompiler<'a> {
    commands: &'a [Command],
    blocks: HashMap<usize, JitBlock>,
    fixups: HashMap<usize, Vec<FixUp>>,
    fixup_queue: Vec<(usize, DynamicLabel)>,
    ops: dynasmrt::Assembler
}

enum FixUp {
    Jump(AssemblyOffset, AssemblyOffset),
    Lea(AssemblyOffset, AssemblyOffset)
}

#[derive(Debug, Clone, Copy)]
struct JitBlock {
    start:   AssemblyOffset,
    chained: DynamicLabel
}

impl<'a> JitCompiler<'a> {
    pub fn new(program: &'a Program) -> JitCompiler<'a> {
        JitCompiler {
            commands: &program.commands,
            blocks: HashMap::new(),
            fixups: HashMap::new(),
            fixup_queue: Vec::new(),
            ops: dynasmrt::Assembler::new()
        }
    }

    /// Compiles an extended basic block starting at command_index
    fn compile(&mut self, start_index: usize) -> Result<JitBlock, String> {
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
                            ; mov temp0, QWORD value as i64
                            ; mov Rq(top), temp0
                        );
                        allocator.set_offset(top, offset + 1);
                        (1i32, 1)
                    } else {
                        let value = value as i32;
                        // Optimizations for operations commonly preceded by a Push. tends to shave
                        // away at least 2 instructions that hit memory
                        let c2 = &commands.as_slice()[0];
                        match *c2 {
                            Add => {
                                let mut left = 0;
                                allocator.stage(&mut self.ops).load(&mut left, offset).finish();
                                if value == 1 {
                                    dynasm!(self.ops; inc Rq(left));
                                } else if value == -1 {
                                    dynasm!(self.ops; dec Rq(left));
                                } else {
                                    dynasm!(self.ops; add Rq(left), value);
                                }
                                dynasm!(self.ops
                                    ; jno >overflow
                                    ; sub Rq(left), value
                                    ;; allocator.spill_error(&mut self.ops)
                                    ;; epilogue!(self.ops, stack_effect, command_index)
                                    ;overflow:
                                );
                                allocator.modify(left);
                                commands.next();
                                command_index += 1;
                                (0, 1)
                            },
                            Subtract => {
                                let mut left = 0;
                                allocator.stage(&mut self.ops).load(&mut left, offset).finish();
                                if value == 1 {
                                    dynasm!(self.ops; dec Rq(left));
                                } else if value == -1 {
                                    dynasm!(self.ops; inc Rq(left));
                                } else {
                                    dynasm!(self.ops; sub Rq(left), value);
                                }
                                dynasm!(self.ops
                                    ; jno >overflow
                                    ;; allocator.spill_error(&mut self.ops)
                                    ;; epilogue!(self.ops, stack_effect, command_index)
                                    ;overflow:
                                );
                                allocator.modify(left);
                                commands.next();
                                command_index += 1;
                                (0, 1)
                            },
                            Multiply => {
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
                        allocator.stage(&mut self.ops).load(&mut dest, offset + 1).finish();

                        dynasm!(self.ops
                            ; mov temp0, [rsp + 0x40]
                            ; mov temp0, temp0 => Integer[index as i32]
                            ; mov Rq(dest), temp0
                        );
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
                            ; jno >overflow
                            ; sub Rq(left), Rq(right)
                            ;; allocator.spill_error(&mut self.ops)
                            ;; epilogue!(self.ops, stack_effect, command_index)
                            ;overflow:
                        );
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
                            ; jno >overflow
                            ; add Rq(left), Rq(right)
                            ;; allocator.spill_error(&mut self.ops)
                            ;; epilogue!(self.ops, stack_effect, command_index)
                            ;overflow:
                        );
                        allocator.modify(left);
                        allocator.forget(right);
                        (-1, 1)
                    },
                    Multiply => {
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
                        (-1, 1)
                    },
                    Divide => {
                        let mut left = 0;
                        let mut right = 0;
                        allocator.stage(&mut self.ops).load(&mut left, offset - 1).load(&mut right, offset).finish();
                        dynasm!(self.ops
                            ; cmp Rq(right), BYTE 0
                            ; jnz >div0
                            ;; allocator.spill_error(&mut self.ops)
                            ;; epilogue!(self.ops, stack_effect, command_index)
                            ;div0:
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
                            ; jnz >div0
                            ;; allocator.spill_error(&mut self.ops)
                            ;; epilogue!(self.ops, stack_effect, command_index)
                            ;div0:
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
                        allocator.spill_forget(&mut self.ops);
                        call_extern!(self.ops, JitState::set, offset);
                        (-2, 0)
                    },
                    Get => {
                        allocator.spill_forget(&mut self.ops);
                        call_extern!(self.ops, JitState::get, offset);
                        dynasm!(self.ops
                            ; test al, al
                            ; jz >key_not_found
                            ;; epilogue!(self.ops, stack_effect, command_index)
                            ;key_not_found:
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
                                ; mov r9, 0
                            );
                            Self::add_fixup(&mut self.fixups, command_index + 1, FixUp::Lea(start, self.ops.offset()));
                        }
                        dynasm!(self.ops
                            ; mov r8, command_index as i32 + 1
                            ;; call_extern!(self.ops, JitState::call, offset)
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
                            ;; call_extern!(self.ops, JitState::ret, offset)
                            ; test retval, retval
                            ; jz >interpret
                            ; jmp retval
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
                            ;; call_extern!(self.ops, JitState::print_char, offset)
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
                            ;; call_extern!(self.ops, JitState::print_num, offset)
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
                            ;; call_extern!(self.ops, JitState::input_char, offset)
                            ; test al, al
                            ; jz >io_fail
                            ;; epilogue!(self.ops, stack_effect, command_index)
                            ;io_fail:
                        );
                        (-1, 0)
                    },
                    InputNum => {
                        allocator.spill_forget(&mut self.ops);
                        dynasm!(self.ops
                            ;; call_extern!(self.ops, JitState::input_num, offset)
                            ; test al, al
                            ; jz >io_fail
                            ;; epilogue!(self.ops, stack_effect, command_index)
                            ;io_fail:
                        );
                        (-1, 0)
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
                        for fixup in fixups.drain(..) {
                            match fixup {
                                FixUp::Jump(start, end) => dynasm!(ops
                                    ;; ops.goto(start)
                                    ; jmp =>label
                                    ;; ops.check(end)
                                ),
                                FixUp::Lea(start, end) => dynasm!(ops
                                    ;; ops.goto(start)
                                    ; lea r9, [=>label]
                                    ;; ops.check(end)
                                )
                            }
                        }
                    }
                }
            });
        }
    }

    fn add_fixup(fixups: &mut HashMap<usize, Vec<FixUp>>, target: usize, fixup: FixUp) {
        fixups.entry(target).or_insert_with(|| Vec::new()).push(fixup);
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

pub struct JitState<'a> {
    callstack: Vec<RetLoc>,
    heap: HashMap<Integer, Integer, BuildHasherDefault<FnvHasher>>,
    stack: Vec<Integer>,
    stack_change: isize,
    input: &'a mut (BufRead + 'a),
    output: &'a mut (Write + 'a),
    // This field only exists in order to fool LLVM's optimizer as the optimizer is actually producing
    // worse code by trying to partially inline JitState::reserve into JitState::get_stack. As get_stack
    // is called extremely often in code with flow control, it is vitally important that it is fast 
    // (it is possible to spend almost 30% of the time in get_stack in flow-control heavy code).
    // The problem is that even when reserve is marked as noinline and cold, the compiler will
    // still detect that the end of the hot codepath of get_stack and reserve is similar, and
    // try to deduplicate this code. This results in several extra register spills and no tail-call
    // optimization of reserve in get_stack. However, by putting the address of reserve somewhere
    // where llvm can't optimize its existence out, we force llvm to preserve the semantics of reserve
    // which then causes the code for get_stack to be more optimal. This can result in over 10% faster code!
    #[allow(dead_code)]
    random_field_for_optimiziations_only: 
        unsafe extern "win64" fn (&mut JitState, u64, usize, *mut *mut Integer) -> *mut Integer
}

struct RetLoc(usize, *const u8);

impl<'a> JitState<'a> {
    pub fn new(input: &'a mut (BufRead + 'a), output: &'a mut (Write + 'a)) -> JitState<'a> {
        let fnv = BuildHasherDefault::<FnvHasher>::default();
        JitState {
            callstack: Vec::with_capacity(1024),
            heap: HashMap::with_capacity_and_hasher(1024, fnv),
            stack: Vec::with_capacity(1024),
            stack_change: 0,
            input: input,
            output: output,
            random_field_for_optimiziations_only: Self::reserve
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

    unsafe extern "win64" fn get_unchecked(state: *mut JitState, stack: *mut Integer) {
        *stack = (*state).heap.get(&*stack).cloned().unwrap_or(0);
    }

    unsafe extern "win64" fn set(state: *mut JitState, stack: *mut Integer) {
        (*state).heap.insert(*stack.offset(-1), *stack);
    }

    unsafe extern "win64" fn input_num(state: *mut JitState, stack: *mut Integer) -> u8 {
        if (*state).output.flush().is_err() {
            return 1;
        }

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
        if (*state).output.flush().is_err() {
            return 1;
        }

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

    unsafe extern "win64" fn call(state: *mut JitState, _stack: *mut Integer, index: usize, retptr: *const u8) {
        (*state).callstack.push(RetLoc(index, retptr));
    }

    unsafe extern "win64" fn ret(state: *mut JitState, _stack: *mut Integer, fail_index: usize, ret_index: *mut usize) -> *const u8 {
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
    }

    unsafe extern "win64" fn get_stack(state: *mut JitState, min_stack: usize, max_stack: usize, stack_start: *mut *mut Integer) -> *mut Integer {
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
    }

    #[cold]
    #[inline(never)]
    unsafe extern "win64" fn reserve(state: &mut JitState, _: u64, max_stack: usize, stack_start: *mut *mut Integer) -> *mut Integer {
        state.stack.reserve(max_stack);
        let start = state.stack.as_mut_ptr();
        *stack_start = start;
        start.offset(state.stack.len() as isize)
    }

    fn interpret_block(&mut self, commands: &[Command], command_index: &mut usize) -> Result<(), Cow<'static, str>> {
        use program::Command::*;
        // interpret until we hit something that can cause a flow control convergence (jump, call, ret)
        while let Some(c) = commands.get(*command_index) {
            let stack = &mut self.stack;
            let len = stack.len();

            match *c {
                Push {value} => stack.push(value),
                Duplicate => if let Some(&value) = stack.last() {
                    stack.push(value);
                } else {
                    return Err("Tried to duplicate but stack is empty".into());
                },
                Copy {index} => if let Some(&value) = stack.get(index) {
                    stack.push(value);
                } else {
                    return Err("Tried to copy from outside the stack".into());
                },
                Swap => if len > 1 {
                    stack.swap(len - 1, len - 2);
                } else {
                    return Err("Cannot swap with empty stack".into());
                },
                Discard => if let None = stack.pop() {
                    return Err("Cannot pop from empty stack".into());
                },
                Slide {amount} => if len > amount {
                    let top = stack[len - 1];
                    stack.truncate(len - amount);
                    stack[len - amount - 1] = top;
                } else {
                    return Err("Cannot discard more elements than items exist on stack".into());
                },
                Add => if len > 1 {
                    if let Some(val) = stack[len - 2].checked_add(stack[len - 1]) {
                        stack[len - 2] = val;
                    } else {
                        return Err(format!("Overflow during addition: {} + {}", stack[len - 2], stack[len - 1]).into());
                    }
                    stack.pop();
                } else {
                    return Err("Not enough items on stack to add".into());
                },
                Subtract => if len > 1 {
                    if let Some(val) = stack[len - 2].checked_sub(stack[len - 1]) {
                        stack[len - 2] = val;
                    } else {
                        return Err(format!("Overflow during subtraction: {} - {}", stack[len - 2], stack[len - 1]).into());
                    }
                    stack.pop();
                } else {
                    return Err("Not enough items on stack to subtract".into());
                },
                Multiply => if len > 1 {
                    if let Some(val) = stack[len - 2].checked_mul(stack[len - 1]) {
                        stack[len - 2] = val;
                    } else {
                        return Err(format!("Overflow during multiplication: {} * {}", stack[len - 2], stack[len - 1]).into());
                    }
                    stack.pop();
                } else {
                    return Err("Not enough items on stack to multiply".into());
                },
                Divide => if len > 1 {
                    if let Some(val) = stack[len - 2].checked_div(stack[len - 1]) {
                        stack[len - 2] = val;
                    } else {
                        return Err("Divide by zero".into());
                    }
                    stack.pop();
                } else {
                    return Err("Not enough items on stack to divide".into());
                },
                Modulo => if len > 1 {
                    if let Some(val) = stack[len - 2].checked_rem(stack[len - 1]) {
                        stack[len - 2] = val;
                    } else {
                        return Err("Modulo by zero".into());
                    }
                    stack.pop();
                } else {
                    return Err("Not enough items on stack to modulo".into());
                },
                Set => if len > 1 {
                    self.heap.insert(stack[len - 2], stack[len - 1]);
                    stack.pop();
                    stack.pop();
                } else {
                    return Err("Not enough items on stack to set value".into());
                },
                Get => if let Some(last) = stack.last_mut() {
                    if let Some(&value) = self.heap.get(last) {
                        *last = value;
                    } else {
                        return Err(format!("Key does not exist on the heap: {}", last).into());
                    }
                } else {
                    return Err("not enough items on stack to get value".into());
                },
                Label => {
                    *command_index += 1;
                    break;
                },
                Call {index} => {
                    self.callstack.push(RetLoc(*command_index + 1, ptr::null()));
                    *command_index = index;
                    break;
                },
                Jump {index} => {
                    *command_index = index;
                    break;
                },
                JumpIfZero {index} => match stack.pop() {
                    Some(0) => {
                        *command_index = index;
                        break;
                    },
                    None => {
                        return Err("Not enough items on stack to test if zero".into());
                    },
                    _ => ()
                },
                JumpIfNegative {index} => match stack.pop() {
                    Some(x) if x < 0 => {
                        *command_index = index;
                        break;
                    },
                    None => {
                        return Err("Not enough items on stack to test if negative".into());
                    }
                    _ => (),
                },
                EndSubroutine => if let Some(RetLoc(index, _)) = self.callstack.pop() {
                    *command_index = index;
                    break;
                } else {
                    return Err("Not enough items on callstack to return".into());
                },
                EndProgram => return Err("".into()),
                PrintChar => if let Some(c) = stack.pop() {
                    let c: [u8; 1] = [c as u8];
                    self.output.write_all(&c).expect("Could not write to output");
                } else {
                    return Err("Not enough items on stack to print".into());
                },
                PrintNum               => if let Some(c) = stack.pop() {
                    self.output.write_all(&c.to_string().as_bytes()).expect("Could not write to output");
                } else {
                    return Err("Not enough items on stack to print".into());
                },
                InputChar              => if len > 0 {
                    self.output.flush().expect("Could not flush output");
                    let mut s = [0; 1];
                    self.input.read_exact(&mut s).expect("Could not read a character from input");
                    self.heap.insert(stack.pop().unwrap(), s[0] as Integer);
                } else {
                    return Err("Not enough items on stack to input character".into());
                },
                InputNum               => if len > 0 { // does not match JitState impl. fix plz
                    self.output.flush().expect("Could not flush output");
                    let mut s = String::new();
                    self.input.read_line(&mut s).expect("Could not read a line from input");
                    self.heap.insert(stack.pop().unwrap(), s.trim().parse().expect("Expected a number to be entered"));
                } else {
                    return Err("Not enough items on stack to input number".into());
                }
            };
            *command_index += 1;
        }
        Ok(())
    }
}
