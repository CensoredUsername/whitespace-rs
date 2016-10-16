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
use std::fs::File;
use std::path::Path;
use std::hash::BuildHasherDefault;

use program::{Program, Command, Integer};
#[allow(unused_imports)]
use super::{WsError, Options, IGNORE_OVERFLOW, UNCHECKED_HEAP};


/// This trait represents the API necessary to run
pub trait State<'a> {
    fn new(options: Options,input: &'a mut (BufRead + 'a), output: &'a mut (Write + 'a)) -> Self; 

    /// Return options for the interpreter
    fn options(&self) -> Options;

    /// This method returns the index of the next command that is to be executed
    fn index(&mut self) -> &mut usize;
    /// Returns the stack of this state implementation. The stack is always assumed to be a Vec.
    fn stack(&mut self) -> &mut Vec<Integer>;

    /// Set a key on the heap.
    fn set(&mut self, key: Integer, value: Integer);
    /// Get a key from the heap.
    fn get(&self, key: Integer) -> Option<Integer>;

    /// Push a value onto the callstack
    fn call(&mut self, retloc: usize);
    /// Remove a value from the callstack
    fn ret(&mut self) -> Option<usize>;

    /// access the input stream of the state
    fn input(&mut self) -> &mut BufRead;
    /// access the output stream of the state
    fn output(&mut self) -> &mut Write;

    /// perform input/output functions on the program state
    fn read_char(&mut self) -> Result<Integer, WsError> {
        let mut s = [0; 1];
        try!(self.output().flush()          .map_err(|e| WsError::wrap(e, "Could not flush the output file")));
        try!(self.input().read_exact(&mut s).map_err(|e| WsError::wrap(e, "Could not read from the input file")));
        Ok(s[0] as Integer)
    }
    fn read_num(&mut self) -> Result<Integer, WsError> {
        let mut s = String::new();
        try!(self.output().flush()         .map_err(|e| WsError::wrap(e, "Could not flush the output file")));
        try!(self.input().read_line(&mut s).map_err(|e| WsError::wrap(e, "Could not read a line from the input file")));
        s.trim().parse()                 .map_err(|e| WsError::wrap(e, "Expected a number to be entered"))
    }
    fn write_char(&mut self, c: Integer) -> Result<(), WsError> {
        if c > u8::MAX as Integer {
            return Err(WsError::new("The value is too large to be printed as a character"));
        }
        self.output().write_all(&[c as u8]).map_err(|e| WsError::wrap(e, "Could not write to the output file"))
    }
    fn write_num(&mut self, c: Integer) -> Result<(), WsError> {
        let c = c.to_string();
        self.output().write_all(c.as_bytes())        .map_err(|e| WsError::wrap(e, "Could not write to the output file"))
    }

    fn count_instruction(&mut self) {
    }

    /// Interprets the commands starting at the current command index until
    /// a control flow join is reached. The return value is false if the end
    /// of the program was reached
    fn interpret_block(&mut self, commands: &[Command]) -> Result<bool, WsError> {
        use program::Command::*;
        // interpret until we hit something that can cause a flow control convergence (jump, call, ret)
        while let Some(c) = commands.get(*self.index()) {
            let len = self.stack().len();
            self.count_instruction();

            match *c {
                Push {value} => self.stack().push(value),
                Duplicate => if let Some(&value) = self.stack().last() {
                    self.stack().push(value);
                } else {
                    return Err(WsError::new("Tried to duplicate but stack is empty"));
                },
                Copy {index} => if let Some(&value) = self.stack().get(index) {
                    self.stack().push(value);
                } else {
                    return Err(WsError::new("Tried to copy from outside the stack"));
                },
                Swap => if len > 1 {
                    self.stack().swap(len - 1, len - 2);
                } else {
                    return Err(WsError::new("Cannot swap with empty stack"));
                },
                Discard => if let None = self.stack().pop() {
                    return Err(WsError::new("Cannot pop from empty stack"));
                },
                Slide {amount} => if len > amount {
                    let top = self.stack()[len - 1];
                    self.stack().truncate(len - amount);
                    self.stack()[len - amount - 1] = top;
                } else {
                    return Err(WsError::new("Cannot discard more elements than items exist on stack"));
                },
                Add => if len > 1 {
                    let (val, overflow) = self.stack()[len - 2].overflowing_add(self.stack()[len - 1]);
                    if overflow && !self.options().contains(IGNORE_OVERFLOW) {
                        let stack = self.stack();
                        return Err(WsError::new(format!("Overflow during addition: {} + {}", stack[len - 2], stack[len - 1])));
                    }
                    self.stack()[len - 2] = val;
                    self.stack().pop();
                } else {
                    return Err(WsError::new("Not enough items on stack to add"));
                },
                Subtract => if len > 1 {
                    let (val, overflow) = self.stack()[len - 2].overflowing_sub(self.stack()[len - 1]);
                    if overflow && !self.options().contains(IGNORE_OVERFLOW) {
                        let stack = self.stack();
                        return Err(WsError::new(format!("Overflow during subtraction: {} - {}", stack[len - 2], stack[len - 1])));
                    }
                    self.stack()[len - 2] = val;
                    self.stack().pop();
                } else {
                    return Err(WsError::new("Not enough items on stack to subtract"));
                },
                Multiply => if len > 1 {
                    let (val, overflow) = self.stack()[len - 2].overflowing_mul(self.stack()[len - 1]);
                    if overflow && !self.options().contains(IGNORE_OVERFLOW) {
                        let stack = self.stack();
                        return Err(WsError::new(format!("Overflow during multiplication: {} * {}", stack[len - 2], stack[len - 1])));
                    }
                    self.stack()[len - 2] = val;
                    self.stack().pop();
                } else {
                    return Err(WsError::new("Not enough items on stack to multiply"));
                },
                Divide => if len > 1 {
                    // note: technically isize::MIN / -1 can overflow. Something to keep in mind when extending to arbitrary sized integers
                    if let Some(val) = self.stack()[len - 2].checked_div(self.stack()[len - 1]) {
                        self.stack()[len - 2] = val;
                    } else {
                        return Err(WsError::new("Divide by zero"));
                    }
                    self.stack().pop();
                } else {
                    return Err(WsError::new("Not enough items on stack to divide"));
                },
                Modulo => if len > 1 {
                    if let Some(val) = self.stack()[len - 2].checked_rem(self.stack()[len - 1]) {
                        self.stack()[len - 2] = val;
                    } else {
                        return Err(WsError::new("Modulo by zero"));
                    }
                    self.stack().pop();
                } else {
                    return Err(WsError::new("Not enough items on stack to modulo"));
                },
                Set => if len > 1 {
                    let value = self.stack().pop().unwrap();
                    let key = self.stack().pop().unwrap();
                    self.set(key, value);
                } else {
                    return Err(WsError::new("Not enough items on stack to set value"));
                },
                Get => if let Some(&last) = self.stack().last() {
                    if let Some(value) = self.get(last) {
                        self.stack()[len - 1] = value;
                    } else if self.options().contains(UNCHECKED_HEAP) {
                        self.stack()[len - 1] = 0;
                    } else {
                        return Err(WsError::new(format!("Key does not exist on the heap: {}", last)));
                    }
                } else {
                    return Err(WsError::new("not enough items on stack to get value"));
                },
                Label => {
                    *self.index() += 1;
                    return Ok(true);
                },
                Call {index} => {
                    let retloc = *self.index() + 1;
                    self.call(retloc);
                    *self.index() = index;
                    return Ok(true);
                },
                Jump {index} => {
                    *self.index() = index;
                    return Ok(true);
                },
                JumpIfZero {index} => match self.stack().pop() {
                    Some(0) => {
                        *self.index() = index;
                        return Ok(true);
                    },
                    None => {
                        return Err(WsError::new("Not enough items on stack to test if zero"));
                    },
                    _ => ()
                },
                JumpIfNegative {index} => match self.stack().pop() {
                    Some(x) if x < 0 => {
                        *self.index() = index;
                        return Ok(true);
                    },
                    None => {
                        return Err(WsError::new("Not enough items on stack to test if negative"));
                    }
                    _ => (),
                },
                EndSubroutine => if let Some(index) = self.ret() {
                    *self.index() = index;
                    return Ok(true);
                } else {
                    return Err(WsError::new("Not enough items on callstack to return"));
                },
                EndProgram => return Ok(false),
                PrintChar => if let Some(c) = self.stack().pop() {
                    try!(self.write_char(c));
                } else {
                    return Err(WsError::new("Not enough items on stack to print"));
                },
                PrintNum => if let Some(c) = self.stack().pop() {
                    try!(self.write_num(c));
                } else {
                    return Err(WsError::new("Not enough items on stack to print"));
                },
                InputChar => if len > 0 {
                    let c = try!(self.read_char());
                    let key = self.stack().pop().unwrap();
                    self.set(key, c);
                } else {
                    return Err(WsError::new("Not enough items on stack to input character"));
                },
                InputNum => if len > 0 {
                    let c = try!(self.read_char());
                    let key = self.stack().pop().unwrap();
                    self.set(key, c);
                } else {
                    return Err(WsError::new("Not enough items on stack to input number"));
                }
            };
            *self.index() += 1;
        }
        Err(WsError::new("Invalid program counter"))
    }
}

struct SimpleState<'a> {
    options: Options,
    count: usize,
    index: usize,
    stack: Vec<Integer>,
    heap:  HashMap<Integer, Integer>,
    callstack: Vec<usize>,
    input: &'a mut (BufRead + 'a),
    output: &'a mut (Write + 'a)
}

impl<'a> State<'a> for SimpleState<'a> {
    fn new(options: Options, input: &'a mut (BufRead + 'a), output: &'a mut (Write + 'a)) -> SimpleState<'a> {
        SimpleState {
            options: options,
            count: 0,
            index: 0,
            callstack: Vec::new(),
            heap: HashMap::new(),
            stack: Vec::new(),
            input: input,
            output: output,
        }
    }

    fn options(&self) -> Options {
        self.options
    }

    fn index(&mut self) -> &mut usize {
        &mut self.index
    }

    fn stack(&mut self) -> &mut Vec<Integer> {
        &mut self.stack
    }

    fn set(&mut self, key: Integer, value: Integer) {
        self.heap.insert(key, value);
    }

    fn get(&self, key: Integer) -> Option<Integer> {
        self.heap.get(&key).cloned()
    }

    fn call(&mut self, retloc: usize) {
        self.callstack.push(retloc);
    }
    fn ret(&mut self) -> Option<usize> {
        self.callstack.pop()
    }

    fn input(&mut self) -> &mut BufRead {
        self.input
    }

    fn output(&mut self) -> &mut Write {
        self.output
    }

    fn count_instruction(&mut self) {
        self.count += 1;
    }
}

fn interpret<'a, T: State<'a>>(state: &mut T, program: &Program) -> Result<(), WsError> {
    loop {
        match state.interpret_block(&program.commands) {
            Ok(true) => continue,
            Ok(false) => return Ok(()),
            Err(mut e) => {
                e.set_location(*state.index());
                return Err(e)
            }
        }
    }
}

pub fn simple_interpret<'a>(program: &Program, options: Options, input: &'a mut (BufRead + 'a), output: &'a mut (Write + 'a)) -> Result<(), WsError> {
    let mut state = SimpleState::new(options, input, output);
    try!(interpret(&mut state, program));
    println!("evaluated {} instructions", state.count);
    Ok(())
}

pub fn jit_interpret<'a>(program: &Program, options: Options, input: &'a mut (BufRead + 'a), output: &'a mut (Write + 'a)) -> Result<(), WsError> {
    let mut state = JitState::new(options, input, output);
    try!(interpret(&mut state, program));
    Ok(())
}

pub struct JitInterpreter<'a> {
    pub state: JitState<'a>,
    program: &'a Program<'a>,
    compiler: JitCompiler<'a>,
    jit_handles: Vec<Option<AssemblyOffset>>
}

impl<'a> JitInterpreter<'a> {
    pub fn new(program: &'a Program, options: Options, input: &'a mut (BufRead + 'a), output: &'a mut (Write + 'a)) -> JitInterpreter<'a> {
        JitInterpreter {
            state: JitState::new(options, input, output),
            program: program,
            compiler: JitCompiler::new(program, options),
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

    pub fn simple_jit(&mut self) -> Result<(), WsError> {
        let executor = self.compiler.executor();
        let lock = executor.lock();

        while let Some(&offset) = self.jit_handles.get(*self.state.index()) {
            // can we jit?
            if let Some(offset) = offset {
                let new_index = unsafe {
                    JitCompiler::run_block(lock.ptr(offset), &mut self.state)
                };
                // if we exit on the same instruction as we started we need to try interpreting first 
                // as otherwise we could get stuck in a loop due to stack errors
                if new_index != *self.state.index() {
                    *self.state.index() = new_index;
                    continue;
                }
            }

            // fallback interpreting. If this returns false we're finished
            if ! try!(self.state.interpret_block(&self.program.commands)) {
                return Ok(())
            }
        }

        Err(WsError::new("Invalid program counter"))
    }

    pub fn synchronous_jit(&mut self) -> Result<(), WsError> {
        let executor = self.compiler.executor();
        let mut lock = executor.lock();

        // avoid compiling the first part
        if ! try!(self.state.interpret_block(&self.program.commands)) {
            return Ok(());
        }

        while let Some(&offset) = self.jit_handles.get(*self.state.index()) {

            // can we jit?
            if let Some(offset) = offset {
                let new_index = unsafe {
                    let lock = executor.lock();
                    JitCompiler::run_block(lock.ptr(offset), &mut self.state)
                };
                // not a bail-out
                if new_index != *self.state.index() {
                    *self.state.index() = new_index;
                    continue;
                }
            } else if let Some(start) = self.compiler.compile_index(*self.state.index()) {
                drop(lock);
                self.compiler.commit();
                lock = executor.lock();

                self.jit_handles[*self.state.index()] = Some(start);
                let new_index = unsafe {
                    JitCompiler::run_block(lock.ptr(start), &mut self.state)
                };
                // not a bail-out
                if new_index != *self.state.index() {
                    *self.state.index() = new_index;
                    continue;
                }
            }

            // fallback interpreting. If this returns false we're finished
            if ! try!(self.state.interpret_block(&self.program.commands)) {
                return Ok(())
            }
        }

        Err(WsError::new("Invalid program counter"))
    }

    pub fn threaded_jit(&mut self) -> Result<(), WsError> {
        use program::Command::*;
        let executor = self.compiler.executor();

        let (jit_finished_send, jit_finished_receive) = mpsc::channel();

        // this thread compiles our code in the background.
        let compiler = &mut self.compiler;
        let _compile_thread = crossbeam::scope(|scope| scope.spawn(move || {
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

        while let Some(&offset) = self.jit_handles.get(*self.state.index()) {
            // can we jit?
            if let Some(offset) = offset {
                let new_index = unsafe {
                    let lock = executor.lock();
                    JitCompiler::run_block(lock.ptr(offset), &mut self.state)
                };
                // not a bail-out
                if new_index != *self.state.index() {
                    *self.state.index() = new_index;
                    continue;
                }
            }

            // hot loop optimization: only check for new chunks when we fall back to interpreting.
            while let Ok((index, offset)) = jit_finished_receive.try_recv() {
                self.jit_handles[index] = Some(offset);
            }

            // fallback interpreting. If this returns false we're finished
            match self.state.interpret_block(&self.program.commands) {
                Ok(true) => (),
                Ok(false) => {
                    drop(jit_finished_receive);
                    return Ok(())
                },
                Err(e) => {
                    drop(jit_finished_receive);
                    return Err(e)
                }
            }
        }

        // drop the channel so the compilation thread will terminate soon
        drop(jit_finished_receive);
        Err(WsError::new("Invalid program counter"))
    }

    pub fn dump<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        let mut f = try!(File::create(path));
        let executor = self.compiler.executor();
        let buf = executor.lock();
        f.write_all(&buf)
    }
}

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
    options: Options,
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
    fn new(program: &'a Program, options: Options) -> JitCompiler<'a> {
        JitCompiler {
            options: options,
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
                                if !self.options.contains(IGNORE_OVERFLOW) {
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
                                if !self.options.contains(IGNORE_OVERFLOW) {
                                    dynasm!(self.ops
                                        ; jno >overflow
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
                            Multiply => {
                                if !self.options.contains(IGNORE_OVERFLOW) {
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
                        );

                        if !self.options.contains(IGNORE_OVERFLOW) {
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

                        if !self.options.contains(IGNORE_OVERFLOW) {
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
                        if !self.options.contains(IGNORE_OVERFLOW) {
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
                        // allocator.spill_forget(&mut self.ops);
                        // call_extern!(self.ops, JitState::set, offset);

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
                            ;;call_extern!(self.ops, JitState::cache_evict, offset)
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
                        // allocator.spill_forget(&mut self.ops);
                        // call_extern!(self.ops, JitState::get, offset);
                        // dynasm!(self.ops
                        //     ; test al, al
                        //     ; jz >key_not_found
                        //     ;; epilogue!(self.ops, stack_effect, command_index)
                        //     ;key_not_found:
                        // );

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
                            ;;call_extern!(self.ops, JitState::cache_bypass_get, offset)
                        );
                        if !self.options.contains(UNCHECKED_HEAP) {
                            dynasm!(self.ops
                                ; test al, al
                                ; jz >end
                                ;;epilogue!(self.ops, stack_effect, command_index)
                            );
                        } else {
                            dynasm!(self.ops
                                ; jmp >end
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
    options: Options,
    command_index: usize,
    callstack: Vec<RetLoc>,
    heap: Cache,
    heap_cache: *mut CacheEntry,
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

impl<'a> State<'a> for JitState<'a> {
    fn new(options: Options, input: &'a mut (BufRead + 'a), output: &'a mut (Write + 'a)) -> JitState<'a> {
        let mut heap = Cache::new();
        let ptr  = heap.entries_ptr();
        JitState {
            options: options,
            command_index: 0,
            callstack: Vec::with_capacity(1024),
            heap: heap,
            heap_cache: ptr,
            stack: Vec::with_capacity(1024),
            stack_change: 0,
            input: input,
            output: output,
            random_field_for_optimiziations_only: Self::reserve
        }
    }

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

    fn get(&self, key: Integer) -> Option<Integer> {
        self.heap.get(key)
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
}

impl<'a> JitState<'a> {
    unsafe extern "win64" fn cache_bypass_get(state: *mut JitState, stack: *mut Integer) -> u8 {
        if let Some(value) = (*state).heap.cache_bypass_get(*stack) {
            *stack = value;
            0
        } else if (*state).options().contains(UNCHECKED_HEAP) {
            *stack = 0;
            0
        } else {
            1
        }
    }

    unsafe extern "win64" fn get(state: *mut JitState, stack: *mut Integer) -> u8 {
        if let Some(value) = (*state).heap.get(*stack) {
            *stack = value;
            0
        } else {
            1
        }
    }

    unsafe extern "win64" fn set(state: *mut JitState, stack: *mut Integer) {
        (*state).heap.set(*stack.offset(-1), *stack);
    }

    unsafe extern "win64" fn cache_evict(state: *mut JitState, stack: *mut Integer, entry: *mut CacheEntry, key: Integer) -> *mut CacheEntry {
        (*state).heap.evict_entry(entry, key);
        (*entry).key = key as usize | 1;
        (*entry).value = *stack;
        entry
    }

    unsafe extern "win64" fn input_num(state: *mut JitState, stack: *mut Integer) -> u8 {
        if let Ok(c) = (*state).read_num() {
            (*state).set(*stack, c);
            0
        } else {
            1
        }
    }

    unsafe extern "win64" fn print_num(state: *mut JitState, stack: *mut Integer) -> u8 {
        (*state).write_num(*stack).is_err() as u8
    }

    unsafe extern "win64" fn input_char(state: *mut JitState, stack: *mut Integer) -> u8 {
        if let Ok(c) = (*state).read_char() {
            (*state).set(*stack, c);
            0
        } else {
            1
        }
    }

    unsafe extern "win64" fn print_char(state: *mut JitState, stack: *mut Integer) -> u8 {
        (*state).write_char(*stack).is_err() as u8
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
}

struct Cache {
    entries: Vec<CacheEntry>,
    map: HashMap<usize, Integer, BuildHasherDefault<FnvHasher>>
}

#[derive(Debug, Clone)]
struct CacheEntry {
    key:   usize,
    value: Integer
}
// Currently using 2^16 cache entries for about 1MB of space. 
// idealy, the cache should be sized so it
const CACHE_ENTRIES: usize = 0x10000;
const CACHE_MASK:    usize = CACHE_ENTRIES - 1;

impl Cache {
    fn new() -> Cache {
        use std::mem::size_of;
        assert!(size_of::<CacheEntry>() == 1 << 4);

        let fnv = BuildHasherDefault::<FnvHasher>::default();
        Cache {
            entries: vec![CacheEntry {key: 0, value: 0}; CACHE_ENTRIES],
            map: HashMap::with_capacity_and_hasher(1024, fnv)
        }
    }

    fn entries_ptr(&mut self) -> *mut CacheEntry {
        self.entries.as_mut_ptr()
    }

    fn set(&mut self, key: Integer, value: Integer) {
        let key = key as usize;

        let mut entry = &mut self.entries[key & CACHE_MASK];

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

    unsafe fn evict_entry(&mut self, entry: *const CacheEntry, key: Integer) {
        Self::_evict_entry(&mut self.map, &*entry, key as usize)
    }

    fn cache_bypass_get(&self, key: Integer) -> Option<Integer> {
        self.map.get(&(key as usize)).cloned()
    }

    fn get(&self, key: Integer) -> Option<Integer> {
        let entry = &self.entries[key as usize & CACHE_MASK];
        if entry.key == key as usize | 1 {
            Some(entry.value)
        } else {
            self.cache_bypass_get(key)
        }
    }
}
