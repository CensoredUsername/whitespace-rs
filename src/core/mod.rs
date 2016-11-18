use dynasmrt::{self, DynasmApi, DynasmLabelApi, AssemblyOffset, DynamicLabel};
use crossbeam;
use num_bigint::Sign;
use num_traits::{FromPrimitive, ToPrimitive};
use byteorder::{ReadBytesExt, WriteBytesExt, LittleEndian};
use memmap::{Mmap, Protection};
use bincode::rustc_serialize::{encode_into, decode_from};
use bincode::SizeLimit::Infinite;

use std::{i32, i64, u8};
use std::collections::HashMap;
use std::cmp::{min, max};
use std::io::{Read, BufRead, Write, Seek, SeekFrom, Cursor};
use std::sync::mpsc;
use std::fmt::Display;
use std::str::FromStr;

use program::{Program, Command, Integer, BigInteger};
use super::{WsError, WsErrorKind, Options, IGNORE_OVERFLOW, UNCHECKED_HEAP, NO_FALLBACK};

mod cached_map;
use self::cached_map::{CacheEntry, CACHE_MASK};

mod simple_state;
use self::simple_state::SimpleState;

mod jit_state;
use self::jit_state::JitState;

mod bigint_state;
use self::bigint_state::BigIntState;


pub trait CheckedArith : Sized + Clone + Default + Display + FromStr + ToString + From<i64> {
    fn overflowing_add(&self, rhs: &Self) -> (Self, bool);

    fn overflowing_sub(&self, rhs: &Self) -> (Self, bool);

    fn overflowing_mul(&self, rhs: &Self) -> (Self, bool);

    fn checked_div(&self, rhs: &Self) -> Option<Self>;

    fn checked_rem(&self, rhs: &Self) -> Option<Self>;

    fn is_zero(&self) -> bool;

    fn is_negative(&self) -> bool;

    fn from_u8(from: u8) -> Self;

    fn into_u8(&self) -> Option<u8>;
}

pub trait State<'a> {
    type Var: CheckedArith;
    type HeapIterator: Iterator<Item=(Self::Var, Self::Var)> + 'a;

    /// Return options for the interpreter
    fn options(&self) -> Options;

    /// This method returns the index of the next command that is to be executed
    fn index(&mut self) -> &mut usize;
    /// Returns the stack of this state implementation. The stack is always assumed to be a Vec.
    fn stack(&mut self) -> &mut Vec<Self::Var>;

    /// Set a key on the heap.
    fn set(&mut self, key: Self::Var, value: Self::Var);
    /// Get a key from the heap.
    fn get(&self, key: &Self::Var) -> Option<&Self::Var>;
    /// Iterate through the heap
    fn iter_heap(&'a self) -> Self::HeapIterator;

    /// Push a value onto the callstack
    fn call(&mut self, retloc: usize);
    /// Remove a value from the callstack
    fn ret(&mut self) -> Option<usize>;

    /// access the input stream of the state
    fn input(&mut self) -> &mut BufRead;
    /// access the output stream of the state
    fn output(&mut self) -> &mut Write;

    /// perform input/output functions on the program state
    fn read_char(&mut self) -> Result<Self::Var, WsError> {
        let mut s = [0; 1];
        try!(self.output().flush()          .map_err(|e| WsError::wrap(e, WsErrorKind::IOError, "Could not flush the output file")));
        try!(self.input().read_exact(&mut s).map_err(|e| WsError::wrap(e, WsErrorKind::IOError, "Could not read from the input file")));
        Ok(Self::Var::from_u8(s[0]))
    }
    fn read_num(&mut self) -> Result<String, WsError> {
        let mut s = String::new();
        try!(self.output().flush()         .map_err(|e| WsError::wrap(e, WsErrorKind::IOError, "Could not flush the output file")));
        try!(self.input().read_line(&mut s).map_err(|e| WsError::wrap(e, WsErrorKind::IOError, "Could not read a line from the input file")));
        Ok(s)
    }
    fn write_char(&mut self, c: Self::Var) -> Result<(), WsError> {
        if let Some(val) = c.into_u8() {
            self.output().write_all(&[val]).map_err(|e| WsError::wrap(e, WsErrorKind::IOError, "Could not write to the output file"))
        } else {
            Err(WsError::new(WsErrorKind::RuntimeParseError, "The value is too large to be printed as a character"))
        }
    }
    fn write_num(&mut self, c: Self::Var) -> Result<(), WsError> {
        let c = c.to_string();
        self.output().write_all(c.as_bytes()).map_err(|e| WsError::wrap(e, WsErrorKind::IOError, "Could not write to the output file"))
    }

    fn count_instruction(&mut self) { }

    fn push_large(&mut self, _: &BigInteger) -> Result<(), WsError> {
        Err(WsError::new(WsErrorKind::Overflow, "A large integer was pushed"))
    }

    fn input_num(&mut self) -> Result<(), WsError> {
        let s = try!(self.read_num());
        let s = s.trim();
        let value = try!(s.parse::<Self::Var>().map_err(|_| WsError::new(WsErrorKind::RuntimeParseError, "Expected a number to parse")));
        let key = self.stack().pop().unwrap();
        self.set(key, value);
        Ok(())
    }

    /// Interprets the commands starting at the current command index until
    /// a control flow join is reached. The return value is false if the end
    /// of the program was reached
    fn interpret_block(&mut self, commands: &[Command]) -> Result<bool, WsError> {
        use ::program::Command::*;
        let options = self.options();

        // interpret until we hit something that can cause a flow control convergence (jump, call, ret)
        while let Some(c) = commands.get(*self.index()) {
            let len = self.stack().len();
            self.count_instruction();

            match *c {
                Push {ref value} => self.stack().push(value.clone().into()),
                PushBig {ref value} => try!(self.push_large(value)),
                Duplicate => if let Some(value) = self.stack().last().cloned() {
                    self.stack().push(value);
                } else {
                    return Err(WsError::new(WsErrorKind::StackError, "Tried to duplicate but stack is empty"));
                },
                Copy {index} => if let Some(value) = self.stack().get(index).cloned() {
                    self.stack().push(value);
                } else {
                    return Err(WsError::new(WsErrorKind::StackError, "Tried to copy from outside the stack"));
                },
                Swap => if len > 1 {
                    self.stack().swap(len - 1, len - 2);
                } else {
                    return Err(WsError::new(WsErrorKind::StackError, "Cannot swap with empty stack"));
                },
                Discard => if let None = self.stack().pop() {
                    return Err(WsError::new(WsErrorKind::StackError, "Cannot pop from empty stack"));
                },
                Slide {amount} => if len > amount {
                    let top = self.stack().pop().unwrap();
                    self.stack().truncate(len - amount);
                    self.stack()[len - amount - 1] = top;
                } else {
                    return Err(WsError::new(WsErrorKind::StackError, "Cannot discard more elements than items exist on stack"));
                },
                Add => if len > 1 {
                    let stack = self.stack();
                    let (val, overflow) = stack[len - 2].overflowing_add(&stack[len - 1]);
                    if overflow && !options.contains(IGNORE_OVERFLOW) {
                        return Err(WsError::new(WsErrorKind::Overflow, format!("Overflow during addition: {} + {}", &stack[len - 2], &stack[len - 1])));
                    }
                    stack[len - 2] = val;
                    stack.pop();
                } else {
                    return Err(WsError::new(WsErrorKind::StackError, "Not enough items on stack to add"));
                },
                Subtract => if len > 1 {
                    let stack = self.stack();
                    let (val, overflow) = stack[len - 2].overflowing_sub(&stack[len - 1]);
                    if overflow && !options.contains(IGNORE_OVERFLOW) {
                        return Err(WsError::new(WsErrorKind::Overflow, format!("Overflow during subtraction: {} - {}", &stack[len - 2], &stack[len - 1])));
                    }
                    stack[len - 2] = val;
                    stack.pop();
                } else {
                    return Err(WsError::new(WsErrorKind::StackError, "Not enough items on stack to subtract"));
                },
                Multiply => if len > 1 {
                    let stack = self.stack();
                    let (val, overflow) = stack[len - 2].overflowing_mul(&stack[len - 1]);
                    if overflow && !options.contains(IGNORE_OVERFLOW) {
                        return Err(WsError::new(WsErrorKind::Overflow, format!("Overflow during multiplication: {} * {}", &stack[len - 2], &stack[len - 1])));
                    }
                    stack[len - 2] = val;
                    stack.pop();
                } else {
                    return Err(WsError::new(WsErrorKind::StackError, "Not enough items on stack to multiply"));
                },
                Divide => if len > 1 {
                    let mut stack = self.stack();
                    if let Some(val) = stack[len - 2].checked_div(&stack[len - 1]) {
                        stack[len - 2] = val;
                    } else if stack[len - 1].is_zero() {
                        return Err(WsError::new(WsErrorKind::DivisionError, "Divide by zero"));
                    } else {
                        return Err(WsError::new(WsErrorKind::Overflow, format!("Overflow during division: {} / {}", stack[len - 2], stack[len - 1])));
                    }
                    stack.pop();
                } else {
                    return Err(WsError::new(WsErrorKind::StackError, "Not enough items on stack to divide"));
                },
                Modulo => if len > 1 {
                    let mut stack = self.stack();
                    if let Some(val) = stack[len - 2].checked_rem(&stack[len - 1]) {
                        stack[len - 2] = val;
                    } else if stack[len - 1].is_zero() {
                        return Err(WsError::new(WsErrorKind::DivisionError, "Modulo by zero"));
                    } else {
                        return Err(WsError::new(WsErrorKind::Overflow, format!("Overflow during modulo: {} % {}", stack[len - 2], stack[len - 1])));
                    }
                    stack.pop();
                } else {
                    return Err(WsError::new(WsErrorKind::StackError, "Not enough items on stack to modulo"));
                },
                Set => if len > 1 {
                    let value = self.stack().pop().unwrap();
                    let key = self.stack().pop().unwrap();
                    self.set(key, value);
                } else {
                    return Err(WsError::new(WsErrorKind::StackError, "Not enough items on stack to set value"));
                },
                Get => if let Some(last) = self.stack().pop() {
                    if let Some(value) = self.get(&last).cloned() {
                        self.stack().push(value);
                    } else if options.contains(UNCHECKED_HEAP) {
                        self.stack().push(Default::default());
                    } else {
                        let err = Err(WsError::new(WsErrorKind::KeyError, format!("Key does not exist on the heap: {}", &last)));
                        self.stack().push(last);
                        return err;
                    }
                } else {
                    return Err(WsError::new(WsErrorKind::StackError, "not enough items on stack to get value"));
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
                JumpIfZero {index} => if let Some(x) = self.stack().pop() {
                    if x.is_zero() {
                        *self.index() = index;
                        return Ok(true);
                    }
                } else {
                    return Err(WsError::new(WsErrorKind::StackError, "Not enough items on stack to test if zero"));
                },
                JumpIfNegative {index} => if let Some(x) = self.stack().pop() {
                    if x.is_negative() {
                        *self.index() = index;
                        return Ok(true);
                    }
                } else {
                    return Err(WsError::new(WsErrorKind::StackError, "Not enough items on stack to test if negative"));
                },
                EndSubroutine => if let Some(index) = self.ret() {
                    *self.index() = index;
                    return Ok(true);
                } else {
                    return Err(WsError::new(WsErrorKind::CallStackError, "Not enough items on callstack to return"));
                },
                EndProgram => return Ok(false),
                PrintChar => if let Some(c) = self.stack().pop() {
                    try!(self.write_char(c));
                } else {
                    return Err(WsError::new(WsErrorKind::StackError, "Not enough items on stack to print"));
                },
                PrintNum => if let Some(c) = self.stack().pop() {
                    try!(self.write_num(c));
                } else {
                    return Err(WsError::new(WsErrorKind::StackError, "Not enough items on stack to print"));
                },
                InputChar => if len > 0 {
                    let c = try!(self.read_char());
                    let key = self.stack().pop().unwrap();
                    self.set(key, c);
                } else {
                    return Err(WsError::new(WsErrorKind::StackError, "Not enough items on stack to input character"));
                },
                InputNum => if len > 0 {
                    try!(self.input_num());
                } else {
                    return Err(WsError::new(WsErrorKind::StackError, "Not enough items on stack to input number"));
                }
            };
            *self.index() += 1;
        }
        Err(WsError::new(WsErrorKind::IOError, "Invalid program counter"))
    }
}

pub trait SmallIntState<'a> : State<'a, Var=Integer> {
    fn into_bigintstate(&'a mut self) -> BigIntState<'a>;
}

impl CheckedArith for BigInteger {
    fn overflowing_add(&self, rhs: &Self) -> (Self, bool) {
        (self + rhs, false)
    }

    fn overflowing_sub(&self, rhs: &Self) -> (Self, bool) {
        (self - rhs, false)
    }

    fn overflowing_mul(&self, rhs: &Self) -> (Self, bool) {
        (self * rhs, false)
    }

    fn checked_div(&self, rhs: &Self) -> Option<Self> {
        BigInteger::checked_div(self, rhs)
    }

    fn checked_rem(&self, rhs: &Self) -> Option<Self> {
        if rhs.sign() == Sign::NoSign {
            return None;
        }
        Some(self % rhs)
    }

    fn is_zero(&self) -> bool {
        self.sign() == Sign::NoSign
    }

    fn is_negative(&self) -> bool {
        self.sign() == Sign::Minus
    }

    fn from_u8(from: u8) -> Self {
        <Self as FromPrimitive>::from_u8(from).unwrap()
    }

    fn into_u8(&self) -> Option<u8> {
        self.to_u8()
    }
}

impl CheckedArith for Integer {
    fn overflowing_add(&self, rhs: &Self) -> (Self, bool) {
        Integer::overflowing_add(*self, *rhs)
    }

    fn overflowing_sub(&self, rhs: &Self) -> (Self, bool) {
        Integer::overflowing_sub(*self, *rhs)
    }

    fn overflowing_mul(&self, rhs: &Self) -> (Self, bool) {
        Integer::overflowing_mul(*self, *rhs)
    }

    fn checked_div(&self, rhs: &Self) -> Option<Self> {
        Integer::checked_div(*self, *rhs)
    }

    fn checked_rem(&self, rhs: &Self) -> Option<Self> {
        Integer::checked_rem(*self, *rhs)
    }

    fn is_zero(&self) -> bool {
        *self == 0
    }

    fn is_negative(&self) -> bool {
        *self < 0
    }

    fn from_u8(from: u8) -> Self {
        from as Self
    }

    fn into_u8(&self) -> Option<u8> {
        if *self <= u8::MAX as Self && *self >= 0 {
            Some(*self as u8)
        } else {
            None
        }
    }
}


/// A whitespace interpreter. This struct provides various strategies for interpreting a whitespace program.
pub struct Interpreter<'a> {
    program: &'a Program,
    options: Options,
    input: &'a mut (BufRead + 'a),
    output: &'a mut (Write + 'a)
}

impl<'a> Interpreter<'a> {
    /// Construct a new whitespace interpreter from a program, input stream and output stream with the specified options.
    pub fn new(program: &'a Program, options: Options, input: &'a mut (BufRead + 'a), output: &'a mut (Write + 'a)) -> Interpreter<'a> {
        Interpreter {
            program: program,
            options: options,
            input: input,
            output: output,
        }
    }

    fn interpret<'b, T: State<'b>>(state: &mut T, program: &Program) -> Result<(), WsError> {
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

    fn bigint_fallback<'b, T: SmallIntState<'b>>(state: &'b mut T, program: &Program, e: WsError) -> Result<(), WsError> {
        match e {
            WsError {kind: WsErrorKind::Overflow, ..} => {
                let mut state = state.into_bigintstate();
                Self::interpret(&mut state, program)
            }
            WsError {kind: WsErrorKind::InumOverflow(key, val), ..} => {
                let mut state = state.into_bigintstate();
                state.set(key.into(), val);
                Self::interpret(&mut state, program)
            }
            e => Err(e)
        }
    }

    /// Similar to Interpreter::interpret_with_simple_state but does not have bigint fallback
    /// and returns the amount of executed instructions on success.
    pub fn count_with_simple_state(&'a mut self) -> Result<usize, WsError> {
        let mut state = SimpleState::new(self.options, &mut self.input, &mut self.output);
        try!(Self::interpret(&mut state, &self.program));
        Ok(state.count)
    }

    /// The reference interpreter implementation. It uses simple data structures internally, falling back to
    /// bignum-based simple datastructures if values become too large.
    pub fn interpret_with_simple_state(&'a mut self) -> Result<(), WsError> {
        let mut state = SimpleState::new(self.options, &mut self.input, &mut self.output);
        match Self::interpret(&mut state, &self.program) {
            Ok(()) => Ok(()),
            Err(e) => if self.options.contains(NO_FALLBACK) {
                Err(e)
            } else {
                Self::bigint_fallback(&mut state, &self.program, e)
            }
        }
    }

    /// Interprets the program with optimized data structures, falling back to
    /// bignum-based simple datastructures if values become too large. 
    pub fn interpret_with_fast_state(&'a mut self) -> Result<(), WsError> {
        let mut state = JitState::new(self.options, &mut self.input, &mut self.output);
        match Self::interpret(&mut state, &self.program) {
            Ok(()) => Ok(()),
            Err(e) => if self.options.contains(NO_FALLBACK) {
                Err(e)
            } else {
                Self::bigint_fallback(&mut state, &self.program, e)
            }
        }
    }

    /// Interpret the program using only bignum-based simple datastructures. This is slow.
    pub fn interpret_with_bigint_state(&'a mut self) -> Result<(), WsError> {
        let mut state = BigIntState::new(self.options, &mut self.input, &mut self.output);
        Self::interpret(&mut state, &self.program)
    }

    /// Use a jit compiler that compiles the entire program in advance to execute
    /// the program. It is backed by an optimized datastructure, and will fall back
    /// to interpretation in unsafe situations. When values become too large it will
    /// fall back to bignum-based interpretation.
    #[cfg(target_arch = "x86_64")]
    pub fn jit_aot(&mut self) -> Result<(), WsError> {
        let program = &self.program;
        let mut state = JitState::new(self.options, &mut self.input, &mut self.output);
        let mut compiler = JitCompiler::new(program, self.options);
        let mut jit_handles = vec![None; program.commands.len()];

        // first compile everything (except the starting block as there's no reason to do that)
        use program::Command::*;
        for (i, c) in program.commands.iter().enumerate() {
            let i = match *c {
                Label | Call {..} if i + 1 != program.commands.len() => i + 1,
                _ => continue
            };

            if let Some(start) = compiler.compile_index(i) {
                jit_handles[i] = Some(start);
            }
        }
        compiler.commit();

        // then run it
        let executor = compiler.executor();
        let lock = executor.lock();

        while let Some(&offset) = jit_handles.get(*state.index()) {
            // can we jit?
            if let Some(offset) = offset {
                let old_index = *state.index();

                unsafe {
                    state.run_block(lock.ptr(offset));
                }
                // if we exit on the same instruction as we started we need to try interpreting first 
                // as otherwise we could get stuck in a loop due to stack errors
                if old_index != *state.index() {
                    continue;
                }
            }

            // fallback interpreting.
            match state.interpret_block(&program.commands) {
                Ok(true) => (),
                Ok(false) => return Ok(()),
                Err(mut e) => if self.options.contains(NO_FALLBACK) {
                    e.set_location(*state.index());
                    return Err(e)
                } else { 
                    e.set_location(*state.index());
                    return Self::bigint_fallback(&mut state, program, e)
                }
            }
        }

        Err(WsError::new(WsErrorKind::InvalidIndex, "Invalid program counter"))
    }

    /// Use a jit compiler that compiles code synchronously while executing
    /// the program. It is backed by an optimized datastructure, and will fall back
    /// to interpretation in unsafe situations. When values become too large it will
    /// fall back to bignum-based interpretation.
    #[cfg(target_arch = "x86_64")]
    pub fn jit_sync(&mut self) -> Result<(), WsError> {
        let program = &self.program;
        let mut state = JitState::new(self.options, &mut self.input, &mut self.output);
        let mut compiler = JitCompiler::new(program, self.options);
        let mut jit_handles = vec![None; self.program.commands.len()];

        let executor = compiler.executor();
        let mut lock = executor.lock();

        // avoid compiling the first part
        match state.interpret_block(&program.commands) {
            Ok(true) => (),
            Ok(false) => return Ok(()),
            Err(mut e) => if self.options.contains(NO_FALLBACK) {
                e.set_location(*state.index());
                return Err(e)
            } else {
                e.set_location(*state.index());
                return Self::bigint_fallback(&mut state, program, e)
            }
        }

        while let Some(&offset) = jit_handles.get(*state.index()) {

            // can we jit?
            if let Some(offset) = offset {
                let old_index = *state.index();
                unsafe {
                    let lock = executor.lock();
                    state.run_block(lock.ptr(offset));
                }
                // not a bail-out
                if old_index != *state.index() {
                    continue;
                }

            } else if let Some(start) = compiler.compile_index(*state.index()) {
                drop(lock);
                compiler.commit();
                lock = executor.lock();

                let old_index = *state.index();
                jit_handles[old_index] = Some(start);

                unsafe {
                    state.run_block(lock.ptr(start));
                }
                // not a bail-out
                if old_index != *state.index() {
                    continue;
                }
            }

            // fallback interpreting.
            match state.interpret_block(&program.commands) {
                Ok(true) => (),
                Ok(false) => return Ok(()),
                Err(mut e) => if self.options.contains(NO_FALLBACK) {
                    e.set_location(*state.index());
                    return Err(e)
                } else {
                    e.set_location(*state.index());
                    return Self::bigint_fallback(&mut state, program, e)
                }
            }
        }

        Err(WsError::new(WsErrorKind::InvalidIndex, "Invalid program counter"))
    }

    /// Use a jit compiler that compiles the program in a separate thread while executing
    /// the program. It is backed by an optimized datastructure, and will fall back
    /// to interpretation in unsafe situations. When values become too large it will
    /// fall back to bignum-based interpretation.
    #[cfg(target_arch = "x86_64")]
    pub fn jit_threaded(&mut self) -> Result<(), WsError> {
        let program = &self.program;
        let mut state = JitState::new(self.options, &mut self.input, &mut self.output);
        let mut compiler = JitCompiler::new(program, self.options);
        let mut jit_handles = vec![None; program.commands.len()];
        let (jit_finished_send, jit_finished_receive) = mpsc::channel();

        let executor = compiler.executor();

        // this thread compiles our code in the background.
        let compiler_borrow = &mut compiler;
        let _compile_thread = crossbeam::scope(|scope| scope.spawn(move || {
            use program::Command::*;
            for (i, c) in compiler_borrow.commands.iter().enumerate() {
                let i = match *c {
                    Label | Call {..} if i + 1 != compiler_borrow.commands.len() => i + 1,
                    _ => continue
                };

                if let Some(start) = compiler_borrow.compile_index(i) {
                    compiler_borrow.commit();
                    if jit_finished_send.send((i, start)).is_err() {
                        break;
                    }
                }
            }
        }));

        while let Some(&offset) = jit_handles.get(*state.index()) {
            // can we jit?
            if let Some(offset) = offset {
                let old_index = *state.index();

                unsafe {
                    let lock = executor.lock();
                    state.run_block(lock.ptr(offset));
                }
                // not a bail-out
                if old_index != *state.index() {
                    continue;
                }
            }

            // hot loop optimization: only check for new chunks when we fall back to interpreting.
            while let Ok((index, offset)) = jit_finished_receive.try_recv() {
                jit_handles[index] = Some(offset);
            }

            // fallback interpreting.
            match state.interpret_block(&program.commands) {
                Ok(true) => (),
                Ok(false) => {
                    drop(jit_finished_receive);
                    return Ok(())
                },
                Err(mut e) => if self.options.contains(NO_FALLBACK) {
                    drop(jit_finished_receive);
                    e.set_location(*state.index());
                    return Err(e);
                } else {
                    drop(jit_finished_receive);
                    e.set_location(*state.index());
                    return Self::bigint_fallback(&mut state, program, e)
                }
            }
        }

        // drop the channel so the compilation thread will terminate soon
        drop(jit_finished_receive);
        Err(WsError::new(WsErrorKind::InvalidIndex, "Invalid program counter"))
    }

    pub fn jit_serialize<F: Write + Seek>(&mut self, f: &mut F) -> Result<usize, WsError> {
        let program = &self.program;
        let mut compiler = JitCompiler::new(program, self.options);
        let mut jit_handles = Vec::new();

        // first compile everything 
        use program::Command::*;
        if let Some(start) = compiler.compile_index(0) {
            jit_handles.push((0, start));
        }
        for (i, c) in program.commands.iter().enumerate() {
            let i = match *c {
                Label | Call {..} if i + 1 != program.commands.len() => i + 1,
                _ => continue
            };

            if let Some(start) = compiler.compile_index(i) {
                jit_handles.push((i, start));
            }
        }
        compiler.commit();

        // header will go here later
        let header_pos = f.seek(SeekFrom::Current(0))?;
        f.write_u64::<LittleEndian>(0)?;
        f.write_u64::<LittleEndian>(0)?;
        f.write_u64::<LittleEndian>(0)?;
        f.write_u64::<LittleEndian>(0)?;
        f.write_u64::<LittleEndian>(0)?;
        f.write_u64::<LittleEndian>(0)?;
        f.write_u64::<LittleEndian>(self.options.bits() as u64)?;

        // serialize the compiled buffer
        let executor = compiler.executor();
        let buf = executor.lock();

        let compiled_pos = f.seek(SeekFrom::Current(0))?;
        let compiled_len = buf.len();
        f.write_all(&*buf)?;

        // serialize handles
        let handles_pos = f.seek(SeekFrom::Current(0))?;
        let handles_len = jit_handles.len();
        for (index, offset) in jit_handles {
            f.write_u64::<LittleEndian>(index as u64)?;
            f.write_u64::<LittleEndian>(offset.0 as u64)?;
        }

        // serialize commands
        let commands_pos = f.seek(SeekFrom::Current(0))?;
        let commands_len = program.commands.len();
        for command in &program.commands {
            encode_into(command, f, Infinite)?;
        }

        let end_pos = f.seek(SeekFrom::Current(0))?;

        // fixup header
        f.seek(SeekFrom::Start(header_pos))?;
        f.write_u64::<LittleEndian>(compiled_pos - header_pos)?;
        f.write_u64::<LittleEndian>(compiled_len as u64)?;
        f.write_u64::<LittleEndian>(commands_pos - header_pos)?;
        f.write_u64::<LittleEndian>(commands_len as u64)?;
        f.write_u64::<LittleEndian>(handles_pos - header_pos)?;
        f.write_u64::<LittleEndian>(handles_len as u64)?;

        f.seek(SeekFrom::Start(end_pos))?;
        Ok((end_pos - header_pos) as usize)
    }

    #[cfg(target_arch = "x86_64")]
    pub fn jit_run_from_serialized<F: Read + Seek>(f: &mut F, input: &mut BufRead, output: &mut Write) -> Result<(), WsError> {
        // parse the header
        let header_pos = f.seek(SeekFrom::Current(0))?;
        let compiled_pos = f.read_u64::<LittleEndian>()? + header_pos;
        let compiled_len = f.read_u64::<LittleEndian>()? as usize;
        let command_pos = f.read_u64::<LittleEndian>()? + header_pos;
        let command_len = f.read_u64::<LittleEndian>()? as usize;
        let handles_pos = f.read_u64::<LittleEndian>()? + header_pos;
        let handles_len = f.read_u64::<LittleEndian>()? as usize;
        let options     = f.read_u64::<LittleEndian>()?;

        // deserialize options

        let options = Options::from_bits_truncate(options as u8);

        // deserialize commands

        f.seek(SeekFrom::Start(command_pos))?;
        let commands = (0..command_len).map(|_| decode_from(f, Infinite)).collect::<Result<Vec<_>, _>>()?;

        let program = Program {
            source: None,
            locs: None,
            source_is_whitespace: false,
            commands: commands
        };

        // deserialize handles

        f.seek(SeekFrom::Start(handles_pos))?;
        let mut jit_handles = vec![None; command_len];

        for _ in 0..handles_len {
            let index = f.read_u64::<LittleEndian>()? as usize;
            let offset = f.read_u64::<LittleEndian>()? as usize;
            if index >= command_len {
                return Err(WsError::new(WsErrorKind::ParseError(0, 0, 0), "Invalid jit table index"));
            }
            jit_handles[index] = Some(offset);
        }
        let jit_handles = jit_handles;

        // load compiled code into an executable buffer

        let mut executable_buffer = Mmap::anonymous(compiled_len, Protection::ReadWrite).unwrap();
        f.seek(SeekFrom::Start(compiled_pos))?;
        f.read_exact(unsafe { executable_buffer.as_mut_slice() })?;

        // patch the imports table
        {
            let mut c = Cursor::new(unsafe { executable_buffer.as_mut_slice() });
            c.write_i64::<LittleEndian>(JitState::cache_bypass_get as i64)?;
            c.write_i64::<LittleEndian>(JitState::cache_evict as i64)?;
            c.write_i64::<LittleEndian>(JitState::print_num as i64)?;
            c.write_i64::<LittleEndian>(JitState::print_char as i64)?;
            c.write_i64::<LittleEndian>(JitState::input_char as i64)?;
            c.write_i64::<LittleEndian>(JitState::call as i64)?;
            c.write_i64::<LittleEndian>(JitState::ret as i64)?;
            c.write_i64::<LittleEndian>(JitState::get_stack as i64)?;
        }

        executable_buffer.set_protection(Protection::ReadExecute).unwrap();
        let executable_buffer = executable_buffer;

        // then run it
        let mut state = JitState::new(options, input, output);

        while let Some(&offset) = jit_handles.get(*state.index()) {
            // can we jit?
            if let Some(offset) = offset {
                let old_index = *state.index();

                unsafe {
                    state.run_block(executable_buffer.ptr().offset(offset as isize));
                }
                // if we exit on the same instruction as we started we need to try interpreting first 
                // as otherwise we could get stuck in a loop due to stack errors
                if old_index != *state.index() {
                    continue;
                }
            }

            // fallback interpreting.
            match state.interpret_block(&program.commands) {
                Ok(true) => (),
                Ok(false) => return Ok(()),
                Err(mut e) => if options.contains(NO_FALLBACK) {
                    e.set_location(*state.index());
                    return Err(e)
                } else { 
                    e.set_location(*state.index());
                    return Self::bigint_fallback(&mut state, &program, e)
                }
            }
        }

        Err(WsError::new(WsErrorKind::InvalidIndex, "Invalid program counter"))
    }
}

/// Returns a buffer containing the output of the compilation process of the
/// specified program. This is mainly useful for debugging and optimizing the
/// performance of the JIT compiler.
#[cfg(target_arch = "x86_64")]
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
    ($ops:expr, $addr:ident, $offset:expr) => {dynasm!($ops
        ; lea stack, stack => Integer[$offset]
        ; mov temp0, [->$addr]
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
        let mut comp = JitCompiler {
            options: options,
            commands: &program.commands,
            blocks: HashMap::new(),
            fixups: HashMap::new(),
            fixup_queue: Vec::new(),
            ops: dynasmrt::Assembler::new()
        };

        // create the import section
        dynasm!(comp.ops
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
            ; mov temp0, [->get_stack]
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
                            ; mov Rq(top), QWORD value as i64
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
                        if !self.options.contains(UNCHECKED_HEAP) {
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
                                ; mov r9, 0
                            );
                            Self::add_fixup(&mut self.fixups, command_index + 1, FixUp::Lea(start, self.ops.offset()));
                        }
                        dynasm!(self.ops
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
}
