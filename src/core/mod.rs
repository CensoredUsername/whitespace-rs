
use crossbeam;
use num_bigint::Sign;
use num_traits::{FromPrimitive, ToPrimitive};

use std::io::{BufRead, Write};
use std::sync::mpsc;
use std::fmt::Display;
use std::str::FromStr;
use std::u8;

use program::{Program, Command, Integer, BigInteger};
use super::{WsError, WsErrorKind, Options, IGNORE_OVERFLOW, UNCHECKED_HEAP, NO_FALLBACK, NO_IMPLICIT_EXIT};

mod cached_map;

mod simple_state;
use self::simple_state::SimpleState;

mod jit_state;
use self::jit_state::JitState;

mod bigint_state;
use self::bigint_state::BigIntState;


pub trait CheckedArith : Sized + Clone + Default + Display + FromStr + ToString + From<isize> {
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
        self.output().flush()          .map_err(|e| WsError::wrap(e, WsErrorKind::IOError, "Could not flush the output file"))?;
        self.input().read_exact(&mut s).map_err(|e| WsError::wrap(e, WsErrorKind::IOError, "Could not read from the input file"))?;
        Ok(Self::Var::from_u8(s[0]))
    }
    fn read_num(&mut self) -> Result<String, WsError> {
        let mut s = String::new();
        self.output().flush()         .map_err(|e| WsError::wrap(e, WsErrorKind::IOError, "Could not flush the output file"))?;
        self.input().read_line(&mut s).map_err(|e| WsError::wrap(e, WsErrorKind::IOError, "Could not read a line from the input file"))?;
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
        let s = self.read_num()?;
        let s = s.trim();
        let value = s.parse::<Self::Var>().map_err(|_| WsError::new(WsErrorKind::RuntimeParseError, "Expected a number to parse"))?;
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
                PushBig {ref value} => self.push_large(value)?,
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
                    self.write_char(c)?;
                } else {
                    return Err(WsError::new(WsErrorKind::StackError, "Not enough items on stack to print"));
                },
                PrintNum => if let Some(c) = self.stack().pop() {
                    self.write_num(c)?;
                } else {
                    return Err(WsError::new(WsErrorKind::StackError, "Not enough items on stack to print"));
                },
                InputChar => if len > 0 {
                    let c = self.read_char()?;
                    let key = self.stack().pop().unwrap();
                    self.set(key, c);
                } else {
                    return Err(WsError::new(WsErrorKind::StackError, "Not enough items on stack to input character"));
                },
                InputNum => if len > 0 {
                    self.input_num()?;
                } else {
                    return Err(WsError::new(WsErrorKind::StackError, "Not enough items on stack to input number"));
                }
            };
            *self.index() += 1;
        }

        if options.contains(NO_IMPLICIT_EXIT) {
            Err(WsError::new(WsErrorKind::InvalidIndex, "Invalid program counter"))
        } else {
            Ok(false)
        }
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
        Self::interpret(&mut state, &self.program)?;
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
    #[cfg(any(target_arch="x86_64", target_arch="x86"))]
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

        if self.options.contains(NO_IMPLICIT_EXIT) {
            Err(WsError::new(WsErrorKind::InvalidIndex, "Invalid program counter"))
        } else {
            Ok(())
        }
    }

    /// Use a jit compiler that compiles code synchronously while executing
    /// the program. It is backed by an optimized datastructure, and will fall back
    /// to interpretation in unsafe situations. When values become too large it will
    /// fall back to bignum-based interpretation.
    #[cfg(any(target_arch="x86_64", target_arch="x86"))]
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

        if self.options.contains(NO_IMPLICIT_EXIT) {
            Err(WsError::new(WsErrorKind::InvalidIndex, "Invalid program counter"))
        } else {
            Ok(())
        }
    }

    /// Use a jit compiler that compiles the program in a separate thread while executing
    /// the program. It is backed by an optimized datastructure, and will fall back
    /// to interpretation in unsafe situations. When values become too large it will
    /// fall back to bignum-based interpretation.
    #[cfg(any(target_arch="x86_64", target_arch="x86"))]
    pub fn jit_threaded(&mut self) -> Result<(), WsError> {
        // the compiler and comms channel need to live to the end of the crossbeam scope
        let mut compiler = JitCompiler::new(self.program, self.options);
        let (jit_finished_send, jit_finished_receive) = mpsc::channel();

        crossbeam::scope(|scope| {
            // get an executor before we move the compiler to the thread.
            let executor = compiler.executor();

            // this thread compiles our code in the background.
            scope.spawn(move || {
                use program::Command::*;
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
            });

            let mut state = JitState::new(self.options, &mut self.input, &mut self.output);
            let mut jit_handles = vec![None; self.program.commands.len()];

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
                match state.interpret_block(&self.program.commands) {
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
                        return Self::bigint_fallback(&mut state, self.program, e)
                    }
                }
            }

            // drop the channel so the compilation thread will terminate soon
            drop(jit_finished_receive);

            if self.options.contains(NO_IMPLICIT_EXIT) {
                Err(WsError::new(WsErrorKind::InvalidIndex, "Invalid program counter"))
            } else {
                Ok(())
            }
        })
    }
}

#[cfg(target_arch="x86_64")]
mod compiler_x64;
#[cfg(target_arch="x86")]
mod compiler_x86;

#[cfg(target_arch="x86_64")]
use self::compiler_x64::JitCompiler;
#[cfg(target_arch="x86")]
use self::compiler_x86::JitCompiler;

#[cfg(target_arch="x86_64")]
pub use self::compiler_x64::debug_compile;
#[cfg(target_arch="x86")]
pub use self::compiler_x86::debug_compile;

#[cfg(any(target_arch="x86_64", target_arch="x86"))]
mod allocator;
