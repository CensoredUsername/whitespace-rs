#![feature(plugin)]
#![plugin(dynasm)]
extern crate dynasmrt;
extern crate itertools;
extern crate crossbeam;
#[macro_use]
extern crate bitflags;
extern crate fnv;
extern crate num_bigint;
extern crate num_traits;

use std::borrow::Cow;
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::borrow::Borrow;
use std::collections::HashMap;
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::rc::Rc;
use std::str;

mod core;
mod label;
mod program;
mod parser;
mod assembler;
use program::{Command, SourceLoc, Integer, BigInteger};
use label::Label;

// re-export public api
pub use core::Interpreter;
#[cfg(target_arch = "x86_64")]
pub use core::debug_compile;

// This is here for visibility reasons
/// This struct contains the internal representation of a whitespace program.
#[derive(Debug, Clone)]
pub struct Program {
    source: Option<Vec<u8>>,
    commands: Vec<Command>,
    locs: Option<Vec<SourceLoc>>,
    source_is_whitespace: bool
}

// Also here for visibility reasons.
impl Program {
    fn compile(&mut self) -> Result<(), WsParseError> {
        let mut index_map = HashMap::<Rc<Label>, usize>::new();

        let locs = self.locs.as_ref().expect("Tried to compile a stripped program");

        for (index, (command, loc)) in self.commands.iter().zip(locs).enumerate() {
            if let Command::Label = *command {
                match index_map.entry(loc.label.clone().unwrap()) {

                    Occupied(_) => return Err(WsParseError::new(
                        format!("Duplicate label: {}", loc.label.as_ref().unwrap()),
                        loc.span.start, loc.line, loc.column
                    )),
                    Vacant(e)   => e.insert(index + 1)
                };
            }
        }

        for (command, loc) in self.commands.iter_mut().zip(locs) {
            match *command {
                Command::Call {ref mut index} |
                Command::Jump {ref mut index} |
                Command::JumpIfZero {ref mut index} |
                Command::JumpIfNegative {ref mut index} => match index_map.entry(loc.label.clone().unwrap()) {
                    Occupied(e) => *index = *e.get(),
                    Vacant(_)   => return Err(WsParseError::new(
                        format!("Undefined label: {}", loc.label.as_ref().unwrap()),
                        loc.span.start, loc.line, loc.column
                    ))
                },
                _ => ()
            };
        }

        Ok(())
    }
}


bitflags! {
    /// These are bitflag options to alter the behaviour of the interpreter.
    pub flags Options: u8 {
        /// Use wrapping arithmetric instead of detecting overflow and falling back
        const IGNORE_OVERFLOW    = 0b0000_0001,
        /// When an unknown key used to get an item from the heap, return 0 instead of causing an error.
        const UNCHECKED_HEAP     = 0b0000_0010,
        /// Don't fall back to biginteger interpretation on overflow, instead return an overflow error.
        const NO_FALLBACK        = 0b0001_0000,

        // features
        // const NEGATIVE_COPY   = 0b0000_1000,
    }
}

/// The common error type returned from all whitespacers execution functions.
#[derive(Debug)]
pub struct WsError {
    message: Cow<'static, str>,
    /// The kind of error that occurred.
    pub kind: WsErrorKind,
    location: Option<usize>,
    cause: Option<Box<Error>>
}

/// Simple information on what kind of error occurred.
#[derive(Debug, Clone)]
pub enum WsErrorKind {
    /// The stack was not of the correct size to execute an instruction.
    StackError,
    /// A missing key was requested from the heap.
    KeyError,
    /// The program tried to execute an instruction that doesn't exist (generally caused by control flow hitting the end of the program).
    InvalidIndex,
    /// The program tried to return but there was no location to return to on the callstack.
    CallStackError,
    /// Division or Modulo by zero.
    DivisionError,
    /// Something went wrong while trying to read from input or write to output.
    IOError,
    /// The program tried to read a number but no number was given.
    ParseError,
    // The following are often not reported to the user, rather they exist as signals
    // that indicate we need to switch to a bigint-based interpreter
    // Any resettable operation that results in a bigint having to be stored somewhere returns an Overflow
    /// An overflow occurred during an arithmetric operation. This will normally not be returned unless fallback is disabled.
    Overflow,
    // miss minor.
    /// An overflow occurred when a number input was requested. This is a bit of a special case, as the state cannot
    /// be rewound to before the number was parsed. Therefore, the key where the number will be read to, and the 
    /// oversized integer that was parsed are returned in the error, and the location at which the error occurred
    /// is set to be the operation after the failed inum operation. Again, this will not be returned unless fallback is disabled.
    InumOverflow(Integer, BigInteger),
}

impl WsError {
    fn new<T: Into<Cow<'static, str>>>(kind: WsErrorKind, message: T) -> WsError {
        WsError {
            message: message.into(),
            kind: kind,
            location: None,
            cause: None
        }
    }

    fn wrap<E: Error + 'static, T: Into<Cow<'static, str>>>(error: E, kind: WsErrorKind, message: T) -> WsError {
        WsError {
            message: message.into(),
            kind: kind,
            location: None,
            cause: Some(Box::new(error))
        }
    }

    fn set_location(&mut self, index: usize) {
        self.location = Some(index);
    }

    /// Provide a nice error message using information stored in the program structure
    pub fn format_with_program(&self, program: &Program) -> String {
        if let Some(index) = self.location {
            if let Some(ref locs) = program.locs {
                let loc = &locs[index];
                if let Ok(text) = str::from_utf8(&program.source.as_ref().unwrap()[loc.span.clone()]) {
                    return format!("At command {} (line {}, column {}):\n{}\n\"{}\"",
                                   index + 1, loc.line, loc.column, self.message, text);
                } else {
                    return format!("At command {} (line {}, column {}):\n{}",
                                   index + 1, loc.line, loc.column, self.message);
                }
            } 
            return format!("At command {}:\n{}", index + 1, self.message);
        }
        return format!("{}", self.message);
    }
}

impl Display for WsError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        if let Some(index) = self.location {
            write!(f, "At command {}:\n{}", index + 1, self.message)
        } else {
            f.write_str(&self.message)
        }
    }
}

impl Error for WsError {
    fn description(&self) -> &str {
        &self.message
    }

    fn cause(&self) -> Option<&Error> {
        self.cause.as_ref().map(|x| x.borrow())
    }
}

/// The error type returned by whitespacers parsing and assembling functions.
#[derive(Debug)]
pub struct WsParseError {
    message: Cow<'static, str>,
    /// The line at which the error occurred, starting from 1.
    pub line: usize,
    /// The column at which the error occurred, starting from 1.
    pub column: usize,
    /// The character index at which the error occurred, starting from 0.
    pub index: usize,
}

impl WsParseError {
    fn new<T: Into<Cow<'static, str>>>(message: T, index: usize, line: usize, column: usize) -> WsParseError {
        WsParseError {
            message: message.into(),
            line: line,
            column: column,
            index: index
        }
    }
}

impl Error for WsParseError {
    fn description(&self) -> &str {
        &self.message
    }
}

impl Display for WsParseError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(f, "At index {} (line {}, column {}):\n{}", self.index, self.line, self.column, self.message)
    }
}
