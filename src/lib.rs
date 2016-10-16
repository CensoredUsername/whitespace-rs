#![feature(plugin)]
#![plugin(dynasm)]
extern crate dynasmrt;
extern crate itertools;
extern crate crossbeam;
#[macro_use]
extern crate bitflags;
extern crate fnv;

use std::borrow::Cow;
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::borrow::Borrow;

bitflags! {
    pub flags Options: u8 {
        // safety checks
        const IGNORE_OVERFLOW = 0b0001,
        const UNCHECKED_HEAP  = 0b0010,
        // const UNCHECKED_STACK = 0b0100,

        // features
        // const NEGATIVE_COPY   = 0b1000,
    }
}

#[derive(Debug)]
pub struct WsError {
    message: Cow<'static, str>,
    location: Option<usize>,
    cause: Option<Box<Error>>
}

impl WsError {
    fn new<T: Into<Cow<'static, str>>>(message: T) -> WsError {
        WsError {
            message: message.into(),
            location: None,
            cause: None
        }
    }

    fn wrap<E: Error + 'static, T: Into<Cow<'static, str>>>(error: E, message: T) -> WsError {
        WsError {
            message: message.into(),
            location: None,
            cause: Some(Box::new(error))
        }
    }

    pub fn set_location(&mut self, index: usize) {
        self.location = Some(index);
        self.message = format!("At instruction {}: {}", index, self.message).into();
    }
}

impl Display for WsError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.write_str(&self.message)
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

mod label;
mod program;
mod parser;
mod assembler;

#[cfg(target_arch = "x86_64")]
mod core;

// re-export public api
pub use program::Program;
pub use core::JitInterpreter;
pub use core::simple_interpret;
pub use core::jit_interpret;
pub use core::State;