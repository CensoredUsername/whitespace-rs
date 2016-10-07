#![feature(plugin)]
#![plugin(dynasm)]
extern crate dynasmrt;
extern crate itertools;
extern crate crossbeam;
#[macro_use]
extern crate bitflags;
extern crate fnv;

use std::borrow::Cow;


bitflags! {
    pub flags Options: u8 {
        // safety checks
        const INGORE_OVERFLOW = 0b0001,
        const UNCHECKED_HEAP  = 0b0010,
        const UNCHECKED_STACK = 0b0100,

        // features
        const NEGATIVE_COPY   = 0b1000,
    }
}

struct WsError<'a> {
    error: Cow<'a, str>
}

/*impl Error for WsError {

}
*/
mod label;
mod program;
mod parser;
mod assembler;

#[cfg(target_arch = "x86_64")]
mod core;

// re-export public api
pub use program::Program;
pub use core::JitInterpreter;