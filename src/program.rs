use num_bigint::BigInt;

use std::rc::Rc;
use std::ops::Range;

use label::Label;

pub use ::Program;

#[derive(Debug, Clone)]
pub struct SourceLoc {
    pub line: usize,
    pub column: usize,
    pub span: Range<usize>,
    pub label: Option<Rc<Label>>
}

#[derive(Debug, Clone)]
pub enum Command {
    Push {value: Integer},
    PushBig {value: BigInteger},
    Duplicate,
    Copy {index: usize},
    Swap,
    Discard,
    Slide {amount: usize},
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Set,
    Get,
    Label,
    Call {index: usize},
    Jump {index: usize},
    JumpIfZero {index: usize},
    JumpIfNegative {index: usize},
    EndSubroutine,
    EndProgram,
    PrintChar,
    PrintNum,
    InputChar,
    InputNum
}

pub type Integer = i64;

pub type BigInteger = BigInt;

#[derive(Debug, Clone)]
pub enum SizedInteger {
    Big(BigInteger),
    Small(Integer)
}

impl Program {
    /// Remove source location information from the program.
    pub fn strip(&mut self) {
        self.source = None;
        self.locs = None;
    }
}
