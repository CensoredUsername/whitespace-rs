use num_bigint::BigInt;

use std::rc::Rc;
use std::ops::Range;
use std::collections::HashMap;

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

pub type Integer = isize;

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

    /// Rewrites labels to ensure the resulting program is as small as possible.
    pub fn minify(&mut self) {
        // get the source locs, otherwise, invent some new sourcelocs.
        let locs = if let Some(ref mut locs) = self.locs {
            locs
        } else {
            self.locs = Some(self.commands.iter().map(|_| SourceLoc {line: 0, column: 0, span: 0..0, label: None}).collect());
            self.locs.as_mut().unwrap()
        };

        // create an index => times_used map
        let mut label_count = HashMap::new();
        for (i, op) in self.commands.iter().enumerate() {
            match *op {
                Command::Label => *label_count.entry(i + 1).or_insert(0) += 1,
                Command::Call {index}
                | Command::Jump {index}
                | Command::JumpIfZero {index}
                | Command::JumpIfNegative {index} => *label_count.entry(index).or_insert(0) += 1,
                _ => ()
            }
        }

        // turn the map into a list of (index, times_used) that is sorted by times_used
        let mut label_count: Vec<_> = label_count.into_iter().collect();
        label_count.sort_by_key(|&(_, w)| w);

        // create an allocator that produces unique labels, starting from the smallest possible label
        let mut length = 0usize;
        let mut value = 0usize;
        let mut new_label = || {
            // create label
            let mut label = Label::new();
            for i in 0..length {
                label.push(value & (1 << i) != 0);
            }
            // increment state
            value += 1;
            if value == (1 << length) {
                length += 1;
                value = 0;
            }
            Rc::new(label)
        };

        // from the sorted list, create a map of index => new_label
        let label_map: HashMap<_, _> = label_count.iter().rev().map(|&(i, _)| (i, new_label())).collect();

        // and edit locs using them.
        for ((i, op), loc) in self.commands.iter().enumerate().zip(locs) {
            match *op {
                Command::Label => loc.label = Some(label_map[&(i + 1)].clone()),
                Command::Call {index}
                | Command::Jump {index}
                | Command::JumpIfZero {index}
                | Command::JumpIfNegative {index} => loc.label = Some(label_map[&index].clone()),
                _ => ()
            }
        }
    }
}
