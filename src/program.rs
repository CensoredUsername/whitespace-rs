use std::collections::HashMap;
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::str;
use std::fmt;
use std::rc::Rc;
use std::borrow::Cow;

use label::Label;

#[derive(Debug, Clone)]
pub struct Program<'a> {
    pub source: Option<&'a [u8]>,
    pub commands: Vec<Command>,
    pub locs: Option<Vec<SourceLoc<'a>>>
}

#[derive(Debug, Clone)]
pub struct SourceLoc<'a> {
    pub line: usize,
    pub column: usize,
    pub text: &'a [u8],
    pub label: Option<Rc<Label>>
}

#[derive(Debug, Clone)]
pub enum Command {
    Push {value: Integer},
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

impl<'a> Program<'a> {
    pub fn compile(&mut self) -> Result<(), String> {
        let mut index_map = HashMap::<Rc<Label>, usize>::new();

        let locs = if let Some(ref locs) = self.locs {
            locs
        } else {
            return Err("This program has been stripped".to_string());
        };

        for (index, (command, loc)) in self.commands.iter().zip(locs).enumerate() {
            if let Command::Label = *command {
                match index_map.entry(loc.label.clone().unwrap()) {
                    Occupied(_) => return Err(format!("Duplicate label {}", loc.label.as_ref().unwrap())),
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
                    Vacant(_)   => return Err("Undefined label".to_string())
                },
                _ => ()
            };
        }

        Ok(())
    }

    pub fn strip(&mut self) {
        self.source = None;
        self.locs = None;
    }

    pub fn format_error(&self, index: usize, message: Cow<'static, str>) -> String {
        if let Some(command) = self.commands.get(index) {
            if let Some(ref locs) = self.locs {
                let loc = &locs[index];
                return format!("At command {} (line {}, column {}): {}\n{:?}", index + 1, loc.line, loc.column, message, command);
            } 
            return format!("At command {}: {}\n{:?}", index + 1, message, command);
        } else {
            return format!("At the end of the program: {}", message);
        }
    }
}

impl<'a> fmt::Display for SourceLoc<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {

        try!(write!(f, "line {}, column {}: ", self.line, self.column));
        if let Ok(s) = str::from_utf8(self.text) {
            f.write_str(s)
        } else {
            f.write_str("Invalid utf-8")
        }
    }
}
