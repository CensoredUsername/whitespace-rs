use std::cell::Cell;
use std::collections::HashMap;
use std::io::{Read, Write, BufRead};
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::str;
use std::fmt;
use std::rc::Rc;

use label::Label;

pub struct Interpreter<'a> {
    pub state: Cell<InterpreterState>,
    pub program: Program<'a>,
    pub instruction_counter: usize,
    pub callstack: Vec<usize>,
    pub stack: Stack,
    pub heap: Heap,
    pub input: Box<BufRead + 'a>,
    pub output: Box<Write + 'a>
}

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

#[derive(Debug, Clone, Copy)]
pub enum InterpreterState {
    Ready,
    Finished,
    Fault
}

pub type Heap = HashMap<Integer, Integer>;
pub type Stack = Vec<Integer>;
pub type Integer = isize;

impl<'a> Interpreter<'a> {
    pub fn new(program: Program<'a>, input: Box<BufRead + 'a>, output: Box<Write + 'a>) -> Interpreter<'a> {
        Interpreter {
            state: Cell::new(InterpreterState::Ready),
            program: program,
            instruction_counter: 0,
            callstack: Vec::new(),
            stack: Stack::new(),
            heap: Heap::new(),
            input: input,
            output: output
        }
    }

    pub fn run(&mut self) -> Result<usize, String> {
        let mut i = 0usize;
        while let InterpreterState::Ready = self.state.get() {
            try!(self.step());
            i += 1;
        }
        Ok(i)
    }

    fn error(&self, s: &str) -> Result<(), String> {
        self.state.set(InterpreterState::Fault);
        Err(s.to_string())
    }

    pub fn step(&mut self) -> Result<(), String> {
        use self::Command::*;
        if let Some(command) = self.program.commands.get(self.instruction_counter) {
            let len = self.stack.len();
            let mut new_instruction_counter = self.instruction_counter + 1;
            match *command {
                Push {value} => self.stack.push(value),
                Duplicate => if len > 0 {
                    let value = self.stack[len - 1];
                    self.stack.push(value);
                } else {
                    return self.error("Tried to duplicate but stack is empty");
                },
                Copy {index} => if let Some(&value) = self.stack.get(index) {
                    self.stack.push(value);
                } else {
                    return self.error("Tried to copy from outside the stack");
                },
                Swap => if len > 1 {
                    self.stack.swap(len - 1, len - 2);
                } else {
                    return self.error("Cannot swap with empty stack");
                },
                Discard => if let None = self.stack.pop() {
                    return self.error("Cannot pop from empty stack");
                },
                Slide {amount} => if len > amount {
                    let top = self.stack[len - 1];
                    self.stack.truncate(len - amount);
                    self.stack[len - amount - 1] = top;
                } else {
                    return self.error("Cannot discard more elements than items exist on stack");
                },
                Add => if len > 1 {
                    if let Some(val) = self.stack[len - 2].checked_add(self.stack[len - 1]) {
                        self.stack[len - 2] = val;
                    } else {
                        return self.error("Overflow during addition");
                    }
                    self.stack.pop();
                } else {
                    return self.error("Not enough items on stack to add");
                },
                Subtract => if len > 1 {
                    if let Some(val) = self.stack[len - 2].checked_sub(self.stack[len - 1]) {
                        self.stack[len - 2] = val;
                    } else {
                        return self.error("Overflow during addition");
                    }
                    self.stack.pop();
                } else {
                    return self.error("Not enough items on stack to subtract");
                },
                Multiply => if len > 1 {
                    if let Some(val) = self.stack[len - 2].checked_mul(self.stack[len - 1]) {
                        self.stack[len - 2] = val;
                    } else {
                        return self.error("Overflow during addition");
                    }
                    self.stack.pop();
                } else {
                    return self.error("Not enough items on stack to multiply");
                },
                Divide => if len > 1 {
                    if let Some(val) = self.stack[len - 2].checked_div(self.stack[len - 1]) {
                        self.stack[len - 2] = val;
                    } else {
                        return self.error("Divide by zero");
                    }
                    self.stack.pop();
                } else {
                    return self.error("Not enough items on stack to divide");
                },
                Modulo => if len > 1 {
                    if let Some(val) = self.stack[len - 2].checked_rem(self.stack[len - 1]) {
                        self.stack[len - 2] = val;
                    } else {
                        return self.error("Divide by zero");
                    }
                    self.stack.pop();
                } else {
                    return self.error("Not enough items on stack to modulo");
                },
                Set => if len > 1 {
                    self.heap.insert(self.stack[len - 2], self.stack[len - 1]);
                    self.stack.pop();
                    self.stack.pop();
                } else {
                    return self.error("Not enough items on stack to set value");
                },
                Get => if len > 0 {
                    if let Some(&value) = self.heap.get(&self.stack[len - 1]) {
                        self.stack[len - 1] = value;
                    } else {
                        return self.error("Key does not exist on the heap");
                    }
                } else {
                    return self.error("not enough items on stack to get value");
                },
                Label => (),
                Call {index} => {
                    self.callstack.push(new_instruction_counter);
                    new_instruction_counter = index;
                },
                Jump {index} => new_instruction_counter = index,
                JumpIfZero {index} => match self.stack.pop() {
                    Some(0) => new_instruction_counter = index,
                    Some(_) => (),
                    _       => return self.error("Not enough items on stack to test if zero")
                },
                JumpIfNegative {index} => match self.stack.pop() {
                    Some(x) if x < 0 => new_instruction_counter = index,
                    Some(_)          => (),
                    _                => return self.error("Not enough items on stack to test if zero")
                },
                EndSubroutine => if let Some(index) = self.callstack.pop() {
                    new_instruction_counter = index;
                } else {
                    return self.error("Not enough items on callstack to return");
                },
                EndProgram => self.state.set(InterpreterState::Finished),
                PrintChar => if let Some(c) = self.stack.pop() {
                    let c: [u8; 1] = [c as u8];
                    self.output.write_all(&c).expect("Could not write to output");
                } else {
                    return self.error("Not enough items on stack to print");
                },
                PrintNum               => if let Some(c) = self.stack.pop() {
                    self.output.write_all(&c.to_string().as_bytes()).expect("Could not write to output");
                } else {
                    return self.error("Not enough items on stack to print");
                },
                InputChar              => if len > 0 {
                    self.output.flush().expect("Could not flush output");
                    let mut s = [0; 1];
                    self.input.read_exact(&mut s).expect("Could not read a character from input");
                    self.heap.insert(self.stack[len - 1], s[0] as Integer);
                } else {
                    return self.error("Not enough items on stack to input character");
                },
                InputNum               => if len > 0 {
                    self.output.flush().expect("Could not flush output");
                    let mut s = Vec::new();
                    self.input.read_until(b'\n', &mut s).expect("Could not read a line from input");
                    self.heap.insert(self.stack[len - 1], str::from_utf8(&s)
                                                          .expect("Input was not valid utf-8")
                                                          .trim().parse::<Integer>()
                                                          .expect("Expected a number to be entered"));
                } else {
                    return self.error("Not enough items on stack to input number");
                }
            };
            self.instruction_counter = new_instruction_counter;
            Ok(())
        } else {
            self.error("Invalid instruction counter")
        }
    }
}

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
