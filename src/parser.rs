use std::cell::Cell;

use label::Label;
use interpreter::{Program, Command, CommandType, Integer, SourceLoc};

#[derive(Debug, Clone)]
struct ParseState<'a> {
    line: usize,
    column: usize,
    index: usize,
    item: Option<u8>,
    buffer: &'a [u8]
}

impl<'a> Iterator for ParseState<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<u8> {
        loop {
            match self.item {
                Some(b'\n') => {self.index += 1; self.line   += 1; self.column = 1;},
                Some(_)     => {self.index += 1; self.column += 1;}
                None        => ()
            }

            return match self.buffer.get(self.index) {
                Some(&new) => {
                    self.item = Some(new);
                    match new {
                        b'\n' | b'\t' | b' ' => Some(new),
                        _ => continue
                    }
                } 
                None => None
            }
        }
    }
}

impl<'a> ParseState<'a> {
    fn new(buffer: &'a [u8]) -> ParseState {
        ParseState {line: 1, column: 1, index: 0, item: None, buffer: buffer}
    }

    fn parse_arg(&mut self) -> Result<Integer, String> {
        let mut accum = 0isize;
        let negative;

        // this could be rewritten to a while let loop if rust'd support while { } else { }
        negative = match self.next() {
            Some(b'\n') => return Ok(0),
            Some(b'\t') => true,
            Some(b' ' ) => false,
            None        => return Err("Hit EOF while expecting argument".to_string()),
            _           => unreachable!()
        };

        while let Some(byte) = self.next() {
            accum = accum << 1 | match byte {
                b'\n' => return Ok(if negative {-accum} else {accum}),
                b'\t' => 1,
                b' '  => 0,
                _     => unreachable!()
            };
        };
        Err("Hit EOF while parsing argument".to_string())
    }

    fn parse_label(&mut self) -> Result<Label, String> {
        let mut label = Label::new();

        while let Some(byte) = self.next() {
            match byte {
                b'\n' => return Ok(label),
                b'\t' => label.push(true),
                b' '  => label.push(false),
                _ => unreachable!()
            };
        }
        Err("Hit EOF while parsing label".to_string())
    }
}

impl<'a> Program<'a> {
    pub fn parse (code: &[u8]) -> Result<Program, String> {

        let mut commands = Vec::<Command>::new();
        let mut state = ParseState::new(code);

        let mut hash = 0;
        let mut hash_length = 0;

        let mut startline = 0usize;
        let mut startcolumn = 0usize;
        let mut startindex = 0usize;

        while let Some(byte) = state.next() {
            if hash_length == 0 {
                startline   = state.line;
                startcolumn = state.column;
                startindex  = state.index;
            }

            hash = hash * 3 + match byte {
                b' '  => 0,
                b'\t' => 1,
                b'\n' => 2,
                _     => unreachable!()
            };

            hash_length += 1;

            let command_type = match (hash_length, hash) {
                (2, 0)  => CommandType::Push {value: try!(state.parse_arg())},
                (3, 6)  => CommandType::Duplicate,
                (3, 3)  => CommandType::Copy {index: try!(state.parse_arg()) as usize},
                (3, 7)  => CommandType::Swap,
                (3, 8)  => CommandType::Discard,
                (3, 5)  => CommandType::Slide {amount: try!(state.parse_arg()) as usize},

                (4, 27) => CommandType::Add,
                (4, 28) => CommandType::Subtract,
                (4, 29) => CommandType::Multiply,
                (4, 30) => CommandType::Divide,
                (4, 31) => CommandType::Modulo,

                (3, 12) => CommandType::Set,
                (3, 13) => CommandType::Get,

                (3, 18) => CommandType::Label,
                (3, 19) => CommandType::Call {index: Cell::new(0)},
                (3, 20) => CommandType::Jump {index: Cell::new(0)},
                (3, 21) => CommandType::JumpIfZero {index: Cell::new(0)},
                (3, 22) => CommandType::JumpIfNegative {index: Cell::new(0)},
                (3, 23) => CommandType::EndSubroutine,
                (3, 26) => CommandType::EndProgram,

                (4, 45) => CommandType::PrintChar,
                (4, 46) => CommandType::PrintNum,
                (4, 48) => CommandType::InputChar,
                (4, 49) => CommandType::InputNum,

                (3, 4) | 
                (3, 11) | 
                (3, 14) | 
                (3, 17) | 
                (3, 24) | 
                (3, 25) | 
                (4, _) => return Err(format!("invalid command at line {}, column {}: hash {} hash_len {}",
                                             startline, startcolumn, hash, hash_length)),
                (_, _) => continue
            };

            hash = 0;
            hash_length = 0;

            let label = match command_type {
                CommandType::Label | CommandType::Call {..} | CommandType::Jump {..} |
                CommandType::JumpIfZero {..} | CommandType::JumpIfNegative {..} => Some(try!(state.parse_label())),
                _ => None
            };

            commands.push(Command {
                data: command_type,
                source: Some(Box::new(SourceLoc {
                    line: startline,
                    column: startcolumn,
                    text: &code[startindex .. state.index + 1],
                    label: label
                }))
            });
        }

        if hash_length != 0 {
            return Err("Hit EOF while parsing command".to_string());
        }

        let mut program = Program {source: Some(code), commands: commands};

        try!(program.compile());

        Ok(program)
    }

    pub fn dump(&self, reconstruct: bool) -> Vec<u8> {
        if let (false, Some(source)) = (reconstruct, self.source) {
            Vec::from(source)
        } else {
            let mut buffer = Vec::<u8>::new();
            for (index, command) in self.commands.iter().enumerate() {
                if let (false, &Some(ref loc)) = (reconstruct, &command.source) {
                    buffer.extend(loc.text);
                } else {
                    let (command, arg): (&[u8], Option<Vec<u8>>) = match command.data { // todo: command decoding
                        CommandType::Push {value}               => (b"  ", Some(number_to_ws(value))),
                        CommandType::Duplicate                  => (b" \n ", None),
                        CommandType::Copy {index}               => (b" \t ", Some(number_to_ws(index as isize))),
                        CommandType::Swap                       => (b" \n\t", None),
                        CommandType::Discard                    => (b" \n\n", None),
                        CommandType::Slide {amount}             => (b" \t\n", Some(number_to_ws(amount as isize))),
                        CommandType::Add                        => (b"\t   ", None),
                        CommandType::Subtract                   => (b"\t  \t", None),
                        CommandType::Multiply                   => (b"\t  \n", None),
                        CommandType::Divide                     => (b"\t \t ", None),
                        CommandType::Modulo                     => (b"\t \t\t", None),
                        CommandType::Set                        => (b"\t\t ", None),
                        CommandType::Get                        => (b"\t\t\t", None),
                        CommandType::Label                      => (b"\n  ", Some(label_to_ws(index, &command.source))),
                        CommandType::Call {ref index}           => (b"\n \t", Some(label_to_ws(index.get() - 1, &command.source))),
                        CommandType::Jump {ref index}           => (b"\n \n", Some(label_to_ws(index.get() - 1, &command.source))),
                        CommandType::JumpIfZero {ref index}     => (b"\n\t ", Some(label_to_ws(index.get() - 1, &command.source))),
                        CommandType::JumpIfNegative {ref index} => (b"\n\t\t", Some(label_to_ws(index.get() - 1, &command.source))),
                        CommandType::EndSubroutine              => (b"\n\t\n", None),
                        CommandType::EndProgram                 => (b"\n\n\n", None),
                        CommandType::PrintChar                  => (b"\t\n  ", None),
                        CommandType::PrintNum                   => (b"\t\n \t", None),
                        CommandType::InputChar                  => (b"\t\n\t ", None),
                        CommandType::InputNum                   => (b"\t\n\t\t", None),
                    };
                    buffer.extend(command);
                    if let Some(arg) = arg {
                        buffer.extend(arg);
                    }
                }
            }
            buffer
        }
    }
}

use std::mem::size_of;

fn number_to_ws(mut n: isize) -> Vec<u8> {
    let mut res = Vec::new();
    if n < 0 {
        n = -n;
        res.push(b'\t');
    } else if n > 0{
        res.push(b' ');
    }

    let n = n as usize;
    let mut i = size_of::<usize>() * 8;
    let mut force = false;
    while i != 0 {
        i -= 1;
        if (n & (1 << i)) != 0 {
            force = true;
            res.push(b'\t');
        } else if force {
            res.push(b' ');
        }
    }
    res.push(b'\n');
    res
}

fn label_to_ws(i: usize, l: &Option<Box<SourceLoc>>) -> Vec<u8> {
    let label;
    let l: &Label = if let &Some(ref loc) = l {
        loc.label.as_ref().unwrap()
    } else {
        label = i.to_string().as_bytes().into();
        &label
    };
    let mut res = l.into_iter().map(|i| if i {b'\t'} else {b' '}).collect::<Vec<u8>>();
    res.push(b'\n');
    res
}
