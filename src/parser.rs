use label::Label;
use program::{Program, Command, Integer, SourceLoc};

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
        let mut sourcelocs = Vec::<SourceLoc>::new();
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

            let command = match (hash_length, hash) {
                (2, 0)  => Command::Push {value: try!(state.parse_arg())},
                (3, 6)  => Command::Duplicate,
                (3, 3)  => Command::Copy {index: try!(state.parse_arg()) as usize},
                (3, 7)  => Command::Swap,
                (3, 8)  => Command::Discard,
                (3, 5)  => Command::Slide {amount: try!(state.parse_arg()) as usize},

                (4, 27) => Command::Add,
                (4, 28) => Command::Subtract,
                (4, 29) => Command::Multiply,
                (4, 30) => Command::Divide,
                (4, 31) => Command::Modulo,

                (3, 12) => Command::Set,
                (3, 13) => Command::Get,

                (3, 18) => Command::Label,
                (3, 19) => Command::Call {index: 0},
                (3, 20) => Command::Jump {index: 0},
                (3, 21) => Command::JumpIfZero {index: 0},
                (3, 22) => Command::JumpIfNegative {index: 0},
                (3, 23) => Command::EndSubroutine,
                (3, 26) => Command::EndProgram,

                (4, 45) => Command::PrintChar,
                (4, 46) => Command::PrintNum,
                (4, 48) => Command::InputChar,
                (4, 49) => Command::InputNum,

                (3, 4) | 
                (3, 11) | 
                (3, 14) | 
                (3, 17) | 
                (3, 24) | 
                (3, 25) | 
                (4, _) => {
                    let mut buf = [0u8; 5];
                    for i in 0 .. hash_length {
                        buf[i] = hash % 3;
                        hash /= 3;
                    }

                    let s = buf[..hash_length].iter().rev().map(|c| if *c == 0 {
                        'S'
                    } else if *c == 1 {
                        'T'
                    } else {
                        'N'
                    }).collect::<String>();

                    return Err(format!("invalid command at line {}, column {}: {}", startline, startcolumn, s));
                },
                (_, _) => continue
            };

            hash = 0;
            hash_length = 0;

            let label = match command {
                Command::Label | Command::Call {..} | Command::Jump {..} |
                Command::JumpIfZero {..} | Command::JumpIfNegative {..} => Some(try!(state.parse_label()).into()),
                _ => None
            };

            commands.push(command);
            sourcelocs.push(SourceLoc {
                line: startline,
                column: startcolumn,
                text: &code[startindex .. state.index + 1],
                label: label
            });
        }

        if hash_length != 0 {
            return Err("Hit EOF while parsing command".to_string());
        }

        let mut program = Program {
            source: Some(code),
            commands: commands,
            locs: Some(sourcelocs)
        };

        try!(program.compile());

        Ok(program)
    }

    pub fn dump(&self, reconstruct: bool) -> Vec<u8> {
        if let (false, Some(source)) = (reconstruct, self.source) {
            Vec::from(source)
        } else {
            let mut buffer = Vec::<u8>::new();
            if !reconstruct {
                if let Some(ref locs) = self.locs {
                    for loc in locs {
                        buffer.extend(loc.text);
                    }
                    return buffer;
                }
            }
            use program::Command::*;
            for (index, command) in self.commands.iter().enumerate() {
                let (code, arg): (&[u8], _) = match *command {
                    Push {value}           => (b"  ", Some(number_to_ws(value))),
                    Duplicate              => (b" \n ", None),
                    Copy {index}           => (b" \t ", Some(number_to_ws(index as isize))),
                    Swap                   => (b" \n\t", None),
                    Discard                => (b" \n\n", None),
                    Slide {amount}         => (b" \t\n", Some(number_to_ws(amount as isize))),
                    Add                    => (b"\t   ", None),
                    Subtract               => (b"\t  \t", None),
                    Multiply               => (b"\t  \n", None),
                    Divide                 => (b"\t \t ", None),
                    Modulo                 => (b"\t \t\t", None),
                    Set                    => (b"\t\t ", None),
                    Get                    => (b"\t\t\t", None),
                    Label                  => (b"\n  ", Some(label_to_ws(index))),
                    Call {index}           => (b"\n \t", Some(label_to_ws(index - 1))),
                    Jump {index}           => (b"\n \n", Some(label_to_ws(index - 1))),
                    JumpIfZero {index}     => (b"\n\t ", Some(label_to_ws(index - 1))),
                    JumpIfNegative {index} => (b"\n\t\t", Some(label_to_ws(index - 1))),
                    EndSubroutine          => (b"\n\t\n", None),
                    EndProgram             => (b"\n\n\n", None),
                    PrintChar              => (b"\t\n  ", None),
                    PrintNum               => (b"\t\n \t", None),
                    InputChar              => (b"\t\n\t ", None),
                    InputNum               => (b"\t\n\t\t", None),
                };
                buffer.extend(code);
                if let Some(arg) = arg {
                    buffer.extend(arg);
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

fn label_to_ws(i: usize) -> Vec<u8> {
    let label: Label = i.to_string().as_bytes().into();
    let mut res: Vec<u8> = (&label).into_iter()
                                   .map(|i| if i {b'\t'} else {b' '})
                                   .collect();
    res.push(b'\n');
    res
}
