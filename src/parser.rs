use num_bigint::Sign;

use std::i64;
use std::rc::Rc;

use crate::label::Label;
use crate::program::{Program, Command, Integer, BigInteger, SizedInteger, SourceLoc};
use crate::WsError;
use crate::WsErrorKind::ParseError;

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

    fn parse_arg(&mut self) -> Result<SizedInteger, WsError> {
        let mut accum = 0u64;
        let negative;

        // this could be rewritten to a while let loop if rust'd support while { } else { }
        negative = match self.next() {
            Some(b'\n') => return Ok(SizedInteger::Small(0)),
            Some(b'\t') => true,
            Some(b' ' ) => false,
            None        => return Err(WsError::new(ParseError(self.line, self.column, self.index), "Hit EOF while expecting argument")),
            _           => unreachable!()
        };

        loop {
            match self.next() {
                Some(byte) => {
                    accum = accum << 1 | match byte {
                        b'\n' => return Ok(SizedInteger::Small(if negative {
                            -(accum as Integer)
                        } else {
                            accum as Integer
                        })),
                        b'\t' => 1,
                        b' '  => 0,
                        _     => unreachable!()
                    };
                    if accum > i64::MAX as u64 {
                        break;
                    }
                }
                None => return Err(WsError::new(ParseError(self.line, self.column, self.index), "Hit EOF while parsing argument"))
            }
        }

        // The only way we get here is if the upper bit of accum is set. This
        // means that exactly two segments can be filled. Due to the way the ws
        // format works we store in big-endian order first. Then the order is
        // reversed, making the smallest digit the frontmost one. An extra 0 is
        // added to the end, and the whole vector is shifted to make up for space
        // left over in the smallest digit.
        let mut segments: Vec<u32> = Vec::new();
        segments.push((accum >> 32) as u32);
        segments.push(accum as u32);

        let mut bit = 31;
        let mut accum = 0u32;
        while let Some(byte) = self.next() {
            accum |= match byte {
                b'\n' => {
                    segments.push(accum);
                    segments.reverse();
                    segments.push(0);
                    bit += 1; // the amount we need to shift everything to align the lowest bit
                    for i in 0..segments.len() - 1 {
                        segments[i] = ((segments[i] as u64 | ((segments[i + 1] as u64)<< 32)) >> bit) as u32;
                    };
                    return Ok(SizedInteger::Big(BigInteger::new(if negative {Sign::Minus} else {Sign::Plus}, segments)));
                },
                b'\t' => 1,
                b' '  => 0,
                _     => unreachable!()
            } << bit;

            if bit == 0 {
                segments.push(accum);
                bit = 32;
                accum = 0;
            }
            bit -= 1;
        }
        Err(WsError::new(ParseError(self.line, self.column, self.index), "Hit EOF while parsing argument"))
    }

    fn parse_label(&mut self) -> Result<Label, WsError> {
        let mut label = Label::new();

        while let Some(byte) = self.next() {
            match byte {
                b'\n' => return Ok(label),
                b'\t' => label.push(true),
                b' '  => label.push(false),
                _ => unreachable!()
            };
        }
        Err(WsError::new(ParseError(self.line, self.column, self.index), "Hit EOF while parsing label"))
    }
}

impl Program {
    /// Parse a program written in whitespace to a format suitable
    /// for execution.
    pub fn parse(code: Vec<u8>) -> Result<Program, WsError> {

        let mut commands = Vec::<Command>::new();
        let mut sourcelocs = Vec::<SourceLoc>::new();
        {
            let mut state = ParseState::new(&code);

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
                    (2, 0)  => match state.parse_arg()? {
                        SizedInteger::Small(value) => Command::Push {value: value},
                        SizedInteger::Big(value) => Command::PushBig {value: value}
                    },
                    (3, 6)  => Command::Duplicate,
                    (3, 3)  => match state.parse_arg()? {
                        SizedInteger::Small(value) => Command::Copy {index: value as usize},
                        SizedInteger::Big(value) => return Err(WsError::new(ParseError(startline, startcolumn, startindex), format!("Copy argument too large: {}", value)))
                    },
                    (3, 7)  => Command::Swap,
                    (3, 8)  => Command::Discard,
                    (3, 5)  => match state.parse_arg()? {
                        SizedInteger::Small(value) => Command::Slide {amount: value as usize},
                        SizedInteger::Big(value) => return Err(WsError::new(ParseError(startline, startcolumn, startindex), format!("Slide argument too large: {}", value)))
                    },

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

                        return Err(WsError::new(ParseError(startline, startcolumn, startindex), format!("invalid command: {}", s)));
                    },
                    (_, _) => continue
                };

                hash = 0;
                hash_length = 0;

                let label = match command {
                    Command::Label | Command::Call {..} | Command::Jump {..} |
                    Command::JumpIfZero {..} | Command::JumpIfNegative {..} => Some(state.parse_label()?.into()),
                    _ => None
                };

                commands.push(command);
                sourcelocs.push(SourceLoc {
                    line: startline,
                    column: startcolumn,
                    span: startindex .. state.index + 1,
                    label: label
                });
            }

            if hash_length != 0 {
                return Err(WsError::new(ParseError(state.line, state.column, state.index), "Hit EOF while parsing command"));
            }
        }

        let mut program = Program {
            source: Some(code),
            commands: commands,
            locs: Some(sourcelocs),
            source_is_whitespace: true
        };

        program.compile()?;

        Ok(program)
    }

    /// Serialize the internal representation back into a whitespace program.
    pub fn dump(&self) -> Vec<u8> {
        let mut buffer = Vec::<u8>::new();

        use crate::program::Command::*;
        for (index, command) in self.commands.iter().enumerate() {
            let label = self.locs.as_ref().and_then(|l| l[index].label.as_ref());
            let (code, arg): (&[u8], _) = match *command {
                Push {value}           => (b"  ", Some(number_to_ws(value))),
                PushBig {ref value}    => (b"  ", Some(large_number_to_ws(value))),
                Duplicate              => (b" \n ", None),
                Copy {index}           => (b" \t ", Some(number_to_ws(index as Integer))),
                Swap                   => (b" \n\t", None),
                Discard                => (b" \n\n", None),
                Slide {amount}         => (b" \t\n", Some(number_to_ws(amount as Integer))),
                Add                    => (b"\t   ", None),
                Subtract               => (b"\t  \t", None),
                Multiply               => (b"\t  \n", None),
                Divide                 => (b"\t \t ", None),
                Modulo                 => (b"\t \t\t", None),
                Set                    => (b"\t\t ", None),
                Get                    => (b"\t\t\t", None),
                Label                  => (b"\n  ", Some(label_to_ws(label, index))),
                Call {index}           => (b"\n \t", Some(label_to_ws(label, index - 1))),
                Jump {index}           => (b"\n \n", Some(label_to_ws(label, index - 1))),
                JumpIfZero {index}     => (b"\n\t ", Some(label_to_ws(label, index - 1))),
                JumpIfNegative {index} => (b"\n\t\t", Some(label_to_ws(label, index - 1))),
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

use std::mem::size_of;

fn number_to_ws(mut n: Integer) -> Vec<u8> {
    let mut res = Vec::new();
    if n < 0 {
        n = -n;
        res.push(b'\t');
    } else {
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

fn large_number_to_ws(n: &BigInteger) -> Vec<u8> {
    let mut res = Vec::new();
    let (sign, bytes) = n.to_bytes_be();

    match sign {
        Sign::Minus  => res.push(b'\t'),
        Sign::Plus   => res.push(b' '),
        Sign::NoSign => {
            res.push(b'\n');
            return res;
        }
    }

    let mut force = false;
    for byte in bytes {
        for n in 0..8 {
            if byte & (1 << (7 - n)) != 0 {
                force = true;
                res.push(b'\t');
            } else if force {
                res.push(b' ');
            }
        }
    }

    res.push(b'\n');
    return res;
}

fn label_to_ws(label: Option<&Rc<Label>>, i: usize) -> Vec<u8> {
    let label_storage: Label;
    let label = if let Some(l) = label {
        l
    } else {
        label_storage =  i.to_string().as_bytes().into();
        &label_storage
    };
    let mut res: Vec<u8> = (&label).into_iter()
                                   .map(|i| if i {b'\t'} else {b' '})
                                   .collect();
    res.push(b'\n');
    res
}
