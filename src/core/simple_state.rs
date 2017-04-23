use std::collections::HashMap;
use std::collections::hash_map;
use std::io::{BufRead, Write};

use super::{State, SmallIntState};
use ::program::{Integer, BigInteger};
use ::{Options, IGNORE_OVERFLOW};
use super::bigint_state::BigIntState;
use ::{WsError, WsErrorKind};

pub struct SimpleState<'a> {
    options: Options,
    pub count: usize,
    index: usize,
    stack: Vec<Integer>,
    heap:  HashMap<Integer, Integer>,
    callstack: Vec<usize>,
    input: &'a mut (BufRead + 'a),
    output: &'a mut (Write + 'a)
}

impl<'a, 'b> State<'a> for SimpleState<'b> {
    type Var = Integer;
    type HeapIterator = Iter<'a>;

    fn options(&self) -> Options {
        self.options
    }

    fn index(&mut self) -> &mut usize {
        &mut self.index
    }

    fn stack(&mut self) -> &mut Vec<Integer> {
        &mut self.stack
    }

    fn set(&mut self, key: Integer, value: Integer) {
        self.heap.insert(key, value);
    }

    fn get(&self, key: &Integer) -> Option<&Integer> {
        self.heap.get(key)
    }

    fn iter_heap(&'a self) -> Self::HeapIterator {
        Iter {
            inner: self.heap.iter()
        }
    }

    fn call(&mut self, retloc: usize) {
        self.callstack.push(retloc);
    }
    fn ret(&mut self) -> Option<usize> {
        self.callstack.pop()
    }

    fn input(&mut self) -> &mut BufRead {
        self.input
    }

    fn output(&mut self) -> &mut Write {
        self.output
    }

    fn count_instruction(&mut self) {
        self.count += 1;
    }

    fn input_num(&mut self) -> Result<(), WsError> {
        let s = try!(self.read_num());
        let s = s.trim();
        match s.parse::<Integer>() {
            Ok(value) => {
                let key = self.stack().pop().unwrap();
                self.set(key, value);
                Ok(())
            },
            Err(e) => if self.options.contains(IGNORE_OVERFLOW) {
                Err(WsError::wrap(e, WsErrorKind::RuntimeParseError, "Parsed number is outside arithmetic range"))
            } else {
                match s.parse::<BigInteger>() {
                    Ok(i) => {
                        *self.index() += 1;
                        Err(WsError::new(WsErrorKind::InumOverflow(self.stack().pop().unwrap(), i), "Overflow while parsing number"))
                    },
                    Err(e) => Err(WsError::wrap(e, WsErrorKind::RuntimeParseError, "Expected a number to parse"))
                }
            }
        }
    }
}



impl<'a, 'b> SmallIntState<'a> for SimpleState<'b> {
    fn into_bigintstate(&'a mut self) -> BigIntState<'a> {
        BigIntState::from_components(
            self.options,
            self.index,
            self.stack.iter().cloned().map(|i| i.clone().into()).collect(),
            self.iter_heap().map(|(key, value)| (key.into(), value.into())).collect(),
            self.callstack.clone(),
            self.input,
            self.output
        )
    }
}

impl<'a> SimpleState<'a> {
    pub fn new(options: Options, input: &'a mut (BufRead + 'a), output: &'a mut (Write + 'a)) -> SimpleState<'a> {
        SimpleState {
            options: options,
            count: 0,
            index: 0,
            callstack: Vec::new(),
            heap: HashMap::new(),
            stack: Vec::new(),
            input: input,
            output: output,
        }
    }
}

pub struct Iter<'a> {
    inner: hash_map::Iter<'a, Integer, Integer>
}

impl<'a> Iterator for Iter<'a> {
    type Item = (Integer, Integer);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(k, v)| (*k, *v))
    }
}
