use std::collections::HashMap;
use std::collections::hash_map;
use std::io::{BufRead, Write};

use crate::Options;
use crate::{WsError, WsErrorKind};
use crate::program::BigInteger;
use super::State;

pub struct BigIntState<'a> {
    options: Options,
    index: usize,
    stack: Vec<BigInteger>,
    heap:  HashMap<BigInteger, BigInteger>,
    callstack: Vec<usize>,
    input: &'a mut (dyn BufRead + 'a),
    output: &'a mut (dyn Write + 'a)
}

impl<'a> BigIntState<'a> {
    pub fn new(options: Options, input: &'a mut (dyn BufRead + 'a), output: &'a mut (dyn Write + 'a)) -> BigIntState<'a> {
        BigIntState {
            options: options,
            index: 0,
            heap: HashMap::new(),
            stack: Vec::new(),
            callstack: Vec::new(),
            input: input,
            output: output,
        }
    }

    pub fn from_components(options: Options,
                           index: usize,
                           stack: Vec<BigInteger>,
                           heap: HashMap<BigInteger, BigInteger>,
                           callstack: Vec<usize>,
                           input: &'a mut (dyn BufRead + 'a),
                           output: &'a mut (dyn Write + 'a)) -> BigIntState<'a> {
        BigIntState {
            options: options,
            index: index,
            stack: stack,
            heap: heap,
            callstack: callstack,
            input: input,
            output: output
        }
    }
}


impl<'a> State<'a> for BigIntState<'a> {
    type Var = BigInteger;
    type HeapIterator = Iter<'a>; 

    fn options(&self) -> Options {
        self.options
    }

    fn index(&mut self) -> &mut usize {
        &mut self.index
    }

    fn stack(&mut self) -> &mut Vec<BigInteger> {
        &mut self.stack
    }

    fn set(&mut self, key: BigInteger, value: BigInteger) {
        self.heap.insert(key, value);
    }

    fn get(&self, key: &BigInteger) -> Option<&BigInteger> {
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

    fn input(&mut self) -> &mut dyn BufRead {
        self.input
    }

    fn output(&mut self) -> &mut dyn Write {
        self.output
    }

    fn push_large(&mut self, value: &BigInteger) -> Result<(), WsError> {
        self.stack().push(value.clone());
        Ok(())
    }

    fn input_num(&mut self) -> Result<(), WsError> {
        let s = self.read_num()?;
        let s = s.trim();
        let value = s.parse::<BigInteger>().map_err(|e| WsError::wrap(e, WsErrorKind::RuntimeParseError, "Expected a number to parse"))?;
        let key = self.stack().pop().unwrap();
        self.set(key, value);
        Ok(())
    }
}

pub struct Iter<'a> {
    inner: hash_map::Iter<'a, BigInteger, BigInteger>
}

impl<'a> Iterator for Iter<'a> {
    type Item = (BigInteger, BigInteger);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(k, v)| (k.clone(), v.clone()))
    }
}
