use std::fmt::{Write, self};
use std::iter;
use std::str;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Label {
    bits: u8,       // amount of bits in tail (last element of vec). always between 0 and 7
    buffer: Vec<u8> // backing buffer
}

impl Label {
    /**
     * Small layout intermezzo
     * A label is basically a bitvec, but the internal representation
     * means the first pushed bit will always occupy the MSB position
     * in the backing buffer. This as to allow the conversion from bitvec
     * to unicode to happen smoothly
     */
    pub fn new() -> Label {
        Label {bits: 0, buffer: Vec::new()}
    }

    pub fn len(&self) -> usize {
        let len = self.buffer.len();
        if len == 0 {
            return 0;
        }
        return len * 8 - 8 + self.bits as usize
    }

    pub fn push(&mut self, val: bool) {
        if self.buffer.len() == 0 {
            self.buffer.push(0)
        }

        self.bits += 1;
        let len = self.buffer.len();
        self.buffer[len - 1] |= (val as u8) << (8 - self.bits);

        if self.bits == 8 {
            self.buffer.push(0);
            self.bits = 0;
        }
    }

    pub fn pop(&mut self) -> Option<bool> {
        if self.bits == 0 {
            if self.buffer.len() > 1 {
                self.buffer.pop();
                self.bits = 8;
            } else {
                return None;
            }
        }

        let len = self.buffer.len();
        let tail = &mut self.buffer[len - 1];
        let val: u8 = *tail & (1 << (8 - self.bits));
        *tail ^= val;
        self.bits -= 1;

        return Some(val != 0);
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let len = self.buffer.len();
        if self.bits == 0 && len > 0 {
            if self.buffer[..len - 1].iter().all(|c| match *c {
                b'a'...b'z'
                | b'A'...b'Z'
                | b'_' => true,
                _ => false
            }) {
                // as the above characters are all ascii we can safely convert to utf-8
                return f.write_str(str::from_utf8(&self.buffer[..len - 1]).unwrap());
            }
        }

        try!(f.write_char('_'));
        for bit in self.into_iter() {
            try!(f.write_char(if bit {'1'} else {'0'}));
        }
        Ok(())
    }
}

impl<'a> From<&'a [u8]> for Label {
    fn from(buffer: &[u8]) -> Label {
        let mut buffer = Vec::from(buffer);
        buffer.push(0);
        Label {bits: 0, buffer: buffer}
    }
}

impl<'a> iter::IntoIterator for &'a Label {
    type Item = bool;
    type IntoIter = LabelIterator<'a>;

    fn into_iter(self) -> LabelIterator<'a> {
        LabelIterator {
            label: self,
            index: 0
        }
    }
}

#[derive(Debug, Clone)]
pub struct LabelIterator<'a> {
    label: &'a Label,
    index: usize,
}

impl<'a> iter::Iterator for LabelIterator<'a> {
    type Item = bool;

    fn next(&mut self) -> Option<bool> {
        if self.index == self.label.len() {
            return None
        } else {
            let byte = self.label.buffer[self.index / 8];
            let bit = byte & (1 << (7 - self.index % 8));
            self.index += 1;
            Some(bit != 0)
        }
    }
}
