#![macro_use]

// note: this assembler only implements commands relevant to 64-bit values. We do encode smaller immediates, but all register/memory accesses are 64bit

use std::{i8, u8, u16, i32, u32};
use std::mem;

/*
 * Shorthand macros
 */

// the following macro is not for the faint of heart but the result is nice
// it delimits comma-separated lists of token-trees using a tt muncher
macro_rules! op {
    ($e:ident += $ins:ident [$($args:tt)*] [$($accum:tt)*] , $($t:tt)*)        => (op!(      $e += $ins [$($args)* [$($accum)*]] []                 $($t)*));
    ($e:ident += $ins:ident [$($args:tt)*] [$($accum:tt)*] $head:tt $($t:tt)*) => (op!(      $e += $ins [$($args)*             ] [$($accum)* $head] $($t)*));
    ($e:ident += $ins:ident [$($args:tt)*] [$($accum:tt)*])                    => (op_parse!($e += $ins  $($args)* [$($accum)*]                           ));
    ($e:ident += $ins:ident $head:tt $($t:tt)*)                                => (op!(      $e += $ins []                       [$head]            $($t)*));
    ($e:ident += $ins:ident                   )                                => (op_parse!($e += $ins                                                   ));
}

macro_rules! op_parse {
    ($e:ident += $ins:ident) => (
        try!(encode(X64Asm {op: Opcode::$ins, arg1: Arg::None, arg2: Arg::None, arg3: Arg::None}, &mut $e))
    );
    ($e:ident += $ins:ident [$($arg1:tt)*]) => (
        try!(encode(X64Asm {op: Opcode::$ins, arg1: op_arg!($($arg1)*), arg2: Arg::None, arg3: Arg::None}, &mut $e))
    );
    ($e:ident += $ins:ident [$($arg1:tt)*] [$($arg2:tt)*]) => (
        try!(encode(X64Asm {op: Opcode::$ins, arg1: op_arg!($($arg1)*), arg2: op_arg!($($arg2)*), arg3: Arg::None}, &mut $e))
    );
    ($e:ident += $ins:ident [$($arg1:tt)*] [$($arg2:tt)*] [$($arg3:tt)*]) => (
        try!(encode(X64Asm {op: Opcode::$ins, arg1: op_arg!($($arg1)*), arg2: op_arg!($($arg2)*), arg2: op_arg!($($arg3)*)}, &mut $e))
    );
}

macro_rules! op_arg {
    ( [ % $r:ident          , % $b:ident, $e:expr ] ) => (Arg::Scaled    {sc: 1,  ind: Register::$r  , base: Register::$b  , disp: $e as i64});
    ( [ % $r:ident          , % $b:ident          ] ) => (Arg::Scaled    {sc: 1,  ind: Register::$r  , base: Register::$b  , disp: 0});
    ( [ % RSP               ,   $e:expr           ] ) => (Arg::Scaled    {sc: 1,  ind: Register::RSP , base: Register::RSP , disp: $e as i64});
    ( [ % RSP                                     ] ) => (Arg::Scaled    {sc: 1,  ind: Register::RSP , base: Register::RSP , disp: 0});
    ( [ % RBP               ,   $e:expr           ] ) => (Arg::Scaled    {sc: 1,  ind: Register::RBP , base: Register::RBP , disp: $e as i64});
    ( [ % RBP                                     ] ) => (Arg::Scaled    {sc: 1,  ind: Register::RBP , base: Register::RBP , disp: 0});
    ( [ % $r:ident          ,   $e:expr           ] ) => (Arg::Indirect  {                             reg:  Register::$r  , disp: $e as i64});
    ( [ % $r:ident                                ] ) => (Arg::Indirect  {                             reg:  Register::$r  , disp: 0});
    ( [ % $r:ident * $s:expr, % $b:ident, $e:expr ] ) => (Arg::Scaled    {sc: $s, ind: Register::$r  , base: Register::$b  , disp: $e as i64});
    ( [ % $r:ident * $s:expr, % $b:ident          ] ) => (Arg::Scaled    {sc: $s, ind: Register::$r  , base: Register::$b  , disp: 0});
    ( [ % $r:ident * $s:expr,             $e:expr ] ) => (Arg::Scaled    {sc: $s, ind: Register::$r  , base: Register::RBP , disp: $e as i64});
    ( [ % $r:ident * $s:expr                      ] ) => (Arg::Scaled    {sc: $s, ind: Register::$r  , base: Register::RBP , disp: 0});
    ( [   $e:expr                                 ] ) => (Arg::Scaled    {sc: 1,  ind: Register::RSP , base: Register::RBP , disp: $e as i64});
    (   % $r:ident   )                                => (Arg::Direct    {                             reg:  Register::$r});
    (     $e:expr    )                                => (Arg::Immediate {value: $e as i64});
}

/*
 * Data structure used for serialization
 */

#[derive(Debug, Clone)]
pub struct X64Asm {
    pub op: Opcode,
    pub arg1: Arg,
    pub arg2: Arg,
    pub arg3: Arg
}

#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
pub enum Opcode {
    Mov,
    Lea,
    Cmovl,
    Cmovz,
    Cqo,
    Ret,
    Add,
    Sub,
    Imul,
    Idiv,
    Cmp,
    Push,
    Pop,
    Call,
    Jz,
    Jnz,
    Jl,
    Jge,
    Jle,
    Jg
}

#[derive(Debug, Clone)]
pub enum Arg {
    Direct    {                       reg:  Register},
    Indirect  {                       reg:  Register, disp: i64},
    Scaled    {sc: u8, ind: Register, base: Register, disp: i64},
    Immediate {value: i64},
    None
}

#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
pub enum Register {
    RAX = 0b0000,
    RCX = 0b0001,
    RDX = 0b0010,
    RBX = 0b0011,
    RSP = 0b0100,
    RBP = 0b0101,
    RSI = 0b0110,
    RDI = 0b0111,
    R8  = 0b1000,
    R9  = 0b1001,
    R10 = 0b1010,
    R11 = 0b1011,
    R12 = 0b1100,
    R13 = 0b1101,
    R14 = 0b1110,
    R15 = 0b1111
}

/*
 * Utility defs for encoding
 */

macro_rules! fits {
    ($e:expr, $t:ident) => ($e >= $t::MIN as i64 && $e <= $t::MAX as i64);
}

const MOD_DIRECT: u8 = 0b11;
const MOD_NODISP: u8 = 0b00;
const MOD_DISP8:  u8 = 0b01;
const MOD_DISP32: u8 = 0b10;

const SIB: Register = Register::RSP;

macro_rules! modrm {
    ($mo:expr, $reg:expr, $rm:expr) => (($rm as u8 & 7) |
                                        (($reg as u8 & 7) << 3) |
                                        ($mo << 6));
}

macro_rules! sib {
    ($sc:expr, $ind:expr, $base:expr) => (($base as u8 & 7) |
                                          (($ind as u8 & 7) << 3) |
                                          (($sc as u8) << 6));
}

macro_rules! rex {
    ($size:expr, $reg:expr, $ind:expr, $rm:expr) => (0x40 | 
                                                     (($size & 1) << 3) |
                                                     (($reg as u8 & 0x8) >> 1) |
                                                     (($ind as u8 & 0x8) >> 2) |
                                                     (($rm as u8 & 0x8) >> 3));
    ($size:expr, $reg:expr, $rm:expr) => (0x40 | 
                                          (($size & 1) << 3) | 
                                          (($reg as u8 & 0x8) >> 1) | 
                                          (($rm as u8 & 0x8) >> 3));
}

macro_rules! err {
    ($e:expr) => (return Err($e.to_string()));
}

fn push_16(buffer: &mut Vec<u8>, n: u16) {
    buffer.extend(unsafe { 
        mem::transmute::<u16, [u8; 2]>(n.to_le()).into_iter()
    } );
}

fn push_32(buffer: &mut Vec<u8>, n: u32) {
    buffer.extend(unsafe { 
        mem::transmute::<u32, [u8; 4]>(n.to_le()).into_iter()
    } );
}

fn push_64(buffer: &mut Vec<u8>, n: u64) {
    buffer.extend(unsafe { 
        mem::transmute::<u64, [u8; 8]>(n.to_le()).into_iter()
    } );
}

fn push_reg_rm(buffer: &mut Vec<u8>, op: &[u8], reg: u8, rm: Arg, w: u8) -> Result<(), String> {
    // write direct args or decode the disp size
    let (mode, disp) = match rm {
        Arg::Direct{reg: rm} => {
            if w | ((reg | rm as u8) >> 3) != 0 {
                buffer.push(rex!(w, reg, rm));
            }
            buffer.extend(op);
            buffer.push(modrm!(MOD_DIRECT, reg, rm));
            return Ok(())
        },
        Arg::Indirect {disp, ..} |
        Arg::Scaled   {disp, ..} => if disp == 0 {
            (MOD_NODISP, 0)
        } else if fits!(disp, i8) {
            (MOD_DISP8, disp)
        } else if fits!(disp, i32) {
            (MOD_DISP32, disp)
        } else {
            err!("Offset too large")
        },
        Arg::None => err!("Missing argument"),
        _ => err!("Invalid argument type")
    };

    // write indirect and scaled args
    match rm {
        Arg::Indirect {reg: rm, ..} => {
            if w | ((reg | rm as u8) >> 3) != 0 {
                buffer.push(rex!(w, reg, rm));
            }
            buffer.extend(op);
            buffer.push(modrm!(mode, reg, rm));
        }
        Arg::Scaled {sc, ind, base, ..} => {
            if w | ((reg | ind as u8 | base as u8) >> 3) != 0 {
                buffer.push(rex!(w, reg, ind, base));
            }
            buffer.extend(op);
            buffer.push(modrm!(mode, reg, SIB));
            let sc_code = sc.trailing_zeros();
            if sc_code > 3 || sc.count_ones() != 1 {
                err!("Invalid scaling")
            }
            buffer.push(sib!(sc_code, ind, base));
        }
        _ => unreachable!()
    }

    // write disp
    if mode == MOD_DISP8 {
        buffer.push(disp as u8);
    } else if mode == MOD_DISP32 {
        push_32(buffer, disp as u32);
    }

    Ok(())
}

/*
 * Encoding functions start here
 */

fn encode_mov(op: X64Asm, buffer: &mut Vec<u8>) -> Result<(), String> {
    // opcodes implemented here: 89(64) 8b(64) B8(64) C7/0(64)
    if let Arg::None = op.arg3 { } else {
        err!("Too many arguments");
    }

    match op.arg2 {
        Arg::Immediate {value} => {
            if fits!(value, i32) {
                try!(push_reg_rm(buffer, &[0xC7], 0, op.arg1, 1));
                push_32(buffer, value as u32);
            } else if let Arg::Direct {reg} = op.arg1 {
                buffer.push(rex!(1, 0, reg));
                buffer.push(0xB8 + ((reg as u8) & 7));
                push_64(buffer, value as u64);
            } else {
                err!("Invalid argument type")
            }
        },
        Arg::Direct {reg} => {
            try!(push_reg_rm(buffer, &[0x89], reg as u8, op.arg1, 1));
        },
        rm @ Arg::Scaled   {..} |
        rm @ Arg::Indirect {..} => if let Arg::Direct {reg} = op.arg1 {
            try!(push_reg_rm(buffer, &[0x8B], reg as u8, rm, 1));
        } else {
            err!("Invalid argument type");
        },
        Arg::None => err!("Too few arguments")
    }
    Ok(())
}

fn encode_lea(op: X64Asm, buffer: &mut Vec<u8>) -> Result<(), String> {
    // opcodes implemented here: 89(64) 8b(64) B8(64) C7/0(64)
    if let Arg::None = op.arg3 { } else {
        err!("Too many arguments");
    }

    match op.arg2 {
        rm @ Arg::Scaled   {..} |
        rm @ Arg::Indirect {..} => if let Arg::Direct {reg} = op.arg1 {
            try!(push_reg_rm(buffer, &[0x8D], reg as u8, rm, 1));
        } else {
            err!("Invalid argument type");
        },
        Arg::None => err!("Too few arguments"),
        _ => err!("Invalid argument type")
    }
    Ok(())
}

fn encode_cmov(op: X64Asm, buffer: &mut Vec<u8>) -> Result<(), String> {
    if let Arg::None = op.arg3 { } else {
        err!("Too many arguments");
    }
    match op.arg2 {
        Arg::None | Arg::Immediate {..} => err!("Invalid argument type"),
        rm => {
            let reg = match op.arg1 {
                Arg::Direct {reg} => reg as u8,
                _ => err!("Invalid argument type")
            };

            let code = match op.op {
                Opcode::Cmovz => 0x44,
                Opcode::Cmovl => 0x4C,
                _ => unimplemented!()
            };
            return push_reg_rm(buffer, &[0x0F, code], reg, rm, 1);
        }
    }
}

fn encode_cqo(op: X64Asm, buffer: &mut Vec<u8>) -> Result<(), String> {
    if let Arg::None = op.arg1 { } else {
        err!("Too many arguments");
    }
    buffer.push(0x99);
    Ok(())
}

fn encode_ret(op: X64Asm, buffer: &mut Vec<u8>) -> Result<(), String> {
    match op.arg2 {
        Arg::None => (),
        _ => err!("Too many arguments")
    }
    match op.arg1 {
        Arg::Immediate {value} => {
            buffer.push(0xC2);
            if !fits!(value, u16) {
                err!("Argument out of range");
            }
            push_16(buffer, value as u16);
        },
        Arg::None => buffer.push(0xC3),
        _ => err!("Wrong argument type")
    }
    Ok(())
}

fn encode_add(op: X64Asm, buffer: &mut Vec<u8>) -> Result<(), String> {
    if let Arg::None = op.arg3 { } else {
        err!("Too many arguments");
    }
    match op.arg2 {
        Arg::Immediate {value} => {
            try!(push_reg_rm(buffer, &[if fits!(value, i8) {0x83} else {0x81}], 0, op.arg1, 1));

            if fits!(value, i8) {
                buffer.push(value as u8);
            } else if fits!(value, i32) {
                push_32(buffer, value as u32);
            } else {
                err!("Immediate too large");
            }
        },
        Arg::Direct {reg} => {
            try!(push_reg_rm(buffer, &[0x01], reg as u8, op.arg1, 1));
        },
        rm @ Arg::Scaled   {..} |
        rm @ Arg::Indirect {..} => if let Arg::Direct {reg} = op.arg1 {
            try!(push_reg_rm(buffer, &[0x03], reg as u8, rm, 1));
        } else {
            err!("Invalid argument type");
        },
        Arg::None => err!("Too few arguments")
    }
    Ok(())
}

fn encode_sub(op: X64Asm, buffer: &mut Vec<u8>) -> Result<(), String> {
    // Similar to Add, just other opcodes
    if let Arg::None = op.arg3 { } else {
        err!("Too many arguments");
    }
    match op.arg2 {
        Arg::Immediate {value} => {
            try!(push_reg_rm(buffer, &[if fits!(value, i8) {0x83} else {0x81}], 5, op.arg1, 1));

            if fits!(value, i8) {
                buffer.push(value as u8);
            } else if fits!(value, i32) {
                push_32(buffer, value as u32);
            } else {
                err!("Immediate too large");
            }
        },
        Arg::Direct {reg} => {
            try!(push_reg_rm(buffer, &[0x29], reg as u8, op.arg1, 1));
        },
        rm @ Arg::Scaled   {..} |
        rm @ Arg::Indirect {..} => if let Arg::Direct {reg} = op.arg1 {
            try!(push_reg_rm(buffer, &[0x2B], reg as u8, rm, 1));
        } else {
            err!("Invalid argument type");
        },
        Arg::None => err!("Too few arguments")
    }
    Ok(())
}

fn encode_imul(op: X64Asm, buffer: &mut Vec<u8>) -> Result<(), String> {
    match op.arg3 {
        Arg::Immediate {value} => {
            let reg = match op.arg1 {
                Arg::Direct {reg} => reg as u8,
                _ => err!("Invalid argument type")
            };

            try!(push_reg_rm(buffer, &[if fits!(value, i8) {0x6B} else {0x69}], reg, op.arg2, 1));

            if fits!(value, i8) {
                buffer.push(value as u8);
            } else if fits!(value, i32) {
                push_32(buffer, value as u32);
            } else {
                err!("Immediate too large");
            }

            return Ok(());
        },
        Arg::None => (),
        _ => err!("Invalid argument type")
    }
    match op.arg2 {
        Arg::None => (),
        arg2 @ Arg::Immediate {..} => return encode_imul(X64Asm {op: op.op, arg1: op.arg1.clone(), arg2: op.arg1, arg3: arg2}, buffer),
        rm => {
            let reg = match op.arg1 {
                Arg::Direct {reg} => reg as u8,
                _ => err!("Invalid argument type")
            };

            return push_reg_rm(buffer, &[0x0F, 0xAF], reg, rm, 1);
        }
    }
    push_reg_rm(buffer, &[0xF7], 5, op.arg1, 1)
}

fn encode_idiv(op: X64Asm, buffer: &mut Vec<u8>) -> Result<(), String> {
    if let Arg::None = op.arg2 { } else {
        err!("Too many arguments");
    }
    push_reg_rm(buffer, &[0xF7], 7, op.arg1, 1)
}

fn encode_cmp(op: X64Asm, buffer: &mut Vec<u8>) -> Result<(), String> {
    // This is very similar to the ADD opcode
    if let Arg::None = op.arg3 { } else {
        err!("Too many arguments");
    }
    match op.arg2 {
        Arg::Immediate {value} => {
            try!(push_reg_rm(buffer, &[if fits!(value, i8) {0x83} else {0x81}], 7, op.arg1, 1));

            if fits!(value, i8) {
                buffer.push(value as u8);
            } else if fits!(value, i32) {
                push_32(buffer, value as u32);
            } else {
                err!("Immediate too large");
            }
        },
        Arg::Direct {reg} => {
            try!(push_reg_rm(buffer, &[0x39], reg as u8, op.arg1, 1));
        },
        rm @ Arg::Scaled   {..} |
        rm @ Arg::Indirect {..} => if let Arg::Direct {reg} = op.arg1 {
            try!(push_reg_rm(buffer, &[0x3B], reg as u8, rm, 1));
        } else {
            err!("Invalid argument type");
        },
        Arg::None => err!("Too few arguments")
    }
    Ok(())
}

fn encode_push(op: X64Asm, buffer: &mut Vec<u8>) -> Result<(), String> {
    // This is very similar to the ADD opcode
    if let Arg::None = op.arg2 { } else {
        err!("Too many arguments");
    }
    match op.arg1 {
        Arg::Immediate {value} => {
            buffer.push(rex!(1, 0, 0));
            if fits!(value, i8) {
                buffer.push(0x6A);
                buffer.push(value as u8);
            } else if fits!(value, i32) {
                buffer.push(0x68);
                push_32(buffer, value as u32);  
            } else {
                err!("Immediate too large");
            }
        },
        Arg::Direct {reg} => {
            let reg = reg as u8;
            if reg > 7 {
                buffer.push(rex!(0, 0, reg));
            }
            buffer.push(0x50 + (reg & 7));
        },
        rm @ Arg::Scaled   {..} |
        rm @ Arg::Indirect {..} => {
            try!(push_reg_rm(buffer, &[0xFF], 6, rm, 0));
        },
        Arg::None => err!("Too few arguments")
    }
    Ok(())
}

fn encode_pop(op: X64Asm, buffer: &mut Vec<u8>) -> Result<(), String> {
    // This is very similar to the ADD opcode
    if let Arg::None = op.arg2 { } else {
        err!("Too many arguments");
    }
    match op.arg1 {
        Arg::Immediate {..} => {
            err!("Invalid argument type");
        },
        Arg::Direct {reg} => {
            let reg = reg as u8;
            if reg > 7 {
                buffer.push(rex!(0, 0, reg));
            }
            buffer.push(0x58 + (reg & 7));
        },
        rm @ Arg::Scaled   {..} |
        rm @ Arg::Indirect {..} => {
            try!(push_reg_rm(buffer, &[0x8F], 0, rm, 0));
        },
        Arg::None => err!("Too few arguments")
    }
    Ok(())
}

fn encode_call(op: X64Asm, buffer: &mut Vec<u8>) -> Result<(), String> {
    // This is very similar to the ADD opcode
    if let Arg::None = op.arg2 { } else {
        err!("Too many arguments");
    }
    match op.arg1 {
        Arg::Immediate {value} => {
            if !fits!(value, i32) {
                err!("Immediate value too large");
            }
            buffer.push(0xE8);
            push_32(buffer, value as u32);
        },
        rm @ Arg::Direct   {..} |
        rm @ Arg::Scaled   {..} |
        rm @ Arg::Indirect {..} => {
            try!(push_reg_rm(buffer, &[0xFF], 2, rm, 0));
        },
        Arg::None => err!("Too few arguments")
    }
    Ok(())
}

fn encode_jcc(op: X64Asm, buffer: &mut Vec<u8>) -> Result<(), String> {
    if let Arg::None = op.arg2 { } else {
        err!("Too many arguments");
    }
    match op.arg1 {
        Arg::Immediate {value} => {
            if !fits!(value, i32) {
                err!("Immediate value too large");
            }
            buffer.push(0x0F);
            buffer.push(match op.op {
                Opcode::Jz  => 0x84,
                Opcode::Jnz => 0x85,
                Opcode::Jl  => 0x8C,
                Opcode::Jge => 0x8D,
                Opcode::Jle => 0x8E,
                Opcode::Jg  => 0x8F,
                _ => unreachable!()
            });
            push_32(buffer, value as u32);
        },
        Arg::None => err!("Too few arguments"),
        _ => err!("Invalid argument type")
    }
    Ok(())
}

/*
 * entry point
 */
pub fn encode(op: X64Asm, buffer: &mut Vec<u8>) -> Result<(), String> {
    match op.op {
        Opcode::Mov   => encode_mov( op, buffer),
        Opcode::Lea   => encode_lea( op, buffer),
        Opcode::Cmovl |
        Opcode::Cmovz => encode_cmov(op, buffer),
        Opcode::Cqo   => encode_cqo( op, buffer),
        Opcode::Ret   => encode_ret( op, buffer),
        Opcode::Add   => encode_add( op, buffer),
        Opcode::Sub   => encode_sub( op, buffer),
        Opcode::Imul  => encode_imul(op, buffer),
        Opcode::Idiv  => encode_idiv(op, buffer),
        Opcode::Cmp   => encode_cmp( op, buffer),
        Opcode::Push  => encode_push(op, buffer),
        Opcode::Pop   => encode_pop( op, buffer),
        Opcode::Call  => encode_call(op, buffer),
        Opcode::Jz    | 
        Opcode::Jnz   | 
        Opcode::Jl    | 
        Opcode::Jge   | 
        Opcode::Jle   | 
        Opcode::Jg    => encode_jcc( op, buffer)
    }
}

pub struct CheckPoint {
    offset: usize
}

impl CheckPoint {
    pub fn new(ops: &Vec<u8>) -> CheckPoint {
        CheckPoint { offset: ops.len() }
    }

    pub fn patch(&self, ops: &mut Vec<u8>) {
        let dest = (ops.len() - self.offset) as i32;
        let slice: [u8; 4] = unsafe { mem::transmute(dest) };
        &ops[(self.offset - 4)..self.offset].copy_from_slice(&slice);
    }
}
