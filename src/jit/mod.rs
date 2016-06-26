mod x64assembler;
mod memory;

use std::mem;
use std::{i32, i64};
use std::collections::HashMap;

use interpreter::{Interpreter, InterpreterState, Command, CommandType, Integer};
use self::x64assembler::{Opcode, Arg, Register, X64Asm, encode, CheckPoint};
use self::memory::Memory;

#[derive(Debug)]
pub struct JitContext {
    mem: Memory,
    chunks: HashMap<usize, JitData>
}

#[derive(Debug)]
pub struct JitData {
    index: usize,
    min_stack: isize,
    max_stack: isize,
    offset: usize
}

// but we can have it a bit easier. the following is how it is represented in asm
// (we limit ourselves to long mode ins)

// some jit impl ideas

pub fn jit(commands: &[Command]) -> Result<JitContext, String> {
    use interpreter::CommandType::*;
    // things easily jittable:
    // push dup copy swap pop slide
    // add sub mul div mod

    // somewhat more interesting things, probably doable (jz and jn cause control flow joins on the true branch)
    // jz jn set get pchr

    // things very hard to jit nicely (or just not very relevant to jit) :
    // pnum ichr inum exit

    // things which halt the jit afterwards as they are possibly control flow joins
    // j call label ret
    let mut ops = Vec::new();

    let mut jit_chunks = Vec::new();
    let mut labels = HashMap::new(); // index: (jit_chunk_index, op_offset)

    let mut iter = commands.iter().map(|c| &c.data).enumerate().peekable();

    while !iter.peek().is_none() {
        // skip things that aren't jittable and things which end the jit (only jitting one op is rather high on overhead)
        // also, don't start the jit at instructions that can fail. These instructions fall back to interpreting themselves
        // but this requires them to jump to their own address from the jit. This doesn't work if a jit block starts at their pos.
        let index;
        let &(i, c) = iter.peek().unwrap();
        match c {
            &Push {..}  |
            &Duplicate  |
            &Swap       |
            &Discard    |
            &Slide {..} |
            &Add        |
            &Subtract   |
            &Multiply   |
            &Set        |
            &JumpIfZero {..}  |
            &JumpIfNegative {..} |
            &PrintChar  => index = i,
            _  => {
                iter.next();
                continue;
            }
        };

        // prepare a jit block
        let mut stack_pos = 0;
        let mut min_stack = 0;
        let mut max_stack = 0;

        let mut next = true;
        let offset = ops.len();

        // prologue
        op!(ops += Mov [%RSP, 0x20], %R9  );
        op!(ops += Mov [%RSP, 0x18], %R8  );
        op!(ops += Mov [%RSP, 0x8 ], %RCX );

        // start the jit
        while next {
            let (i, c) = if let Some(&x) = iter.peek() {
                x
            } else {
                op!(ops += Mov %RAX, commands.len() );
                op!(ops += Add [%R9], stack_pos     );
                op!(ops += Ret                      );
                break;
            };

            let offset = stack_pos * 8 - 8;

            /*
             * Register usage in the jit
             * RAX: scratch reg. used as return reg. the return value is the index of the next instruction to go to
             * RCX: contains a pointer to the top of the stack as from when the jit started.
             * RDX: scratch reg. used for swap, mul, div etc.
             * R8:  contains a pointer to the interpreter struct. used for runtime functions
             * R9:  pointer to an i64 holding the length of the stack when the jit started
             */

            match c {
                // easily jittable
                &Push {value} => {
                    if value <= i32::MAX as Integer && value >= i32::MIN as Integer {
                        op!(ops += Mov [%RCX, offset + 8], value    );
                    } else {
                        op!(ops += Mov %RAX, value                  );
                        op!(ops += Mov [%RCX, offset + 8], %RAX     );
                    }
                    iter.next();
                },
                &Duplicate => {
                    op!(ops += Mov %RAX, [%RCX, offset]         );
                    op!(ops += Mov [%RCX, offset + 8], %RAX     );
                    iter.next();
                },
                &Swap => {
                    op!(ops += Mov %RAX, [%RCX, offset]         );
                    op!(ops += Mov %RDX, [%RCX, offset - 8]     );
                    op!(ops += Mov [%RCX, offset], %RDX         );
                    op!(ops += Mov [%RCX, offset - 8], %RAX     );
                    iter.next();
                },
                &Copy {index} => {
                    // adjust min_stack if necessary
                    let stack_depth_needed = stack_pos - index as isize;
                    if stack_depth_needed < min_stack {
                        min_stack = stack_depth_needed
                    }
                    op!(ops += Mov %RAX, %RCX                   );
                    op!(ops += Sub %RAX, [%R9 * 8]              );
                    op!(ops += Mov %RAX,  [%RAX, index * 8]     );
                    op!(ops += Mov [%RCX, offset + 8], %RAX     );
                },
                &Discard => {
                    iter.next();
                },
                &Slide {amount} => {
                    let amount = amount as isize;
                    op!(ops += Mov %RAX, [%RCX, offset]         );
                    op!(ops += Mov [%RCX, offset - amount], %RAX);
                    iter.next();
                },
                &Add => {
                    op!(ops += Mov %RAX, [%RCX, offset]         );
                    op!(ops += Add [%RCX, offset - 8], %RAX     );
                    iter.next();
                },
                &Subtract => {
                    op!(ops += Mov %RAX, [%RCX, offset]         );
                    op!(ops += Sub [%RCX, offset - 8],  %RAX    );
                    iter.next();
                },
                &Multiply => {
                    op!(ops += Mov %RAX, [%RCX, offset]         );
                    op!(ops += Imul [%RCX, offset - 8]          );
                    op!(ops += Mov [%RCX, offset - 8], %RAX     );
                    iter.next();
                },
                &Divide => {
                    op!(ops += Cmp [%RCX, offset], 0            );
                    op!(ops += Jnz 0x77777777                   );
                    let check = CheckPoint::new(&ops);
                    op!(ops += Mov %RAX, i                      );
                    op!(ops += Add [%R9], stack_pos             );
                    op!(ops += Ret                              );
                    check.patch(&mut ops);
                    op!(ops += Mov %RAX, [%RCX, offset - 8]     );
                    op!(ops += Cqo                              );
                    op!(ops += Idiv [%RCX, offset]              );
                    op!(ops += Mov [%RCX, offset - 8], %RAX     );
                    iter.next();
                },
                &Modulo => {
                    op!(ops += Cmp [%RCX, offset], 0            );
                    op!(ops += Jnz 0x77777777                   );
                    let check = CheckPoint::new(&ops);
                    op!(ops += Mov %RAX, i                      );
                    op!(ops += Add [%R9], stack_pos             );
                    op!(ops += Ret                              );
                    check.patch(&mut ops);
                    op!(ops += Mov %RAX, [%RCX, offset - 8]     );
                    op!(ops += Cqo                              );
                    op!(ops += Idiv [%RCX, offset]              );
                    op!(ops += Mov [%RCX, offset - 8], %RDX     );
                    iter.next();
                },
                &Set => {
                    let func = ws_set as i64;
                    op!(ops += Mov %RAX, func                   );
                    // we decrement by 0x28 as to keep the stack properly aligned in a frame with no base pointer. (32 bytes of space for the callee, 8 bytes for the return address)
                    op!(ops += Sub %RSP, 0x28                   );
                    op!(ops += Mov %RDX, [%RCX, offset]         ); // value
                    op!(ops += Mov %RCX, [%RCX, offset - 8]     ); // key
                    op!(ops += Call %RAX                        );
                    op!(ops += Add %RSP, 0x28                   );
                    op!(ops += Mov %RCX, [%RSP, 0x8]            );
                    op!(ops += Mov %R8,  [%RSP, 0x18]           );
                    op!(ops += Mov %R9,  [%RSP, 0x20]           );
                    iter.next();
                },
                &Get => {
                    let func = ws_get as i64;
                    op!(ops += Mov %RAX, func                   );
                    op!(ops += Sub %RSP, 0x28                   );
                    op!(ops += Lea %RCX, [%RCX, offset]         ); // *key and *value
                    op!(ops += Call %RAX                        );
                    op!(ops += Add %RSP, 0x28                   );
                    op!(ops += Mov %RCX, [%RSP, 0x8]            );
                    op!(ops += Mov %R8,  [%RSP, 0x18]           );
                    op!(ops += Mov %R9,  [%RSP, 0x20]           );
                    // bail out if the key wasn't found
                    op!(ops += Cmp %RAX, 0                      );
                    op!(ops += Jz 0x77777777                    );
                    let check = CheckPoint::new(&ops);
                    op!(ops += Mov %RAX, i                      );
                    op!(ops += Add [%R9], stack_pos             );
                    op!(ops += Ret                              );
                    check.patch(&mut ops);
                    iter.next();
                },
                // these end the jit but we do consume them.
                &Label => {
                    // enter the label location into the locmap
                    labels.insert(i, ops.len());
                    op!(ops += Mov %RAX, i + 1                  );
                    op!(ops += Add [%R9], stack_pos             );
                    op!(ops += Ret                              );
                    iter.next();
                    next = false;
                },
                &Call {ref index} => {
                    let func = ws_call as i64;
                    op!(ops += Mov %RAX, func                   );
                    op!(ops += Sub %RSP, 0x28                   );
                    op!(ops += Mov %RCX, i + 1                  );
                    op!(ops += Call %RAX                        );
                    op!(ops += Add %RSP, 0x28                   );
                    op!(ops += Mov %RAX, index.get()            );
                    op!(ops += Mov %R9,  [%RSP, 0x20]           );
                    op!(ops += Add [%R9], stack_pos             );
                    op!(ops += Ret                              );
                    iter.next();
                    next = false;
                },
                &Jump {ref index} => {
                    op!(ops += Mov %RAX, index.get()            );
                    op!(ops += Add [%R9], stack_pos             );
                    op!(ops += Ret                              );
                    iter.next();
                    next = false;
                },
                &JumpIfZero {ref index} => {
                    op!(ops += Cmp [%RCX, offset], 0            );
                    op!(ops += Jnz 0x77777777                   );
                    let check = CheckPoint::new(&ops);
                    op!(ops += Mov %RAX, index.get()            );
                    op!(ops += Add [%R9], stack_pos - 1         );
                    op!(ops += Ret                              );
                    check.patch(&mut ops);
                    iter.next();
                },
                &JumpIfNegative {ref index} => {
                    op!(ops += Cmp [%RCX, offset], 0            );
                    op!(ops += Jge 0x77777777                   );
                    let check = CheckPoint::new(&ops);
                    op!(ops += Mov %RAX, index.get()            );
                    op!(ops += Add [%R9], stack_pos - 1         );
                    op!(ops += Ret                              );
                    check.patch(&mut ops);

                    iter.next();
                },
                &EndSubroutine => {
                    let func = ws_ret as i64;
                    op!(ops += Mov %RAX, func                   );
                    op!(ops += Sub %RSP, 0x28                   );
                    op!(ops += Mov %RCX, i                      );
                    op!(ops += Call %RAX                        );
                    op!(ops += Add %RSP, 0x28                   );
                    op!(ops += Mov %RCX, [%RSP, 0x8]            );
                    op!(ops += Mov %R8,  [%RSP, 0x18]           );
                    op!(ops += Mov %R9,  [%RSP, 0x20]           );
                    op!(ops += Add [%R9], stack_pos             );
                    op!(ops += Ret                              );
                    iter.next();
                    next = false;
                },
                &PrintChar => {
                    let func = ws_pchar as i64;
                    op!(ops += Mov %RAX, func                   );
                    op!(ops += Sub %RSP, 0x28                   );
                    op!(ops += Mov %RCX, [%RCX, offset]         ); // char
                    op!(ops += Call %RAX                        );
                    op!(ops += Add %RSP, 0x28                   );
                    op!(ops += Mov %RCX, [%RSP, 0x8]            );
                    op!(ops += Mov %R8,  [%RSP, 0x18]           );
                    op!(ops += Mov %R9,  [%RSP, 0x20]           );
                    iter.next();
                },
                &PrintNum => {
                    let func = ws_pnum as i64;
                    op!(ops += Mov %RAX, func                   );
                    op!(ops += Sub %RSP, 0x28                   );
                    op!(ops += Mov %RCX, [%RCX, offset]         ); // num
                    op!(ops += Call %RAX                        );
                    op!(ops += Add %RSP, 0x28                   );
                    op!(ops += Mov %RCX, [%RSP, 0x8]            );
                    op!(ops += Mov %R8,  [%RSP, 0x18]           );
                    op!(ops += Mov %R9,  [%RSP, 0x20]           );
                    iter.next();
                },
                // Any other command currently just ends the jit
                _ => {
                    op!(ops += Mov %RAX, i                      );
                    op!(ops += Add [%R9], stack_pos             );
                    op!(ops += Ret                              );
                    break;
                }
            }
            
            let (change, extra) = stack_effect(c);
            stack_pos += change;
            let stack_bot = stack_pos - extra;

            if stack_pos > max_stack { max_stack = stack_pos; }
            if stack_bot < min_stack { min_stack = stack_bot; }
        }

        jit_chunks.push(JitData {
            index: index,
            min_stack: min_stack,
            max_stack: max_stack,
            offset: offset
        });
    }

    // move ops to executable memory
    let mut mem = Memory::new(ops.len());
    &mut mem.copy_from_slice(&ops);
    mem.make_executable();

    Ok(JitContext {
        mem: mem,
        chunks: jit_chunks.into_iter().map(|c| (c.index, c)).collect()
    })
}

fn stack_effect(c: &CommandType) -> (isize, isize) {
    use interpreter::CommandType::*;
    match c {
        &Push {..} => ( 1, 1),
        &Duplicate => ( 1, 2),
        &Copy {..} => ( 1, 1), // last one does not take the source in account for Copy but that one is not w.r.t. top of the stack
        &Swap      => ( 0, 2),
        &Discard   => (-1, 0),
        &Slide {amount} => (-(amount as isize), 1),
        &Add      |
        &Subtract |
        &Multiply |
        &Divide   |
        &Modulo   => (-1, 1),
        &Get      => ( 0, 1),
        &Set      => (-2, 0),
        &Label     |
        &Call {..} |
        &Jump {..} => (0, 0),
        &JumpIfZero {..}     |
        &JumpIfNegative {..} => (-1, 0),
        &EndSubroutine |
        &EndProgram    => (0, 0),
        &PrintChar |
        &PrintNum  => (-1, 0),
        &InputChar |
        &InputNum  => ( 0, 1)
    }
}

impl<'a> Interpreter<'a> {
    pub fn jit(&mut self) -> Result<(), String> {

        let ctx = try!(jit(&self.program.commands));

        while let InterpreterState::Ready = self.state.get() {

            if let Some(chunk) = ctx.chunks.get(&self.instruction_counter) {

                // check space on the stack
                if self.stack.len() >= (-chunk.min_stack) as usize {

                    self.stack.reserve(chunk.max_stack as usize);

                    let mut len = self.stack.len() as isize;

                    unsafe {

                        let stacktop  = self.stack.as_mut_ptr().offset(len) as *mut Integer     as isize;
                        let interp    = self                                as *mut Interpreter as isize;
                        let stacklenp = &mut len                            as *mut isize       as isize;

                        let f: extern "win64" fn(isize, isize, isize, isize) -> usize = mem::transmute(ctx.mem.offset(chunk.offset));

                        self.instruction_counter = f(stacktop, 0, interp, stacklenp) as usize; // passing 0 in RDX as it's only used as scratch
                        self.stack.set_len(len as usize);
                    }

                    continue;
                }
            }
            try!(self.step());
        }

        Ok(())
    }
}

/*
 * Runtime support
 */

// note: using the win64 calling convention as rustc currently does not expose the sysV convention on windows (annoying as this means RDX is the 2nd arg instaed of the fourth)
extern "win64" fn ws_set(key: isize, value: isize, interp: *mut Interpreter) {
    unsafe { &mut *interp }.heap.insert(key, value);
}

extern "win64" fn ws_get(keyval: *mut isize, _: isize, interp: *mut Interpreter) -> isize {
    unsafe {
        if let Some(&x) = (*interp).heap.get(&*keyval) {
            *keyval = x;
            0
        } else {
            -1
        }
    }
}

extern "win64" fn ws_call(retloc: usize, _: isize, interp: *mut Interpreter) {
    unsafe { &mut *interp }.callstack.push(retloc);
}

extern "win64" fn ws_ret(ownaddr: usize, _: isize, interp: *mut Interpreter) -> usize {
    unsafe { &mut *interp }.callstack.pop().unwrap_or(ownaddr) as usize
}

extern "win64" fn ws_pchar(c: isize, _: isize, interp: *mut Interpreter) {
    // todo: do we want to panic here? We're technically panicing into foreign code here.
    let c: [u8; 1] = [c as u8];
    unsafe { &mut *interp }.output.write_all(&c).expect("Could not write to output");
}

extern "win64" fn ws_pnum(n: isize, _: isize, interp: *mut Interpreter) {
    unsafe { &mut *interp }.output.write_all(&n.to_string().as_bytes()).expect("Could not write to output");
}
