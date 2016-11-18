extern crate whitespacers;
extern crate byteorder;

use byteorder::{LittleEndian, ReadBytesExt};

use std::env;
use std::error::Error;
use std::fs::File;
use std::io::{Seek, SeekFrom, stdin, stdout, stderr, Write};

fn main() {
    match console_main() {
        Ok(()) => (),
        Err(s) => write!(stderr(), "Error: {}\n", s.to_string()).unwrap()
    }
}

fn console_main() -> Result<(), Box<Error>> {
    let exe_path = env::current_exe()?;
    let mut exe = File::open(exe_path)?;

    exe.seek(SeekFrom::End(-0x08))?;

    let serialized_offset = exe.read_u64::<LittleEndian>()?;

    exe.seek(SeekFrom::Start(serialized_offset))?;

    let stdin = stdin();
    whitespacers::Interpreter::jit_run_from_serialized(&mut exe, &mut stdin.lock(), &mut stdout())?;

    Ok(())
}
