use std::io::{BufRead, Read, Write, self};
use std::fs::File;
use std::string::ToString;
use std::env;

mod label;
mod parser;
mod interpreter;
mod assembler;
use interpreter::{Interpreter, Program};

#[derive(Debug, Clone)]
struct Args {
    program: String,        // this is where we read the program from.
    input: Option<String>,  // this is where we read input for the program from. if None, stdin
    output: Option<String>, // this is where we output data to. if None, stdin
    format: FileFormat,     // format of input file. default is Whitespace
    action: Action          // output format. translate or execute. 
}

#[derive(Debug, Clone)]
enum FileFormat {
    Whitespace,
    Assembly
}

#[derive(Debug, Clone)]
enum Action {
    Translate,
    Execute
}

fn main() {
    match console_main() {
        Ok(()) => (),
        Err(s) => write!(io::stderr(), "{}\n", s).unwrap()
    }
}

fn console_main() -> Result<(), String> {
    let args = try!(parse_args());

    
    let data = {
        let mut file = try!(File::open(&args.program).map_err(|e| e.to_string()));

        let mut data = String::new();

        try!(file.read_to_string(&mut data).map_err(|e| e.to_string()));

        data
    };

    let program = try!(match args.format {
        FileFormat::Whitespace => Program::parse(data.as_bytes()),
        FileFormat::Assembly => Program::assemble(&data)
    });

    let mut output: Box<Write> = if let Some(path) = args.output {
        Box::new(try!(File::create(&path).map_err(|e| e.to_string())))
    } else {
        Box::new(io::stdout())
    };

    match args.action {
        Action::Translate => try!(
            match args.format {
                FileFormat::Whitespace => output.write_all(program.disassemble().as_bytes()),
                FileFormat::Assembly   => output.write_all(program.dump().as_slice())
            }.map_err(|e| e.to_string())
        ),
        Action::Execute => {
            let input: Box<BufRead> = if let Some(path) = args.input {
                Box::new(io::BufReader::new(
                    try!(File::open(&path).map_err(|e| e.to_string()))
                ))
            } else {
                Box::new(io::BufReader::new(
                    io::stdin()
                ))
            };
            let mut interpreter = Interpreter::new(program, input, output);
            try!(interpreter.run());
        }
    }
    Ok(())
}

macro_rules! try_opt {
    ($o:expr, $e:expr) => (
        match $o {
            Some(x) => x,
            None => return Err($e)
        }
    )
}

fn parse_args() -> Result<Args, String> {;
    let mut input = None;
    let mut output = None;
    let mut format = None;
    let mut action = Action::Execute;

    let mut args = env::args();
    let mut pos_args = Vec::new();
    // discard executable name
    args.next();

    // sort out args and kwargs (also parsing kwargs)
    loop {
        match args.next() {
            Some(arg) => match arg.as_ref() {
                "-i" | "--input" => if let Some(_) = input {
                    return Err("Option --input was specified twice".to_string());
                } else {
                    input = Some(try_opt!(args.next(), "Missing argument to --input".to_string()));
                },
                "-o" | "--output" => if let Some(_) = output {
                    return Err("Option --output was specified twice".to_string());
                } else {
                    output = Some(try_opt!(args.next(), "Missing argument to --output".to_string()));
                },
                "-f" | "--format" => if let Some(_) = format {
                    return Err("Option --format was specified twice".to_string());
                } else {
                    format = match try_opt!(args.next(), "Missing argument to --format".to_string()).as_ref() {
                        "whitespace" | "ws" => Some(FileFormat::Whitespace),
                        "assembly" | "asm"  => Some(FileFormat::Assembly),
                        f => return Err(format!("Unrecognized input format {}", f))
                    };
                },
                "-t" | "--translate" => if let Action::Translate = action {
                    return Err("Option --translate was specified twice".to_string())
                } else {
                    action = Action::Translate;
                },
                "--" => {
                    pos_args.extend(args);
                    break;
                },
                _ => if arg.chars().next() == Some('-') {
                    return Err(format!("Unrecognized option {}", arg));
                } else {
                    pos_args.push(arg);
                }
            },
            None => break
        }
    }

    // parse positional args
    let mut pos_args = pos_args.into_iter();
    let program = try_opt!(pos_args.next(), "Missing required positional argument 'program'".to_string());
    if let Some(x) = pos_args.next() {
        return Err(format!("Unexpected positional argument {}", x));
    }

    return Ok(Args {
        program: program,
        input: input,
        output: output,
        format: format.unwrap_or(FileFormat::Whitespace),
        action: action
    })
}
