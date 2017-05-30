extern crate whitespacers;

use std::env;
use std::error::Error;
use std::io::{self, Write, Read, BufRead};
use std::fs::File;
use std::time::Instant;

use whitespacers::{Program, Interpreter, Options, IGNORE_OVERFLOW, UNCHECKED_HEAP, NO_FALLBACK, debug_compile};

#[derive(Debug, Clone)]
struct Args {
    program: String,        // this is where we read the program from.
    options: Options,       // any options to influence execution
    input: Option<String>,  // this is where we read input for the program from. if None, stdin
    output: Option<String>, // this is where we output data to. if None, stdin
    format: FileFormat,     // format of input file. default is Whitespace
    action: Action,         // output format. translate or execute.
    perf: bool,             // print perf info to stdout?
    minify: bool            // when translating, minify the code as much as possible
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum FileFormat {
    Whitespace,
    Assembly
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Action {
    Translate,
    Execute(Strategy),
    Dump(String)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Strategy {
    SimpleState,
    BigState,
    FastState,
    AoT,
    Sync,
    Async,
    Count
}

fn main() {
    match console_main() {
        Ok(()) => (),
        Err(s) => write!(io::stderr(), "Error: {}\n", s.to_string()).unwrap()
    }
}

fn console_main() -> Result<(), Box<Error>> {
    let time_start = Instant::now();

    let args = parse_args()?;

    let time_args = Instant::now();

    let data = {
        let mut file = File::open(&args.program)?;
        let mut data = String::new();
        file.read_to_string(&mut data)?;
        data
    };

    let time_input = Instant::now();

    let mut program = match args.format {
        FileFormat::Whitespace => Program::parse(data.into_bytes())?,
        FileFormat::Assembly => Program::assemble(data)?
    };

    let time_parse = Instant::now();

    let mut output: Box<Write> = if let Some(path) = args.output {
        Box::new(io::BufWriter::new(
            File::create(&path)?
        ))
    } else {
        Box::new(io::BufWriter::new(
            io::stdout()
        ))
    };

    let time_finish;

    match args.action {
        Action::Translate => {
            if args.minify {
                program.minify();
            }

            match args.format {
                FileFormat::Whitespace => output.write_all(program.disassemble().as_bytes()),
                FileFormat::Assembly   => output.write_all(program.dump().as_slice()),
            }?;

            time_finish = Instant::now();
        },
        _ => {
            let mut input: Box<BufRead> = if let Some(path) = args.input {
                Box::new(io::BufReader::new(
                    File::open(&path)?
                ))
            } else {
                Box::new(io::BufReader::new(
                    io::stdin()
                ))
            };
            match args.action {
                Action::Execute(strategy) => {
                    let mut interpreter = Interpreter::new(&program, args.options, &mut input, &mut output);
                    match strategy {
                        Strategy::SimpleState => interpreter.interpret_with_simple_state(),
                        Strategy::BigState => interpreter.interpret_with_bigint_state(),
                        Strategy::FastState => interpreter.interpret_with_fast_state(),
                        Strategy::AoT => interpreter.jit_aot(),
                        Strategy::Sync => interpreter.jit_sync(),
                        Strategy::Async => interpreter.jit_threaded(),
                        Strategy::Count => interpreter.count_with_simple_state().map(|count| println!("Executed {} instructions", count))
                    }.map_err(|e| {e.format_with_program(&program)})?;
                    time_finish = Instant::now();
                },
                Action::Dump(ref filename) => {
                    let buffer = debug_compile(&program, args.options);
                    time_finish = Instant::now();
                    let mut f = File::create(filename)?;
                    f.write_all(&buffer)?;
                },
                _ => unreachable!()
            };
        }
    }

    if args.perf {
        let duration = time_args - time_start;
        println!("Time spent parsing args: {}.{:09}", duration.as_secs(), duration.subsec_nanos());

        let duration = time_input - time_args;
        println!("Time spent reading:      {}.{:09}", duration.as_secs(), duration.subsec_nanos());

        let duration = time_parse - time_input;
        println!("Time spent parsing:      {}.{:09}", duration.as_secs(), duration.subsec_nanos());

        let duration = time_finish - time_parse;
        match args.action {
            Action::Dump(_)    => println!("Time spent compiling:    {}.{:09}", duration.as_secs(), duration.subsec_nanos()),
            Action::Execute(_) => println!("Time spent executing:    {}.{:09}", duration.as_secs(), duration.subsec_nanos()),
            Action::Translate  => println!("Time spent translating:  {}.{:09}", duration.as_secs(), duration.subsec_nanos()),
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

fn parse_args() -> Result<Args, String> {
    let mut perf = false;
    let mut minify = false;
    let mut input = None;
    let mut output = None;
    let mut format = None;
    let mut action = None;
    let mut options = Options::empty();

    let mut args = env::args();
    let mut pos_args = Vec::new();
    // discard executable name
    args.next();

    // sort out args and kwargs (also parsing kwargs)
    loop {
        match args.next() {
            Some(arg) => match arg.as_ref() {
                "-d" | "--dump" => if let Some(_) = action {
                    return Err("Option --dump, --translate, --count or --execute was specified twice".to_string());
                } else {
                    action = Some(Action::Dump(try_opt!(args.next(), "Missing argument to --dump".to_string())));
                },
                "-p" | "--perf" => if perf {
                    return Err("Option --perf was specified twice".to_string());
                } else {
                    perf = true;
                },
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
                "-t" | "--translate" => if let Some(_) = action {
                    return Err("Option --dump, --translate, --count or --execute was specified twice".to_string());
                } else {
                    action = Some(Action::Translate);
                },
                "-m" | "--minify" => if minify {
                    return Err("Option --minify was specified twice".to_string());
                } else {
                    minify = true;
                },
                "-c" | "--count" => if let Some(_) = action {
                    return Err("Option --dump, --translate, --count or --execute was specified twice".to_string());
                } else {
                    action = Some(Action::Execute(Strategy::Count));
                },
                "-e" | "--execute" => if let Some(_) = action {
                    return Err("Option --dump, --translate, --count or --execute was specified twice".to_string());
                } else {
                    action = Some(match try_opt!(args.next(), "Missing argument to --execute".to_string()).as_ref() {
                        "ref" |   "reference"   => Action::Execute(Strategy::SimpleState),
                        "big" |   "bigint"      => Action::Execute(Strategy::BigState),
                        "opt" |   "optimized"   => Action::Execute(Strategy::FastState),
                        "aot" |   "precompiled" => Action::Execute(Strategy::AoT),
                        "sync" |  "synchronous" => Action::Execute(Strategy::Sync),
                        "async" | "threaded"    => Action::Execute(Strategy::Async),
                        a => return Err(format!("Unrecognized strategy {}", a))
                    });
                },
                "--ignore-overflow" => if options.contains(IGNORE_OVERFLOW) {
                    return Err("Option --ignore-overflow was specified twice".to_string());
                } else {
                    options |= IGNORE_OVERFLOW;
                },
                "--unchecked-heap" => if options.contains(UNCHECKED_HEAP) {
                    return Err("Option --unchecked-heap was specified twice".to_string());
                } else {
                    options |= UNCHECKED_HEAP;
                },
                "--no-fallback" => if options.contains(NO_FALLBACK) {
                    return Err("Option --no-fallback was specified twice".to_string());
                } else {
                    options |= NO_FALLBACK;
                },
                "-h" | "--help" => return Err("Usage: whitespacers PROGRAM [-h | -i INFILE | -o OUTFILE | [-t | -e STRATEGY | -d DUMPFILE | -c] | -f FORMAT | -p | --ignore-overflow | --unchecked-heap | --no-fallback]

wsc - A really fast whitespace JIT-compiler.

Required arguments:
    PROGRAM                 The whitespace program to execute
Options:
    -h --help               Display this message
    -f --format  FORMAT     Input file format. The default is plain whitespace. Options are:
        ws|whitespace       Plain whitespace.
        asm|assembly        A human-readable assembly format. A description can be found below.
    -i --input   INFILE     File to read input from (defaults to stdin)
    -o --output  OUTFILE    File to write output to (defaults to stdout)
    -e --execute STRATEGY   Execute the file using specific settings. This is the default using
                             the precompiled setting. Options are as following:
        ref|reference       Use a simple reference interpreter that falls back onto a bignum
                             based interpreter.
        opt|optimized       Use the reference interpreter with optimized data structures.
        big|bigint          Use the bignum based fallback interpreter directly. This is the
                             slowest option.
        aot|precompiled     Compile the program into native code in advance, and then execute
                             it using optimized datastructures. This is the fastest for short
                             programs, or programs that have a long execution time. It falls back
                             to the optimized interpreter and bignum interpreter when the native
                             code encounters errors.
        sync|synchronous    Similar to precompiled, but this implementation compiles code it
                             encounters while interpreting. This is faster for large programs that
                             only actually execute a small part of their code.
        async|threaded      Similar to precompiled, but compiles code in a separate thread while
                             already interpreting. It is faster on large programs.
    -d --count              Use the reference interpreter with no bignum fallback to count the amount
                             of instructions executed.
    -t --translate          Instead of executing, translate the file to/from assembly, and write
                             the result to the specified output.
    -m --minify             When translating, minify the resulting code by crushing label size.
    -d --dump    DUMPFILE   Just compiles the program into assembly and dumps the result into a
                             file. This is mainly for debugging.
    -p --perf               Prints performance information to stdout.
    --ignore-overflow       Use wrapping arithmetic instead of switching to bignum-based
                             interpretation when overflow occurs.
    --unchecked-heap        By default the interpreter generates an error when a missing key is
                             requested from the heap. As the behaviour of the reference
                             implementation of this is somewhat inconsistent, this option
                             configures the interpreter to return 0 instead.
    --no-fallback           On overflow, generate an error instead of switching to a bignum
                             interpreter.

Assembly format:
    
The assembly format used by wsc is very nasm-like. It supports the following instructions:

Stack manipulation - push INTEGER, dup, swap, copy INTEGER, pop, slide INTEGER
Arithmetic         - add, sub, mul, div, mod
Heap manipulation  - get, set
Control flow       - label LABEL, call LABEL, jmp LABEL, jz LABEL, jn LABEL, ret, exit
IO                 - pnum, pchr, inum, ichr

instead of the label opcode, labels can also be declared using the more familiar \"LABEL:\" syntax.
Comments are denoted using the ; symbol. Integers can be arbitrarily sized, but decimal only. Labels
can consist out of all letters and underscore, and all but the first character can be a number.

Below is an example program that prints the fibonacci sequence to stdout:

----------------
    push 1
    push 1
loop:
    copy 0
    add
    dup
    pnum
    push 10
    pchr
    swap
    jmp loop
----------------

".to_string()),
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
    let program = try_opt!(pos_args.next(), "Missing required positional argument 'PROGRAM'".to_string());
    if let Some(x) = pos_args.next() {
        return Err(format!("Unexpected positional argument {}", x));
    }

    return Ok(Args {
        program: program,
        options: options,
        input: input,
        output: output,
        format: format.unwrap_or(FileFormat::Whitespace),
        action: action.unwrap_or(Action::Execute(Strategy::AoT)),
        perf: perf,
        minify: minify
    })
}
