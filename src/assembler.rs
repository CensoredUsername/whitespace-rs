use std::str;
use std::fmt;
use std::rc::Rc;

use program::{Program, Command, Integer, SourceLoc};

impl<'a> Program<'a> {
    pub fn disassemble(&self) -> String {
        use program::Command::*;

        let mut buffer = String::new();
        for (index, command) in self.commands.iter().enumerate() {
            buffer.push_str(match *command {
                Push {..} =>           "    push  ",
                Duplicate =>           "    dup",
                Copy {..} =>           "    copy  ",
                Swap =>                "    swap",
                Discard =>             "    pop",
                Slide {..} =>          "    slide ",
                Add =>                 "    add",
                Subtract =>            "    sub",
                Multiply =>            "    mul",
                Divide =>              "    div",
                Modulo =>              "    mod",
                Set =>                 "    set",
                Get =>                 "    get",
                Label =>               "",
                Call {..} =>           "    call  ",
                Jump {..} =>           "    jmp   ",
                JumpIfZero {..} =>     "    jz    ",
                JumpIfNegative {..} => "    jn    ",
                EndSubroutine =>       "    ret",
                EndProgram =>          "    exit",
                PrintChar =>           "    pchr",
                PrintNum =>            "    pnum",
                InputChar =>           "    ichr",
                InputNum =>            "    inum",
            });
            if let Label = *command {
                buffer.push_str(&if let Some(ref locs) = self.locs {
                    let label = locs[index].label.as_ref().unwrap();
                    format!("{}:", label)
                } else {
                    format!("{:>04}:", index)
                });
            }
            match *command {
                Push {value: x} => buffer.push_str(&format!("{}\n", x)),
                Copy {index: x} => buffer.push_str(&format!("{}\n", x)),
                Slide {amount: x} => buffer.push_str(&format!("{}\n", x)),
                Call {index: x} |
                Jump {index: x} |
                JumpIfZero {index: x} |
                JumpIfNegative {index: x} => buffer.push_str(
                    &if let Some(ref locs) = self.locs {
                        let label = locs[index].label.as_ref().unwrap();
                        format!("{}\n", label)
                    } else {
                        format!("{:>04}\n", x)
                    }),
                _ => buffer.push_str("\n")
            };
        }
        buffer
    }

    pub fn assemble(source: &'a str) -> Result<Program<'a>, String> {
        // this is a bit more complex parser, we can't parse it in one go, need to tokenize
        let tokens = try!(TokenizerState::tokenize(source));
        let node = try!(parse(&tokens));
        let (commands, locs) = try!(compile(node));
        let mut program = Program {
            source: Some(source.as_bytes()),
            commands: commands,
            locs: Some(locs)
        };
        try!(program.compile());
        Ok(program)
    }
}

/**
 * Code below tokenizes the input string
 */

#[derive(Debug, Clone)]
struct Token<'a> {
    data: TokenType<'a>,
    loc:  TextLoc<'a>
}

#[derive(Debug, Clone)]
enum TokenType<'a> {
    Name    {value: &'a str},
    Integer {value: Integer},
    Newline,
    Comment,
    Colon,
    Comma,
    End
}

#[derive(Debug, Clone)]
struct TextLoc<'a> {
    line:   usize,
    column: usize,
    text:   &'a str
}

#[derive(Clone)]
struct TokenizerState<'a> {
    source: str::CharIndices<'a>,
    line:   usize,
    column: usize,
    index:  usize,
    item:   Option<char>
}

impl<'a> Iterator for TokenizerState<'a> {
    type Item = char;
    fn next(&mut self) -> Option<char> {
        if let Some((index, c)) = self.source.next() {
            match self.item {
                Some('\n') => {self.line   += 1; self.column = 1},
                Some(_)    => {self.column += 1},
                None => ()
            }

            self.index = index;
            self.item = Some(c);
            Some(c)
        } else {
            self.index += 1;
            self.item = None;
            None
        }
    }
}

impl<'a> TokenizerState<'a> {
    fn tokenize<'b>(source: &'b str) -> Result<Vec<Token<'b>>, String> {
        let mut tokens = Vec::new();
        let mut state = TokenizerState {
            source: source.char_indices(),
            line: 1,
            column: 1,
            index: 0,
            item: None
        };
        state.next();
        loop {
            let item;
            // consume whitespace
            match state.item {
                Some(' ') | Some('\t') => {state.next(); continue},
                Some(c)                => item = c,
                None                   => break
            }
            // match tokens
            // we can distinguish what token to match just on the starting symbol.
            let start = (state.index, state.line, state.column);
            let data = match item {
                ':'             => {
                    state.next();
                    TokenType::Colon
                },
                ','             => {
                    state.next();
                    TokenType::Comma
                },
                '\n'            => {
                    // skip ahead to the next non-whitespace so we don't emit a million newline tokens
                    loop {
                        match state.next() {
                            Some('\n') |
                            Some(' ')  |
                            Some('\t') => continue,
                            _          => break
                        }
                    }
                    TokenType::Newline
                },
                ';'             => {
                    loop {
                        match state.next() {
                            Some('\n') | None => break,
                            _                 => continue
                        }
                    }
                    TokenType::Comment
                },
                'a'...'z' | 'A'...'Z' | '_' => {
                    loop {
                        match state.next() {
                            Some('a'...'z') |
                            Some('A'...'Z') |
                            Some('_') |
                            Some('0'...'9') => continue,
                            _               => break
                        }
                    }
                    TokenType::Name {value: &source[start.0 .. state.index]}
                },
                '0'...'9' | '-' => {
                    loop {
                        match state.next() {
                            Some('0'...'9') => continue,
                            _               => break
                        }
                    }
                    TokenType::Integer {value: source[start.0 .. state.index].parse().unwrap()}
                },
                x => return Err(format!("Unrecognized symbol {} at line {}, column {}", x, state.line, state.column))
            };
            tokens.push(Token {
                loc: TextLoc {
                    line: start.1,
                    column: start.2,
                    text: &source[start.0 .. state.index]
                },
                data: data
            });
        }
        tokens.push(Token {
            loc: TextLoc {
                line: state.line,
                column: state.column,
                text: ""
            },
            data: TokenType::End
        });
        Ok(tokens)
    }
}

/**
 * Code below constructs an Abstract Syntax Tree from the Token Stream
 */

#[derive(Debug, Clone)]
struct Node<'a> {
    data: NodeType<'a>,
    loc: TextLoc<'a>
}

#[derive(Debug, Clone)]
enum NodeType<'a> {
    Root    {nodes: Vec<Node<'a>>},
    Label   {name: &'a str},
    Op      {name: &'a str, args: Vec<Node<'a>>},
    Name    {name: &'a str},
    Integer {value: Integer}
}

#[derive(Debug, Clone)]
enum ParseResult<'a: 'b, 'b> {
    Match(Node<'a>, &'b [Token<'a>]),
    Err(String),
    None
}

macro_rules! parse {
    ($f: ident, $t:ident) => (
        match $f($t) {
            ParseResult::Err(s) => return ParseResult::Err(s),
            ParseResult::Match(n, t) => {$t = t; Some(n)},
            ParseResult::None => None
        }
    )
}

macro_rules! token {
    ($m:pat) => (Some(&Token {data: $m, ..}));
    ($m:pat, $l:ident) => (Some(&Token {data: $m, loc: ref $l}));
}

fn parse<'a, 'b>(tokens: &'b [Token<'a>]) -> Result<Node<'a>, String> {
    match parse_root(tokens) {
        ParseResult::Err(s)         => Err(s),
        ParseResult::Match(node, _) => Ok(node),
        ParseResult::None           => unreachable!()
    }
}

fn parse_root<'a, 'b>(mut tail: &'b [Token<'a>]) -> ParseResult<'a, 'b> {
    // matches line (NEWLINE + line) END
    // where line = Label * op ? COMMENT ?
    let mut nodes = Vec::new();
    loop {
        while let Some(n) = parse!(parse_label, tail) {
            nodes.push(n);
        }
        if let Some(n) = parse!(parse_op, tail) {
            nodes.push(n);
        }
        if let token!(TokenType::Comment) = tail.get(0) {
            tail = &tail[1 ..];
        }

        let items = tail.len();
        while let token!(TokenType::Newline) = tail.get(0) {
            tail = &tail[1 ..];
        }

        if let token!(TokenType::End) = tail.get(0) {
            return ParseResult::Match(
                Node {
                    data: NodeType::Root {nodes: nodes},
                    loc: TextLoc {line: 1, column: 1, text: ""}
                },
                &tail[1..]
            );
        }
        // we should have hit end or parsed at least one newline. if we didn't do either our state should be the same.
        if items == tail.len() {
            return ParseResult::Err(format!("Expected newline at {}", tail[0].loc));
        }
    }
}

fn parse_label<'a, 'b>(tail: &'b [Token<'a>]) -> ParseResult<'a, 'b> {
    // matches NAME COLON

    let (name, loc) = if let token!(TokenType::Name {value}, l) = tail.get(0) {
        (value, l)
    } else {
        return ParseResult::None;
    };

    if let token!(TokenType::Colon) = tail.get(1) {
        ParseResult::Match(
            Node {
                data: NodeType::Label {name: name},
                loc: loc.clone()
            }, 
            &tail[2..]
        )
    } else {
        ParseResult::None
    }
}

fn parse_op<'a, 'b>(mut tail: &'b [Token<'a>]) -> ParseResult<'a, 'b> {
    // matches NAME (arg (COMMA arg) *) ?
    // where arg = integer | name
    let (op, loc) = if let token!(TokenType::Name {value}, l) = tail.get(0) {
        tail = &tail[1..];
        (value, l)
    } else {
        return ParseResult::None;
    };

    let mut args = Vec::new();

    let arg = if let Some(n) = parse!(parse_name, tail) {
        n
    } else if let Some(n) = parse!(parse_integer, tail) {
        n
    } else {
        return ParseResult::Match(
            Node {
                data: NodeType::Op {name: op, args: args},
                loc: loc.clone()
            },
            tail
        );
    };
    args.push(arg);

    while let token!(TokenType::Comma) = tail.get(0) {
        tail = &tail[1..];
        args.push(if let Some(n) = parse!(parse_name, tail) {
                n
            } else if let Some(n) = parse!(parse_integer, tail) {
                n
            } else {
                return ParseResult::Err("Expected argument".to_string());
            }
        );
    }
    ParseResult::Match(
        Node {
            data: NodeType::Op{name: op, args: args},
            loc: loc.clone()
        },
        tail
    )
}

fn parse_name<'a, 'b>(tail: &'b [Token<'a>]) -> ParseResult<'a, 'b> {
    // matches NAME
    if let token!(TokenType::Name {value}, l) = tail.get(0) {
        ParseResult::Match(
            Node {
                data: NodeType::Name {name: value},
                loc: l.clone()
            },
            &tail[1..]
        )
    } else {
        ParseResult::None
    }
}

fn parse_integer<'a, 'b>(tail: &'b [Token<'a>]) -> ParseResult<'a, 'b> {
    // matches INTEGER
    if let token!(TokenType::Integer {value}, l) = tail.get(0) {
        ParseResult::Match(
            Node {
                data: NodeType::Integer {value: value},
                loc: l.clone()
            },
            &tail[1..]
        )
    } else {
        ParseResult::None
    }
}

/**
 * Code below deals with compiling the AST in a program
 */

macro_rules! validate_type {
    ($p:pat => $rv:expr, $a:ident[$i:expr]) => (
        if let $p = $a[$i].data {
            $rv
        } else {
            return Err(format!("Argument {} type mismatch, expected {}, got {:?}", $i + 1, stringify!($p), $a[$i]))
        }
    );
}

fn validate_args(name: &str, args: &[Node], nargs: usize) -> Result<(), String> {
    if args.len() != nargs {
        Err(format!("opcode {} called with {} arguments while expecting {}", name, args.len(), nargs))
    } else {
        Ok(())
    }
}

fn compile<'a>(root: Node<'a>) -> Result<(Vec<Command>, Vec<SourceLoc>), String> {
    let nodes = match root {
        Node {data: NodeType::Root {nodes}, ..} => nodes,
        _ => panic!("Called compile on non-root node")
    };

    let mut commands = Vec::new();
    let mut locs = Vec::new();
    for node in &nodes {
        let mut label = None;
        let command = match node.data {
            NodeType::Label {name} => {
                let name = name.as_bytes();
                label = Some(name.into());
                Command::Label
            },
            NodeType::Op {name, ref args} => match name {
                "push" => {
                    try!(validate_args(name, args, 1));
                    Command::Push {value: validate_type!(NodeType::Integer {value} => value, args[0])}
                },
                "dup"  => {
                    try!(validate_args(name, args, 0));
                    Command::Duplicate
                },
                "copy" => {
                    try!(validate_args(name, args, 1));
                    let value = validate_type!(NodeType::Integer {value} => value, args[0]);
                    if value < 0 {
                        return Err(format!("Negative copy argument: {}", value))
                    }
                    Command::Copy {index: value as usize}
                },
                "swap" => {
                    try!(validate_args(name, args, 0));
                    Command::Swap
                },
                "pop"  => {
                    try!(validate_args(name, args, 0));
                    Command::Discard
                },
                "slide" => {
                    try!(validate_args(name, args, 1));
                    let value = validate_type!(NodeType::Integer {value} => value, args[0]);
                    if value < 0 {
                        return Err(format!("Negative slide argument: {}", value))
                    }
                    Command::Slide {amount: value as usize}
                },
                "add"  => {
                    try!(validate_args(name, args, 0));
                    Command::Add
                },
                "sub"  => {
                    try!(validate_args(name, args, 0));
                    Command::Subtract
                },
                "mul"  => {
                    try!(validate_args(name, args, 0));
                    Command::Multiply
                },
                "div"  => {
                    try!(validate_args(name, args, 0));
                    Command::Divide
                },
                "mod"  => {
                    try!(validate_args(name, args, 0));
                    Command::Modulo
                },
                "set"  => {
                    try!(validate_args(name, args, 0));
                    Command::Set
                },
                "get"  => {
                    try!(validate_args(name, args, 0));
                    Command::Get
                },
                "lbl"  => {
                    try!(validate_args(name, args, 1));
                    let name = validate_type!(NodeType::Name {name} => name, args[0]).as_bytes();
                    label = Some(name.into());
                    Command::Label
                },
                "call" => {
                    try!(validate_args(name, args, 1));
                    let name = validate_type!(NodeType::Name {name} => name, args[0]).as_bytes();
                    label = Some(name.into());
                    Command::Call {index: 0}
                },
                "jmp"  => {
                    try!(validate_args(name, args, 1));
                    let name = validate_type!(NodeType::Name {name} => name, args[0]).as_bytes();
                    label = Some(name.into());
                    Command::Jump {index: 0}
                },
                "jz"   => {
                    try!(validate_args(name, args, 1));
                    let name = validate_type!(NodeType::Name {name} => name, args[0]).as_bytes();
                    label = Some(name.into());
                    Command::JumpIfZero {index: 0}
                },
                "jn"   => {
                    try!(validate_args(name, args, 1));
                    let name = validate_type!(NodeType::Name {name} => name, args[0]).as_bytes();
                    label = Some(name.into());
                    Command::JumpIfNegative {index: 0}
                },
                "ret"  => {
                    try!(validate_args(name, args, 0));
                    Command::EndSubroutine
                },
                "exit" => {
                    try!(validate_args(name, args, 0));
                    Command::EndProgram
                },
                "pchr" => {
                    try!(validate_args(name, args, 0));
                    Command::PrintChar
                },
                "pnum" => {
                    try!(validate_args(name, args, 0));
                    Command::PrintNum
                },
                "ichr" => {
                    try!(validate_args(name, args, 0));
                    Command::InputChar
                },
                "inum" => {
                    try!(validate_args(name, args, 0));
                    Command::InputNum
                },
                op     => return Err(format!("Unrecognized opcode {}", op))
            },
            _ => unreachable!()
        };
        commands.push(command);
        locs.push(SourceLoc {
            line: node.loc.line,
            column: node.loc.column,
            text: node.loc.text.as_bytes(),
            label: label.map(Rc::new)
        });
    }
    Ok((commands, locs))
}

/**
 * formatting code
 */

impl<'a> fmt::Display for TextLoc<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "line {}, column {}: ", self.line, self.column));
        f.write_str(self.text)
    }
}
