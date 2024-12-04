use std::str;
use std::rc::Rc;
use std::ops::Range;

use program::{Program, Command, Integer, BigInteger, SizedInteger, SourceLoc};
use ::WsError;
use ::WsErrorKind::ParseError;

impl Program {
    /// Disassemble a program into a human-readable whitespace assembly.
    pub fn disassemble(&self) -> String {
        use program::Command::*;

        let mut buffer = String::new();
        for (index, command) in self.commands.iter().enumerate() {
            buffer.push_str(match *command {
                Push {..} =>           "    push  ",
                PushBig {..} =>        "    push  ",
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
                    format!("_{:>04}:", index)
                });
            }
            match *command {
                Push {value: x} => buffer.push_str(&x.to_string()),
                PushBig {value: ref x} => buffer.push_str(&x.to_string()),
                Copy {index: x} => buffer.push_str(&x.to_string()),
                Slide {amount: x} => buffer.push_str(&x.to_string()),
                Call {index: x} |
                Jump {index: x} |
                JumpIfZero {index: x} |
                JumpIfNegative {index: x} => buffer.push_str(
                    &if let Some(ref locs) = self.locs {
                        let label = locs[index].label.as_ref().unwrap();
                        format!("{}", label)
                    } else {
                        format!("_{:>04}", x)
                    }),
                _ => ()
            };
            buffer.push_str("\n");
        }
        buffer
    }

    /// Parse a program written in whitespace assembly into a program.
    pub fn assemble(source: String) -> Result<Program, WsError> {
        // this is a bit more complex parser, we can't parse it in one go, need to tokenize
        let (commands, locs) = {
            let tokens = TokenizerState::tokenize(&source)?;
            let node = parse(&source, &tokens)?;
            compile(node)?
        };
        let mut program = Program {
            source: Some(source.into_bytes()),
            commands: commands,
            locs: Some(locs),
            source_is_whitespace: false
        };
        program.compile()?;
        Ok(program)
    }
}

/*
 * Code below tokenizes the input string
 */

#[derive(Debug, Clone)]
struct Token<'a> {
    data: TokenType<'a>,
    loc:  TextLoc
}

#[derive(Debug, Clone)]
enum TokenType<'a> {
    Name    {value: &'a str},
    Integer {value: SizedInteger},
    Newline,
    Comment,
    Colon,
    Comma,
    End
}

#[derive(Debug, Clone)]
struct TextLoc {
    line:   usize,
    column: usize,
    span:   Range<usize>,
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
    fn tokenize<'b>(source: &'b str) -> Result<Vec<Token<'b>>, WsError> {
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
            // consume whitespace
            let item = match state.item {
                Some(c) if c.is_ascii_whitespace() && c != '\n' => {
                    state.next();
                    continue;
                },
                Some(c) => c,
                None    => break
            };
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
                            Some(c) if c.is_ascii_whitespace() => continue,
                            _                                  => break
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
                'a'..='z' | 'A'..='Z' | '_' => {
                    loop {
                        match state.next() {
                            Some('a'..='z') |
                            Some('A'..='Z') |
                            Some('_') |
                            Some('0'..='9') => continue,
                            _               => break
                        }
                    }
                    TokenType::Name {value: &source[start.0 .. state.index]}
                },
                '0'..='9' | '-' => {
                    loop {
                        match state.next() {
                            Some('0'..='9') => continue,
                            _               => break
                        }
                    }
                    TokenType::Integer {
                        value: if let Ok(value) = source[start.0 .. state.index].parse::<Integer>() {
                            SizedInteger::Small(value)
                        } else if let Ok(value) = source[start.0 .. state.index].parse::<BigInteger>() {
                            SizedInteger::Big(value)
                        } else {
                            unreachable!()
                        }
                    }
                },
                x => return Err(WsError::new(
                    ParseError(state.line, state.column, state.index),
                    format!("Unrecognized symbol {}", x)
                ))
            };
            tokens.push(Token {
                loc: TextLoc {
                    line: start.1,
                    column: start.2,
                    span: start.0 .. state.index
                },
                data: data
            });
        }
        tokens.push(Token {
            loc: TextLoc {
                line: state.line,
                column: state.column,
                span: state.index .. state.index
            },
            data: TokenType::End
        });
        Ok(tokens)
    }
}

/*
 * Code below constructs an Abstract Syntax Tree from the Token Stream
 */

#[derive(Debug, Clone)]
struct Node<'a> {
    data: NodeType<'a>,
    loc: TextLoc
}

#[derive(Debug, Clone)]
enum NodeType<'a> {
    Root       {nodes: Vec<Node<'a>>},
    Label      {name: &'a str},
    Op         {name: &'a str, args: Vec<Node<'a>>},
    Name       {name: &'a str},
    Integer    {value: SizedInteger},
}

impl<'a> NodeType<'a> {
    fn as_type(&self) -> &'static str {
        match *self {
            NodeType::Name    {..} => "Name",
            NodeType::Label   {..} => "Label",
            NodeType::Integer {..} => "Integer",
            _ => unreachable!()
        }
    }
}

#[derive(Debug)]
enum ParseResult<'a: 'b, 'b> {
    Match(Node<'a>, &'b [Token<'a>]),
    Err(WsError),
    None
}

macro_rules! parse {
    ($f:ident, $t:ident) => (
        match $f($t) {
            ParseResult::Err(s) => return ParseResult::Err(s),
            ParseResult::Match(n, t) => {$t = t; Some(n)},
            ParseResult::None => None
        }
    );
    ($f:ident, $s:ident, $t:ident) => (
        match $f($s, $t) {
            ParseResult::Err(s) => return ParseResult::Err(s),
            ParseResult::Match(n, t) => {$t = t; Some(n)},
            ParseResult::None => None
        }
    );
}

macro_rules! token {
    ($m:pat) => (Some(&Token {data: $m, ..}));
    ($m:pat, $l:ident) => (Some(&Token {data: $m, loc: ref $l}));
}

fn parse<'a, 'b>(source: &str, tokens: &'b [Token<'a>]) -> Result<Node<'a>, WsError> {
    match parse_root(source, tokens) {
        ParseResult::Err(s)         => Err(s),
        ParseResult::Match(node, _) => Ok(node),
        ParseResult::None           => unreachable!()
    }
}

fn parse_root<'a, 'b>(source: &str, mut tail: &'b [Token<'a>]) -> ParseResult<'a, 'b> {
    // matches line (NEWLINE + line) END
    // where line = Label * op ? COMMENT ?
    let mut nodes = Vec::new();
    loop {
        while let Some(n) = parse!(parse_label, tail) {
            nodes.push(n);
        }
        if let Some(n) = parse!(parse_op, source, tail) {
            nodes.push(n);
        }
        if let token!(TokenType::Comment) = tail.get(0) {
            tail = &tail[1 ..];
        }

        let items = tail.len();
        while let token!(TokenType::Newline) = tail.get(0) {
            tail = &tail[1 ..];
        }

        if let token!(TokenType::End, l) = tail.get(0) {
            return ParseResult::Match(
                Node {
                    data: NodeType::Root {nodes: nodes},
                    loc: l.clone()
                },
                &tail[1..]
            );
        }
        // we should have hit end or parsed at least one newline. if we didn't do either our state should be the same.
        if items == tail.len() {
            let loc = &tail[0].loc;
            return ParseResult::Err(WsError::new(
                ParseError(loc.line, loc.column, loc.span.start),
                format!("Expected newline at {}", &source[loc.span.clone()])
            ));
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

fn parse_op<'a, 'b>(source: &str, mut tail: &'b [Token<'a>]) -> ParseResult<'a, 'b> {
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
                let loc = &tail[0].loc;
                return ParseResult::Err(WsError::new(
                    ParseError(loc.line, loc.column, loc.span.start),
                    format!("Expected argument at {}", &source[loc.span.clone()])
                ));
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
    if let token!(TokenType::Integer {ref value}, l) = tail.get(0) {
        ParseResult::Match(
            Node {
                data: NodeType::Integer {value: value.clone()},
                loc: l.clone()
            },
            &tail[1..]
        )
    } else {
        ParseResult::None
    }
}

/*
 * Code below deals with compiling the AST in a program
 */

macro_rules! validate_type {
    ($s:expr, $p:pat => $rv:expr, $a:ident[$i:expr], $loc:expr) => (
        if let $p = $a[$i].data {
            $rv
        } else {
            return Err(WsError::new(
                ParseError($loc.line, $loc.column, $loc.span.start),
                format!("Argument {} type mismatch, expected {}, got {}", $i + 1, $s, $a[$i].data.as_type())
            ));
        }
    );
}

fn validate_args(name: &str, args: &[Node], nargs: usize, loc: &TextLoc) -> Result<(), WsError> {
    if args.len() != nargs {
        Err(WsError::new(
            ParseError(loc.line, loc.column, loc.span.start),
            format!("opcode {} called with {} arguments while expecting {}", name, args.len(), nargs)
        ))
    } else {
        Ok(())
    }
}

fn compile<'a>(root: Node<'a>) -> Result<(Vec<Command>, Vec<SourceLoc>), WsError> {
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
                    validate_args(name, args, 1, &node.loc)?;
                    match *validate_type!("Integer", NodeType::Integer {ref value} => value, args[0], &node.loc) {
                        SizedInteger::Big(ref value) => Command::PushBig {value: value.clone()},
                        SizedInteger::Small(value) => Command::Push {value: value}
                    }
                },
                "dup"  => {
                    validate_args(name, args, 0, &node.loc)?;
                    Command::Duplicate
                },
                "copy" => {
                    validate_args(name, args, 1, &node.loc)?;
                    let value = match *validate_type!("Integer", NodeType::Integer {ref value} => value, args[0], &node.loc) {
                        SizedInteger::Small(value) => value,
                        SizedInteger::Big(ref value) => return Err(WsError::new(
                            ParseError(node.loc.line, node.loc.column, node.loc.span.start),
                            format!("Copy argument too large: {}", value)
                        ))
                    };
                    if value < 0 {
                        return Err(WsError::new(
                            ParseError(node.loc.line, node.loc.column, node.loc.span.start),
                            format!("Negative copy argument: {}", value)
                        ))
                    }
                    Command::Copy {index: value as usize}
                },
                "swap" => {
                    validate_args(name, args, 0, &node.loc)?;
                    Command::Swap
                },
                "pop"  => {
                    validate_args(name, args, 0, &node.loc)?;
                    Command::Discard
                },
                "slide" => {
                    validate_args(name, args, 1, &node.loc)?;
                    let value = match *validate_type!("Integer", NodeType::Integer {ref value} => value, args[0], &node.loc) {
                        SizedInteger::Small(value) => value,
                        SizedInteger::Big(ref value) => return Err(WsError::new(
                            ParseError(node.loc.line, node.loc.column, node.loc.span.start),
                            format!("Slide argument too large: {}", value)
                        ))
                    };
                    if value < 0 {
                        return Err(WsError::new(
                            ParseError(node.loc.line, node.loc.column, node.loc.span.start),
                            format!("Negative slide argument: {}", value)
                        ))
                    }
                    Command::Slide {amount: value as usize}
                },
                "add"  => {
                    validate_args(name, args, 0, &node.loc)?;
                    Command::Add
                },
                "sub"  => {
                    validate_args(name, args, 0, &node.loc)?;
                    Command::Subtract
                },
                "mul"  => {
                    validate_args(name, args, 0, &node.loc)?;
                    Command::Multiply
                },
                "div"  => {
                    validate_args(name, args, 0, &node.loc)?;
                    Command::Divide
                },
                "mod"  => {
                    validate_args(name, args, 0, &node.loc)?;
                    Command::Modulo
                },
                "set"  => {
                    validate_args(name, args, 0, &node.loc)?;
                    Command::Set
                },
                "get"  => {
                    validate_args(name, args, 0, &node.loc)?;
                    Command::Get
                },
                "lbl"  => {
                    validate_args(name, args, 1, &node.loc)?;
                    let name = validate_type!("Name", NodeType::Name {name} => name, args[0], &node.loc).as_bytes();
                    label = Some(name.into());
                    Command::Label
                },
                "call" => {
                    validate_args(name, args, 1, &node.loc)?;
                    let name = validate_type!("Name", NodeType::Name {name} => name, args[0], &node.loc).as_bytes();
                    label = Some(name.into());
                    Command::Call {index: 0}
                },
                "jmp"  => {
                    validate_args(name, args, 1, &node.loc)?;
                    let name = validate_type!("Name", NodeType::Name {name} => name, args[0], &node.loc).as_bytes();
                    label = Some(name.into());
                    Command::Jump {index: 0}
                },
                "jz"   => {
                    validate_args(name, args, 1, &node.loc)?;
                    let name = validate_type!("Name", NodeType::Name {name} => name, args[0], &node.loc).as_bytes();
                    label = Some(name.into());
                    Command::JumpIfZero {index: 0}
                },
                "jn"   => {
                    validate_args(name, args, 1, &node.loc)?;
                    let name = validate_type!("Name", NodeType::Name {name} => name, args[0], &node.loc).as_bytes();
                    label = Some(name.into());
                    Command::JumpIfNegative {index: 0}
                },
                "ret"  => {
                    validate_args(name, args, 0, &node.loc)?;
                    Command::EndSubroutine
                },
                "exit" => {
                    validate_args(name, args, 0, &node.loc)?;
                    Command::EndProgram
                },
                "pchr" => {
                    validate_args(name, args, 0, &node.loc)?;
                    Command::PrintChar
                },
                "pnum" => {
                    validate_args(name, args, 0, &node.loc)?;
                    Command::PrintNum
                },
                "ichr" => {
                    validate_args(name, args, 0, &node.loc)?;
                    Command::InputChar
                },
                "inum" => {
                    validate_args(name, args, 0, &node.loc)?;
                    Command::InputNum
                },
                op     => return Err(WsError::new(
                    ParseError(node.loc.line, node.loc.column, node.loc.span.start),
                    format!("Unrecognized opcode {}", op)
                ))
            },
            _ => unreachable!()
        };
        commands.push(command);
        locs.push(SourceLoc {
            line: node.loc.line,
            column: node.loc.column,
            span: node.loc.span.clone(),
            label: label.map(Rc::new)
        });
    }
    Ok((commands, locs))
}
