# A [whitespace](https://web.archive.org/web/20150623025348/http://compsoc.dur.ac.uk/whitespace/) jit compiler, written in [Rust](https://www.rust-lang.org/)

## Features
- Fast: Able to parse and run [this program](https://web.archive.org/web/20150612005338/http://compsoc.dur.ac.uk/whitespace/quine-copy.ws) in a tenth of a second (executing 7776282 whitespace commands) on an average computer.
- Assembler: Convert whitespace code back and forth between an assembly language and whitespace.
- Safe: The program can either use a fully safe interpreter, or an unsafe jit compiler. And even then the jit compiler rigurously checks it's inputs to ensure safety.

## Why
- I needed a project to learn Rust with.
- Every language needs a whitespace interpreter!

## License
WTFPL - http://www.wtfpl.net/
