# A [whitespace](https://web.archive.org/web/20150623025348/http://compsoc.dur.ac.uk/whitespace/) JIT compiler, written in [Rust](https://www.rust-lang.org/)

## Features

- Provides both a library for embedding and a command-line tool.
- Implements the whole whitespace standard, including arbitrary precision integers.
- Extremely fast. Whitespace code can be compiled to near-native speed levels. Executing over a billion whitespace instructions per second is common.
- Provides options for interpretation for unclear parts of the standard.
- Can convert whitespace code to/from a readable assembly language.

## Why

- I needed a project to learn Rust with.
- Every language needs a whitespace interpreter!
- I ended up overdoing it a bit.

## Benchmarks:

Whitespacers offers several interpretation methods. These benchmarks show the difference between the techniques and fallbacks. These benchmarks were created by executing [wsinterws.ws](https://github.com/hostilefork/whitespacers/blob/master/whitespace/wsinterws.ws) (a whitespace interpreter written in whitespace) on [this](https://web.archive.org/web/20150612005338/http://compsoc.dur.ac.uk/whitespace/quine-copy.ws) whitespace program. Correct execution requires the execution of 3.329.985.013 whitespace instructions. Executing this program requires --unchecked-heap.

The time mentioned is purely the execution time. Other operations are very insignificant compared to it as the second largest time consumer is parsing at approximately 0.001 s.

The used machine for benchmarking is a 2.6 GHz i7-4720HQ.

Execution tactic                | Time [s]      | whitespace instructions per second
:-------------------------------|--------------:|----------------------------------:
The fallback bignum interpreter | 193.514444790 |    17.207.940
The reference interpreter       | 35.265381255  |    94.426.457
Optimized interpretation        | 12.077388375  |   275.720.620
JIT-compilation                 | 3.311839939   | 1.005.478.849

It should however be noted that this benchmark is heap-heavy code. In pure stack code speeds in excess of 3.000.000.000 instructions per second have been reached.

## Documentation

Documentation is available [here](https://censoredusername.github.io/whitespace-rs/whitespacers/index.html).

## Build instructions

Just run `cargo run --bin wsc --release` to run the interpreter. The library can also be downloaded directly from [crates.io](https://crates.io/crates/whitespacers).

## License

MPL-2.0, see LICENSE
