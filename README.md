# calc
[![Build Status](https://travis-ci.org/redox-os/calc.svg?branch=master)](https://travis-ci.org/redox-os/calc)

`calc` is a Rust library for tokenizing and evaluating arithmetic expressions with a command line application of the same name included.

# Usage

## As a Library

Add `calc` as a dependency in your `Cargo.toml`:
```toml
[dependencies]
calc = "0.1.*"
```

Then make use of the library functions:
```rust
extern crate calc;

use calc::eval;
use std::io::{self, BufRead, stdout, stdin, Write};

fn main() {
    let stdout = stdout();
    let mut stdout = stdout.lock();
    let stdin = stdin();
    for line in stdin.lock().lines() {
        match line.unwrap().trim() {
            "" => (),
            "exit" => break,
            s => writeln!(stdout, "{}", eval(s)).unwrap(),
        }
    }
}
```

## As an Executable

Coming Soon!
