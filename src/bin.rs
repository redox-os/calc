extern crate calc;
extern crate liner;

use std::fmt;
use std::env::args;
use std::process::exit;

use std::io::{self, stdout, Write};

use calc::{eval, eval_polish, CalcError};

use liner::Context;

const PROMPT: &'static str = "[]> ";

pub fn prompt<W: Write>(out: &mut W) -> io::Result<()> {
    write!(out, "{}", PROMPT)?;
    out.flush()
}

pub enum RuntimeError {
    Calc(CalcError),
    IO(io::Error),
}

impl From<CalcError> for RuntimeError {
    fn from(data: CalcError) -> RuntimeError {
        RuntimeError::Calc(data)
    }
}

impl From<io::Error> for RuntimeError {
    fn from(data: io::Error) -> RuntimeError {
        RuntimeError::IO(data)
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            RuntimeError::Calc(ref c) => write!(f, "calc: {}", c),
            RuntimeError::IO(ref e) => write!(f, "calc: {}", e),
        }
    }
}

pub fn calc(args: Vec<String>) -> Result<(), RuntimeError> {
    let stdout = stdout();
    let mut stdout = stdout.lock();

    // Check if the polish notation flag was given.
    let (polish, args) = if let Some(arg) = args.get(0) {
        if arg == "--polish" || arg == "-p" {
            (true, &args[1..])
        } else {
            (false, &args[..])
        }
    } else {
        (false, &args[..])
    };

    if !args.is_empty() {
        if polish {
            writeln!(stdout, "{}", eval_polish(&args.join(""))?)?;
        } else {
            writeln!(stdout, "{}", eval(&args.join(""))?)?;
        }
    } else {
        let mut con = Context::new();
        loop {
            let line = con.read_line(PROMPT, &mut |_| {})?;
            match line.trim() {
                "" => (),
                "exit" => break,
                s => {
                    writeln!(
                        stdout,
                        "{}",
                        if polish { eval_polish(s)? } else { eval(s)? }
                    )?
                }
            }
            con.history.push(line.into())?;
        }
    }
    Ok(())
}

fn main() {
    let code = match calc(args().skip(1).collect()) {
        Ok(()) => 0,
        Err(e) => {
            println!("{}", e);
            1
        }
    };
    exit(code)
}
