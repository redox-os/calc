extern crate calc;

use std::fmt;
use std::env::args;
use std::process::exit;

use std::io::{self, stdin, stdout, BufRead, Write};

use calc::{eval, eval_polish, CalcError};

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
        // Print out the prompt before we try to read the first line of stdin
        prompt(&mut stdout)?;
        let stdin = stdin();
        if polish {
            for line in stdin.lock().lines() {
                match line?.trim() {
                    "" => (),
                    "exit" => break,
                    s => writeln!(stdout, "{}", eval_polish(s)?)?,
                }
                prompt(&mut stdout)?;
            }
        } else {
            for line in stdin.lock().lines() {
                match line?.trim() {
                    "" => (),
                    "exit" => break,
                    s => writeln!(stdout, "{}", eval(s)?)?,
                }
                prompt(&mut stdout)?;
            }
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
