extern crate calc;

use std::env::args;
use std::process::exit;

use std::io::{self, BufRead, stdout, stdin, Write};

use calc::{eval, CalcError};

const PROMPT: &'static str = "[]> ";

pub fn prompt<W: Write>(out: &mut W) -> io::Result<()> {
    write!(out, "{}", PROMPT)?;
    out.flush()
}

#[derive(Debug)]
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

pub fn calc(args: Vec<String>) -> Result<(), RuntimeError> {
    let stdout = stdout();
    let mut stdout = stdout.lock();
    if !args.is_empty() {
        writeln!(stdout, "{}", eval(&args.join(""))?)?;
    } else {
        // Print out the prompt before we try to read the first line of stdin
        prompt(&mut stdout)?;
        let stdin = stdin();
        for line in stdin.lock().lines() {
            match line?.trim() {
                "" => (),
                "exit" => break,
                s => writeln!(stdout, "{}", eval(s)?)?,
            }
            prompt(&mut stdout)?;
        }
    }
    Ok(())
}

fn main() {
    let code = match calc(args().skip(1).collect()) {
        Ok(()) => 0,
        Err(e) => {
            println!("{:?}", e);
            1
        }
    };
    exit(code)
}
