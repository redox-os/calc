extern crate calc;

use std::env::args;
use std::process::exit;

use std::io::{BufRead, stdout, stdin, Write, self};

use calc::{eval, CalcError};

const PROMPT : &'static str = "[]> ";

pub fn prompt<W : Write>(out : &mut W) -> io::Result<()> {
    write!(out, "{}", PROMPT)?;
    out.flush()
}

pub fn calc(args: Vec<String>) -> Result<(), CalcError> {
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
            println!("{}", String::from(e));
            1
        }
    };
    exit(code)
}
