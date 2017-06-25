extern crate calc;

use std::env::args;
use std::process::exit;

use std::io::{stdout, stdin, Write};

use calc::{eval, CalcError};

pub fn calc(args: Vec<String>) -> Result<(), CalcError> {
    let stdout = stdout();
    let mut stdout = stdout.lock();
    if !args.is_empty() {
        writeln!(stdout, "{}", eval(&args.join(""))?)?;
    } else {
        let prompt = b"[]> ";
        loop {
            let _ = stdout.write(prompt)?;
            let mut input = String::new();
            stdin().read_line(&mut input)?;
            if input.is_empty() {
                break;
            } else {
                match input.trim() {
                    "" => (),
                    "exit" => break,
                    s => writeln!(stdout, "{}", eval(s)?)?,
                }
            }
        }
    }
    Ok(())
}

fn main() {
    let code = match calc(args().collect()) {
        Ok(()) => 0,
        Err(e) => {
            println!("Encountered error: {}", String::from(e));
            1
        }
    };
    exit(code)
}
