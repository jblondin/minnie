extern crate minnie;

use std::io::{self, BufRead, Write};

use minnie::lex::Lexer;

type Result = ::std::result::Result<(), String>;

struct Repl {}

impl Repl {
    fn new() -> Repl { Repl {} }
    fn start<R, W, E>(&self, mut cin: R, mut cout: W, mut cerr: E) -> Result
            where R: BufRead, W: Write, E: Write {
        let mut running = true;
        let stdout_errstr = "unable to write to stdout";
        let stderr_errstr = "unable to write to stderr";
        let stdout_flush_errstr = "unable to flush stdout";
        while running {
            write!(cout, ">> ").map_err(|e| format!("{}: {}", stdout_errstr, e))?;
            cout.flush().map_err(|e| format!("{}: {}", stdout_flush_errstr, e))?;
            let mut input = String::new();
            match cin.read_line(&mut input) {
                Ok(_) => {
                    if input.len() == 0 {
                        running = false;
                        writeln!(cout).map_err(|e| format!("{}: {}", stdout_errstr, e))?;
                        continue;
                    }
                    match Lexer::lex(&input) {
                        Ok(tokens) => {
                            for token in &tokens {
                                writeln!(cout, "{:?}", token).map_err(|e| format!("{}: {}",
                                    stdout_errstr, e))?;
                            }
                        },
                        Err(e) => {
                            writeln!(cout, "{}", e).map_err(|e| format!("{}: {}",
                                    stdout_errstr, e))?;
                        }
                    }
                }
                Err(error) => {
                    writeln!(cerr, "Error: {}", error).map_err(|e| format!("{}: {}", stderr_errstr,
                        e))?;
                }
            }
        }
        Ok(())
    }
}

fn main() {
    println!("Friya version 0.0.1");
    let (stdin, stdout, stderr) = (io::stdin(), io::stdout(), io::stderr());
    let result = Repl::new().start(stdin.lock(), stdout.lock(), stderr.lock());

    match result {
        Ok(_) => { ::std::process::exit(0); },
        Err(e) => {
            writeln!(io::stderr(), "REPL error: {}", e).unwrap();
            ::std::process::exit(1);
        }
    }
}
