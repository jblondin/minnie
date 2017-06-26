extern crate minnie;

use std::io::{self, BufRead, Write};

use minnie::lex::Lexer;
use minnie::parse::Parser;

type Result = ::std::result::Result<(), String>;

struct Repl<R, W, E> {
    input: String,

    cin: R,
    cout: W,
    cerr: E,

    pub print_tokens: bool,
    pub print_parsed_statements: bool,
    pub print_evaluation: bool,
}
const STDOUT_ERRSTR: &str = "unable to write to stdout";
const STDERR_ERRSTR: &str = "unable to write to stderr";
const STDOUT_FLUSH_ERRSTR: &str = "unable to flush stdout";

impl<R, W, E> Repl<R, W, E> where R: BufRead, W: Write, E: Write {
    fn new(cin: R, cout: W, cerr: E) -> Repl<R, W, E> {
        Repl {
            input: String::new(),

            cin: cin,
            cout: cout,
            cerr: cerr,

            print_tokens: false,
            print_parsed_statements: true,
            print_evaluation: false,
        }
    }

    fn start(&mut self) -> Result {
        let mut running = true;
        while running {
            write!(self.cout, ">> ").map_err(|e| format!("{}: {}", STDOUT_ERRSTR, e))?;
            self.cout.flush().map_err(|e| format!("{}: {}", STDOUT_FLUSH_ERRSTR, e))?;
            self.input = String::new();
            match self.cin.read_line(&mut self.input) {
                Ok(_) => {
                    if self.input.len() == 0 {
                        running = false;
                        writeln!(self.cout).map_err(|e| format!("{}: {}", STDOUT_ERRSTR, e))?;
                        continue;
                    }
                    self.read_eval_print()?;
                }
                Err(error) => {
                    writeln!(self.cerr, "Error: {}", error).map_err(|e| format!("{}: {}",
                        STDERR_ERRSTR, e))?;
                }
            }
        }
        Ok(())
    }

    fn read_eval_print(&mut self) -> Result {
        match Lexer::lex(&self.input) {
            Ok(tokens) => {
                if self.print_tokens {
                    for token in &tokens {
                        writeln!(self.cout, "{:?}", token).map_err(|e| format!("{}: {}",
                            STDOUT_ERRSTR, e))?;
                    }
                }
                match Parser::parse(tokens) {
                    Ok(program) => {
                        if self.print_parsed_statements {
                            writeln!(self.cout, "{:?}", program).map_err(|e| format!("{}: {}",
                                STDOUT_ERRSTR, e))?;
                        }
                    },
                    Err(e) => {
                        writeln!(self.cout, "{}", e).map_err(|e| format!("{}: {}", STDOUT_ERRSTR,
                            e))?
                    }
                }
            },
            Err(e) => {
                writeln!(self.cout, "{}", e).map_err(|e| format!("{}: {}", STDOUT_ERRSTR, e))?;
            }
        }
        Ok(())
    }
}

fn main() {
    println!("Minnie version 0.0.1");
    let (stdin, stdout, stderr) = (io::stdin(), io::stdout(), io::stderr());
    let result = Repl::new(stdin.lock(), stdout.lock(), stderr.lock()).start();

    match result {
        Ok(_) => { ::std::process::exit(0); },
        Err(e) => {
            writeln!(io::stderr(), "REPL error: {}", e).unwrap();
            ::std::process::exit(1);
        }
    }
}
