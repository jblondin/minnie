extern crate rustyline;
extern crate minnie;

use std::io::{self, Write};

use rustyline::{CompletionType, Editor};
use rustyline::completion::FilenameCompleter;
use rustyline::error::ReadlineError;

use minnie::lex::Lexer;
use minnie::parse::Parser;
use minnie::eval::Evaluator;


mod result { pub type Result<T> = ::std::result::Result<T, String>; }
type Result = result::Result<()>;

struct Repl<W, E> {
    editor: Editor<FilenameCompleter>,

    cout: W,
    cerr: E,

    pub print_tokens: bool,
    pub print_parsed_statements: bool,
    pub print_value: bool,

    evaluator: Evaluator,
}
const HISTORY_FILE: &str = "repl_history.txt";
const PROMPT: &str = ">> ";
const STDOUT_ERRSTR: &str = "unable to write to stdout";
const STDERR_ERRSTR: &str = "unable to write to stderr";

impl<W, E> Repl<W, E> where W: Write, E: Write {
    fn new(mut cout: W, cerr: E) -> Repl<W, E> {
        Repl {
            editor: {
                let config = rustyline::Config::builder()
                    .history_ignore_space(true)
                    .completion_type(CompletionType::List)
                    .build();
                let completer = FilenameCompleter::new();
                let mut editor = Editor::with_config(config);
                editor.set_completer(Some(completer));
                if editor.load_history(HISTORY_FILE).is_err() {
                    writeln!(cout, "No previous history.").expect(STDOUT_ERRSTR);
                }
                editor
            },

            cout: cout,
            cerr: cerr,

            print_tokens: false,
            print_parsed_statements: true,
            print_value: true,

            evaluator: Evaluator::new(),
        }
    }

    fn start(&mut self) -> Result {
        let mut running = true;
        while running {
            let line = self.editor.readline(PROMPT);
            match line {
                Ok(line) => {
                    self.editor.add_history_entry(line.as_ref());
                    self.read_eval_print(line)?;
                },
                Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                    running = false;
                },
                Err(err) => {
                    writeln!(self.cerr, "Read error: {}", err)
                        .map_err(|e| format!("{}: {}", STDERR_ERRSTR, e))?;
                }
            }
        }
        self.editor.save_history(HISTORY_FILE)
            .map_err(|e| format!("Unable to save history: {}", e))?;
        Ok(())
    }

    fn read_eval_print(&mut self, input: String) -> Result {
        match Lexer::lex(&input) {
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
                        let value = self.evaluator.evaluate(program);
                        if self.print_value {
                            writeln!(self.cout, "{:?}", value).map_err(|e| format!("{}: {}",
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
    let (stdout, stderr) = (io::stdout(), io::stderr());
    let result = Repl::new(stdout.lock(), stderr.lock()).start();

    match result {
        Ok(_) => { ::std::process::exit(0); },
        Err(e) => {
            writeln!(io::stderr(), "REPL error: {}", e).unwrap();
            ::std::process::exit(1);
        }
    }
}
