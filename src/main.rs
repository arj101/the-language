#![feature(let_else)]

mod environment;
mod expr;
mod interpreter;
mod lexer;
mod parser;
mod repl;
mod tokens;
mod utils;

use environment::EnvVal;
use lexer::Lexer;
use std::fs;

use std::rc::Rc;

use crate::environment::Environment;
use crate::{interpreter::Interpreter, parser::Parser};

use repl::Repl;

fn main() {
    let mut args = std::env::args().collect::<Vec<String>>();
    args.remove(0);

    let show_ast = args.contains(&"--show-ast".to_string()); 
    let show_tokens = args.contains(&"--show-tokens".to_string()); 
    let verbose_output = args.contains(&"--verbose".to_string());
    let tracker_info = args.contains(&"--track-parser".to_string());

    let args_filename: Vec<&String> = args.iter().filter(|s| !s.starts_with("--")).collect();

    if args_filename.is_empty() {
        let mut repl = Repl::new(show_tokens, show_ast);

        if let Err(err) = repl.load_history("./.repl_history") {
            if let Ok(_) = std::fs::File::open("./.repl_history") {
                println!("Error loading repl history: {:?}", err);
            }
        }
        repl.run();
    } else {
        let mut lexer = Lexer::new("");
        let mut parser = Parser::new(vec![], "".to_owned());
        let global_env = Environment::new();
        let mut interpreter = Interpreter::new(false, global_env);

        for filename in args_filename {
            match fs::read_to_string(filename) {
                Ok(src) => {
                    let border_len_top = isize::max(48 - (filename.len() as isize), 0) as usize;

                    if verbose_output {
                        println!("`{filename}`{}",  "=".repeat(border_len_top))
                    }

                    lexer.reset(&src);
                    let tokens = lexer.tokenise().clone();

                    if show_tokens {
                        println!("tokens:\n{tokens:#?}\n")
                    }

                    parser.reset(tokens, src);
                    if let Ok(ast) = parser.parse() {
                        if show_ast {
                            println!("ast:\n{ast:#?}\n")
                        }
                        if tracker_info {
                            parser.print_tracker_info();
                        }

                        if verbose_output {
                            println!("running interpreter:")
                        }

                        interpreter.interpret(&ast);
                    }

                    if verbose_output {
                        println!("{}", "=".repeat(50))
                    }
                }
                Err(e) => {
                    println!("Error reading file `{filename}`: {e}");
                }
            }
        }
    }
}
