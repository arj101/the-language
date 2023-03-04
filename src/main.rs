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

    let show_ast = if args.contains(&"--show-ast".to_owned()) {
        args.retain(|s| s != &"--show-ast");
        true
    } else {
        false
    };

    let show_tokens = if args.contains(&"--show-tokens".to_owned()) {
        args.retain(|s| s != &"--show-ast");
        true
    } else {
        false
    };

    let verbose_output = if args.contains(&"--verbose".to_owned()) {
        args.retain(|s| s != &"--show-ast");
        true
    } else {
        false
    };

    let tracker_info = if args.contains(&"--track-parser".to_owned()) {
        true
    } else {
        false
    };

    let args_filename: Vec<&String> = args.iter().filter(|s| !s.starts_with("--")).collect();

    if args_filename.is_empty() {
        let mut repl = Repl::new(show_tokens, show_ast);
        repl.run();
    } else {
        let mut lexer = Lexer::new("");
        let mut parser = Parser::new(vec![], "".to_owned());
        let mut global_env = Environment::new();
        let mut interpreter = Interpreter::new(false, global_env);

        for filename in args_filename {
            match fs::read_to_string(filename) {
                Ok(src) => {
                    let border_len_top = if 48 - (filename.len() as isize) < 0 {
                        0
                    } else {
                        48 - filename.len()
                    };
                    if verbose_output {
                        println!("`{}`{}", filename, "=".repeat(border_len_top))
                    }
                    lexer.reset(&src);
                    let tokens = lexer.tokenise().clone();
                    if show_tokens && verbose_output {
                        println!("tokens:\n{tokens:#?}\n")
                    }
                    parser.reset(tokens, src);
                    if let Ok(ast) = parser.parse() {
                        if show_ast && verbose_output {
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

    // if args.len() == 1 {
    //     let stdin = io::stdin();
    //     let stdin = stdin.lock();
    //     let mut stdout = stdout();

    //     print!(
    //         "{}{}>{}{} ",
    //         Bold,
    //         color::Fg(color::Green),
    //         color::Fg(Reset),
    //         style::Reset
    //     );

    //     stdout.flush();
    //     for line in stdin.lines() {
    //         lexer.reset(line.unwrap());
    //         let mut parser = Parser::new(lexer.tokenise().clone());
    //         let expr = parser.parse();
    //         if show_ast {
    //             println!("{:?}", expr);
    //             println!();
    //         }
    //         let interpreter = Interpreter::new(expr);
    //         match interpreter.evaluate() {
    //             LiteralType::Number(n) => {
    //                 println!("{}{}{}", color::Fg(color::LightYellow), n, color::Fg(Reset))
    //             }
    //             LiteralType::Null => println!("{}null{}", color::Fg(color::Red), color::Fg(Reset)),
    //             LiteralType::Bool(b) => {
    //                 println!("{}{}{}", color::Fg(color::Cyan), b, color::Fg(Reset))
    //             }
    //             LiteralType::Str(StrType::Strict(s)) => {
    //                 println!("{}\"{}\"{}", color::Fg(color::Green), s, color::Fg(Reset))
    //             }
    //             LiteralType::Str(StrType::Loose(s)) => println!(
    //                 "{}'{}'{}",
    //                 color::Fg(color::LightGreen),
    //                 s,
    //                 color::Fg(Reset)
    //             ),
    //         }
    //         print!(
    //             "{}{}>{}{} ",
    //             Bold,
    //             color::Fg(color::Green),
    //             color::Fg(Reset),
    //             style::Reset
    //         );

    //         stdout.flush();
    //     }
    // } else if args.len() == 2 {
    //     let file = std::fs::read_to_string(&args[1]).unwrap();
    //     lexer.reset(file);
    //     println!("{:#?}", lexer.tokenise());
    // } else {
    //     eprintln!("Invalid number of arguments.");
    // }
}
