use crate::expr::{Expr, LiteralType, Stmt};
use crate::interpreter::Interpreter;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::utils::pretty_print_literal;

use std::io;
use std::io::{stdout, Read, Stdout, Write};
use std::rc::Rc;
use termion::color::{self, Green};

use termion::style;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::environment::Environment;

pub struct Repl {
    lexer: Lexer,
    parser: Parser,
    interpreter: Interpreter,
    print_tokens: bool,
    print_ast: bool,
    stdout: Stdout,
    env: Option<Rc<Environment>>,
    rl: Editor<()>,
    history_file: Option<std::path::PathBuf>,
}

impl Repl {
    pub fn new(print_tokens: bool, print_ast: bool) -> Self {
        Self {
            lexer: Lexer::new(""),
            parser: Parser::new(vec![], "".to_owned()),
            interpreter: Interpreter::new(true, Environment::new()),
            print_tokens,
            print_ast,
            stdout: stdout(),
            env: Some(Rc::new(Environment::new())),
            rl: Editor::new(),
            history_file: None,
        }
    }

    pub fn load_history(&mut self, path: &str) -> Result<(), ReadlineError> {
        self.history_file = Some(std::path::PathBuf::from(&path));
        self.rl.load_history(&self.history_file.clone().unwrap())?;
        Ok(())
    }

    pub fn run(&mut self) {
        let env = self.env.take().unwrap();

        self.env = Some(self.read(env));
    }

    fn read(&mut self, mut env: Rc<Environment>) -> Rc<Environment> {
        // let mut line = String::new();
        // self.print_arrow();
        // for c in stdin.keys() {
        //     match c.unwrap() {
        //         Key::Left => print!("{}", cursor::Left(1)),
        //         Key::Right => print!("{}", cursor::Right(1)),
        //         Key::Char('\n') => {
        //             self.evaluate(line.clone());
        //             line.clear();
        //             self.print_arrow();
        //         }
        //         Key::Char(c) => line.push(c),
        //         c => (),
        //     }
        //     self.stdout.flush();
        // }

        let prompt_arrow = format!(
            "{}{}>{}{} ",
            style::Bold,
            color::Fg(Green),
            color::Fg(color::Reset),
            style::Reset
        );

        let mut int_count = 0;

        loop {
            match self.rl.readline(&prompt_arrow) {
                Ok(line) => {
                    self.rl.add_history_entry(&line);
                    self.evaluate(line);
                    int_count = 0;
                }
                Err(ReadlineError::Eof) => {
                    break;
                }
                Err(ReadlineError::Interrupted) => {
                    int_count += 1;
                    if int_count >= 2 {
                        if let Some(path) = self.history_file.clone() {
                            if let Err(err) = self.rl.save_history(&path) {
                                println!("Error saving history: {:?}", err);
                            }
                        }
                        break;
                    } else {
                        println!("(Press Ctrl+C again or Ctrl+D to exit)")
                    }
                }
                Err(err) => {
                    println!("{err}");
                    int_count = 0;
                }
            }
        }

        env
    }

    fn evaluate(&mut self, src: String) {
        self.lexer.reset(&src);
        let tokens = self.lexer.tokenise().clone();
        if self.print_tokens {
            println!("tokens: {tokens:#?}");
        }
        self.parser.reset(tokens.to_vec(), src);
        if let Ok(ast) = self.parser.parse() {
            if self.print_ast {
                println!("ast: \n{ast:#?}");
            }
            self.interpreter.interpret(&ast);
        }
    }

    fn print_literal(&mut self, literal: LiteralType) {
        pretty_print_literal(literal);
        self.stdout.flush().unwrap();
    }

    fn print_arrow(&mut self) {
        print!(
            "{}{}>{}{} ",
            style::Bold,
            color::Fg(Green),
            color::Fg(color::Reset),
            style::Reset
        );
        self.stdout.flush().unwrap();
    }
}
