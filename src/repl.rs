use crate::expr::{Expr, LiteralType, Stmt};
use crate::interpreter::Interpreter;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::utils::pretty_print_literal;

use std::io::{self, stdin};
use std::io::{stdout, Read, Stdout, Write};
use std::rc::Rc;
use std::sync::atomic::AtomicBool;
use std::sync::mpsc::Sender;
use termion::color::{self, Green};

use termion::style;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::environment::Environment;
use std::sync::{self, mpsc, Arc};
use std::thread;

use termion::async_stdin;
use termion::cursor::DetectCursorPos;
use termion::input::{Events, TermRead};
use termion::raw::IntoRawMode;

use crate::print_raw;
use crate::println_raw;

enum Message {
    Val(String),
    Exit,
}

pub struct Repl {
    print_tokens: bool,
    print_ast: bool,
    stdout: Stdout,
    rl: Editor<()>,
    history_file: Option<std::path::PathBuf>,
    stop_signal: Arc<AtomicBool>,
    is_interpreting: Arc<AtomicBool>,
    is_active: Arc<AtomicBool>,
    tx: Sender<Message>,
}

impl Drop for Repl {
    fn drop(&mut self) {
        if let Ok(_) = self.tx.send(Message::Exit) {
            while self.is_active.load(sync::atomic::Ordering::Relaxed) {}
        }
    }
}

impl Repl {
    pub fn new(print_tokens: bool, print_ast: bool) -> Self {
        let stop_signal = sync::Arc::new(AtomicBool::new(false));
        let stop_signal_clone = stop_signal.clone();

        let is_interpreting = sync::Arc::new(AtomicBool::new(false));
        let is_interpreting_clone = is_interpreting.clone();
        let is_interpreting_clone1 = is_interpreting.clone();

        let is_active = sync::Arc::new(AtomicBool::new(true));
        let is_active_clone = is_active.clone();
        let is_active_clone1 = is_active.clone();

        let (tx, rx) = mpsc::channel::<Message>();

        thread::spawn(move || {
            let is_active = is_active_clone;
            let is_running = is_interpreting_clone;

            let result = std::panic::catch_unwind(|| {
                let is_active = is_active_clone1;
                let is_running = is_interpreting_clone1;

                let mut lexer = Lexer::new("");
                let mut parser = Parser::new(vec![], "".to_owned());
                let mut interpreter = Interpreter::new(true, Environment::new(), stop_signal_clone);

                'outer: loop {
                    if let Ok(msg) = rx.try_recv() {
                        match msg {
                            Message::Val(src) => {
                                is_running.store(true, sync::atomic::Ordering::Relaxed);

                                lexer.reset(&src);

                                let tokens = lexer.tokenise().clone();
                                if print_tokens {
                                    println!("tokens: {tokens:#?}");
                                }
                                parser.reset(tokens, src);
                                if let Ok(ast) = parser.parse() {
                                    if print_ast {
                                        println!("ast: \n{ast:#?}");
                                    }
                                    interpreter.interpret(&ast);
                                }

                                is_running.store(false, sync::atomic::Ordering::Relaxed);
                            }
                            Message::Exit => {
                                println!("bye!");
                                break 'outer;
                            }
                        }
                    }
                }
            });

            is_running.store(false, sync::atomic::Ordering::Relaxed);
            is_active.store(false, sync::atomic::Ordering::Relaxed);

            if let Err(err) = result {
                let stdout = stdout().into_raw_mode().unwrap();
                stdout.suspend_raw_mode();
                println!("Interpreter panicked with \"{err:?}\"");
                println!("bye!");
            }
        });

        Self {
            print_tokens,
            print_ast,
            stdout: stdout(),
            rl: Editor::new(),
            history_file: None,
            stop_signal,
            tx,
            is_interpreting,
            is_active,
        }
    }

    pub fn load_history(&mut self, path: &str) -> Result<(), ReadlineError> {
        self.history_file = Some(std::path::PathBuf::from(&path));
        self.rl.load_history(&self.history_file.clone().unwrap())?;
        Ok(())
    }

    pub fn run(&mut self) {
        self.read();
    }

    fn read(&mut self) {
        let prompt_arrow = format!(
            "{}{}>{}{} ",
            style::Bold,
            color::Fg(Green),
            color::Fg(color::Reset),
            style::Reset
        );

        let mut int_count = 0;

        loop {
            let val = self.rl.readline(&prompt_arrow);
            match val {
                Ok(line) => {
                    self.stop_signal
                        .store(false, sync::atomic::Ordering::Relaxed);
                    self.is_interpreting
                        .store(true, sync::atomic::Ordering::Relaxed);
                    self.rl.add_history_entry(&line);
                    self.tx.send(Message::Val(line));
                    int_count = 0;


                    {
                        let mut stdin = termion::async_stdin().keys();
                        let stdout = stdout().into_raw_mode().unwrap();
                        while self.is_interpreting.load(sync::atomic::Ordering::Relaxed) {
                            if let Some(Ok(termion::event::Key::Ctrl('c'))) = stdin.next() {
                                break;
                            }
                        }
                        stdout.suspend_raw_mode().unwrap();
                    }

                    self.stop_signal
                        .store(true, sync::atomic::Ordering::Relaxed);
                    while self.is_interpreting.load(sync::atomic::Ordering::Relaxed) {}
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
                        if self.is_interpreting.load(sync::atomic::Ordering::Relaxed) {
                            int_count = 0;
                            self.stop_signal
                                .store(true, sync::atomic::Ordering::Relaxed);
                            while self.is_interpreting.load(sync::atomic::Ordering::Relaxed) {}
                        } else {
                            println!("(Press Ctrl+C again or Ctrl+D to exit)")
                        }
                    }
                }
                Err(err) => {
                    println!("{err}");
                    int_count = 0;
                }
            }
        }
    }

    fn evaluate(&mut self, src: String) {}
}
