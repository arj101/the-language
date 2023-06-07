use crate::expr::{Expr, LiteralType, Stmt};
use crate::interpreter::Interpreter;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::utils::pretty_print_literal;

use std::io::{self, stdin};
use std::io::{stdout, Read, Stdout, Write};
use std::rc::Rc;
use std::sync::atomic::AtomicBool;
use std::sync::mpsc::{channel, Sender};
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

use std::fs;

enum Message {
    Val(String),
    Exit,
}

pub struct Repl {
    print_tokens: bool,
    print_ast: bool,
    stdout: Stdout,
    rl: Editor<()>,
    stop_signal: Arc<AtomicBool>,
    is_interpreting: Arc<AtomicBool>,
    is_active: Arc<AtomicBool>,
    tx: Sender<Message>,
    history_file: Option<fs::File>,
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
                let mut interpreter = Interpreter::new(true, Environment::new(16), stop_signal_clone);

                'outer: loop {
                    if let Ok(msg) = rx.try_recv() {
                        match msg {
                            Message::Val(src) => {
                                is_running.store(true, sync::atomic::Ordering::Relaxed);

                                lexer.reset(&src);

                                let (tokens, interner) = lexer.tokenise().clone();
                                if print_tokens {
                                    println_raw!("tokens: \n{:?}", tokens);
                                }
                                parser.reset(tokens.to_vec(), src);
                                if let Ok((ast, interner)) = parser.parse(interner) {
                                    if print_ast {
                                        println_raw!("ast: \n{:?}", ast);
                                    }
                                    
                                    interpreter.expand_env_capacity(interner.len());
                                    interpreter.interpret(&ast, interner);
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
                let _ = stdout.suspend_raw_mode();
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

    pub fn load_history(&mut self, path: &str) -> Result<(), io::Error> {
        self.history_file = Some({
            fs::OpenOptions::new()
                .write(true)
                .read(true)
                .append(true)
                .create(true)
                .open(path)?
        });
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

        let mut posx = 0;
        let mut history = vec![];
        if let Some(file_handle) = &mut self.history_file {
            let mut buf = String::new();
            let _ = file_handle.read_to_string(&mut buf);
            for line in buf.lines() {
                history.push(line.to_owned());
            }
        }

        let mut history_pos = None;
        let mut buf = String::with_capacity(256);

        let mut stdout = stdout().into_raw_mode().unwrap();

        use termion::event::Key;

        print_raw!("{}{}", prompt_arrow, termion::cursor::Save);
        stdout.flush().unwrap();

        let (tx, rx) = channel();

        thread::spawn(move || {
            let stdin = stdin().lock();
            for key in stdin.keys() {
                tx.send(key).unwrap();
            }
        });

        let mut was_interpreting = false;

        loop {
            let is_interpreting = self.is_interpreting.load(sync::atomic::Ordering::Relaxed);

            if was_interpreting && !is_interpreting {
                print_raw!("{}{}", prompt_arrow, termion::cursor::Save);
                stdout.flush().unwrap();
                was_interpreting = false;
            }

            if let Ok(Ok(key)) = rx.try_recv() {
                match key {
                    Key::Char('\n') if !is_interpreting => {
                        int_count = 0;
                        print_raw!("\n\r");
                        stdout.flush().unwrap();
                        self.tx.send(Message::Val(buf.clone())).unwrap();
                        history.push(buf.clone());
                        if let Some(file) = &mut self.history_file {
                            let _ = write!(file, "\n{}", buf.clone());
                        }
                        history_pos = None;
                        buf.clear();
                        posx = 0;
                        was_interpreting = true;
                        self.is_interpreting
                            .store(true, sync::atomic::Ordering::Relaxed);
                    }
                    Key::Ctrl('c') | Key::Ctrl('d') if !is_interpreting => {
                        int_count += 1;
                        if int_count < 2 {
                            println_raw!(
                                "\n\r{}(Press ctrl+c or ctrl+d again to exit){}",
                                color::Fg(color::Magenta),
                                color::Fg(color::Reset)
                            );
                            print_raw!("{}{}", prompt_arrow, termion::cursor::Save);
                            stdout.flush().unwrap();
                            buf.clear();
                            posx = 0;
                        } else {
                            break;
                        }
                    }
                    Key::Ctrl('c') | Key::Ctrl('d') if is_interpreting => {
                        self.stop_signal
                            .store(true, sync::atomic::Ordering::Relaxed);
                        while self.is_interpreting.load(sync::atomic::Ordering::Relaxed) {}
                    }
                    Key::Ctrl('l') if !is_interpreting => {
                        print!("{}{}", termion::clear::All, termion::cursor::Goto(1, 1),);
                        stdout.flush().unwrap();
                        int_count = 0;
                        print_raw!("{}{}", prompt_arrow, termion::cursor::Save);
                        stdout.flush().unwrap();
                        buf.clear();
                        posx = 0;
                    }
                    Key::Ctrl('l') if is_interpreting => {
                        print!("{}{}", termion::clear::All, termion::cursor::Goto(1, 1),);
                        stdout.flush().unwrap();
                    }
                    Key::Up if !is_interpreting => {
                        if let None = history_pos {
                            if !history.is_empty() {
                                {
                                    let mut offset = 0;
                                    if !buf.is_empty() {
                                        history.push(buf.clone());
                                        if let Some(file) = &mut self.history_file {
                                            let _ = write!(file, "\n{}", buf.clone());
                                        }
                                        offset += 1;
                                    };
                                    history_pos = Some(history.len() - 1 - offset);
                                }
                                buf = history[history_pos.unwrap()].clone();
                                print_raw!(
                                    "{}{}{}{}",
                                    termion::cursor::Restore,
                                    termion::clear::UntilNewline,
                                    buf,
                                    termion::cursor::Left(
                                        (buf.len() as i16 - posx as i16).max(0) as u16
                                    )
                                );
                                if posx >= buf.len() {
                                    posx = (buf.len() as isize - 1).max(0) as usize
                                }
                                stdout.flush().unwrap();
                            };
                        } else {
                            if history_pos.unwrap() > 0 {
                                history_pos = Some(history_pos.unwrap() - 1);

                                buf = history[history_pos.unwrap()].clone();
                                print_raw!(
                                    "{}{}{}{}",
                                    termion::cursor::Restore,
                                    termion::clear::UntilNewline,
                                    buf,
                                    termion::cursor::Left(
                                        (buf.len() as i16 - posx as i16).max(0) as u16
                                    )
                                );
                                if posx >= buf.len() {
                                    posx = (buf.len() as isize - 1).max(0) as usize
                                }
                                stdout.flush().unwrap();
                            }
                        }
                    }
                    Key::Down if !is_interpreting => {
                        if let Some(pos) = history_pos {
                            if pos < history.len() - 1 {
                                history_pos = Some(pos + 1);

                                buf = history[history_pos.unwrap()].clone();
                                print_raw!(
                                    "{}{}{}{}",
                                    termion::cursor::Restore,
                                    termion::clear::UntilNewline,
                                    buf,
                                    termion::cursor::Left(
                                        (buf.len() as i16 - posx as i16).max(0) as u16
                                    )
                                );
                                stdout.flush().unwrap();
                            } else if !buf.is_empty() {
                                history_pos = None;
                                buf = String::new();
                                print_raw!(
                                    "{}{}{}",
                                    termion::cursor::Restore,
                                    termion::clear::UntilNewline,
                                    buf,
                                );
                                stdout.flush().unwrap();
                            }
                        }
                    }
                    Key::Left if !is_interpreting => {
                        if posx <= 0 {
                            continue;
                        }
                        posx -= 1;
                        print_raw!("{}", termion::cursor::Left(1));
                        stdout.flush().unwrap();
                    }
                    Key::Right if !is_interpreting => {
                        if posx >= buf.len() {
                            continue;
                        };
                        posx += 1;
                        print_raw!("{}", termion::cursor::Right(1));
                        stdout.flush().unwrap();
                    }
                    Key::Backspace if !is_interpreting => {
                        if posx <= 0 {
                            continue;
                        }
                        posx -= 1;
                        buf.remove(posx);
                        print_raw!(
                            "{}{} {}",
                            termion::cursor::Restore,
                            buf,
                            termion::cursor::Left((buf.len() - posx) as u16 + 1)
                        );
                        stdout.flush().unwrap();
                    }
                    Key::Char(c) if !is_interpreting => {
                        int_count = 0;

                        posx += 1;
                        if posx >= buf.len() {
                            buf.push(c);
                            print_raw!("{}", c);
                            stdout.flush().unwrap();
                        } else {
                            buf.insert(posx - 1, c);
                            print_raw!(
                                "{}{}{}",
                                termion::cursor::Restore,
                                buf,
                                termion::cursor::Left(
                                    (buf.len() as i16 - posx as i16).max(0) as u16
                                )
                            );
                            stdout.flush().unwrap();
                        }
                    }
                    _ => (),
                }
            }
        }
    }

    fn evaluate(&mut self, src: String) {}
}
