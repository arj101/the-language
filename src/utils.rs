use crate::expr::LiteralType;
use crate::expr::StrType;

use crate::print_raw;
use crate::println_raw;

use termion::color;
use termion::color::{Cyan, Green, LightGreen, LightYellow, Red};

pub fn pretty_print_literal(literal: LiteralType) {
    match literal {
        LiteralType::Number(n) => {
            println_raw!("{}{}{}", color::Fg(LightYellow), n, color::Fg(color::Reset))
        }
        LiteralType::Null => println_raw!("{}null{}", color::Fg(Red), color::Fg(color::Reset)),
        LiteralType::Bool(b) => {
            println_raw!("{}{}{}", color::Fg(Cyan), b, color::Fg(color::Reset))
        }
        LiteralType::Str(StrType::Strict(s)) => {
            println_raw!("{}\"{}\"{}", color::Fg(Green), s, color::Fg(color::Reset))
        }
        LiteralType::Str(StrType::Loose(s)) => println_raw!(
            "{}'{}'{}",
            color::Fg(LightGreen),
            s,
            color::Fg(color::Reset)
        ),
    };
}

pub fn print_literal(literal: LiteralType) {
    match literal {
        LiteralType::Number(n) => println_raw!("{n}"),
        LiteralType::Null => println_raw!("null"),
        LiteralType::Bool(b) => println_raw!("{b}"),
        LiteralType::Str(StrType::Strict(s) | StrType::Loose(s)) => println_raw!("{s}"),
    };
}
