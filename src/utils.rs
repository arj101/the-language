use crate::expr::LiteralType;
use crate::expr::StrType;

use termion::color;
use termion::color::{Cyan, Green, LightGreen, LightYellow, Red};

pub fn pretty_print_literal(literal: LiteralType) {
    match literal {
        LiteralType::Number(n) => {
            println!("{}{}{}", color::Fg(LightYellow), n, color::Fg(color::Reset))
        }
        LiteralType::Null => println!("{}null{}", color::Fg(Red), color::Fg(color::Reset)),
        LiteralType::Bool(b) => {
            println!("{}{}{}", color::Fg(Cyan), b, color::Fg(color::Reset))
        }
        LiteralType::Str(StrType::Strict(s)) => {
            println!("{}\"{}\"{}", color::Fg(Green), s, color::Fg(color::Reset))
        }
        LiteralType::Str(StrType::Loose(s)) => println!(
            "{}'{}'{}",
            color::Fg(LightGreen),
            s,
            color::Fg(color::Reset)
        ),
    };
}

pub fn print_literal(literal: LiteralType) {
    match literal {
        LiteralType::Number(n) => println!("{n}"),
        LiteralType::Null => println!("null"),
        LiteralType::Bool(b) => println!("{b}"),
        LiteralType::Str(StrType::Strict(s) | StrType::Loose(s)) => println!("{s}"),
    };
}
