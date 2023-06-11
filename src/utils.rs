use crate::expr::LiteralType;

use crate::print_raw;
use crate::println_raw;

use termion::color;
use termion::color::{Cyan, Green, LightGreen, LightYellow, Red};

fn format_literal_pretty(literal: &LiteralType) -> String {
    match literal {
        LiteralType::Number(n) => {
            format!("{}{}{}", color::Fg(LightYellow), n, color::Fg(color::Reset))
        }
        LiteralType::Null => format!("{}null{}", color::Fg(Red), color::Fg(color::Reset)),
        LiteralType::Bool(b) => {
            format!("{}{}{}", color::Fg(Cyan), b, color::Fg(color::Reset))
        }
        LiteralType::Str(s) => format!(
            "{}'{}'{}",
            color::Fg(LightGreen),
            unsafe { &**s },
            color::Fg(color::Reset)
        ),
        LiteralType::Array(array) => {
            format!(
                "[ {} ]",
                unsafe { &**array }
                    .iter()
                    .map(format_literal_pretty)
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        }
    }
}

fn format_literal(literal: &LiteralType) -> String {
    match literal {
        LiteralType::Number(n) => format!("{n}"),
        LiteralType::Null => format!("null"),
        LiteralType::Bool(b) => format!("{b}"),
        LiteralType::Str(s) => format!("{}", unsafe { &**s }),
        LiteralType::Array(array) => format!(
            "[ {} ]",
            unsafe { &**array }
                .iter()
                .map(format_literal)
                .collect::<Vec<String>>()
                .join(", ")
        ),
    }
}

pub fn pretty_print_literal(literal: LiteralType) {
    println_raw!("{}", format_literal_pretty(&literal));
}

pub fn print_literal(literal: LiteralType) {
    println_raw!("{}", format_literal(&literal));
}
