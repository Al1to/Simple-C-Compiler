mod lex {
    pub mod lexer;
}

mod parse {
    pub mod parser;
}

use crate::lex::lexer::lex;
use crate::parse::parser::parse;

fn main() {
    let s = String::from
    ("
        int main() {
            x += test(a) + 2;

            return x*6;
        }
    ");
    println!("{:?}", parse(&lex(&s)));
}
