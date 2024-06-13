#[derive(Debug)]
#[derive(PartialEq)]
pub enum Token {
    LP, RP, LC, RC, LB, RB,
    Inum { n: i32 }, Fnum { n: f64 }, Cnum { n: u8 },
    Id { id: String },
    Plus, Minus, Star, Slash, Percent,
    Bigger, Lesser, Equal, NotEqual,
    BiggerEqual, LesserEqual,
    Assignment, 
    AddAssign, SubAssign, MulAssign, DivAssign, ModAssign,
    Inc, Dec,
    Semicolon, Comma, Dot, Ampersand,
    And, Or, Negation,
    If, Else, For, While, Break, Continue, Return, 
    IntT, FloatT, CharT, VoidT,
}

pub fn lex(s: &String) -> Vec<Token> {
    let mut toks: Vec<Token> = Vec::new();
    let mut chs = s.chars().peekable();
    while let Some(ch) = chs.next() {
        if ch.is_whitespace() { continue; }
        match ch {
            '(' => toks.push(Token::LP),
            ')' => toks.push(Token::RP),
            '{' => toks.push(Token::LC),
            '}' => toks.push(Token::RC),
            '[' => toks.push(Token::LB),
            ']' => toks.push(Token::RB),
            ';' => toks.push(Token::Semicolon),
            ',' => toks.push(Token::Comma),
            '.' => toks.push(Token::Dot),
            '>' => match chs.peek() {
                Some(&'=') => { toks.push(Token::BiggerEqual); chs.next(); },
                _ => toks.push(Token::Bigger), },
            '<' => match chs.peek() {
                Some(&'=') => { toks.push(Token::LesserEqual); chs.next(); },
                _ => toks.push(Token::Lesser), },
            '*' => match chs.peek() {
                Some(&'=') => { toks.push(Token::MulAssign); chs.next(); },
                _ => toks.push(Token::Star), },
            '/' => match chs.peek() {
                Some(&'=') => { toks.push(Token::DivAssign); chs.next(); },
                Some(&'/') => loop { if chs.next() == Some('\n') { break; } },
                _ => toks.push(Token::Slash), },
            '%' => match chs.peek() {
                Some(&'=') => { toks.push(Token::ModAssign); chs.next(); },
                _ => toks.push(Token::Percent), },
            '-' => match chs.peek() {
                Some(&'=') => { toks.push(Token::SubAssign); chs.next(); },
                Some(&'-') => { toks.push(Token::Dec); chs.next(); },
                _ => toks.push(Token::Minus), },
            '+' => match chs.peek() {
                Some(&'=') => { toks.push(Token::AddAssign); chs.next(); },
                Some(&'+') => { toks.push(Token::Inc); chs.next(); },
                _ => toks.push(Token::Plus), },
            '&' => match chs.peek() {
                Some(&'&') => { toks.push(Token::And); chs.next(); },
                _ => toks.push(Token::Ampersand), },
            '=' => match chs.peek() {
                Some(&'=') => { toks.push(Token::Equal); chs.next(); },
                _ => toks.push(Token::Assignment), },
            '!' => match chs.peek() {
                Some(&'=') => { toks.push(Token::NotEqual); chs.next(); },
                _ => toks.push(Token::Negation), },
            '|' if chs.peek() == Some(&'|') => {
                chs.next();
                toks.push(Token::Or); 
            },
            _ if ch.is_alphabetic() => {
                let mut lex = String::new(); lex.push(ch);
                while let Some(&nch) = chs.peek() {
                    match nch {
                        _ if nch.is_alphanumeric() 
                          || nch == '_' => lex.push(nch),
                        _ => break
                    } chs.next();
                }

                match lex.as_str() {
                    "if" => toks.push(Token::If),
                    "else" => toks.push(Token::Else),
                    "for" => toks.push(Token::For),
                    "while" => toks.push(Token::While),
                    "break" => toks.push(Token::Break),
                    "continue" => toks.push(Token::Continue),
                    "return" => toks.push(Token::Return),
                    "int" => toks.push(Token::IntT),
                    "float" => toks.push(Token::FloatT),
                    "char" => toks.push(Token::CharT),
                    "void" => toks.push(Token::VoidT),
                    _ => toks.push(Token::Id { id: lex })
                }
            },
            _ if ch.is_numeric() => {
                let mut points: u8 = 0;
                let mut lex = String::new(); lex.push(ch);
                while let Some(&nch) = chs.peek() {
                    match nch {
                        _ if nch.is_numeric() => lex.push(nch),
                        '.' => { points += 1; lex.push(nch); },
                        _ => break
                    } chs.next();
                }
            
                match points {
                    0 => match lex.parse::<i32>() {
                        Ok(num) => toks.push(Token::Inum { n: num }),
                        Err(_) => panic!()
                    },
                    1 => match lex.parse::<f64>() {
                        Ok(num) => toks.push(Token::Fnum { n: num }),
                        Err(_) => panic!()
                    },
                    _ => panic!()
                }
            },
            _ if ch == '\'' => {
                let lex = chs.next();
                match chs.next() {
                    Some('\'') => match lex.is_some() {
                        true => toks.push(Token::Cnum { n: lex.unwrap() as u8 }),
                        false => panic!()
                    },
                    _ => panic!()
                }
            },
            _ => panic!("'{}' ..?", ch)
        }
    }
    toks
}
