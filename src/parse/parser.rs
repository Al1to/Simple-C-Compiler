use std::rc::Rc;
use core::cell::RefCell;

use crate::lex::lexer::Token;

#[derive(Debug)]
#[derive(Clone)]
pub enum Type {
    Int,
    Float,
    Char,

    Ptr   { t: Box<Type> },
    Array { t: Box<Type>, n: Box<Node> },

    Void, 
}

#[derive(Debug)]
#[derive(Clone)]
pub enum UnOp {
    UnPlus  { e: Box<Node> },
    UnMinus { e: Box<Node> },

    PreInc  { e: Box<Node> },
    PostInc { e: Box<Node> },
    PreDec  { e: Box<Node> },
    PostDec { e: Box<Node> },

    LogNot { e: Box<Node> },

    Indirection { e: Box<Node>}, 
    AddressOf   { e:Box<Node> },

    Subscript { le: Box<Node> , re: Box<Node> },
    FuncCall  { id: String, args: Box<Node> },

    // TypeCast { t: Type, e: Box<Node> }, TODO:
}

#[derive(Debug)]
#[derive(Clone)]
pub enum BinOp {
    Assign    { le: Box<Node> , re: Box<Node> },
    AddAssign { le: Box<Node> , re: Box<Node> },
    SubAssign { le: Box<Node> , re: Box<Node> },
    MulAssign { le: Box<Node> , re: Box<Node> },
    DivAssign { le: Box<Node> , re: Box<Node> },
    ModAssign { le: Box<Node> , re: Box<Node> },

    LogOr  { le: Box<Node> , re: Box<Node> },
    LogAnd { le: Box<Node> , re: Box<Node> },

    Equal       { le: Box<Node> , re: Box<Node> },
    NotEqual    { le: Box<Node> , re: Box<Node> },
    Bigger      { le: Box<Node> , re: Box<Node> },
    Lesser      { le: Box<Node> , re: Box<Node> },
    BiggerEqual { le: Box<Node> , re: Box<Node> },
    LesserEqual { le: Box<Node> , re: Box<Node> },

    Add { le: Box<Node> , re: Box<Node> },
    Sub { le: Box<Node> , re: Box<Node> },
    Mul { le: Box<Node> , re: Box<Node> }, 
    Div { le: Box<Node> , re: Box<Node> },
    Mod { le: Box<Node> , re: Box<Node> },
}

#[derive(Debug)]
#[derive(Clone)]
pub enum Node {
    Id { s: String },

    Block { v: Vec<Rc<RefCell<Node>>> },

    FuncDecl { t: Type, id: String, args: Box<Node>, block: Box<Node> },
    ArgDecl  { t: Type, id: String },
    Return   { e: Box<Node> },
    Arg      { e: Box<Node> },

    VarDecl { t: Type, id: Box<Node>, val: Box<Node> },

    If   { cond: Box<Node>, block: Box<Node>, next: Box<Node> },
    Else { block: Box<Node> },
    
    For { var: Box<Node>, cond: Box<Node>, e: Box<Node>, block: Box<Node> },
    
    While { cond: Box<Node>, block: Box<Node> },
    
    Break,
    Continue,
    
    Expr   { e: Box<Node> },
    IntN   { num: i32 },
    FloatN { num: f64 },
    CharN  { num: u8 },

    BinOp { o: BinOp },
    UnOp { o: UnOp },

    None,
}

impl Node {
    fn get_vec_mut(&mut self) -> &mut Vec<Rc<RefCell<Node>>> {
        if let Node::Block { v } = self {
            return v;
        }
        panic!();
    }
}

// Program ::= [ FuncDecl ]

// FuncDecl  ::= Type ID ArgsDecl Block
// ArgsDecl  ::= "(" _ ")" | "(" ArgDecl ")" | "(" [ ArgDecl "," ] ArgDecl ")"
// Args      ::= "(" _ ")" | "(" Arg ")" | "(" [ Arg "," ] Arg ")"
// ArgDecl   ::= Type ID
// Arg       ::= Expr

// VarDecl ::= Type ID ";" | ...TODO...

// If     ::= "if" "(" Expr ")" Block [ ElseIf ] { Else }
// ElseIf ::= "else" If
// Else   ::= "else" Block

// For ::= "for" "(" Stmt Stmt Expr ")" Block

// While ::= "while" "(" Expr ")" Block

// Block ::= "{" [ Stmt ] "}" | Stmt

// Stmt  ::= Expr ";" | VarDecl | Var | If | For | While | "break" ";" | "continue" ";" | "return" Expr ";" | ";"
// Expr  ::= Terma | Terma "=" Expr | Terma "+=" Expr | Terma "-=" Expr | Terma "*=" Expr | Terma "/=" Expr | Terma "%=" Expr
// Terma ::= Termb | Termb "||" Terma
// Termb ::= Termc | Termc "&&" Termb
// Termc ::= Termd | Termd "==" Termc | Termd "!=" Termc
// Termd ::= Terme | Terme ">" Termd | Terme "<" Termd | Terme ">=" Termd  | Terme "<=" Termd
// Terme ::= Termf | Termf "+" Terme | Termf "-" Terme
// Termf ::= Termg | Termg "*" Termf | Termg "/" Termf | Termg "%" Termf
// Termg ::= "&" Termh | "*" Termh | "!" Termh | "++" Termh | "--" Termh | "+" Termh | "-" Termh | Termh
// Termh ::= Fact "++" | Fact "--" | Fact Args | Fact "[" Expr "]" | Type "(" Expr ")"
// Fact  ::= NUM | ID | "(" Expr ")"

// Type ::= "int" | "float" | "char" | "void" | "*" Type | "&" Type | Type ID "[" Expr "]"

pub fn parse(toks: &Vec<Token>) -> Node {
    let mut root = Node::Block { v: Vec::new() };
    let mut iter = toks.iter().peekable();
    if let Some(parsed_root) = parse_prg(&mut root, &mut iter) { return parsed_root; }
    panic!();
    
    fn next_t<'a>(toks: &mut std::iter::Peekable<impl Iterator<Item = &'a Token>>) -> &'a Token {
        match toks.next() {
            Some(t) => t,
            _ => panic!(), 
        }
    }

    fn parse_prg<'a>(root: &mut Node, toks: &mut std::iter::Peekable<impl Iterator<Item = &'a Token>>) -> Option<Node> {
        while let Some(t) = toks.peek() {
            match **t {
                Token::IntT | Token::FloatT | Token::CharT | Token::VoidT => {
                    root.get_vec_mut().push(Rc::new(RefCell::new(parse_func_decl(toks))));
                },
                _ => panic!(),
            }
        }
        Some(root.clone())
    }

    fn parse_func_decl<'a>(toks: &mut std::iter::Peekable<impl Iterator<Item = &'a Token>>) -> Node {
        let func_ret_type = parse_type(next_t(toks), toks);
        let func_id = parse_id(next_t(toks));
        let func_args = match *next_t(toks) {
            Token::LP => parse_args_decl(toks),
            _ => panic!() };
        let func_block = match *next_t(toks) {
            Token::LC => parse_block(toks),
            _ => panic!() };

        Node::FuncDecl { t: func_ret_type, id: func_id, args: Box::new(func_args), block: Box::new(func_block) }
    }

    fn parse_args_decl<'a>(toks: &mut std::iter::Peekable<impl Iterator<Item = &'a Token>>) -> Node {
        let mut args_decl = Node::Block { v: Vec::new() };
        loop {
            match toks.peek() {
                Some(Token::Comma) => {
                    next_t(toks);
                    args_decl.get_vec_mut().push(parse_arg_decl(toks));
                }
                Some(Token::IntT) | Some(Token::FloatT) | Some(Token::CharT) | Some(Token::VoidT) => {
                    args_decl.get_vec_mut().push(parse_arg_decl(toks));
                }
                Some(Token::RP) => { next_t(toks); break; },
                _ => panic!(),
            }
        }

        args_decl
    }

    fn parse_arg_decl<'a>(toks: &mut std::iter::Peekable<impl Iterator<Item = &'a Token>>) -> Rc<RefCell<Node>> {
        Rc::new(RefCell::new(
            Node::ArgDecl { 
                t: parse_type(next_t(toks), toks), 
                id: parse_id(next_t(toks)) 
            }
        ))
    }

    fn parse_stmt<'a>(toks: &mut std::iter::Peekable<impl Iterator<Item = &'a Token>>) -> Node {
        let node;

        match toks.peek() {
            Some(Token::Break)    => { toks.next(); node = Node::Break; },
            Some(Token::Continue) => { toks.next(); node = Node::Continue; },
            Some(Token::Return)   => { toks.next(); node = Node::Return { e: Box::new(parse_expr(toks)) }; },
            _ => node = Node::Expr { e: Box::new(parse_expr(toks)) },
        };

        match toks.peek() {
            Some(Token::Semicolon) | Some(Token::Comma) => { toks.next(); },
            _ => {},
        };

        node
    }

    fn parse_expr<'a>(toks: &mut std::iter::Peekable<impl Iterator<Item = &'a Token>>) -> Node {
        let node = parse_terma(toks);
        if let Some(t) = toks.peek() {
            match t {
                Token::Assignment => { toks.next(); return Node::BinOp { o: BinOp::Assign    { le: Box::new(node), re: Box::new(parse_expr(toks)) } } },
                Token::AddAssign  => { toks.next(); return Node::BinOp { o: BinOp::AddAssign { le: Box::new(node), re: Box::new(parse_expr(toks)) } } },
                Token::SubAssign  => { toks.next(); return Node::BinOp { o: BinOp::SubAssign { le: Box::new(node), re: Box::new(parse_expr(toks)) } } },
                Token::MulAssign  => { toks.next(); return Node::BinOp { o: BinOp::MulAssign { le: Box::new(node), re: Box::new(parse_expr(toks)) } } },
                Token::DivAssign  => { toks.next(); return Node::BinOp { o: BinOp::DivAssign { le: Box::new(node), re: Box::new(parse_expr(toks)) } } },
                Token::ModAssign  => { toks.next(); return Node::BinOp { o: BinOp::ModAssign { le: Box::new(node), re: Box::new(parse_expr(toks)) } } },
                _ => return node
            }
        }
        panic!();
    }

    fn parse_terma<'a>(toks: &mut std::iter::Peekable<impl Iterator<Item = &'a Token>>) -> Node {
        let node = parse_termb(toks);
        if let Some(t) = toks.peek() {
            match t {
                Token::Or => return Node::BinOp { o: BinOp::LogOr { le: Box::new(node), re: Box::new(parse_terma(toks)) } },
                _ => return node,
            }
        }
        panic!();
    }

    fn parse_termb<'a>(toks: &mut std::iter::Peekable<impl Iterator<Item = &'a Token>>) -> Node {
        let node = parse_termc(toks);
        if let Some(t) = toks.peek() {
            match t {
                Token::And => return Node::BinOp { o: BinOp::LogAnd { le: Box::new(node), re: Box::new(parse_termb(toks)) } },
                _ => return node,
            }
        }
        panic!();
    }

    fn parse_termc<'a>(toks: &mut std::iter::Peekable<impl Iterator<Item = &'a Token>>) -> Node {
        let node = parse_termd(toks);
        if let Some(t) = toks.peek() {
            match t {
                Token::Equal    => { toks.next(); return Node::BinOp { o: BinOp::Equal    { le: Box::new(node), re: Box::new(parse_termc(toks)) } } },
                Token::NotEqual => { toks.next(); return Node::BinOp { o: BinOp::NotEqual { le: Box::new(node), re: Box::new(parse_termc(toks)) } } },
                _ => return node,
            }
        }
        panic!();
    }

    fn parse_termd<'a>(toks: &mut std::iter::Peekable<impl Iterator<Item = &'a Token>>) -> Node {
        let node = parse_terme(toks);
        if let Some(t) = toks.peek() {
            match t {
                Token::Bigger => { toks.next(); return Node::BinOp { o: BinOp::Bigger { le: Box::new(node), re: Box::new(parse_termd(toks)) } } },
                Token::Lesser => { toks.next(); return Node::BinOp { o: BinOp::Lesser { le: Box::new(node), re: Box::new(parse_termd(toks)) } } },
                Token::LesserEqual => { toks.next(); return Node::BinOp { o: BinOp::LesserEqual { le: Box::new(node), re: Box::new(parse_termd(toks)) } } },
                Token::BiggerEqual => { toks.next(); return Node::BinOp { o: BinOp::BiggerEqual { le: Box::new(node), re: Box::new(parse_termd(toks)) } } },
                _ => return node,
            }
        }
        panic!();
    }

    fn parse_terme<'a>(toks: &mut std::iter::Peekable<impl Iterator<Item = &'a Token>>) -> Node {
        let node = parse_termf(toks);
        if let Some(t) = toks.peek() {
            match t {
                Token::Plus  => { toks.next(); return Node::BinOp { o: BinOp::Add { le: Box::new(node), re: Box::new(parse_terme(toks)) } } },
                Token::Minus => { toks.next(); return Node::BinOp { o: BinOp::Sub { le: Box::new(node), re: Box::new(parse_terme(toks)) } } },
                _ => return node,
            }
        }
        panic!();
    }

    fn parse_termf<'a>(toks: &mut std::iter::Peekable<impl Iterator<Item = &'a Token>>) -> Node {
        let node = parse_termg(toks);
        if let Some(t) = toks.peek() {
            match t {
                Token::Star    => { toks.next(); return Node::BinOp { o: BinOp::Mul { le: Box::new(node), re: Box::new(parse_termf(toks)) } } },
                Token::Slash   => { toks.next(); return Node::BinOp { o: BinOp::Div { le: Box::new(node), re: Box::new(parse_termf(toks)) } } },
                Token::Percent => { toks.next(); return Node::BinOp { o: BinOp::Mod { le: Box::new(node), re: Box::new(parse_termf(toks)) } } },
                _ => return node,
            }
        }
        panic!();
    }

    fn parse_termg<'a>(toks: &mut std::iter::Peekable<impl Iterator<Item = &'a Token>>) -> Node {
        if let Some(t) = toks.peek() {
            match t {
                Token::Ampersand => { toks.next(); return Node::UnOp { o: UnOp::AddressOf   { e: Box::new(parse_termh(toks)) } } },
                Token::Star      => { toks.next(); return Node::UnOp { o: UnOp::Indirection { e: Box::new(parse_termh(toks)) } } },
                Token::Negation  => { toks.next(); return Node::UnOp { o: UnOp::LogNot      { e: Box::new(parse_termh(toks)) } } },
                Token::Inc       => { toks.next(); return Node::UnOp { o: UnOp::PreInc      { e: Box::new(parse_termh(toks)) } } },
                Token::Dec       => { toks.next(); return Node::UnOp { o: UnOp::PreDec      { e: Box::new(parse_termh(toks)) } } },
                Token::Plus      => { toks.next(); return Node::UnOp { o: UnOp::UnPlus      { e: Box::new(parse_termh(toks)) } } },
                Token::Minus     => { toks.next(); return Node::UnOp { o: UnOp::UnMinus     { e: Box::new(parse_termh(toks)) } } },
                _ => return parse_termh(toks),
            }
        }
        panic!();
    }

    fn parse_termh<'a>(toks: &mut std::iter::Peekable<impl Iterator<Item = &'a Token>>) -> Node { // TODO:
        let node = parse_fact(toks);
        if let Some(t) = toks.peek() {
            match t {
                Token::Inc => { toks.next(); return Node::UnOp { o: UnOp::PostInc { e: Box::new(node) } } },
                Token::Dec => { toks.next(); return Node::UnOp { o: UnOp::PostDec { e: Box::new(node) } } }, 
                Token::LP  => { toks.next(); 
                    let node2 = match node {
                        Node::Id { s } => {
                            Node::UnOp { o: UnOp::FuncCall { id: s, args: Box::new(parse_args(toks)) } }
                        },
                        _ => panic!(),
                    }; return node2;
                },
                _ => return node,
            }
        }
        panic!();
    }

    fn parse_fact<'a>(toks: &mut std::iter::Peekable<impl Iterator<Item = &'a Token>>) -> Node {
        if let Some(t) = toks.peek() {
            match t {
                Token::Inum { n }     => { toks.next(); return Node::IntN   { num: *n } },
                Token::Fnum { n }     => { toks.next(); return Node::FloatN { num: *n } },
                Token::Cnum { n }      => { toks.next(); return Node::CharN  { num: *n } },
                Token::Id   { id } => { toks.next(); return Node::Id     { s: id.to_string() } },
                Token::LP  => { 
                    toks.next(); let node = parse_expr(toks); toks.next();
                    return node;
                },
                _ => panic!("{:?}", t),
            }
        }
        panic!();
    }

    fn parse_args<'a>(toks: &mut std::iter::Peekable<impl Iterator<Item = &'a Token>>) -> Node {
        let mut block = Node::Block { v: Vec::new() };
        loop {
            match toks.peek() {
                Some(Token::RP) => { toks.next(); break; },
                Some(Token::Id  { id: _ }) | Some(Token::LP) | Some(Token::Star) | 
                Some(Token::Inum { n: _ }) | Some(Token::Fnum { n: _ }) | Some(Token::Cnum { n: _ }) 
                  => block.get_vec_mut().push(Rc::new(RefCell::new(parse_stmt(toks)))), // TODO:
                _ => panic!("{:?}", toks.peek()),
            }
        }

        block
    }

    fn parse_block<'a>(toks: &mut std::iter::Peekable<impl Iterator<Item = &'a Token>>) -> Node {
        let mut block = Node::Block { v: Vec::new() };
        loop {
            match toks.peek() {
                Some(Token::RC) => { toks.next(); break; },
                Some(Token::Return) | Some(Token::Break) | Some(Token::Continue) |
                Some(Token::Id { id: _ }) | Some(Token::LP) | Some(Token::Star)  
                => block.get_vec_mut().push(Rc::new(RefCell::new(parse_stmt(toks)))), // TODO:
                _ => panic!("{:?}", toks.peek()),
            }
        }

        block
    }

    fn parse_id(t: &Token) -> String {
        match t {
            Token::Id { id } => id.to_string(),
            _ => panic!()
        }
    }

    fn parse_type<'a>(t: &Token, toks: &mut std::iter::Peekable<impl Iterator<Item = &'a Token>>) -> Type {
        return match t {
            // TODO: array
            Token::IntT => create_ptr(check_ptr(toks), Type::Int),
            Token::FloatT => create_ptr(check_ptr(toks), Type::Float),
            Token::CharT => create_ptr(check_ptr(toks), Type::Char),
            Token::VoidT => create_ptr(check_ptr(toks), Type::Void),
            _ => panic!()
        };

        fn check_ptr<'a>(toks: &mut std::iter::Peekable<impl Iterator<Item = &'a Token>>) -> u8 {
            match toks.peek() {
                Some(Token::Star) => {
                    let mut ptr_count: u8 = 1;
                    next_t(toks);
                    loop {
                        match toks.peek() {
                            Some(Token::Star) => {
                                ptr_count += 1;
                                next_t(toks);
                            },
                            _ => break,
                        };
                    }
                    return ptr_count;
                },
                _ => 0,
            }
        }

        fn create_ptr(ptr_count: u8, end: Type) -> Type {
            if ptr_count == 0 {
                return end;
            }
            Type::Ptr { t: Box::new(create_ptr(ptr_count - 1, end)) }
        }
    }
}
