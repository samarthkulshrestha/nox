use std::fmt;
use std::collections::HashMap;
use std::iter::Peekable;

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Sym(String),
    Fun(String, Vec<Expr>),
}

impl Expr {
    fn parse_peekable(lexer: &mut Peekable<impl Iterator<Item=Token>>) -> Self {
        if let Some(name) = lexer.next() {
            match name.kind {
                TokenKind::Sym => {
                    if let Some(_) = lexer.next_if(|t| t.kind == TokenKind::OpenParen) {
                        let mut args = Vec::new();
                        if let Some(_) = lexer.next_if(|t| t.kind == TokenKind::OpenParen) {
                            return Expr::Fun(name.text, args);
                        }

                        args.push(Self::parse_peekable(lexer));
                        while let Some(_) = lexer.next_if(|t| t.kind == TokenKind::Comma) {
                            args.push(Self::parse_peekable(lexer));
                        }
                        if lexer.next_if(|t| t.kind == TokenKind::CloseParen).is_none() {
                            todo!("EOL: expected close parentheses.");
                        }
                        Expr::Fun(name.text, args)
                    } else {
                        Expr::Sym(name.text)
                    }
                },
                _ => todo!("report that a symbol was expected.")
            }
        } else {
            todo!("report EOF error.")
        }
    }
    fn parse(lexer: impl Iterator<Item=Token>) -> Self {
        Self::parse_peekable(&mut lexer.peekable())
    }

}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Sym(name) => write!(f, "{}", name),
            Expr::Fun(name, args) => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 { write!(f, ", ")? }
                    write!(f, "{}", arg)?;
                }
                return write!(f, ")");
            },
        }
    }
}

#[derive(Debug)]
struct Rule {
    head: Expr,
    body: Expr,
}

impl Rule {
    fn apply_all(&self, expr: &Expr) -> Expr {
        if let Some(bindings) = pattern_match(&self.head, expr) {
            substitute_bindings(&bindings, &self.body)
        } else {
            use Expr::*;
            match expr {
                Sym(_) => expr.clone(),
                Fun(name, args) => {
                    let mut new_args = Vec::new();
                    for arg in args {
                        new_args.push(self.apply_all(arg));
                    }
                    return Fun(name.clone(), new_args);
                }
            }
        }
    }
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{} = {}", self.head, self.body);
    }
}

fn substitute_bindings(bindings: &Binding, expr: &Expr) -> Expr {
    use Expr::*;
    match expr {
        Sym(name) => {
            if let Some(value) = bindings.get(name) {
                value.clone()
            } else {
                expr.clone()
            }
        },

        Fun(name, args) => {
            let new_name = match bindings.get(name) {
                Some(Sym(new_name)) => new_name.clone(),
                None => name.clone(),
                Some(_) => panic!("expected symbol instead of functor name."),
            };
            let mut new_args = Vec::new();
            for arg in args {
                new_args.push(substitute_bindings(bindings, &arg));
            }
            return Fun(new_name, new_args);
        }
    }
}

type Binding = HashMap<String, Expr>;
fn pattern_match(pattern: &Expr, value: &Expr) -> Option<Binding> {
    fn pattern_match_impl(pattern: &Expr, value: &Expr, bindings: &mut Binding) -> bool {
        use Expr::*;
        match (pattern, value) {
            (Sym(name), _) => {
                if let Some(bound_value) = bindings.get(name) {
                    return bound_value == value;
                } else {
                    bindings.insert(name.clone(), value.clone());
                    return true;
                }
            },
            (Fun(name1, args1), Fun(name2, args2)) => {
                if name1 == name2 && args1.len() == args2.len() {
                    for i in 0..args1.len() {
                        if !pattern_match_impl(&args1[i], &args2[i], bindings) {
                            return false;
                        }
                    }
                    return true;
                } else {
                    return false;
                }
            },
            _ => { return false; },
        }
    }

    let mut bindings = HashMap::new();

    if pattern_match_impl(pattern, value, &mut bindings) {
        Some(bindings)
    } else {
        None
    }
}

macro_rules! fun_args {
    () => { vec![] };
    ($name:ident) => { vec![expr!($name)] };
    ($name:ident,$($rest:tt)*) => {
        {
            let mut t = vec![expr!($name)];
            t.append(&mut fun_args!($($rest)*));
            t
        }
    };
    ($name:ident($($args:tt)*)) => {
        vec![expr!($name($($args)*))]
    };
    ($name:ident($($args:tt)*),$($rest:tt)*) => {
        {
            let mut t = vec![expr!($name($($args)*))];
            t.append(&mut fun_args!($($rest)*));
            t
        }
    }
}

macro_rules! expr {
    ($name:ident) => {
        Expr::Sym(stringify!($name).to_string())
    };
    ($name:ident($($args:tt)*)) => {
        Expr::Fun(stringify!($name).to_string(), fun_args!($($args)*))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn rule_apply_all() {
        // swap(pair(a, b)) = pair(b, a)
        let swap = Rule {
                        head: expr!(swap(pair(a, b))),
                        body: expr!(pair(b, a)),
        };

        let input = expr!(foo(swap(pair(f(a), g(b))), swap(pair(q(c), z(d)))));
        let expected = expr!(foo(pair(g(b), f(a)), pair(z(d), q(c))));

        assert_eq!(swap.apply_all(&input), expected);
    }
}

#[derive(Debug, PartialEq)]
enum TokenKind {
    Sym,
    OpenParen,
    CloseParen,
    Comma,
    Equals,
}

#[derive(Debug)]
struct Token {
    kind: TokenKind,
    text: String,
}

struct Lexer<Chars: Iterator<Item=char>> {
    chars: Peekable<Chars>
}

impl<Chars: Iterator<Item=char>> Lexer<Chars> {
    fn from_iter(chars: Chars) -> Self {
        Self { chars: chars.peekable() }
    }
}

impl<Chars: Iterator<Item=char>> Iterator for Lexer<Chars> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        while let Some(_) = self.chars.next_if(|x| x.is_whitespace()) {}

        if let Some(x) = self.chars.next() {
            let mut text = String::new();
            text.push(x);
            match x {
                '(' => Some(Token {kind: TokenKind::OpenParen, text}),
                ')' => Some(Token {kind: TokenKind::CloseParen, text}),
                ',' => Some(Token {kind: TokenKind::Comma, text}),
                '=' => Some(Token {kind: TokenKind::Equals, text}),
                _ => {
                    if !x.is_alphanumeric() {
                        todo!("report unexpected token properly. starts with '{}'", x);
                    }

                    while let Some(x) = self.chars.next_if(|x| x.is_alphanumeric()) {
                        text.push(x)
                    }

                    Some(Token{kind: TokenKind::Sym, text})
                }
            }
        } else {
            None
        }
    }
}

fn main() {
    println!("");

    let source = "swap(pair(a, b))";
    let swap = Rule {
        head: expr!(swap(pair(a, b))),
        body: expr!(pair(b, a)),
    };

    println!("{}", swap.apply_all(&Expr::parse(Lexer::from_iter(source.chars()))));
}
