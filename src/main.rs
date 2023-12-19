use std::io;
use std::fmt;
use std::fs;
use std::iter::Peekable;
use std::io::{stdin, stdout, Write};
use std::collections::HashMap;

mod lexer;
use lexer::*;

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Sym(String),
    Fun(String, Vec<Expr>),
}

impl Expr {
    fn parse_peekable(lexer: &mut Peekable<impl Iterator<Item=Token>>) -> Result<Self, Error> {
        if let Some(name) = lexer.next() {
            match name.kind {
                TokenKind::Sym => {
                    if let Some(_) = lexer.next_if(|t| t.kind == TokenKind::OpenParen) {
                        let mut args = Vec::new();
                        if let Some(_) = lexer.next_if(|t| t.kind == TokenKind::OpenParen) {
                            return Ok(Expr::Fun(name.text, args));
                        }

                        args.push(Self::parse_peekable(lexer)?);
                        while let Some(_) = lexer.next_if(|t| t.kind == TokenKind::Comma) {
                            args.push(Self::parse_peekable(lexer)?);
                        }
                        if let Some(t) = lexer.peek() {
                            if t.kind == TokenKind::CloseParen {
                                lexer.next();
                                Ok(Expr::Fun(name.text, args))
                            } else {
                                Err(Error::UnexpectedToken(TokenKind::CloseParen, t.clone()))
                            }
                        } else {
                            Err(Error::UnexpectedEOF(TokenKind::CloseParen))
                        }
                    } else {
                        Ok(Expr::Sym(name.text))
                    }
                },
                _ => Err(Error::UnexpectedToken(TokenKind::Sym, name))
            }
        } else {
            Err(Error::UnexpectedEOF(TokenKind::Sym))
        }
    }

    fn parse(lexer: &mut impl Iterator<Item=Token>) -> Result<Self, Error> {
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
enum Error {
    UnexpectedToken(TokenKind, Token),
    UnexpectedEOF(TokenKind),
    IoError(io::Error),
}

fn expect_token_kind(lexer: &mut Peekable<impl Iterator<Item=Token>>,
                     kind: TokenKind) -> Result<Token, Error> {
    let token = lexer.next().ok_or(Error::UnexpectedEOF(kind))?;
    if token.kind == kind {
        Ok(token)
    } else {
        Err(Error::UnexpectedToken(kind, token))
    }
}

#[derive(Debug)]
struct Rule {
    head: Expr,
    body: Expr,
}

impl Rule {
    fn parse(lexer: &mut Peekable<impl Iterator<Item=Token>>) -> Result<Rule, Error> {
        let head = Expr::parse_peekable(lexer)?;
        expect_token_kind(lexer, TokenKind::Equals)?;
        let body = Expr::parse_peekable(lexer)?;
        Ok(Rule{head, body})
    }

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

fn parse_rules_from_file(file_path: &str) -> Result<HashMap<String, Rule>, Error> {
    let mut rules = HashMap::new();
    let source = fs::read_to_string(file_path).map_err(|e| Error::IoError(e))?;
    let mut lexer = Lexer::from_iter(source.chars()).peekable();

    while let Some(_) = lexer.peek() {
        let name = expect_token_kind(&mut lexer, TokenKind::Sym)?;
        expect_token_kind(&mut lexer, TokenKind::Colon)?;
        let rule = Rule::parse(&mut lexer)?;
        rules.insert(name.text, rule);
    }
    Ok(rules)
}

fn main() {
    println!("");

    let default_rules_path = "rules.nox";
    let rules = match parse_rules_from_file(default_rules_path) {
        Ok(rules) => {
            println!("INFO: successfully loaded ruled from {}.\n", default_rules_path);
            rules
        }
        Err(err) => {
            eprintln!("ERROR: could not read file {}: {:?}", default_rules_path, err);
            Default::default()
        }
    };

    println!("available rules: ");
    for (name, rule) in rules {
        println!("{}: {}", name, rule);
    }
    println!("");

    let swap = Rule {
        head: expr!(swap(pair(a, b))),
        body: expr!(pair(b, a)),
    };

    let prompt = "nox {$} ";
    let mut command = String::new();

    loop {
        command.clear();
        print!("{}", prompt);
        stdout().flush().unwrap();
        stdin().read_line(&mut command).unwrap();

        if command == "quit\n" || command == "q\n" {
            break;
        }

        match Expr::parse(&mut Lexer::from_iter(command.chars())) {
            Ok(expr) => println!("{}", swap.apply_all(&expr)),
            Err(Error::UnexpectedToken(expected, actual)) => {
                println!("{:>width$}^", "", width=prompt.len() + actual.loc.col);
                println!("ERROR: expected {} but got {} '{}'.", expected, actual.kind, actual.text);
            }
            Err(Error::UnexpectedEOF(expected)) => {
                println!("{:>width$}^", "", width=prompt.len() + command.len());
                println!("ERROR: expected {} but encountered EOF.", expected);
            }
            Err(Error::IoError(io_error)) => {
                unreachable!("IO ERROR: {}", io_error);
            }
        }

        println!("");
    }

}
