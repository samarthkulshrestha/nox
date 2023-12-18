use std::fmt;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Sym(String),
    Fun(String, Vec<Expr>),
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

fn main() {
    println!("");
    println!("{}", expr!(a));
    println!("{}", expr!(f()));
    println!("{}", expr!(f(a)));
    println!("{}", expr!(f(a, b)));
    println!("{}", expr!(f(a, b, c, d)));
    println!("{}", expr!(f(g(x))));
    println!("{}", expr!(f(g(x), k(y))));
}
