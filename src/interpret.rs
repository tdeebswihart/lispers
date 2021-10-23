use crate::op;
use crate::parse::{Atom, Binding, Expr};
use snafu::{ResultExt, Snafu};
use std::collections::HashMap;
use std::convert::TryFrom;

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("fn {} expects {} args but got {}", ident, expected, received))]
    BadArity {
        ident: String,
        expected: usize,
        received: usize,
    },
    #[snafu(display("{} is neither a lambda nor an ident so cannot be invoked", what))]
    Uncallable { what: String },
    #[snafu(display("identifier {} has not been bound", name))]
    Unbound { name: String },
    #[snafu(display("{} is not a valid value", expr))]
    BadValue { expr: String },
    #[snafu(display("op failed: {}", inner))]
    OpError { inner: op::Error },
}

impl From<op::Error> for Error {
    fn from(err: op::Error) -> Self {
        Error::OpError { inner: err }
    }
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, Clone)]
enum Value {
    Literal(Atom),
    Fn(Vec<Binding>, Expr),
}

impl TryFrom<Expr> for Value {
    type Error = Error;

    fn try_from(expr: Expr) -> Result<Self, Self::Error> {
        use Expr::*;
        match expr {
            Literal(a) => Ok(Value::Literal(a)),
            Lambda(bindings, body) => Ok(Value::Fn(bindings, *body)),
            _ => Err(Error::BadValue {
                expr: expr.to_string(),
            }),
        }
    }
}

#[derive(Debug)]
struct Scope {
    /// Binding from name -> expression
    bindings: HashMap<String, Value>,
}

impl Scope {
    fn new() -> Scope {
        Scope {
            bindings: HashMap::new(),
        }
    }

    fn resolve(&self, ident: &String) -> Option<&Value> {
        self.bindings.get(ident)
    }

    fn bind(&mut self, ident: String, v: Value) {
        self.bindings.insert(ident, v);
    }
}

#[derive(Debug)]
struct VM {
    // Stack of scopes. The tail is the most recent scope
    scopes: std::vec::Vec<Scope>,
}

impl VM {
    fn new() -> VM {
        VM {
            scopes: vec![Scope::new()],
        }
    }

    fn interpret(&mut self, exprs: Vec<Expr>) -> Result<()> {
        for expr in exprs {
            self.interpret_expr(&expr)?;
        }
        Ok(())
    }

    fn resolve(&self, ident: &String) -> Result<&Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.resolve(ident) {
                return Ok(v);
            }
        }
        return Err(Error::Unbound {
            name: ident.to_string(),
        });
    }

    fn bind(&mut self, ident: String, v: Value) {
        self.scopes.last().expect("expected a scope").bind(ident, v);
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn apply(
        &mut self,
        ident: String,
        bindings: Vec<Binding>,
        args: Vec<Expr>,
        body: &Expr,
    ) -> Result<Option<Expr>> {
        if args.len() != bindings.len() {
            return Err(Error::BadArity {
                ident,
                expected: bindings.len(),
                received: args.len(),
            });
        }
        self.push_scope();
        for (name, val) in bindings.iter().zip(args.iter()) {
            self.bind(name.clone(), Value::try_from(val.clone())?);
        }
        let result = self.interpret_expr(body)?;
        self.pop_scope();

        Ok(result)
    }

    fn is_native(&self, ident: &str) -> bool {
        match ident {
            "+" | "-" | "*" | "/" | "%" | "print" => true,
            _ => false,
        }
    }

    fn apply_native(&mut self, ident: &str, args: Vec<Expr>) -> Result<()> {
        match ident {
            "+" => {
                if args.len() != 2 {
                    return Err(Error::BadArity {
                        ident: "+".to_string(),
                        expected: 2,
                        received: args.len(),
                    });
                }
                Ok(Expr::Literal(op::add(args[1], args[2])?))
            }
        }
    }

    fn interpret_expr(&mut self, expr: &Expr) -> Result<Option<Expr>> {
        use Expr::*;
        match expr {
            Call(ident, args) => {
                if self.is_native(ident) {
                    return self.apply_native(&ident, args.clone());
                }
                match self.resolve(ident)? {
                    Value::Literal(l) => Err(Error::Uncallable {
                        what: l.to_string(),
                    }),
                    Value::Fn(bindings, body) => self.apply(bindings, args, body),
                }
            }
            Definition(ident, body) => {
                self.bind(ident.clone(), Value::try_from(body.into())?);
                Ok(None)
            }
            _ => Ok(None),
        }
    }
}
