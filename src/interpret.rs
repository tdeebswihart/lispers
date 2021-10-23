use crate::op;
use crate::parse::{Atom, Binding, Expr};
use snafu::Snafu;
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
    #[snafu(display("type error: `{}` is not defined on [{}]", op, types.join(" ")))]
    BadType { op: String, types: Vec<String> },
    #[snafu(display("`{}` expected a value for arg {}, not `nil`", op, index))]
    ValueExpected { op: String, index: usize },
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
    Fn(Vec<Binding>, Box<Expr>),
}

impl TryFrom<Expr> for Value {
    type Error = Error;

    fn try_from(expr: Expr) -> Result<Self, Self::Error> {
        use Expr::*;
        match expr {
            Literal(a) => Ok(Value::Literal(a)),
            Lambda(bindings, body) => Ok(Value::Fn(bindings, body)),
            _ => Err(Error::BadValue {
                expr: expr.to_string(),
            }),
        }
    }
}

impl TryFrom<Box<Expr>> for Value {
    type Error = Error;

    fn try_from(expr: Box<Expr>) -> Result<Self, Self::Error> {
        use Expr::*;
        match *expr {
            Literal(a) => Ok(Value::Literal(a)),
            Lambda(bindings, body) => Ok(Value::Fn(bindings, body)),
            _ => Err(Error::BadValue {
                expr: expr.to_string(),
            }),
        }
    }
}

impl Into<Expr> for Value {
    fn into(self) -> Expr {
        match self {
            Value::Literal(a) => Expr::Literal(a),
            Value::Fn(bindings, body) => Expr::Lambda(bindings, body),
        }
    }
}

#[derive(Debug)]
struct Scope {
    /// Binding from name -> expression
    bindings: HashMap<String, Value>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            bindings: HashMap::new(),
        }
    }

    pub fn resolve(&self, ident: &String) -> Option<&Value> {
        self.bindings.get(ident)
    }

    pub fn bind(&mut self, ident: String, v: Value) {
        trace!("binding {:?} to {}", v, ident);
        self.bindings.insert(ident, v);
    }
}

#[derive(Debug)]
pub struct VM {
    // Stack of scopes. The tail is the most recent scope
    scopes: std::vec::Vec<Scope>,
}

macro_rules! unit {
    () => {
        Expr::Literal(Atom::Unit)
    };
}

impl VM {
    pub fn new() -> VM {
        VM {
            scopes: vec![Scope::new()],
        }
    }

    pub fn interpret(&mut self, exprs: Vec<Expr>) -> Result<()> {
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
        self.scopes
            .last_mut()
            .expect("expected a scope")
            .bind(ident, v);
    }

    fn apply(
        &mut self,
        ident: &String,
        bindings: Vec<Binding>,
        args: &Vec<Expr>,
        body: Box<Expr>,
    ) -> Result<Expr> {
        if args.len() != bindings.len() {
            return Err(Error::BadArity {
                ident: ident.to_string(),
                expected: bindings.len(),
                received: args.len(),
            });
        }
        let mut call_scope = Scope::new();
        for (name, val) in bindings.into_iter().zip(args.iter()) {
            call_scope.bind(name, Value::try_from(val.clone())?);
        }
        self.scopes.push(call_scope);
        let result = self.interpret_expr(&body)?;
        self.scopes.pop();
        Ok(result)
    }

    fn is_native(&self, ident: &str) -> bool {
        match ident {
            "+" | "-" | "*" | "/" | "%" | "print" => true,
            _ => false,
        }
    }

    fn apply_native(&mut self, ident: &str, args: &Vec<Expr>) -> Result<Expr> {
        match ident {
            "print" => {
                let mut finalized = std::vec::Vec::with_capacity(args.len());
                for arg in args {
                    let expr = self.interpret_expr(arg)?;
                    if let Expr::Literal(a) = expr {
                        finalized.push(a);
                    } else {
                        return Err(Error::BadType {
                            op: ident.to_string(),
                            types: vec![expr.to_string()],
                        });
                    }
                }
                op::print(&finalized);
                Ok(unit!())
            }
            "+" => {
                if args.len() != 2 {
                    return Err(Error::BadArity {
                        ident: ident.to_string(),
                        expected: 2,
                        received: args.len(),
                    });
                }
                let a = self.interpret_expr(&args[0])?;
                let b = self.interpret_expr(&args[1])?;
                match (a, b) {
                    (unit!(), _) => Err(Error::ValueExpected {
                        op: ident.to_string(),
                        index: 0,
                    }),
                    (_, unit!()) => Err(Error::ValueExpected {
                        op: ident.to_string(),
                        index: 1,
                    }),
                    (Expr::Literal(va), Expr::Literal(vb)) => Ok(Expr::Literal(op::add(va, vb)?)),
                    (a, b) => Err(Error::BadType {
                        op: ident.to_string(),
                        types: vec![a.to_string(), b.to_string()],
                    }),
                }
            }
            _ => Err(Error::Unbound {
                name: ident.to_string(),
            }),
        }
    }

    fn interpret_expr(&mut self, expr: &Expr) -> Result<Expr> {
        use Expr::*;
        trace!("interpreting {:?}", expr);
        match expr {
            Literal(Atom::Ident(ident)) => Ok(self.resolve(ident)?.clone().into()),
            Call(ident, args) => {
                if self.is_native(ident) {
                    return self.apply_native(&ident, args);
                }
                let res = self.resolve(ident)?.clone();
                match res {
                    Value::Literal(l) => Err(Error::Uncallable {
                        what: l.to_string(),
                    }),
                    Value::Fn(bindings, body) => self.apply(ident, bindings, args, body),
                }
            }
            Definition(ident, body) => {
                self.bind(ident.clone(), Value::try_from(body.clone())?);
                Ok(unit!())
            }
            _ => Ok(expr.clone()),
        }
    }
}
