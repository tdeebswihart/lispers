use crate::parse::{Atom, Expr};
use snafu::{ResultExt, Snafu};

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("`{}` is not defined on [{}]", op, values.join(", ")))]
    TypeError { op: String, values: Vec<String> },
}
pub type Result<T, E = Error> = std::result::Result<T, E>;

pub fn add(a: Atom, b: Atom) -> Result<Atom> {
    use Atom::*;
    match (a, b) {
        (Integer(ai), Integer(bi)) => Ok(Integer(ai + bi)),
        (Float(af), Float(bf)) => Ok(Float(af + bf)),
        _ => Err(Error::TypeError {
            op: "+".to_string(),
            values: vec![a.to_string(), b.to_string()],
        }),
    }
}
