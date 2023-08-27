use std::fmt::Display;

use oxc_diagnostics::{
    miette::{self, Diagnostic},
    thiserror::{self, Error},
};
use oxc_span::Span;

use crate::TypeId;


#[derive(Debug)]
pub struct CheckerError {
    pub(crate) kind: ErrorKind
}
#[derive(Debug)]
pub enum ErrorKind {
    Type0IsNotAssignableToType1(Type0IsNotAssignableToType1)
}



// TODO: what am i doing here exactly
// TS2322
#[derive(Debug, Error, Diagnostic)]
#[error("TS2322: Type is not assignable to type.")]
pub struct Type0IsNotAssignableToType1 {
    pub(crate) lhs: TypeId,
    pub(crate) rhs: TypeId,
    pub(crate) span: Span
}

