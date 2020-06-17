pub mod expr;
use crate::{lexer::Token, CompileResult};

use expr::Expr;

pub fn parse(_tokens: &[Token]) -> CompileResult<Expr> {
    Ok(Expr::None)
}
