use super::*;

#[derive(Debug)]
pub enum ExprResult {
    PlainOperand(Expr),
    DerefrencedPointer(Expr),
}
