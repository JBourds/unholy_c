use crate::ast;

#[derive(Debug, PartialEq)]
pub struct Program {
    function: Function,
}

impl From<ast::Program<'_>> for Program {
    fn from(_node: ast::Program<'_>) -> Self {
        unimplemented!()
    }
}

#[derive(Debug, PartialEq)]
pub struct Function {
    name: String,
    instructions: Vec<Instruction>,
}
impl From<ast::Function<'_>> for Function {
    fn from(_node: ast::Function<'_>) -> Self {
        unimplemented!()
    }
}

#[derive(Debug, PartialEq)]
pub enum Instruction {
    Return(Option<Val>),
    Unary { op: UnaryOp, src: Val, dst: Val },
}
impl From<ast::Stmt> for Vec<Instruction> {
    fn from(node: ast::Stmt) -> Self {
        let mut instructions = vec![];
        match node {
            //ast::Stmt::Return(Some(expr)) => {
            //
            //}
            ast::Stmt::Return(None) => {
                instructions.push(Instruction::Return(None));
            }
            _ => todo!(),
        }
        instructions
    }
}

// TODO: Other types
#[derive(Debug, PartialEq)]
pub enum Val {
    Constant(i32),
    Var(String),
}
impl From<ast::Literal> for Val {
    fn from(node: ast::Literal) -> Self {
        match node {
            ast::Literal::Int(i) => Self::Constant(i),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Complement,
    Negate,
}

// TODO: I created a separated version of this struct to prevent leaked from
// different modules into each other but these structs are literally identical
impl From<ast::Unary> for UnaryOp {
    fn from(node: ast::Unary) -> Self {
        match node {
            ast::Unary::Complement => Self::Complement,
            ast::Unary::Negate => Self::Negate,
        }
    }
}

#[cfg(test)]
mod tests {
    //use super::*;

    #[test]
    fn test_return_literal() {
        //let ast = ast::Stmt::Return(Some(ast::Expr::Literal(ast::Literal::Int(2))));
        //let actual = Vec::<Instruction>::try_from(ast).unwrap();
        //let expected = vec![Instruction::Return(Val::Constant(2))];
        //assert_eq!(actual, expected);
    }
    #[test]
    fn test_return_unary() {
        //let ast = ast::Stmt::Return(Some(ast::Expr::Unary(
        //    ast::Unary::Complement,
        //    Box::new(ast::Expr::Literal(ast::Literal::Int(2))),
        //)));
        //let actual = Vec::<Instruction>::try_from(ast).unwrap();
        //let expected = vec![
        //    Instruction::Unary {
        //        op: UnaryOp::Complement,
        //        src: Val::Constant(2),
        //        dst: Val::Var("tmp.0".to_string()),
        //    },
        //    Instruction::Return(Val::Var("tmp.0".to_string())),
        //];
        //assert_eq!(actual, expected);
    }
    #[test]
    fn test_return_nested_unary() {
        //let ast = ast::Stmt::Return(Some(ast::Expr::Unary(
        //    ast::Unary::Negate,
        //    Box::new(ast::Expr::Unary(
        //        ast::Unary::Complement,
        //        Box::new(ast::Expr::Unary(
        //            ast::Unary::Negate,
        //            Box::new(ast::Expr::Literal(ast::Literal::Int(2))),
        //        )),
        //    )),
        //)));
        //let actual = Vec::<Instruction>::try_from(ast).unwrap();
        //let expected = vec![
        //    Instruction::Unary {
        //        op: UnaryOp::Negate,
        //        src: Val::Constant(2),
        //        dst: Val::Var("tmp.0".to_string()),
        //    },
        //    Instruction::Unary {
        //        op: UnaryOp::Complement,
        //        src: Val::Var("tmp.0".to_string()),
        //        dst: Val::Var("tmp.1".to_string()),
        //    },
        //    Instruction::Unary {
        //        op: UnaryOp::Negate,
        //        src: Val::Var("tmp.1".to_string()),
        //        dst: Val::Var("tmp.2".to_string()),
        //    },
        //    Instruction::Return(Val::Var("tmp.2".to_string())),
        //];
        //assert_eq!(actual, expected);
    }
}
