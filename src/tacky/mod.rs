use crate::ast;
use crate::sema;
use std::collections::HashMap;
use std::rc::Rc;

mod binary_op;
mod expr;
mod function;
mod instruction;
mod program;
mod static_variable;
mod symbol_table;
mod unary_op;
mod val;

pub use binary_op::*;
pub use expr::*;
pub use function::*;
pub use instruction::*;
pub use program::*;
pub use static_variable::*;
pub use symbol_table::*;
pub use unary_op::*;
pub use val::*;

#[derive(Debug, PartialEq)]
pub enum TopLevel {
    Fun(Function),
    Static(StaticVariable),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_return_literal() {
        let mut symbols = SymbolTable::default();
        let ast = ast::Block(vec![ast::BlockItem::Stmt(ast::Stmt::Return(Some(
            ast::Expr::Constant(ast::Constant::I32(2)),
        )))]);
        let mut counter = 0;
        let mut make_temp_var = Function::make_temp_var(Rc::new("test".to_string()), &mut counter);
        let actual = Instruction::parse_block_with(ast, &mut symbols, &mut make_temp_var);
        let expected = vec![Instruction::Return(Some(Val::Constant(
            ast::Constant::I32(2),
        )))];
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_return_unary() {
        let mut symbols = SymbolTable::default();
        let ast = ast::Block(vec![ast::BlockItem::Stmt(ast::Stmt::Return(Some(
            ast::Expr::Unary {
                op: ast::UnaryOp::Complement,
                expr: Box::new(ast::Expr::Constant(ast::Constant::I32(2))),
            },
        )))]);
        let mut counter = 0;
        let mut make_temp_var = Function::make_temp_var(Rc::new("test".to_string()), &mut counter);
        let actual = Instruction::parse_block_with(ast, &mut symbols, &mut make_temp_var);
        let expected = vec![
            Instruction::Unary {
                op: UnaryOp::Complement,
                src: Val::Constant(ast::Constant::I32(2)),
                dst: Val::Var("tacky.test.0".to_string().into()),
            },
            Instruction::Return(Some(Val::Var("tacky.test.0".to_string().into()))),
        ];
        assert_eq!(actual, expected);
    }
    #[test]
    fn test_return_nested_unary() {
        let mut symbols = SymbolTable::default();
        let ast = ast::Block(vec![ast::BlockItem::Stmt(ast::Stmt::Return(Some(
            ast::Expr::Unary {
                op: ast::UnaryOp::Negate,
                expr: Box::new(ast::Expr::Unary {
                    op: ast::UnaryOp::Complement,
                    expr: Box::new(ast::Expr::Unary {
                        op: ast::UnaryOp::Negate,
                        expr: Box::new(ast::Expr::Constant(ast::Constant::I32(2))),
                    }),
                }),
            },
        )))]);
        let mut counter = 0;
        let mut make_temp_var = Function::make_temp_var(Rc::new("test".to_string()), &mut counter);
        let actual = Instruction::parse_block_with(ast, &mut symbols, &mut make_temp_var);
        let expected = vec![
            Instruction::Unary {
                op: UnaryOp::Negate,
                src: Val::Constant(ast::Constant::I32(2)),
                dst: Val::Var("tacky.test.0".to_string().into()),
            },
            Instruction::Unary {
                op: UnaryOp::Complement,
                src: Val::Var("tacky.test.0".to_string().into()),
                dst: Val::Var("tacky.test.1".to_string().into()),
            },
            Instruction::Unary {
                op: UnaryOp::Negate,
                src: Val::Var("tacky.test.1".to_string().into()),
                dst: Val::Var("tacky.test.2".to_string().into()),
            },
            Instruction::Return(Some(Val::Var("tacky.test.2".to_string().into()))),
        ];
        assert_eq!(actual, expected);
    }
}
