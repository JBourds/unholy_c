use crate::ast;
use crate::sema;
use std::collections::HashMap;
use std::rc::Rc;

mod binary_op;
mod expr;
mod function;
mod instruction;
mod mov_chunker;
mod program;
mod static_constant;
mod static_init;
mod static_variable;
mod symbol_table;
mod unary_op;
mod val;

pub use binary_op::*;
pub use expr::*;
pub use function::*;
pub use instruction::*;
pub use program::*;
pub use static_constant::*;
pub use static_init::*;
pub use static_variable::*;
pub use symbol_table::*;
pub use unary_op::*;
pub use val::*;

#[derive(Debug, PartialEq)]
pub enum TopLevel {
    Fun(Function),
    StaticVariable(StaticVariable),
    StaticConstant(StaticConstant),
}

#[cfg(test)]
mod tests {
    use crate::sema::tc::TypeTable;

    use super::*;

    #[test]
    fn test_return_literal() {
        let symbols = SymbolTable::default();
        let ast = ast::Block(vec![ast::BlockItem::Stmt(ast::Stmt::Return(Some(
            ast::Expr::Constant(ast::Constant::I32(2)),
        )))]);
        let mut ctx = Ctx::new(symbols, TypeTable::new_table());
        let actual = Instruction::parse_block_with(ast, &mut ctx);
        let expected = vec![Instruction::Return(Some(Val::Constant(
            ast::Constant::I32(2),
        )))];
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_return_unary() {
        let symbols = SymbolTable::default();
        let ast = ast::Block(vec![ast::BlockItem::Stmt(ast::Stmt::Return(Some(
            ast::Expr::Unary {
                op: ast::UnaryOp::Complement,
                expr: Box::new(ast::Expr::Constant(ast::Constant::I32(2))),
            },
        )))]);
        let mut ctx = Ctx::new(symbols, TypeTable::new_table());
        let actual = Instruction::parse_block_with(ast, &mut ctx);
        ctx.push_scope("test");
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
        let symbols = SymbolTable::default();
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
        let mut ctx = Ctx::new(symbols, TypeTable::new_table());
        ctx.push_scope("test");
        let actual = Instruction::parse_block_with(ast, &mut ctx);
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
