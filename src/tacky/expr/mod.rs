use super::*;

mod assignment;
mod binary;
mod cast;
mod conditional;
mod fun_call;
mod subscript;
mod unary;

use assignment::*;
use binary::*;
use cast::*;
use conditional::*;
use fun_call::*;
use subscript::*;
use unary::*;

#[derive(Debug)]
pub enum ExprResult {
    PlainOperand(Expr),
    DereferencedPointer(Expr),
}

#[derive(Debug, PartialEq)]
pub struct Expr {
    pub instructions: Vec<Instruction>,
    pub val: Val,
}

impl Expr {
    fn do_pointer_arithmetic(
        op: ast::BinaryOp,
        left: Val,
        right: Val,
        make_temp_var: &mut impl FnMut() -> String,
        symbols: &mut SymbolTable,
    ) -> (Vec<Instruction>, Val) {
        let mut instructions = vec![];
        let left_t = left.get_type(symbols);
        let right_t = right.get_type(symbols);

        // pointer subtraction is special- returns number of indices between them
        if matches!(op, ast::BinaryOp::Subtract) && left_t.is_pointer() && right_t.is_pointer() {
            let byte_diff =
                Function::make_tacky_temp_var(ast::Type::PTRDIFF_T, symbols, make_temp_var);
            instructions.push(Instruction::Binary {
                op: BinaryOp::Subtract,
                src1: left,
                src2: right,
                dst: byte_diff.clone(),
            });
            let index_diff =
                Function::make_tacky_temp_var(ast::Type::PTRDIFF_T, symbols, make_temp_var);
            instructions.push(Instruction::Binary {
                op: BinaryOp::Divide,
                src1: byte_diff,
                src2: Val::Constant(ast::Constant::I64(
                    left_t
                        .deref()
                        .size_of()
                        .try_into()
                        .expect("could not handle ptrdiff size"),
                )),
                dst: index_diff.clone(),
            });
            return (instructions, index_diff);
        }

        let (ptr, ptr_t, mut index) = if left_t.is_pointer() || left_t.is_array() {
            (left, left_t.maybe_decay(), right)
        } else {
            (right, right_t.maybe_decay(), left)
        };

        if op.is_sub() {
            let negated_tmp =
                Function::make_tacky_temp_var(index.get_type(symbols), symbols, make_temp_var);
            instructions.push(Instruction::Unary {
                op: UnaryOp::Negate,
                src: index,
                dst: negated_tmp.clone(),
            });
            index = negated_tmp;
        }
        let scale = ptr_t.clone().deref().size_of();
        let dst = Function::make_tacky_temp_var(ptr_t, symbols, make_temp_var);
        instructions.push(Instruction::AddPtr {
            ptr,
            index,
            scale,
            dst: dst.clone(),
        });
        (instructions, dst)
    }

    fn unary_inc_dec_val(t: &ast::Type) -> ast::Constant {
        // Typechecking will have caught any case where an array is invalid as
        // a pointer
        assert!(
            !t.is_array(),
            "Should not have any arrays being incremented!"
        );
        if t.is_pointer() {
            ast::Constant::U64(t.base.size_of_base_type().try_into().unwrap())
        } else {
            ast::Constant::const_from_type(t, 1)
                .expect("UnaryOp type has an ast::Constant equivalent")
        }
    }

    fn parse_with(
        node: ast::Expr,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> ExprResult {
        match node {
            ast::Expr::Constant(v) => ExprResult::PlainOperand(Self {
                instructions: vec![],
                val: Val::from(v),
            }),
            ast::Expr::Unary {
                op: ast::UnaryOp::Deref,
                expr,
            } => ExprResult::DereferencedPointer(Self::parse_with_and_convert(
                *expr,
                symbols,
                make_temp_var,
            )),
            ast::Expr::Unary { .. } => parse_unary(node, symbols, make_temp_var),
            ast::Expr::Binary { .. } => parse_binary(node, symbols, make_temp_var),
            ast::Expr::Var(name) => ExprResult::PlainOperand(Self {
                instructions: vec![],
                val: Val::Var(name),
            }),
            ast::Expr::Assignment { .. } => parse_assignment(node, symbols, make_temp_var),
            ast::Expr::Conditional { .. } => parse_conditional(node, symbols, make_temp_var),
            ast::Expr::FunCall { .. } => parse_fun_call(node, symbols, make_temp_var),
            ast::Expr::Cast { .. } => parse_cast(node, symbols, make_temp_var),
            ast::Expr::Subscript { .. } => parse_subscript(node, symbols, make_temp_var),
        }
    }

    pub fn cast(
        val: Val,
        target: ast::Type,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> Self {
        // I do this cause get_type currently clones on vars
        let val_type = val.get_type(symbols);
        let mut instructions = vec![];
        if target == val_type {
            return Self { instructions, val };
        }
        let dst = Function::make_tacky_temp_var(target.clone(), symbols, make_temp_var);

        let is_float = |t: &ast::Type| {
            matches!(
                t,
                ast::Type {
                    base: ast::BaseType::Float(_) | ast::BaseType::Double(_),
                    ..
                }
            )
        };

        // Double -> Integer
        if is_float(&val_type) {
            match target {
                ast::Type {
                    base:
                        ast::BaseType::Int {
                            nbytes: _,
                            signed: Some(false),
                        },
                    ..
                } => {
                    instructions.push(Instruction::DoubleToUInt {
                        src: val,
                        dst: dst.clone(),
                    });
                }
                ast::Type {
                    base:
                        ast::BaseType::Int {
                            nbytes: _,
                            signed: _,
                        },
                    ..
                } => {
                    instructions.push(Instruction::DoubleToInt {
                        src: val,
                        dst: dst.clone(),
                    });
                }
                // FIXME: Add chars here
                // We should not ever be trying to cast a double to
                // anything other than an int
                _ => unreachable!("Casting float type to {target:?}"),
            }
        } else if is_float(&target) {
            match val_type {
                ast::Type {
                    base:
                        ast::BaseType::Int {
                            nbytes: _,
                            signed: Some(false),
                        },
                    ..
                } => {
                    instructions.push(Instruction::UIntToDouble {
                        src: val,
                        dst: dst.clone(),
                    });
                }
                ast::Type {
                    base:
                        ast::BaseType::Int {
                            nbytes: _,
                            signed: _,
                        },
                    ..
                } => {
                    instructions.push(Instruction::IntToDouble {
                        src: val,
                        dst: dst.clone(),
                    });
                }
                // FIXME: Add chars here
                // We should not ever be trying to cast a double to
                // anything other than an int
                _ => unreachable!("Casting float type to {target:?}"),
            }
        } else {
            // Integer ops
            // FIXME: This needs to use PartialEq/Eq
            match target.base.nbytes().cmp(&val_type.base.nbytes()) {
                std::cmp::Ordering::Equal => {
                    instructions.push(Instruction::Copy {
                        src: val,
                        dst: dst.clone(),
                    });
                }
                std::cmp::Ordering::Less => {
                    instructions.push(Instruction::Truncate {
                        src: val,
                        dst: dst.clone(),
                    });
                }
                _ => match val_type {
                    ast::Type {
                        base: ast::BaseType::Int { signed, .. },
                        ..
                    } => {
                        if signed.is_none_or(|signed| signed) {
                            instructions.push(Instruction::SignExtend {
                                src: val,
                                dst: dst.clone(),
                            });
                        } else {
                            instructions.push(Instruction::ZeroExtend {
                                src: val,
                                dst: dst.clone(),
                            });
                        }
                    }
                    _ => unimplemented!(),
                },
            }
        }
        Self {
            instructions,
            val: dst,
        }
    }

    pub(crate) fn convert(
        node: ExprResult,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> Expr {
        match node {
            ExprResult::PlainOperand(expr) => expr,
            ExprResult::DereferencedPointer(expr) => {
                let Self {
                    mut instructions,
                    val,
                } = expr;
                let dst = Function::make_tacky_temp_var(
                    val.get_type(symbols).deref(),
                    symbols,
                    make_temp_var,
                );
                instructions.push(Instruction::Load {
                    src_ptr: val,
                    dst: dst.clone(),
                });
                Self {
                    instructions,
                    val: dst,
                }
            }
        }
    }

    pub(crate) fn parse_with_and_convert(
        node: ast::Expr,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> Self {
        Self::convert(
            Self::parse_with(node, symbols, make_temp_var),
            symbols,
            make_temp_var,
        )
    }
}
