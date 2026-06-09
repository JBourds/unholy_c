use super::*;

mod assignment;
mod binary;
mod cast;
mod conditional;
mod fun_call;
mod string;
mod subscript;
mod unary;

use assignment::*;
use binary::*;
use cast::*;
use conditional::*;
use fun_call::*;
use string::*;
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
            ast::Expr::String { .. } => parse_string(node, symbols),
            ast::Expr::SizeOf(_) => todo!(),
            ast::Expr::SizeOfT(_) => todo!(),
        }
    }

    pub fn cast(
        val: Val,
        target: ast::Type,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> Self {
        let val_type = val.get_type(symbols);
        if target == val_type {
            return Self {
                instructions: vec![],
                val,
            };
        }

        let mut emitter = CastEmitter::new(symbols, make_temp_var);
        let dst = emitter.temp(target.clone());
        match (Scalar::of(&val_type), Scalar::of(&target)) {
            (
                Scalar::Int { bytes: src, signed },
                Scalar::Int {
                    bytes: dst_bytes, ..
                },
            ) => {
                emitter.resize(val, src, signed, dst.clone(), dst_bytes);
            }
            (Scalar::Int { bytes, signed }, Scalar::F64) => {
                emitter.int_to_double(val, bytes, signed, dst.clone());
            }
            (Scalar::F64, Scalar::Int { bytes, signed }) => {
                emitter.double_to_int(val, dst.clone(), bytes, signed);
            }
            // We don't have f32 but this is where we would slot them in
            (Scalar::Int { .. }, Scalar::F32)
            | (Scalar::F32, Scalar::Int { .. })
            | (Scalar::F32, Scalar::F64)
            | (Scalar::F64, Scalar::F32) => {
                todo!("conversions involving 32-bit float are not implemented yet")
            }
            // Same-type float casts are caught by the early `target == val_type`.
            (Scalar::F32, Scalar::F32) | (Scalar::F64, Scalar::F64) => {
                unreachable!("identity float cast should have returned early")
            }
        }
        Self {
            instructions: emitter.instructions,
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
        let parsed = Self::parse_with(node, symbols, make_temp_var);
        Self::convert(parsed, symbols, make_temp_var)
    }
}

/// A scalar type reduced to what casting actually depends on. A char is just a
/// 1-byte integer here, so the four char-specific conversion paths collapse into
/// the integer paths. The two floating-point widths are kept distinct so f32
/// support can be slotted in without restructuring.
enum Scalar {
    F32,
    F64,
    Int { bytes: usize, signed: bool },
}

impl Scalar {
    fn of(t: &ast::Type) -> Self {
        match &t.base {
            ast::BaseType::Float(_) => Self::F32,
            ast::BaseType::Double(_) => Self::F64,
            ast::BaseType::Int { nbytes, signed } => Self::Int {
                bytes: *nbytes,
                signed: signed.unwrap_or(true),
            },
            // Pointers cast as unsigned machine-word integers.
            ast::BaseType::Ptr { .. } => Self::Int {
                bytes: t.base.nbytes(),
                signed: false,
            },
            other => unreachable!("not a scalar cast operand: {other:?}"),
        }
    }
}

/// Accumulates instructions for a single scalar conversion adding any
/// intermediate temporaries it needs. Conversions to/from `double` require the
/// integer side to be at least int-width, so a 1-byte operand is widened
/// (before int->double) or produced wide and truncated (after double->int).
struct CastEmitter<'a, F: FnMut() -> String> {
    instructions: Vec<Instruction>,
    symbols: &'a mut SymbolTable,
    make_temp_var: &'a mut F,
}

impl<'a, F: FnMut() -> String> CastEmitter<'a, F> {
    fn new(symbols: &'a mut SymbolTable, make_temp_var: &'a mut F) -> Self {
        Self {
            instructions: vec![],
            symbols,
            make_temp_var,
        }
    }

    fn temp(&mut self, t: ast::Type) -> Val {
        Function::make_tacky_temp_var(t, self.symbols, self.make_temp_var)
    }

    /// A 32-bit temporary matching `signed`, used to stage char conversions.
    fn int32(&mut self, signed: bool) -> Val {
        self.temp(if signed {
            ast::Type::I32
        } else {
            ast::Type::U32
        })
    }

    /// int -> int: sign/zero-extend, truncate, or copy by relative width.
    fn resize(&mut self, src: Val, src_bytes: usize, signed: bool, dst: Val, dst_bytes: usize) {
        self.instructions.push(match dst_bytes.cmp(&src_bytes) {
            std::cmp::Ordering::Equal => Instruction::Copy { src, dst },
            std::cmp::Ordering::Less => Instruction::Truncate { src, dst },
            std::cmp::Ordering::Greater if signed => Instruction::SignExtend { src, dst },
            std::cmp::Ordering::Greater => Instruction::ZeroExtend { src, dst },
        });
    }

    /// int -> double, widening a 1-byte source to int width first.
    fn int_to_double(&mut self, src: Val, src_bytes: usize, signed: bool, dst: Val) {
        let src = if src_bytes == 1 {
            let wide = self.int32(signed);
            self.instructions.push(if signed {
                Instruction::SignExtend {
                    src,
                    dst: wide.clone(),
                }
            } else {
                Instruction::ZeroExtend {
                    src,
                    dst: wide.clone(),
                }
            });
            wide
        } else {
            src
        };
        self.instructions.push(if signed {
            Instruction::IntToDouble { src, dst }
        } else {
            Instruction::UIntToDouble { src, dst }
        });
    }

    /// double -> int, producing a 1-byte destination at int width then truncating.
    fn double_to_int(&mut self, src: Val, dst: Val, dst_bytes: usize, signed: bool) {
        let wide = if dst_bytes == 1 {
            self.int32(signed)
        } else {
            dst.clone()
        };
        self.instructions.push(if signed {
            Instruction::DoubleToInt {
                src,
                dst: wide.clone(),
            }
        } else {
            Instruction::DoubleToUInt {
                src,
                dst: wide.clone(),
            }
        });
        if dst_bytes == 1 {
            self.instructions.push(Instruction::Copy { src: wide, dst });
        }
    }
}
