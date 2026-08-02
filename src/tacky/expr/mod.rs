use crate::sema::tc::TypeTable;

use super::*;

mod arrow;
mod assignment;
mod binary;
mod cast;
mod conditional;
mod dot;
mod fun_call;
mod sizeof;
mod string;
mod subscript;
mod unary;

use arrow::*;
use assignment::*;
use binary::*;
use cast::*;
use conditional::*;
use dot::*;
use fun_call::*;
use sizeof::*;
use std::rc::Rc;
use string::*;
use subscript::*;
use unary::*;

/// Struct containing all necessary context for dependecy-injecting
/// lookup tables throughout stage.
#[derive(Debug)]
pub struct Ctx {
    pub symbols: SymbolTable,
    pub structs: TypeTable,

    // For creating unique names
    _scopes: Vec<String>,
    _counter: usize,
}

impl Ctx {
    pub fn new(symbols: SymbolTable, structs: TypeTable) -> Self {
        Self {
            symbols,
            structs,
            _scopes: Vec::new(),
            _counter: 0,
        }
    }

    pub fn get_struct_member_type(
        &self,
        struct_name: &Rc<String>,
        offset: usize,
    ) -> Option<ast::Type> {
        self.structs
            .get(struct_name)
            .and_then(|struct_t| struct_t.get_member_at_offset(offset))
            .map(|mem_entry| mem_entry.r#type.clone())
    }

    pub fn with_scope<T>(&mut self, scope: impl Into<String>, fun: impl FnOnce() -> T) -> T {
        self.push_scope(scope);
        let res = fun();
        self.pop_scope();
        res
    }

    pub fn push_scope(&mut self, scope: impl Into<String>) {
        self._scopes.push(scope.into());
    }

    pub fn pop_scope(&mut self) -> Option<String> {
        self._scopes.pop()
    }

    pub fn make_temp_var_name(&mut self) -> String {
        self._scopes.push(self._counter.to_string());
        self._counter += 1;
        let s = self._scopes.join(".");
        self._scopes.pop();
        s
    }

    pub fn make_temp_var(&mut self, ty: ast::Type) -> Val {
        let name = Rc::new(self.make_temp_var_name());
        self.symbols.new_entry(Rc::clone(&name), ty);
        Val::Var(name)
    }
}

#[derive(Debug)]
pub enum ExprResult {
    PlainOperand(Expr),
    DereferencedPointer(Expr),
    SubObject { base: Rc<String>, offset: usize },
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
        ctx: &mut Ctx,
    ) -> (Vec<Instruction>, Val) {
        let mut instructions = vec![];
        let left_t = left.get_type(&ctx.symbols);
        let right_t = right.get_type(&ctx.symbols);

        // pointer subtraction is special- returns number of indices between them
        if matches!(op, ast::BinaryOp::Subtract) && left_t.is_pointer() && right_t.is_pointer() {
            let byte_diff = ctx.make_temp_var(ast::Type::PTRDIFF_T);
            instructions.push(Instruction::Binary {
                op: BinaryOp::Subtract,
                src1: left,
                src2: right,
                dst: byte_diff.clone(),
            });
            let index_diff = ctx.make_temp_var(ast::Type::PTRDIFF_T);
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
            let negated_tmp = ctx.make_temp_var(index.get_type(&ctx.symbols));
            instructions.push(Instruction::Unary {
                op: UnaryOp::Negate,
                src: index,
                dst: negated_tmp.clone(),
            });
            index = negated_tmp;
        }
        let scale = ptr_t.clone().deref().size_of();
        let dst = ctx.make_temp_var(ptr_t);
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

    fn parse_with(node: ast::Expr, ctx: &mut Ctx) -> ExprResult {
        match node {
            ast::Expr::Constant(v) => ExprResult::PlainOperand(Self {
                instructions: vec![],
                val: Val::from(v),
            }),
            ast::Expr::Unary {
                op: ast::UnaryOp::Deref,
                expr,
            } => ExprResult::DereferencedPointer(Self::parse_with_and_convert(*expr, ctx)),
            ast::Expr::Unary { .. } => parse_unary(node, ctx),
            ast::Expr::Binary { .. } => parse_binary(node, ctx),
            ast::Expr::Var(name) => ExprResult::PlainOperand(Self {
                instructions: vec![],
                val: Val::Var(name),
            }),
            ast::Expr::Assignment { .. } => parse_assignment(node, ctx),
            ast::Expr::Conditional { .. } => parse_conditional(node, ctx),
            ast::Expr::FunCall { .. } => parse_fun_call(node, ctx),
            ast::Expr::Cast { .. } => parse_cast(node, ctx),
            ast::Expr::Subscript { .. } => parse_subscript(node, ctx),
            ast::Expr::String { .. } => parse_string(node, ctx),
            ast::Expr::SizeOfT(_) => parse_sizeof_type(node),
            ast::Expr::SizeOf(_) => unreachable!(
                "This branch should have been rewritten to SizeOfT during typechecking"
            ),
            ast::Expr::Dot { .. } => todo!(),
            ast::Expr::Arrow { .. } => todo!(),
        }
    }

    pub fn cast(val: Val, target: ast::Type, ctx: &mut Ctx) -> Self {
        let val_type = val.get_type(&ctx.symbols);
        if target == val_type {
            return Self {
                instructions: vec![],
                val,
            };
        }

        let mut emitter = CastEmitter::new(ctx);
        let dst = if target.is_void() {
            Val::dummy()
        } else {
            emitter.temp(target.clone())
        };
        if !target.is_void() {
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
        }
        Self {
            instructions: emitter.instructions,
            val: dst,
        }
    }

    pub(crate) fn convert(node: ExprResult, ctx: &mut Ctx) -> Expr {
        match node {
            ExprResult::PlainOperand(expr) => expr,
            ExprResult::DereferencedPointer(expr) => {
                let Self {
                    mut instructions,
                    val,
                } = expr;
                let dst_t = val.get_type(&ctx.symbols).deref();
                let dst = ctx.make_temp_var(dst_t);
                instructions.push(Instruction::Load {
                    src_ptr: val,
                    dst: dst.clone(),
                });
                Self {
                    instructions,
                    val: dst,
                }
            }
            ExprResult::SubObject { base, offset } => {
                let member_t = ctx
                    .get_struct_member_type(&base, offset)
                    .expect("couldn't get struct member");
                let dst = ctx.make_temp_var(member_t.clone());
                let instructions = vec![Instruction::CopyFromOffset {
                    src: base,
                    dst: dst.clone(),
                    offset: offset.try_into().unwrap(),
                }];
                Self {
                    instructions,
                    val: dst,
                }
            }
        }
    }

    pub(crate) fn parse_with_and_convert(node: ast::Expr, ctx: &mut Ctx) -> Self {
        let parsed = Self::parse_with(node, ctx);
        Self::convert(parsed, ctx)
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
struct CastEmitter<'a> {
    instructions: Vec<Instruction>,
    ctx: &'a mut Ctx,
}

impl<'a> CastEmitter<'a> {
    fn new(ctx: &'a mut Ctx) -> Self {
        Self {
            instructions: vec![],
            ctx,
        }
    }

    fn temp(&mut self, t: ast::Type) -> Val {
        self.ctx.make_temp_var(t)
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
