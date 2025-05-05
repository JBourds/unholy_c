use crate::ast;

use anyhow::{Context, Result, bail};
use num::cast::AsPrimitive;

pub fn eval(expr: ast::Expr) -> Result<ast::Constant> {
    let constant = eval_constant(expr).context("Could not evaluate expression at compile time")?;
    let literal = ast::Constant::from(&constant);
    Ok(literal)
}

fn eval_constant(expr: ast::Expr) -> Result<Constant> {
    match expr {
        ast::Expr::Var(_) => bail!("Variables cannot be constant evaluated"),
        ast::Expr::Assignment { .. } => {
            bail!("Assignment cannot be constant evaluated")
        }
        ast::Expr::Constant(constant) => {
            let constant = Result::<Constant>::from(&constant);
            Ok(constant.context("Failed to perform const evaluation on {constant:?}")?)
        }
        ast::Expr::Unary { op, expr } => eval_unary(op, *expr).context("UnaryOp const eval failed"),
        ast::Expr::Binary { op, left, right } => {
            eval_binary(op, *left, *right).context("BinaryOp const eval failed")
        }
        ast::Expr::Conditional {
            condition,
            then,
            r#else,
        } => {
            let condition = eval_constant(*condition)?;
            if condition.truthy() {
                eval_constant(*then)
            } else {
                eval_constant(*r#else)
            }
        }
        ast::Expr::Cast { target, exp } => {
            let exp = eval_constant(*exp)
                .context("Unable to const-evaluate expression inside of cast.")?;
            let casted = match target.base {
                ast::BaseType::Int { nbytes, signed } => match (nbytes, signed) {
                    (2, None | Some(true)) => Constant::I16(exp.cast::<i16>()),
                    (2, Some(false)) => Constant::U16(exp.cast::<u16>()),
                    (4, None | Some(true)) => Constant::I32(exp.cast::<i32>()),
                    (4, Some(false)) => Constant::U32(exp.cast::<u32>()),
                    (8, None | Some(true)) => Constant::I64(exp.cast::<i64>()),
                    (8, Some(false)) => Constant::U64(exp.cast::<u64>()),
                    _ => unreachable!(),
                },
                ast::BaseType::Float(_) => Constant::F32(exp.cast::<f32>()),
                ast::BaseType::Double(_) => Constant::F64(exp.cast::<f64>()),
                // FIXME: Should not be hardcoded whether chars are signed or not
                ast::BaseType::Char => unimplemented!(),
                _ => unreachable!(),
            };
            Ok(casted)
        }
        _ => unimplemented!(),
    }
}

#[derive(PartialEq, PartialOrd)]
enum Constant {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
}

// Implement these traits knowing they will never get called on floats
impl std::hash::Hash for Constant {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::F32(_) | Self::F64(_) => unreachable!(),
            _ => core::mem::discriminant(self).hash(state),
        }
    }
}

impl Constant {
    pub const _I8: Self = Self::I8(0);
    pub const _I16: Self = Self::I16(0);
    pub const _I32: Self = Self::I32(0);
    pub const _I64: Self = Self::I64(0);
    pub const _U8: Self = Self::U8(0);
    pub const _U16: Self = Self::U16(0);
    pub const _U32: Self = Self::U32(0);
    pub const _U64: Self = Self::U64(0);
    pub const _F32: Self = Self::F32(0.0);
    pub const _F64: Self = Self::F64(0.0);

    pub fn size_bytes(&self) -> usize {
        match self {
            Constant::I8(_) => core::mem::size_of::<i8>(),
            Constant::I16(_) => core::mem::size_of::<i16>(),
            Constant::I32(_) => core::mem::size_of::<i32>(),
            Constant::I64(_) => core::mem::size_of::<i64>(),
            Constant::U8(_) => core::mem::size_of::<u8>(),
            Constant::U16(_) => core::mem::size_of::<u16>(),
            Constant::U32(_) => core::mem::size_of::<u32>(),
            Constant::U64(_) => core::mem::size_of::<u64>(),
            Constant::F32(_) => core::mem::size_of::<f32>(),
            Constant::F64(_) => core::mem::size_of::<f64>(),
        }
    }

    fn rank(&self) -> usize {
        match self {
            Self::I8(..) => 10 * self.size_bytes(),
            Self::I16(..) => 20 * self.size_bytes(),
            Self::I32(..) => 30 * self.size_bytes(),
            Self::I64(..) => 40 * self.size_bytes(),
            // Unsigned integers are above signed counterparts
            Self::U8(..) => 11 * self.size_bytes(),
            Self::U16(..) => 21 * self.size_bytes(),
            Self::U32(..) => 31 * self.size_bytes(),
            Self::U64(..) => 41 * self.size_bytes(),
            // Floats are above everything else
            Self::F32(..) => 100 * self.size_bytes(),
            Self::F64(..) => 200 * self.size_bytes(),
        }
    }

    fn lift(lhs: Self, rhs: Self) -> (Self, Self) {
        let left = lhs.default_promote();
        let right = rhs.promote(&left);
        let left = left.promote(&right);

        (left, right)
    }

    fn default_promote(self) -> Self {
        if self.rank() >= Self::_I32.rank() {
            self
        } else {
            self.promote(&Self::_I32)
        }
    }

    fn promote(self, other: &Self) -> Self {
        let rank = other.rank();
        if self.rank() >= rank {
            return self;
        }
        match rank {
            _ if (rank == Self::_I8.rank()) => Self::I8(self.cast::<i8>()),
            _ if (rank == Self::_I16.rank()) => Self::I16(self.cast::<i16>()),
            _ if (rank == Self::_I32.rank()) => Self::I32(self.cast::<i32>()),
            _ if (rank == Self::_I64.rank()) => Self::I64(self.cast::<i64>()),
            _ if (rank == Self::_U8.rank()) => Self::U8(self.cast::<u8>()),
            _ if (rank == Self::_U16.rank()) => Self::U16(self.cast::<u16>()),
            _ if (rank == Self::_U32.rank()) => Self::U32(self.cast::<u32>()),
            _ if (rank == Self::_U64.rank()) => Self::U64(self.cast::<u64>()),
            _ if (rank == Self::_F32.rank()) => Self::F32(self.cast::<f32>()),
            _ if (rank == Self::_F64.rank()) => Self::F64(self.cast::<f64>()),
            _ => unreachable!(),
        }
    }

    fn cast<T>(&self) -> T
    where
        T: Copy + 'static,
        i8: AsPrimitive<T>,
        i16: AsPrimitive<T>,
        i32: AsPrimitive<T>,
        i64: AsPrimitive<T>,
        u8: AsPrimitive<T>,
        u16: AsPrimitive<T>,
        u32: AsPrimitive<T>,
        u64: AsPrimitive<T>,
        f32: AsPrimitive<T>,
        f64: AsPrimitive<T>,
    {
        match self {
            Self::I8(num) => num.as_(),
            Self::I16(num) => num.as_(),
            Self::I32(num) => num.as_(),
            Self::I64(num) => num.as_(),
            Self::U8(num) => num.as_(),
            Self::U16(num) => num.as_(),
            Self::U32(num) => num.as_(),
            Self::U64(num) => num.as_(),
            Self::F32(num) => num.as_(),
            Self::F64(num) => num.as_(),
        }
    }

    fn truthy(&self) -> bool {
        match *self {
            Self::I8(n) => n != 0,
            Self::I16(n) => n != 0,
            Self::I32(n) => n != 0,
            Self::I64(n) => n != 0,
            Self::U8(n) => n != 0,
            Self::U16(n) => n != 0,
            Self::U32(n) => n != 0,
            Self::U64(n) => n != 0,
            Self::F32(n) => n != 0.0,
            Self::F64(n) => n != 0.0,
        }
    }

    fn falsey(&self) -> bool {
        !self.truthy()
    }
}

impl std::ops::Add for Constant {
    type Output = Constant;

    fn add(self, rhs: Self) -> Self::Output {
        let (left, right) = Constant::lift(self, rhs);

        match (left, right) {
            (Constant::I32(n1), Constant::I32(n2)) => Constant::I32(n1.wrapping_add(n2)),
            (Constant::I64(n1), Constant::I64(n2)) => Constant::I64(n1.wrapping_add(n2)),
            _ => unreachable!(),
        }
    }
}

impl std::ops::Sub for Constant {
    type Output = Constant;

    fn sub(self, rhs: Self) -> Self::Output {
        let (left, right) = Constant::lift(self, rhs);
        match (left, right) {
            (Constant::I32(n1), Constant::I32(n2)) => Constant::I32(n1.wrapping_sub(n2)),
            (Constant::I64(n1), Constant::I64(n2)) => Constant::I64(n1.wrapping_sub(n2)),
            _ => unreachable!(),
        }
    }
}

impl std::ops::Mul for Constant {
    type Output = Constant;

    fn mul(self, rhs: Self) -> Self::Output {
        let (left, right) = Constant::lift(self, rhs);
        match (left, right) {
            (Constant::I32(n1), Constant::I32(n2)) => Constant::I32(n1.wrapping_mul(n2)),
            (Constant::I64(n1), Constant::I64(n2)) => Constant::I64(n1.wrapping_mul(n2)),
            _ => unreachable!(),
        }
    }
}

impl std::ops::Div for Constant {
    type Output = Constant;

    fn div(self, rhs: Self) -> Self::Output {
        let (left, right) = Constant::lift(self, rhs);
        match (left, right) {
            (Constant::I32(n1), Constant::I32(n2)) => Constant::I32(n1.wrapping_div(n2)),
            (Constant::I64(n1), Constant::I64(n2)) => Constant::I64(n1.wrapping_div(n2)),
            _ => unreachable!(),
        }
    }
}

impl std::ops::Rem for Constant {
    type Output = Constant;

    fn rem(self, rhs: Self) -> Self::Output {
        let (left, right) = Constant::lift(self, rhs);
        match (left, right) {
            (Constant::I32(n1), Constant::I32(n2)) => Constant::I32(n1.rem(n2)),
            (Constant::I64(n1), Constant::I64(n2)) => Constant::I64(n1.rem(n2)),
            _ => unreachable!(),
        }
    }
}

impl std::ops::Not for Constant {
    type Output = Constant;

    fn not(self) -> Self::Output {
        let val = self.default_promote();

        match val {
            Constant::I8(n) => Constant::I8(n.not()),
            Constant::I16(n) => Constant::I16(n.not()),
            Constant::I32(n) => Constant::I32(n.not()),
            Constant::I64(n) => Constant::I64(n.not()),
            Constant::U8(n) => Constant::U8(n.not()),
            Constant::U16(n) => Constant::U16(n.not()),
            Constant::U32(n) => Constant::U32(n.not()),
            Constant::U64(n) => Constant::U64(n.not()),
            Constant::F32(n) => Constant::F32((n == 0.0).into()),
            Constant::F64(n) => Constant::F64((n == 0.0).into()),
        }
    }
}

impl std::ops::Neg for Constant {
    type Output = Constant;

    fn neg(self) -> Self::Output {
        let val = self.default_promote();

        match val {
            Constant::I8(n) => Constant::I8(n.neg()),
            Constant::I16(n) => Constant::I16(n.neg()),
            Constant::I32(n) => Constant::I32(n.neg()),
            Constant::I64(n) => Constant::I64(n.neg()),
            Constant::U8(n) => Constant::U8(u8::MAX - n),
            Constant::U16(n) => Constant::U16(u16::MAX - n),
            Constant::U32(n) => Constant::U32(u32::MAX - n),
            Constant::U64(n) => Constant::U64(u64::MAX - n),
            Constant::F32(n) => Constant::F32(n.neg()),
            Constant::F64(n) => Constant::F64(n.neg()),
        }
    }
}

impl std::ops::BitAnd for Constant {
    type Output = Constant;

    fn bitand(self, rhs: Self) -> Self::Output {
        let (left, right) = Constant::lift(self, rhs);
        match (left, right) {
            (Constant::I32(n1), Constant::I32(n2)) => Constant::I32(n1.bitand(n2)),
            (Constant::I64(n1), Constant::I64(n2)) => Constant::I64(n1.bitand(n2)),
            _ => unreachable!(),
        }
    }
}

impl std::ops::BitOr for Constant {
    type Output = Constant;

    fn bitor(self, rhs: Self) -> Self::Output {
        let (left, right) = Constant::lift(self, rhs);
        match (left, right) {
            (Constant::I32(n1), Constant::I32(n2)) => Constant::I32(n1.bitor(n2)),
            (Constant::I64(n1), Constant::I64(n2)) => Constant::I64(n1.bitor(n2)),
            _ => unreachable!(),
        }
    }
}

impl std::ops::BitXor for Constant {
    type Output = Constant;

    fn bitxor(self, rhs: Self) -> Self::Output {
        let (left, right) = Constant::lift(self, rhs);
        match (left, right) {
            (Constant::I32(n1), Constant::I32(n2)) => Constant::I32(n1.bitxor(n2)),
            (Constant::I64(n1), Constant::I64(n2)) => Constant::I64(n1.bitxor(n2)),
            _ => unreachable!(),
        }
    }
}

impl std::ops::Shl for Constant {
    type Output = Constant;

    fn shl(self, rhs: Self) -> Self::Output {
        let (left, right) = Constant::lift(self, rhs);
        match (left, right) {
            // FIXME: This may panic if shifted by too much
            (Constant::I32(n1), Constant::I32(n2)) => Constant::I32(n1 << n2),
            (Constant::I64(n1), Constant::I64(n2)) => Constant::I64(n1 << n2),
            _ => unreachable!(),
        }
    }
}

impl std::ops::Shr for Constant {
    type Output = Constant;

    fn shr(self, rhs: Self) -> Self::Output {
        let (left, right) = Constant::lift(self, rhs);
        match (left, right) {
            (Constant::I32(n1), Constant::I32(n2)) => Constant::I32(n1 >> n2),
            (Constant::I64(n1), Constant::I64(n2)) => Constant::I64(n1 >> n2),
            _ => unreachable!(),
        }
    }
}

impl From<&Constant> for ast::Constant {
    fn from(value: &Constant) -> Self {
        match *value {
            Constant::I8(num) => ast::Constant::I8(num),
            Constant::I16(num) => ast::Constant::I16(num),
            Constant::I32(num) => ast::Constant::I32(num),
            Constant::I64(num) => ast::Constant::I64(num),
            Constant::U8(num) => ast::Constant::U8(num),
            Constant::U16(num) => ast::Constant::U16(num),
            Constant::U32(num) => ast::Constant::U32(num),
            Constant::U64(num) => ast::Constant::U64(num),
            Constant::F32(num) => ast::Constant::F32(num),
            Constant::F64(num) => ast::Constant::F64(num),
        }
    }
}

impl From<&ast::Constant> for Result<Constant> {
    // Returning an error will make more sense as soon as we
    // have string literals
    fn from(value: &ast::Constant) -> Self {
        match *value {
            ast::Constant::I8(num) => Ok(Constant::I8(num)),
            ast::Constant::I16(num) => Ok(Constant::I16(num)),
            ast::Constant::I32(num) => Ok(Constant::I32(num)),
            ast::Constant::I64(num) => Ok(Constant::I64(num)),
            ast::Constant::U8(num) => Ok(Constant::U8(num)),
            ast::Constant::U16(num) => Ok(Constant::U16(num)),
            ast::Constant::U32(num) => Ok(Constant::U32(num)),
            ast::Constant::U64(num) => Ok(Constant::U64(num)),
            ast::Constant::F32(num) => Ok(Constant::F32(num)),
            ast::Constant::F64(num) => Ok(Constant::F64(num)),
        }
    }
}

fn eval_unary(op: ast::UnaryOp, expr: ast::Expr) -> Result<Constant> {
    let val = eval_constant(expr)?;
    let new_val = match op {
        ast::UnaryOp::Complement => !val,
        ast::UnaryOp::Negate => -val,
        ast::UnaryOp::Not => !val,
        ast::UnaryOp::PreInc
        | ast::UnaryOp::PreDec
        | ast::UnaryOp::PostInc
        | ast::UnaryOp::PostDec => bail!("{op:#?} is not allowed in const expresion"),
    };
    Ok(new_val)
}

fn eval_binary(op: ast::BinaryOp, left: ast::Expr, right: ast::Expr) -> Result<Constant> {
    match op {
        ast::BinaryOp::And => {
            let left_val = eval_constant(left)?;
            if left_val.falsey() {
                return Ok(Constant::I32(0));
            }
            let right_val = eval_constant(right)?;
            Ok(Constant::I32(if right_val.truthy() { 1 } else { 0 }))
        }
        ast::BinaryOp::Or => {
            let left_val = eval_constant(left)?;
            if left_val.truthy() {
                return Ok(Constant::I32(1));
            }
            let right_val = eval_constant(right)?;
            Ok(Constant::I32(if right_val.truthy() { 1 } else { 0 }))
        }
        ast::BinaryOp::Add => {
            let left_val = eval_constant(left)?;
            let right_val = eval_constant(right)?;
            Ok(left_val + right_val)
        }
        ast::BinaryOp::Subtract => {
            let left_val = eval_constant(left)?;
            let right_val = eval_constant(right)?;
            Ok(left_val - right_val)
        }
        ast::BinaryOp::Multiply => {
            let left_val = eval_constant(left)?;
            let right_val = eval_constant(right)?;
            Ok(left_val * right_val)
        }
        ast::BinaryOp::Divide => {
            let left_val = eval_constant(left)?;
            let right_val = eval_constant(right)?;
            Ok(left_val / right_val)
        }
        ast::BinaryOp::Remainder => {
            let left_val = eval_constant(left)?;
            let right_val = eval_constant(right)?;
            Ok(left_val % right_val)
        }
        ast::BinaryOp::BitAnd => {
            let left_val = eval_constant(left)?;
            let right_val = eval_constant(right)?;
            Ok(left_val & right_val)
        }
        ast::BinaryOp::BitOr => {
            let left_val = eval_constant(left)?;
            let right_val = eval_constant(right)?;
            Ok(left_val | right_val)
        }
        ast::BinaryOp::Xor => {
            let left_val = eval_constant(left)?;
            let right_val = eval_constant(right)?;
            Ok(left_val ^ right_val)
        }
        ast::BinaryOp::LShift => {
            let left_val = eval_constant(left)?;
            let right_val = eval_constant(right)?;
            Ok(left_val << right_val)
        }
        ast::BinaryOp::RShift => {
            let left_val = eval_constant(left)?;
            let right_val = eval_constant(right)?;
            Ok(left_val << right_val)
        }
        ast::BinaryOp::Equal => {
            let left_val = eval_constant(left)?;
            let right_val = eval_constant(right)?;
            let (left_val, right_val) = Constant::lift(left_val, right_val);
            Ok(Constant::I32(if left_val == right_val { 1 } else { 0 }))
        }
        ast::BinaryOp::NotEqual => {
            let left_val = eval_constant(left)?;
            let right_val = eval_constant(right)?;
            let (left_val, right_val) = Constant::lift(left_val, right_val);
            Ok(Constant::I32(if left_val != right_val { 1 } else { 0 }))
        }
        ast::BinaryOp::LessThan => {
            let left_val = eval_constant(left)?;
            let right_val = eval_constant(right)?;
            let (left_val, right_val) = Constant::lift(left_val, right_val);
            Ok(Constant::I32((left_val < right_val) as i32))
        }
        ast::BinaryOp::LessOrEqual => {
            let left_val = eval_constant(left)?;
            let right_val = eval_constant(right)?;
            let (left_val, right_val) = Constant::lift(left_val, right_val);
            Ok(Constant::I32((left_val <= right_val) as i32))
        }
        ast::BinaryOp::GreaterThan => {
            let left_val = eval_constant(left)?;
            let right_val = eval_constant(right)?;
            let (left_val, right_val) = Constant::lift(left_val, right_val);
            Ok(Constant::I32((left_val > right_val) as i32))
        }
        ast::BinaryOp::GreaterOrEqual => {
            let left_val = eval_constant(left)?;
            let right_val = eval_constant(right)?;
            let (left_val, right_val) = Constant::lift(left_val, right_val);
            Ok(Constant::I32((left_val >= right_val) as i32))
        }
        ast::BinaryOp::Assign
        | ast::BinaryOp::AddAssign
        | ast::BinaryOp::SubAssign
        | ast::BinaryOp::MultAssign
        | ast::BinaryOp::DivAssign
        | ast::BinaryOp::ModAssign
        | ast::BinaryOp::AndAssign
        | ast::BinaryOp::OrAssign
        | ast::BinaryOp::XorAssign
        | ast::BinaryOp::LShiftAssign
        | ast::BinaryOp::RShiftAssign => bail!("Assignment cannot happen in const expression"),
        ast::BinaryOp::Ternary => {
            unreachable!("Ternary expressions are not true binary operands.")
        }
    }
}
