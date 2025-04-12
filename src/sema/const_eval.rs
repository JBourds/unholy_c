use crate::ast;

use anyhow::{bail, Context, Result};
use num::cast::AsPrimitive;

pub fn eval(expr: ast::Expr) -> Result<ast::Literal> {
    let constant = eval_constant(expr).context("Could not evaluate expression at compile time")?;
    let literal = ast::Literal::from(&constant);
    Ok(literal)
}

fn eval_constant(expr: ast::Expr) -> Result<Constant> {
    match expr {
        ast::Expr::Var(_) => bail!("Variables cannot be constant evaluated"),
        ast::Expr::Assignment { .. } => {
            bail!("Assignment cannot be constant evaluated")
        }
        ast::Expr::Literal(literal) => {
            let literal = Result::<Constant>::from(&literal);
            Ok(literal.context("ast::Literal cannot be constant evaluated")?)
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
        _ => unimplemented!(),
    }
}

#[derive(Eq, PartialEq, PartialOrd, Ord)]
enum Constant {
    I32(i32),
    I64(i64),
}

impl Constant {
    // FIXME: Can't name these I32/I64 cause that collides with
    // the enum member names. But INT and LONG kind of go against
    // the point of using sized integers as the names.
    pub const INT: Self = Self::I32(0);
    pub const LONG: Self = Self::I64(0);

    fn rank(&self) -> u8 {
        match self {
            Self::I32(..) => 1,
            Self::I64(..) => 2,
        }
    }

    fn lift(lhs: Self, rhs: Self) -> (Self, Self) {
        let left = lhs.default_promote();
        let right = rhs.promote(&left);
        let left = left.promote(&right);

        (left, right)
    }

    fn default_promote(self) -> Self {
        if self.rank() >= Self::INT.rank() {
            self
        } else {
            self.promote(&Self::INT)
        }
    }

    fn promote(self, other: &Self) -> Self {
        let rank = other.rank();
        if self.rank() >= rank {
            return self;
        }
        match rank {
            _ if (rank == Self::INT.rank()) => Self::I32(self.cast::<i32>()),
            _ if (rank == Self::LONG.rank()) => Self::I64(self.cast::<i64>()),
            _ => unreachable!(),
        }
    }

    fn cast<T>(&self) -> T
    where
        T: Copy + 'static,
        i32: AsPrimitive<T>,
        i64: AsPrimitive<T>,
    {
        match self {
            &Self::I32(num) => num.as_(),
            &Self::I64(num) => num.as_(),
        }
    }

    fn truthy(&self) -> bool {
        match self {
            &Self::I32(n) => n != 0,
            &Self::I64(n) => n != 0,
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
            Constant::I32(n) => Constant::I32(n.not()),
            Constant::I64(n) => Constant::I64(n.not()),
        }
    }
}

impl std::ops::Neg for Constant {
    type Output = Constant;

    fn neg(self) -> Self::Output {
        let val = self.default_promote();

        match val {
            Constant::I32(n) => Constant::I32(n.neg()),
            Constant::I64(n) => Constant::I64(n.neg()),
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

impl From<&Constant> for ast::Literal {
    fn from(value: &Constant) -> Self {
        match value {
            &Constant::I32(num) => ast::Literal::Int(num),
            &Constant::I64(num) => ast::Literal::Long(num),
        }
    }
}

impl From<&ast::Literal> for Result<Constant> {
    // Returning an error will make more sense as soon as we
    // have string literals
    fn from(value: &ast::Literal) -> Self {
        match value {
            &ast::Literal::Int(num) => Ok(Constant::I32(num)),
            &ast::Literal::Long(num) => Ok(Constant::I64(num)),
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
