pub mod constants;
pub mod declaration;
pub mod declarators;
pub mod exprs;
pub mod factor;
pub mod statements;
pub mod types;

use crate::lexer::Token;

use anyhow::{Context, Result, bail, ensure};

// Re-export these types for convenience
pub use constants::Constant;
pub use declaration::{Declaration, FunDecl, VarDecl};
pub use declarators::{AbstractDeclarator, Declarator};
pub use exprs::Expr;
pub use factor::Factor;
pub use statements::Stmt;
pub use types::{BaseType, Type, TypeBuilder};

pub fn parse(tokens: &[Token]) -> Result<Program> {
    let (prog, _) = Program::consume(tokens)?;
    Ok(prog)
}

/// Get the element type which can be copie for a given value.
/// For a scalar or pointer value, this is the value's type.
/// For an array, it is the first non-array type encountered when recursively
/// dereferencing the type.
pub fn get_element_type(t: &Type) -> Type {
    match t {
        Type {
            base: BaseType::Array { element, .. },
            ..
        } => get_element_type(element),
        _ => t.clone(),
    }
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}
impl Program {
    fn consume(tokens: &[Token]) -> Result<(Program, &[Token])> {
        let mut declarations = vec![];
        let mut remaining = tokens;
        while !remaining.is_empty() {
            let (declaration, tokens) = Declaration::consume(remaining)?;
            declarations.push(declaration);
            remaining = tokens;
        }
        Ok((Program { declarations }, remaining))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct RawParameterList(Vec<(Type, Declarator)>);

impl RawParameterList {
    fn consume(tokens: &[Token]) -> Result<(Self, &[Token])> {
        if tokens.first().is_none_or(|t| *t != Token::LParen) {
            bail!("ast.ParameterList.consume(): Expected opening parentheses in parameter list.");
        }
        let tokens = &tokens[1..];
        let mut signature = vec![];
        let remaining = match tokens {
            [Token::RParen, tokens @ ..] => &tokens[1..],
            [Token::Void, Token::RParen, ..] => &tokens[2..],
            [Token::Void, t, ..] => {
                bail!("Expected closing parentheses but found \"{}\"", t)
            }
            _ => {
                let mut keep_going = true;
                let mut remaining = tokens;
                while keep_going {
                    let (stream_offset, r#type, storage) = TypeBuilder::new()
                        .get_base(remaining)
                        .and_then(|b| b.into_type())
                        .context("Error building base type from token stream.")?;
                    ensure!(
                        storage.is_none(),
                        "Cannot have function parameter with storage specifier."
                    );
                    let (declarator, tokens) = Declarator::consume(&remaining[stream_offset..])
                        .context("ast.ParameterList.consume(): Unable to parse declarator.")?;
                    signature.push((r#type, declarator));
                    if let Some(Token::Comma) = tokens.first() {
                        remaining = &tokens[1..];
                    } else {
                        keep_going = false;
                        remaining = tokens;
                    }
                }
                if remaining.first().is_none_or(|t| *t != Token::RParen) {
                    bail!(
                        "ast.ParameterList.consume(): Expected opening parentheses in parameter list."
                    );
                }
                &remaining[1..]
            }
        };
        Ok((Self(signature), remaining))
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum StorageClass {
    Static,
    Extern,
    Auto,
    Register,
}

impl std::fmt::Display for StorageClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::Static => write!(f, "static"),
            Self::Extern => write!(f, "extern"),
            Self::Register => write!(f, "register"),
            Self::Auto => write!(f, "auto"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum BlockItem {
    Stmt(Stmt),
    Decl(Declaration),
}

impl BlockItem {
    fn consume(tokens: &[Token]) -> Result<(BlockItem, &[Token])> {
        if let Ok((decl, tokens)) = Declaration::consume(tokens) {
            Ok((Self::Decl(decl), tokens))
        } else if let Ok((stmt, tokens)) = Stmt::consume(tokens) {
            Ok((Self::Stmt(stmt), tokens))
        } else {
            bail!("Unable to parse a valid block item.")
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Initializer {
    SingleInit(Box<Expr>),
    CompundInit(Vec<Initializer>),
}

impl Initializer {
    pub fn consume(tokens: &[Token]) -> Result<(Self, &[Token])> {
        match tokens {
            [Token::LSquirly, tokens @ ..] => {
                let mut initializers = Vec::new();
                let (first_init, mut left) = Self::consume(tokens)?;
                initializers.push(first_init);
                loop {
                    match left {
                        [Token::RSquirly, tokens @ ..]
                        | [Token::Comma, Token::RSquirly, tokens @ ..] => {
                            left = tokens;
                            break;
                        }
                        [Token::Comma, tokens @ ..] => {
                            let (inner, tokens) = Self::consume(tokens)?;
                            initializers.push(inner);
                            left = tokens;
                        }
                        _ => bail!("ast.Initializer.consume() failed to parse compound init"),
                    }
                }

                Ok((Self::CompundInit(initializers), left))
            }
            _ => {
                let (expr, tokens) = Expr::parse(tokens, 0)?;
                Ok((Self::SingleInit(Box::new(expr)), tokens))
            }
        }
    }

    pub fn zero_initializer(r#type: &Type) -> Result<Self> {
        if r#type.is_array() {
            match &r#type.base {
                BaseType::Array { element, size } => Ok(Self::CompundInit(
                    (0..*size)
                        .map(|_| Self::zero_initializer(element))
                        .collect::<Result<Vec<Self>>>()?,
                )),
                _ => todo!(),
            }
        } else {
            Ok(Self::SingleInit(Box::new(Expr::Constant(
                Constant::const_from_type(r#type, 0)?,
            ))))
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block(pub Vec<BlockItem>);
impl Block {
    pub fn items(&self) -> &Vec<BlockItem> {
        &self.0
    }

    pub fn into_items(self) -> Vec<BlockItem> {
        self.0
    }

    fn consume(tokens: &[Token]) -> Result<(Self, &[Token])> {
        match tokens.first() {
            Some(Token::LSquirly) => {
                let mut remaining = &tokens[1..];
                let mut items = vec![];
                while let Ok((item, tokens)) = BlockItem::consume(remaining) {
                    remaining = tokens;
                    items.push(item);
                }
                match remaining.first() {
                    Some(Token::RSquirly) => Ok((Self(items), &remaining[1..])),
                    Some(token) => bail!("Expected \"}}\" to end block but found {}", token),
                    None => bail!("Missing \"}}\" to end block."),
                }
            }
            Some(token) => bail!("Expected \"{{\" to start block but found {}", token),
            None => bail!("Missing \"{{\" to start block."),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ForInit {
    Decl(VarDecl),
    Expr(Option<Expr>),
}

impl ForInit {
    fn consume(tokens: &[Token]) -> Result<(Self, &[Token])> {
        match tokens {
            [Token::Semi, tokens @ ..] => Ok((ForInit::Expr(None), tokens)),
            tokens => match Declaration::consume(tokens) {
                Ok((Declaration::VarDecl(decl), tokens)) => Ok((ForInit::Decl(decl), tokens)),
                Ok((Declaration::FunDecl(_), _)) => {
                    bail!("ast.ForInit.consume(): Cannot declare function in for loop init.")
                }
                _ => {
                    let (expr, tokens) = Expr::parse(tokens, 0)
                        .context("Expected decleration or expression but failed to parse both")?;
                    if let Some(Token::Semi) = tokens.first() {
                        Ok((ForInit::Expr(Some(expr)), &tokens[1..]))
                    } else {
                        bail!("Missing semicolon after init expression.")
                    }
                }
            },
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitAnd,
    BitOr,
    Xor,
    LShift,
    RShift,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    Assign,
    AddAssign,
    SubAssign,
    MultAssign,
    DivAssign,
    ModAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    LShiftAssign,
    RShiftAssign,
    Ternary,
}

impl BinaryOp {
    pub fn is_logical(&self) -> bool {
        matches!(*self, Self::And | Self::Or)
    }

    pub fn is_relational(&self) -> bool {
        matches!(
            *self,
            Self::Equal
                | Self::NotEqual
                | Self::LessThan
                | Self::LessOrEqual
                | Self::GreaterThan
                | Self::GreaterOrEqual
        )
    }

    pub fn is_add(&self) -> bool {
        matches!(*self, Self::Add | Self::AddAssign)
    }

    pub fn is_sub(&self) -> bool {
        matches!(*self, Self::Subtract | Self::SubAssign)
    }

    pub fn is_add_sub(&self) -> bool {
        matches!(
            *self,
            Self::Add | Self::AddAssign | Self::Subtract | Self::SubAssign
        )
    }

    pub fn is_bitwise(&self) -> bool {
        matches!(
            self,
            Self::BitAnd
                | Self::BitOr
                | Self::Xor
                | Self::LShift
                | Self::RShift
                | Self::AndAssign
                | Self::OrAssign
                | Self::XorAssign
                | Self::LShiftAssign
                | Self::RShiftAssign
        )
    }

    pub fn does_assignment(&self) -> bool {
        matches!(
            *self,
            Self::Assign
                | Self::AddAssign
                | Self::SubAssign
                | Self::MultAssign
                | Self::DivAssign
                | Self::ModAssign
                | Self::AndAssign
                | Self::OrAssign
                | Self::XorAssign
                | Self::LShiftAssign
                | Self::RShiftAssign
        )
    }

    pub fn compound_op(&self) -> Option<Self> {
        match self {
            Self::AddAssign => Some(Self::Add),
            Self::SubAssign => Some(Self::Subtract),
            Self::MultAssign => Some(Self::Multiply),
            Self::DivAssign => Some(Self::Divide),
            Self::ModAssign => Some(Self::Remainder),
            Self::AndAssign => Some(Self::BitAnd),
            Self::OrAssign => Some(Self::BitOr),
            Self::XorAssign => Some(Self::Xor),
            Self::LShiftAssign => Some(Self::LShift),
            Self::RShiftAssign => Some(Self::RShift),
            _ => None,
        }
    }

    pub fn precedence(&self) -> u32 {
        match *self {
            Self::Add => 45,
            Self::Subtract => 45,
            Self::Multiply => 50,
            Self::Divide => 50,
            Self::Remainder => 50,
            Self::BitAnd => 25,
            Self::BitOr => 15,
            Self::Xor => 20,
            Self::LShift => 40,
            Self::RShift => 40,
            Self::And => 10,
            Self::Or => 5,
            Self::Equal => 30,
            Self::NotEqual => 30,
            Self::LessThan => 35,
            Self::LessOrEqual => 35,
            Self::GreaterThan => 35,
            Self::GreaterOrEqual => 35,
            Self::Assign => 1,
            Self::AddAssign => 1,
            Self::SubAssign => 1,
            Self::MultAssign => 1,
            Self::DivAssign => 1,
            Self::ModAssign => 1,
            Self::AndAssign => 1,
            Self::OrAssign => 1,
            Self::XorAssign => 1,
            Self::LShiftAssign => 1,
            Self::RShiftAssign => 1,
            Self::Ternary => 3,
        }
    }

    fn parse(tokens: &[Token]) -> Result<Option<(BinaryOp, &[Token])>> {
        let token = tokens.first().context("No remaining tokens")?;
        let tokens = &tokens[1..];

        match token {
            Token::Plus => Ok(Some((BinaryOp::Add, tokens))),
            Token::Minus => Ok(Some((BinaryOp::Subtract, tokens))),
            Token::Star => Ok(Some((BinaryOp::Multiply, tokens))),
            Token::Divide => Ok(Some((BinaryOp::Divide, tokens))),
            Token::Mod => Ok(Some((BinaryOp::Remainder, tokens))),
            Token::Ampersand => Ok(Some((BinaryOp::BitAnd, tokens))),
            Token::BitOr => Ok(Some((BinaryOp::BitOr, tokens))),
            Token::BitXor => Ok(Some((BinaryOp::Xor, tokens))),
            Token::LShift => Ok(Some((BinaryOp::LShift, tokens))),
            Token::RShift => Ok(Some((BinaryOp::RShift, tokens))),
            Token::And => Ok(Some((BinaryOp::And, tokens))),
            Token::Or => Ok(Some((BinaryOp::Or, tokens))),
            Token::Eq => Ok(Some((BinaryOp::Equal, tokens))),
            Token::NotEq => Ok(Some((BinaryOp::NotEqual, tokens))),
            Token::Less => Ok(Some((BinaryOp::LessThan, tokens))),
            Token::LessEq => Ok(Some((BinaryOp::LessOrEqual, tokens))),
            Token::Great => Ok(Some((BinaryOp::GreaterThan, tokens))),
            Token::GreatEq => Ok(Some((BinaryOp::GreaterOrEqual, tokens))),
            Token::Assign => Ok(Some((BinaryOp::Assign, tokens))),
            Token::AddAssign => Ok(Some((BinaryOp::AddAssign, tokens))),
            Token::SubAssign => Ok(Some((BinaryOp::SubAssign, tokens))),
            Token::MultAssign => Ok(Some((BinaryOp::MultAssign, tokens))),
            Token::DivAssign => Ok(Some((BinaryOp::DivAssign, tokens))),
            Token::ModAssign => Ok(Some((BinaryOp::ModAssign, tokens))),
            Token::OrAssign => Ok(Some((BinaryOp::OrAssign, tokens))),
            Token::AndAssign => Ok(Some((BinaryOp::AndAssign, tokens))),
            Token::LShiftAssign => Ok(Some((BinaryOp::LShiftAssign, tokens))),
            Token::RShiftAssign => Ok(Some((BinaryOp::RShiftAssign, tokens))),
            Token::XorAssign => Ok(Some((BinaryOp::XorAssign, tokens))),
            Token::Ternary => Ok(Some((BinaryOp::Ternary, tokens))),
            _ => Ok(None),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOp {
    Complement,
    Negate,
    Not,
    PreInc,
    PreDec,
    PostInc,
    PostDec,
    AddrOf,
    Deref,
}

impl UnaryOp {
    pub fn is_logical(&self) -> bool {
        *self == Self::Not
    }

    pub fn is_bitwise(&self) -> bool {
        matches!(self, Self::Complement)
    }

    pub fn does_assignment(&self) -> bool {
        matches!(
            self,
            UnaryOp::PreInc | UnaryOp::PreDec | UnaryOp::PostInc | UnaryOp::PostDec
        )
    }

    pub fn is_valid_for(&self, expr: &Expr) -> bool {
        !matches!(
            self,
            UnaryOp::PreInc | UnaryOp::PreDec | UnaryOp::PostInc | UnaryOp::PostDec
        ) || expr.is_lvalue()
    }

    fn consume_prefix(tokens: &[Token]) -> Result<(Self, &[Token])> {
        if let Some(token) = tokens.first() {
            match tokens {
                [Token::Ampersand, tokens @ ..] => Ok((Self::AddrOf, tokens)),
                [Token::Star, tokens @ ..] => Ok((Self::Deref, tokens)),
                [Token::Minus, tokens @ ..] => Ok((Self::Negate, tokens)),
                [Token::BitNot, tokens @ ..] => Ok((Self::Complement, tokens)),
                [Token::Not, tokens @ ..] => Ok((Self::Not, tokens)),
                [Token::Increment, tokens @ ..] => Ok((Self::PreInc, tokens)),
                [Token::Decrement, tokens @ ..] => Ok((Self::PreDec, tokens)),
                _ => bail!("Expected '-', '~', `++`, `--`, or '!', found '{}'", token),
            }
        } else {
            bail!("No remaining tokens")
        }
    }

    fn consume_postfix(tokens: &[Token]) -> Result<(Self, &[Token])> {
        if let Some(token) = tokens.first() {
            match tokens {
                [Token::Increment, tokens @ ..] => Ok((Self::PostInc, tokens)),
                [Token::Decrement, tokens @ ..] => Ok((Self::PostDec, tokens)),
                _ => bail!("Expected '-', '~', `++`, `--`, or '!', found '{}'", token),
            }
        } else {
            bail!("No remaining tokens")
        }
    }
}
