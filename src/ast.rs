use crate::lexer::{self, Token};
use anyhow::{bail, Context, Result};
use std::rc::Rc;

pub fn parse<'a>(tokens: &'a [Token]) -> Result<Program<'a>> {
    let (program, tokens) = Program::consume(tokens)?;
    if !tokens.is_empty() {
        bail!("Found extra tokens when parsing main:\n{:#?}", tokens)
    } else {
        Ok(program)
    }
}

pub trait AstNode<'a> {
    fn consume(tokens: &'a [Token<'a>]) -> Result<(Self, &'a [Token<'a>])>
    where
        Self: Sized;
}

#[derive(Debug, PartialEq)]
pub struct Program<'a> {
    pub function: Function<'a>,
}
impl<'a> AstNode<'a> for Program<'a> {
    fn consume(tokens: &'a [Token<'a>]) -> Result<(Program<'a>, &'a [Token<'a>])> {
        let (function, tokens) = Function::consume(tokens).context("Could not parse function.")?;
        if let Function {
            ret_t: Type::Int,
            name: "main",
            ..
        } = function
        {
            Ok((Self { function }, tokens))
        } else {
            bail!("Could not find a \"main\" function.");
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Function<'a> {
    pub ret_t: Type,
    pub name: &'a str,
    pub signature: Vec<(Type, Option<&'a str>)>,
    pub items: Vec<BlockItem>,
}

impl<'a> AstNode<'a> for Function<'a> {
    fn consume(tokens: &'a [Token<'a>]) -> Result<(Function<'a>, &'a [Token<'a>])> {
        let (ret_t, tokens) = Type::consume(tokens)
            .context("No return type indicated for function. Add a return type, or mark the function as returning \"void\" to signal that it returns nothing.")?;
        if let [Token::Ident(name), Token::LParen, tokens @ ..] = tokens {
            let mut signature = vec![];
            let mut remaining = match tokens {
                [Token::Void, Token::RParen, Token::LSquirly, tokens @ ..] => tokens,
                [Token::Void, t, ..] => {
                    bail!("Expected closing parentheses but found \"{}\"", t)
                }
                tokens => {
                    let mut remaining = tokens;
                    while let Ok((param_t, tokens)) = Type::consume(remaining) {
                        match tokens {
                            [Token::Ident(s), Token::Comma, tokens @ ..]
                            | [Token::Ident(s), tokens @ ..] => {
                                signature.push((param_t, Some(*s)));
                                remaining = tokens;
                            }
                            [Token::Comma, tokens @ ..] => {
                                signature.push((param_t, None));
                                remaining = tokens;
                            }
                            [t, ..] => {
                                bail!("Expected parameter name or comma but found : {}", t)
                            }
                            [] => {
                                bail!("Expected parameter name or comma but found no more tokens.")
                            }
                        }
                    }

                    match remaining {
                        [Token::RParen, Token::LSquirly, tokens @ ..] => tokens,
                        [t, ..] => bail!("Expected a closing parentheses but found {}", t),
                        [] => bail!("Expected a closing parentheses but found no more tokens."),
                    }
                }
            };

            let mut items = vec![];
            while let Ok((item, tokens)) = BlockItem::consume(remaining) {
                remaining = tokens;
                items.push(item);
            }
            if let Some(Token::RSquirly) = remaining.first() {
                Ok((
                    Self {
                        ret_t,
                        name,
                        signature,
                        items,
                    },
                    &remaining[1..],
                ))
            } else {
                bail!("No closing brace for the function. Add a \"}}\" after the statements in the function body. Found tokens:\n{:#?}", remaining)
            }
        } else {
            bail!("Failed to parse function.")
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum BlockItem {
    Stmt(Stmt),
    Decl(Declaration),
}

impl<'a> AstNode<'a> for BlockItem {
    fn consume(tokens: &'a [Token<'a>]) -> Result<(BlockItem, &'a [Token<'a>])> {
        if let Ok((decl, tokens)) = Declaration::consume(tokens) {
            Ok((Self::Decl(decl), tokens))
        } else {
            let (stmt, tokens) = Stmt::consume(tokens)?;
            Ok((Self::Stmt(stmt), tokens))
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Declaration {
    pub typ: Type,
    pub name: Rc<String>,
    pub init: Option<Expr>,
}

impl<'a> AstNode<'a> for Declaration {
    fn consume(tokens: &'a [Token<'a>]) -> Result<(Self, &'a [Token<'a>])> {
        let (typ, tokens) = Type::consume(tokens)?;
        match tokens {
            [lexer::Token::Ident(s), lexer::Token::Assign, tokens @ ..] => {
                let (expr, tokens) = Expr::parse(tokens, 0)?;
                if tokens.first().is_some_and(|x| *x != lexer::Token::Semi) {
                    bail!("Semicolon required after expression in variable declaration.")
                } else {
                    Ok((
                        Self {
                            typ,
                            name: Rc::new(String::from(*s)),
                            init: Some(expr),
                        },
                        &tokens[1..],
                    ))
                }
            }
            [lexer::Token::Ident(s), lexer::Token::Semi, tokens @ ..] => Ok((
                Self {
                    typ,
                    name: Rc::new(String::from(*s)),
                    init: None,
                },
                tokens,
            )),
            _ => bail!("Expected <type> <ident> in variable declaration."),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Return(Option<Expr>),
    Expr(Expr),
    If {
        condition: Expr,
        then: Box<Stmt>,
        r#else: Option<Box<Stmt>>,
    },
    Null,
}

impl<'a> AstNode<'a> for Stmt {
    fn consume(tokens: &'a [Token<'a>]) -> Result<(Stmt, &'a [Token<'a>])> {
        let comma_terminated_expr = |tokens| {
            let (expr, tokens) = Expr::parse(tokens, 0).context(
                "Expected return statement to return an expression but could not parse one.",
            )?;
            if let Some(Token::Semi) = tokens.first() {
                Ok((expr, &tokens[1..]))
            } else {
                bail!("Missing semicolon after return expression.")
            }
        };
        match tokens {
            [Token::Semi, tokens @ ..] => Ok((Self::Null, tokens)),
            [Token::Return, Token::Semi, tokens @ ..] => Ok((Self::Return(None), tokens)),
            [Token::Return, tokens @ ..] => {
                let (expr, tokens) = comma_terminated_expr(tokens)?;
                Ok((Self::Return(Some(expr)), tokens))
            }
            [Token::If, Token::LParen, tokens @ ..] => {
                let (condition, tokens) = Expr::parse(tokens, 0)
                    .context("Failed to parse expression for if statement conditional")?;
                let tokens = if let Some(Token::RParen) = tokens.first() {
                    &tokens[1..]
                } else {
                    bail!("If statment conditional must be closed with right paren");
                };
                let (then, tokens) =
                    Stmt::consume(tokens).context("Failed to parse then branch")?;

                let (r#else, tokens) = if let Some(Token::Else) = tokens.first() {
                    let (stmt, tokens) =
                        Stmt::consume(&tokens[1..]).context("Failed to parse else branch")?;
                    (Some(stmt), tokens)
                } else {
                    (None, tokens)
                };

                Ok((
                    Self::If {
                        condition,
                        then: Box::new(then),
                        r#else: r#else.map(Box::new),
                    },
                    tokens,
                ))
            }
            _ => {
                let (expr, tokens) = comma_terminated_expr(tokens)?;
                Ok((Self::Expr(expr), tokens))
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Var(Rc<String>),
    Assignment {
        lvalue: Box<Expr>,
        rvalue: Box<Expr>,
    },
    Literal(Literal),
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Conditional {
        condition: Box<Expr>,
        then: Box<Expr>,
        r#else: Box<Expr>,
    },
}

impl Expr {
    pub fn parse<'a>(
        tokens: &'a [Token<'a>],
        min_precedence: u32,
    ) -> Result<(Expr, &'a [Token<'a>])> {
        let (mut left, mut tokens) = Factor::parse(tokens)?;
        loop {
            let Some((operator, tokens_inner)) = BinaryOp::parse(tokens)? else {
                break;
            };

            if operator.precedence() < min_precedence {
                break;
            }

            if operator.does_assignment() {
                let (right, tokens_inner) = Expr::parse(tokens_inner, operator.precedence())?;
                let right = if let Some(op) = operator.compound_op() {
                    Expr::Binary {
                        op,
                        left: Box::new(left.clone()),
                        right: Box::new(right.clone()),
                    }
                } else {
                    right
                };
                left = Expr::Assignment {
                    lvalue: Box::new(left),
                    rvalue: Box::new(right),
                };
                tokens = tokens_inner;
            } else if operator == BinaryOp::Ternary {
                let parse_conditional_middle =
                    |tokens: &'a [Token<'_>]| -> Result<(Expr, &'a [Token<'_>])> {
                        let (expr, tokens) = Expr::parse(tokens, 0)
                            .context("Failed to parse middle expression in ternary")?;

                        let tokens = match tokens.first() {
                            Some(Token::Colon) => &tokens[1..],
                            _ => bail!("Missing closing colon in ternary"),
                        };

                        Ok((expr, tokens))
                    };

                let (middle, tokens_inner) = parse_conditional_middle(tokens_inner)?;
                let (right, tokens_inner) = Expr::parse(tokens_inner, operator.precedence())?;

                left = Expr::Conditional {
                    condition: Box::new(left),
                    then: Box::new(middle),
                    r#else: Box::new(right),
                };

                tokens = tokens_inner;
            } else {
                let (right, tokens_inner) = Expr::parse(tokens_inner, operator.precedence() + 1)?;
                left = Expr::Binary {
                    op: operator,
                    left: Box::new(left),
                    right: Box::new(right),
                };
                tokens = tokens_inner;
            }
        }
        Ok((left, tokens))
    }
}

struct Factor;

impl Factor {
    pub fn parse<'a>(tokens: &'a [Token<'a>]) -> Result<(Expr, &'a [Token<'a>])> {
        let check_for_postfix =
            |expr: Expr, tokens: &'a [Token<'a>]| match UnaryOp::consume_postfix(tokens) {
                Ok((op, tokens)) => Ok((
                    Expr::Unary {
                        op,
                        expr: Box::new(expr),
                    },
                    tokens,
                )),
                _ => Ok((expr, tokens)),
            };

        match UnaryOp::consume_prefix(tokens) {
            Ok((op, tokens)) => {
                let (expr, tokens) = Factor::parse(tokens)?;
                Ok((
                    Expr::Unary {
                        op,
                        expr: Box::new(expr),
                    },
                    tokens,
                ))
            }
            _ => match tokens {
                [lexer::Token::Literal(_), ..] => {
                    let (lit, tokens) = Literal::consume(tokens)?;
                    Ok((Expr::Literal(lit), tokens))
                }
                [lexer::Token::Ident(s), tokens @ ..] => {
                    check_for_postfix(Expr::Var(Rc::new(String::from(*s))), tokens)
                }
                [Token::LParen, tokens @ ..] => {
                    let (expr, tokens) = Expr::parse(tokens, 0)
                        .context("Parsing grammer rule: \"(\" <exp> \")\" failed")?;
                    match tokens {
                        [Token::RParen, tokens @ ..] => check_for_postfix(expr, tokens),
                        _ => bail!("Could not find matching right parenthesis"),
                    }
                }
                _ => bail!("Could not match valid grammar rule."),
            },
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Type {
    Int,
    Void,
}
impl<'a> AstNode<'a> for Type {
    fn consume(tokens: &'a [Token<'a>]) -> Result<(Type, &'a [Token<'a>])> {
        if let Some(token) = tokens.first() {
            let t = match token {
                Token::Int => Type::Int,
                Token::Void => Type::Void,
                _ => bail!("Could not parse token into literal."),
            };
            Ok((t, &tokens[1..]))
        } else {
            bail!("No remaining tokens.")
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Literal {
    Int(i32),
}
impl<'a> AstNode<'a> for Literal {
    fn consume(tokens: &'a [Token<'a>]) -> Result<(Literal, &'a [Token<'a>])> {
        if let Some(token) = tokens.first() {
            match token {
                Token::Literal(s) => {
                    if let Ok(int) = s.parse::<i32>() {
                        Ok((Self::Int(int), &tokens[1..]))
                    } else {
                        bail!("Could not parse token into literal.")
                    }
                }
                _ => bail!("Could not parse token into literal."),
            }
        } else {
            bail!("No remaining tokens.")
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

    pub fn is_valid_for(&self, expr: &Expr) -> bool {
        !self.does_assignment() || matches!(expr, Expr::Var(_))
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

    fn parse<'a>(tokens: &'a [Token<'a>]) -> Result<Option<(BinaryOp, &'a [Token<'a>])>> {
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
}

impl UnaryOp {
    pub fn is_valid_for(&self, expr: &Expr) -> bool {
        !matches!(
            self,
            UnaryOp::PreInc | UnaryOp::PreDec | UnaryOp::PostInc | UnaryOp::PostDec
        ) || matches!(expr, Expr::Var(_))
    }

    fn consume_prefix<'a>(tokens: &'a [Token<'a>]) -> Result<(Self, &'a [Token<'a>])> {
        if let Some(token) = tokens.first() {
            match tokens {
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

    fn consume_postfix<'a>(tokens: &'a [Token<'a>]) -> Result<(Self, &'a [Token<'a>])> {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn return_2() {
        let tokens = &[
            Token::Int,
            Token::Ident("main"),
            Token::LParen,
            Token::Void,
            Token::RParen,
            Token::LSquirly,
            Token::Return,
            Token::Literal("2"),
            Token::Semi,
            Token::RSquirly,
        ];
        let program = parse(tokens).unwrap();
        let expected = Program {
            function: Function {
                ret_t: Type::Int,
                name: "main",
                signature: vec![],
                items: vec![BlockItem::Stmt(Stmt::Return(Some(Expr::Literal(
                    Literal::Int(2),
                ))))],
            },
        };
        assert_eq!(expected, program);
    }

    #[test]
    fn test_multiple_params() {
        let tokens = &[
            Token::Int,
            Token::Ident("multiple_args"),
            Token::LParen,
            Token::Int,
            Token::Ident("x"),
            Token::Comma,
            Token::Int,
            Token::Comma,
            Token::Int,
            Token::Ident("z"),
            Token::RParen,
            Token::LSquirly,
            Token::Return,
            Token::Literal("2"),
            Token::Semi,
            Token::RSquirly,
        ];
        let (function, _) = Function::consume(tokens).unwrap();
        let expected = Function {
            ret_t: Type::Int,
            name: "multiple_args",
            signature: vec![
                (Type::Int, Some("x")),
                (Type::Int, None),
                (Type::Int, Some("z")),
            ],
            items: vec![BlockItem::Stmt(Stmt::Return(Some(Expr::Literal(
                Literal::Int(2),
            ))))],
        };
        assert_eq!(expected, function);
    }

    #[test]
    fn rejects_trailing_comma() {
        let tokens = &[
            Token::Int,
            Token::Ident("trailing_comma"),
            Token::LParen,
            Token::Int,
            Token::Ident("x"),
            Token::Comma,
            Token::RParen,
            Token::LSquirly,
            Token::RSquirly,
        ];
        let program = parse(tokens);
        assert!(program.is_err());
    }

    #[test]
    fn missing_return() {
        let tokens = &[
            Token::Int,
            Token::Ident("main"),
            Token::LParen,
            Token::Void,
            Token::RParen,
            Token::LSquirly,
            Token::RSquirly,
        ];
        let program = parse(tokens).unwrap();
        let expected = Program {
            function: Function {
                name: "main",
                items: vec![],
                signature: vec![],
                ret_t: Type::Int,
            },
        };
        assert_eq!(expected, program);
    }

    #[test]
    fn parse_unary_bitnot_empty() {
        let tokens = &[Token::BitNot, Token::Literal("1")];

        let (expr, tokens) = Factor::parse(tokens).unwrap();

        assert!(tokens.is_empty());

        assert_eq!(
            expr,
            Expr::Unary {
                op: UnaryOp::Complement,
                expr: Box::new(Expr::Literal(Literal::Int(1)))
            }
        );
    }

    #[test]
    fn parse_unary_bitnot() {
        let tokens = &[Token::BitNot, Token::Literal("1"), Token::Semi];

        let (expr, tokens) = Factor::parse(tokens).unwrap();

        assert_eq!(tokens, &[Token::Semi]);
        assert_eq!(
            expr,
            Expr::Unary {
                op: UnaryOp::Complement,
                expr: Box::new(Expr::Literal(Literal::Int(1)))
            }
        );
    }

    #[test]
    fn parse_unary_negate_empty() {
        let tokens = &[Token::Minus, Token::Literal("1")];

        let (expr, tokens) = Factor::parse(tokens).unwrap();

        assert!(tokens.is_empty());

        assert_eq!(
            expr,
            Expr::Unary {
                op: UnaryOp::Negate,
                expr: Box::new(Expr::Literal(Literal::Int(1)))
            }
        );
    }

    #[test]
    fn parse_unary_negate() {
        let tokens = &[Token::Minus, Token::Literal("1"), Token::Semi];

        let (expr, tokens) = Factor::parse(tokens).unwrap();

        assert_eq!(tokens, &[Token::Semi]);
        assert_eq!(
            expr,
            Expr::Unary {
                op: UnaryOp::Negate,
                expr: Box::new(Expr::Literal(Literal::Int(1)))
            }
        );
    }

    #[test]
    fn parse_nested_expr() {
        let tokens = &[
            Token::LParen,
            Token::LParen,
            Token::LParen,
            Token::Literal("1"),
            Token::RParen,
            Token::RParen,
            Token::RParen,
        ];

        let (expr, tokens) = Factor::parse(tokens).unwrap();

        assert!(tokens.is_empty());
        assert_eq!(expr, Expr::Literal(Literal::Int(1)));
    }

    #[test]
    fn parse_unary_complex() {
        let tokens = &[
            Token::LParen,
            Token::BitNot,
            Token::LParen,
            Token::Minus,
            Token::LParen,
            Token::Literal("2"),
            Token::RParen,
            Token::RParen,
            Token::RParen,
        ];

        let (expr, tokens) = Factor::parse(tokens).unwrap();

        assert!(tokens.is_empty());

        assert_eq!(
            expr,
            Expr::Unary {
                op: UnaryOp::Complement,
                expr: Box::new(Expr::Unary {
                    op: UnaryOp::Negate,
                    expr: Box::new(Expr::Literal(Literal::Int(2)))
                })
            }
        );
    }

    #[test]
    fn return_2_unary_edition() {
        let tokens = &[
            Token::Int,
            Token::Ident("main"),
            Token::LParen,
            Token::Void,
            Token::RParen,
            Token::LSquirly,
            Token::Return,
            Token::Minus,
            Token::LParen,
            Token::Minus,
            Token::Literal("2"),
            Token::RParen,
            Token::Semi,
            Token::RSquirly,
        ];
        let program = parse(tokens).unwrap();
        let expected = Program {
            function: Function {
                ret_t: Type::Int,
                name: "main",
                signature: vec![],
                items: vec![BlockItem::Stmt(Stmt::Return(Some(Expr::Unary {
                    op: UnaryOp::Negate,
                    expr: Box::new(Expr::Unary {
                        op: UnaryOp::Negate,
                        expr: Box::new(Expr::Literal(Literal::Int(2))),
                    }),
                })))],
            },
        };
        assert_eq!(expected, program);
    }

    #[test]
    fn parse_expr_thing() {
        let tokens = &[
            Token::Minus,
            Token::LParen,
            Token::Minus,
            Token::Literal("2"),
            Token::RParen,
            Token::Semi,
        ];

        let (expr, tokens) = Expr::parse(tokens, 0).unwrap();

        assert_eq!(&tokens, &[Token::Semi]);

        assert_eq!(
            expr,
            Expr::Unary {
                op: UnaryOp::Negate,
                expr: Box::new(Expr::Unary {
                    op: UnaryOp::Negate,
                    expr: Box::new(Expr::Literal(Literal::Int(2)))
                }),
            }
        );
    }

    #[test]
    fn parse_expr_stmt() {
        let tokens = &[
            Token::Return,
            Token::Minus,
            Token::LParen,
            Token::Minus,
            Token::Literal("2"),
            Token::RParen,
            Token::Semi,
        ];

        let (_stmt, tokens) = Stmt::consume(tokens).unwrap();

        assert!(tokens.is_empty());
    }
}
