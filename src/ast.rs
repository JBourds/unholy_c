use crate::lexer::Token;
use anyhow::{bail, Context, Error, Result};
use std::cmp;
use std::rc::Rc;

pub fn parse(tokens: &[Token]) -> Result<Program> {
    let (program, tokens) = Program::consume(tokens)?;
    if !tokens.is_empty() {
        bail!("Found extra tokens when parsing main:\n{:#?}", tokens)
    } else {
        Ok(program)
    }
}

// Get ident nested in arbitrary number of parentheses
fn parse_ident(tokens: &[Token]) -> Result<(Rc<String>, &[Token])> {
    let mut nparens = 0;
    let mut name = None;
    for token in tokens.iter() {
        match token {
            Token::LParen => {
                nparens += 1;
            }
            Token::Ident(s) => {
                name = Some(Rc::clone(s));
                if tokens[nparens + 1..nparens + 1 + nparens]
                    .iter()
                    .any(|t| *t != Token::RParen)
                {
                    bail!("Invalid parentheses around identifier \"{}\"", s);
                }
                break;
            }
            _ => {
                bail!(
                    "Failed to parse identifier in declaration. Found {:?} instead.",
                    token
                );
            }
        }
    }
    if let Some(name) = name {
        Ok::<(std::rc::Rc<String>, &[Token]), Error>((name, &tokens[nparens + nparens + 1..]))
    } else {
        bail!("Unable to parse identifier in declaration.")
    }
}

pub trait AstNode {
    fn consume(tokens: &[Token]) -> Result<(Self, &[Token])>
    where
        Self: Sized;
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub functions: Vec<FunDecl>,
}
impl AstNode for Program {
    fn consume(tokens: &[Token]) -> Result<(Program, &[Token])> {
        let mut seen_main = false;
        let mut functions = vec![];
        let mut remaining = tokens;
        // This will change again very soon with file scope variables
        while let Ok((function, tokens)) = FunDecl::consume(remaining) {
            match (
                function.ret_t,
                function.name.as_ref().cmp(&"main".to_string()),
            ) {
                (Type::Int, cmp::Ordering::Equal) => {
                    if seen_main {
                        bail!("Duplicate \"main\" function definition!.");
                    }
                    seen_main = true;
                    functions.push(function);
                }
                _ => {
                    functions.push(function);
                }
            }
            remaining = tokens;
        }
        if !seen_main {
            bail!("No definition for \"main\" function.");
        }
        if !remaining.is_empty() {
            bail!("Found remaining tokens {:?}.", remaining);
        }
        Ok((Program { functions }, remaining))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunDecl {
    pub ret_t: Type,
    pub name: Rc<String>,
    pub signature: ParameterList,
    pub block: Option<Block>,
}

type ParameterList = Vec<(Type, Option<Rc<String>>)>;

impl FunDecl {
    /// Parses [ <type> [name] ]*.
    /// Does not consume opening or closing parentheses
    fn parse_parameter_list(tokens: &[Token]) -> Result<(ParameterList, &[Token])> {
        let mut signature = vec![];
        let remaining = match tokens {
            [Token::Void, Token::RParen, ..] => &tokens[1..],
            [Token::Void, t, ..] => {
                bail!("Expected closing parentheses but found \"{}\"", t)
            }
            _ => {
                let mut remaining = tokens;
                while let Ok((typ, tokens)) = Type::consume(remaining) {
                    let (name, tokens) = parse_ident(tokens)
                        .map(|(name, tokens)| (Some(name), tokens))
                        .unwrap_or((None, tokens));
                    signature.push((typ, name));

                    if let [Token::Comma, tokens @ ..] = tokens {
                        remaining = tokens;
                    } else {
                        remaining = tokens;
                    }
                }
                remaining
            }
        };
        Ok((signature, remaining))
    }
}

impl AstNode for FunDecl {
    fn consume(tokens: &[Token]) -> Result<(Self, &[Token])> {
        let (ret_t, tokens) = Type::consume(tokens)
            .context("No return type indicated for function. Add a return type, or mark the function as returning \"void\" to signal that it returns nothing.")?;
        let (name, tokens) = parse_ident(tokens).context("Missing function name.")?;
        if let [Token::LParen, tokens @ ..] = tokens {
            let (signature, tokens) = Self::parse_parameter_list(tokens)
                .with_context(|| format!("Unable to parse parameter list for {}", name))?;
            if tokens.first().is_some_and(|t| *t != Token::RParen) {
                bail!("Expected \")\" to close parameter list.");
            }
            let remaining = &tokens[1..];

            let (block, tokens) = match remaining {
                [Token::Semi, tokens @ ..] => (None, tokens),
                [Token::LSquirly, ..] => {
                    let (block, tokens) = Block::consume(remaining)
                        .context("Failed to parse block within function definition.")?;
                    (Some(block), tokens)
                }
                [token, ..] => bail!(
                    "Expected \";\" or \"{{\" after function signature but found token: {}.",
                    token
                ),
                [] => bail!(
                    "Expected \";\" or \"{{\" after function signature but found no more tokens."
                ),
            };

            Ok((
                Self {
                    ret_t,
                    name: Rc::clone(&name),
                    signature,
                    block,
                },
                tokens,
            ))
        } else {
            bail!("Expected start of function parameter list.")
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum BlockItem {
    Stmt(Stmt),
    Decl(Declaration),
}

impl AstNode for BlockItem {
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
pub struct VarDecl {
    pub typ: Type,
    pub name: Rc<String>,
    pub init: Option<Expr>,
}

impl AstNode for VarDecl {
    fn consume(tokens: &[Token]) -> Result<(Self, &[Token])> {
        let (typ, tokens) =
            Type::consume(tokens).context("Unable to parse type in declaration.")?;
        let (name, tokens) =
            parse_ident(tokens).context("Failed to find valid identifer for declaration.")?;
        match tokens {
            [Token::Assign, tokens @ ..] => {
                let (expr, tokens) = Expr::parse(tokens, 0)?;
                if tokens.first().is_some_and(|x| *x != Token::Semi) {
                    bail!("Semicolon required after expression in variable declaration.")
                } else {
                    Ok((
                        Self {
                            typ,
                            name,
                            init: Some(expr),
                        },
                        &tokens[1..],
                    ))
                }
            }
            [Token::Semi, tokens @ ..] => Ok((
                Self {
                    typ,
                    name,
                    init: None,
                },
                tokens,
            )),
            _ => bail!("Unable to parse valid declaration."),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Declaration {
    FunDecl(FunDecl),
    VarDecl(VarDecl),
}

impl AstNode for Declaration {
    fn consume(tokens: &[Token]) -> Result<(Self, &[Token])> {
        if let Ok((decl, tokens)) = FunDecl::consume(tokens) {
            Ok((Self::FunDecl(decl), tokens))
        } else if let Ok((decl, tokens)) = VarDecl::consume(tokens) {
            Ok((Self::VarDecl(decl), tokens))
        } else {
            bail!("Unable to parse valid declaration form.")
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
}

impl AstNode for Block {
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

impl AstNode for ForInit {
    fn consume(tokens: &[Token]) -> Result<(Self, &[Token])> {
        match tokens {
            [Token::Semi, tokens @ ..] => Ok((ForInit::Expr(None), tokens)),
            tokens => {
                if let Ok((decl, tokens)) = VarDecl::consume(tokens) {
                    Ok((ForInit::Decl(decl), tokens))
                } else {
                    let (expr, tokens) = Expr::parse(tokens, 0)
                        .context("Expected decleration or expression but failed to parse both")?;
                    if let Some(Token::Semi) = tokens.first() {
                        Ok((ForInit::Expr(Some(expr)), &tokens[1..]))
                    } else {
                        bail!("Missing semicolon after init expression.")
                    }
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Compound(Block),
    Return(Option<Expr>),
    Expr(Expr),
    If {
        condition: Expr,
        then: Box<Stmt>,
        r#else: Option<Box<Stmt>>,
    },
    Break(Option<Rc<String>>),
    Continue(Option<Rc<String>>),
    While {
        condition: Expr,
        body: Box<Stmt>,
        label: Option<Rc<String>>,
    },
    DoWhile {
        body: Box<Stmt>,
        condition: Expr,
        label: Option<Rc<String>>,
    },
    For {
        init: ForInit,
        condition: Option<Expr>,
        post: Option<Expr>,
        body: Box<Stmt>,
        label: Option<Rc<String>>,
    },
    // In newer compilers these don't have to have a statement after them
    Label {
        name: Rc<String>,
        stmt: Box<Stmt>,
    },
    Case {
        value: Expr,
        stmt: Box<Stmt>,
        label: Option<Rc<String>>,
    },
    Default {
        label: Option<Rc<String>>,
        stmt: Box<Stmt>,
    },
    Switch {
        condition: Expr,
        body: Box<Stmt>,
        label: Option<Rc<String>>,
        cases: Option<Vec<(Literal, Rc<String>)>>,
        default: Option<Rc<String>>,
    },
    Goto(Rc<String>),
    Null,
}

impl AstNode for Stmt {
    fn consume(tokens: &[Token]) -> Result<(Stmt, &[Token])> {
        let semi_terminated_expr = |tokens| {
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
            [Token::LSquirly, ..] => {
                let (block, tokens) = Block::consume(tokens)?;
                Ok((Self::Compound(block), tokens))
            }
            [Token::Semi, tokens @ ..] => Ok((Self::Null, tokens)),
            [Token::Goto, Token::Ident(name), Token::Semi, tokens @ ..] => {
                Ok((Self::Goto(Rc::new(name.to_string())), tokens))
            }
            [Token::Ident(name), Token::Colon, tokens @ ..] => {
                let (stmt, tokens) = Stmt::consume(tokens)?;
                Ok((
                    Self::Label {
                        name: Rc::new(name.to_string()),
                        stmt: Box::new(stmt),
                    },
                    tokens,
                ))
            }
            [Token::Break, Token::Semi, tokens @ ..] => Ok((Self::Break(None), tokens)),
            [Token::Continue, Token::Semi, tokens @ ..] => Ok((Self::Continue(None), tokens)),
            [Token::While, Token::LParen, tokens @ ..] => {
                let (condition, tokens) = Expr::parse(tokens, 0)
                    .context("Failed to parse expression for while statement conditional")?;

                let tokens = if let Some(Token::RParen) = tokens.first() {
                    &tokens[1..]
                } else {
                    bail!("While statment conditional must be closed with right paren");
                };
                let (body, tokens) = Stmt::consume(tokens).context("Failed to parse while body")?;

                Ok((
                    Self::While {
                        condition,
                        body: Box::new(body),
                        label: None,
                    },
                    tokens,
                ))
            }
            [Token::Do, tokens @ ..] => {
                let (body, tokens) = Stmt::consume(tokens).context("Failed to parse while body")?;
                match tokens {
                    [Token::While, Token::LParen, tokens @ ..] => {
                        let (condition, tokens) = Expr::parse(tokens, 0).context(
                            "Failed to parse expression for do while statement conditional",
                        )?;

                        let tokens = if let Some(Token::RParen) = tokens.first() {
                            &tokens[1..]
                        } else {
                            bail!("Do-while statment conditional must be closed with right paren");
                        };
                        let tokens = if let Some(Token::Semi) = tokens.first() {
                            &tokens[1..]
                        } else {
                            bail!("Do-while statment conditional must end with semi colon");
                        };
                        Ok((
                            Self::DoWhile {
                                body: Box::new(body),
                                condition,
                                label: None,
                            },
                            tokens,
                        ))
                    }
                    _ => bail!("Failed to reach while part of do-while"),
                }
            }
            [Token::For, Token::LParen, tokens @ ..] => {
                let (init, tokens) =
                    ForInit::consume(tokens).context("Failed to parse ForInit for for-loop")?;
                let (condition, tokens) = match tokens {
                    [Token::Semi, tokens @ ..] => (None, tokens),
                    tokens => {
                        let (expr, tokens) = Expr::parse(tokens, 0)
                            .context("Expected expression but failed to parse one")?;
                        if let Some(Token::Semi) = tokens.first() {
                            (Some(expr), &tokens[1..])
                        } else {
                            bail!("Missing semicolon after condtition expression.")
                        }
                    }
                };
                let (post, tokens) = if let Ok((expr, tokens)) = Expr::parse(tokens, 0) {
                    (Some(expr), tokens)
                } else {
                    (None, tokens)
                };

                let tokens = if let Some(Token::RParen) = tokens.first() {
                    &tokens[1..]
                } else {
                    bail!("For statment must be closed with right paren");
                };
                let (body, tokens) =
                    Stmt::consume(tokens).context("Failed to parse for-loop body")?;

                Ok((
                    Self::For {
                        init,
                        condition,
                        post,
                        body: Box::new(body),
                        label: None,
                    },
                    tokens,
                ))
            }
            [Token::Case, tokens @ ..] => {
                let (expr, tokens) =
                    Expr::parse(tokens, 0).context("Case statement needs associated expression")?;

                let tokens = if let Some(Token::Colon) = tokens.first() {
                    &tokens[1..]
                } else {
                    bail!("Case statement should end with a colon");
                };
                let (stmt, tokens) = Stmt::consume(tokens).context("Failed to parse case body")?;

                Ok((
                    Self::Case {
                        value: expr,
                        stmt: Box::new(stmt),
                        label: None,
                    },
                    tokens,
                ))
            }
            [Token::Default, Token::Colon, tokens @ ..] => {
                let (stmt, tokens) =
                    Stmt::consume(tokens).context("Failed to parse default body")?;

                Ok((
                    Self::Default {
                        stmt: Box::new(stmt),
                        label: None,
                    },
                    tokens,
                ))
            }
            [Token::Switch, Token::LParen, tokens @ ..] => {
                let (condition, tokens) = Expr::parse(tokens, 0)
                    .context("Failed to parse expression for switch statement conditional")?;
                let tokens = if let Some(Token::RParen) = tokens.first() {
                    &tokens[1..]
                } else {
                    bail!("Switch statment conditional must be closed with right paren");
                };
                let (body, tokens) =
                    Stmt::consume(tokens).context("Failed to parse body of switch statement")?;
                Ok((
                    Self::Switch {
                        condition,
                        body: Box::new(body),
                        label: None,
                        cases: None,
                        default: None,
                    },
                    tokens,
                ))
            }
            [Token::Return, Token::Semi, tokens @ ..] => Ok((Self::Return(None), tokens)),
            [Token::Return, tokens @ ..] => {
                let (expr, tokens) = semi_terminated_expr(tokens)?;
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
                let (expr, tokens) = semi_terminated_expr(tokens)?;
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
    FunCall {
        name: Rc<String>,
        args: Vec<Expr>,
    },
}

impl Expr {
    pub fn parse<'a>(tokens: &'a [Token], min_precedence: u32) -> Result<(Expr, &'a [Token])> {
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
                    |tokens: &'a [Token]| -> Result<(Expr, &'a [Token])> {
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
    fn check_for_postfix(expr: Expr, tokens: &[Token]) -> (Expr, &[Token]) {
        match UnaryOp::consume_postfix(tokens) {
            Ok((op, tokens)) => Self::check_for_postfix(
                Expr::Unary {
                    op,
                    expr: Box::new(expr),
                },
                tokens,
            ),
            _ => (expr, tokens),
        }
    }

    fn check_for_call(expr: Expr, tokens: &[Token]) -> Result<(Expr, &[Token])> {
        match (&expr, tokens.first()) {
            (Expr::Var(name), Some(Token::LParen)) => {
                let mut args = vec![];
                let mut remaining = &tokens[1..];
                if let Some(Token::RParen) = remaining.first() {
                    Ok((
                        Expr::FunCall {
                            name: Rc::clone(name),
                            args,
                        },
                        &remaining[1..],
                    ))
                } else {
                    while let Ok((arg, rest)) = Expr::parse(remaining, 0) {
                        args.push(arg);
                        remaining = match rest {
                            [Token::Comma, ..] => &rest[1..],
                            [Token::RParen, ..] => &rest[1..],
                            _ => bail!("Function call parameter list was not terminated."),
                        }
                    }
                    Ok((
                        Expr::FunCall {
                            name: Rc::clone(name),
                            args,
                        },
                        remaining,
                    ))
                }
            }
            _ => Ok((expr, tokens)),
        }
    }

    pub fn parse(tokens: &[Token]) -> Result<(Expr, &[Token])> {
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
                [Token::Literal(_), ..] => {
                    let (lit, tokens) = Literal::consume(tokens)?;
                    Ok((Expr::Literal(lit), tokens))
                }
                [Token::Ident(s), tokens @ ..] => {
                    let (expr, tokens) = Self::check_for_postfix(Expr::Var(Rc::clone(s)), tokens);
                    Self::check_for_call(expr, tokens)
                }
                [Token::LParen, tokens @ ..] => {
                    let (expr, tokens) = Expr::parse(tokens, 0)
                        .context("Parsing grammer rule: \"(\" <exp> \")\" failed")?;
                    match tokens {
                        [Token::RParen, tokens @ ..] => {
                            let (expr, tokens) = Self::check_for_postfix(expr, tokens);
                            Self::check_for_call(expr, tokens)
                        }
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
impl AstNode for Type {
    fn consume(tokens: &[Token]) -> Result<(Type, &[Token])> {
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Literal {
    Int(i32),
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(v) => write!(f, "{v}"),
        }
    }
}

impl AstNode for Literal {
    fn consume(tokens: &[Token]) -> Result<(Literal, &[Token])> {
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
}

impl UnaryOp {
    pub fn is_valid_for(&self, expr: &Expr) -> bool {
        !matches!(
            self,
            UnaryOp::PreInc | UnaryOp::PreDec | UnaryOp::PostInc | UnaryOp::PostDec
        ) || matches!(expr, Expr::Var(_))
    }

    fn consume_prefix(tokens: &[Token]) -> Result<(Self, &[Token])> {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn return_2() {
        let tokens = &[
            Token::Int,
            Token::Ident(Rc::new("main".to_string())),
            Token::LParen,
            Token::Void,
            Token::RParen,
            Token::LSquirly,
            Token::Return,
            Token::Literal(Rc::new("2".to_string())),
            Token::Semi,
            Token::RSquirly,
        ];
        let program = parse(tokens).unwrap();
        let expected = Program {
            functions: vec![FunDecl {
                ret_t: Type::Int,
                name: Rc::new("main".to_string()),
                signature: vec![],
                block: Some(Block(vec![BlockItem::Stmt(Stmt::Return(Some(
                    Expr::Literal(Literal::Int(2)),
                )))])),
            }],
        };
        assert_eq!(expected, program);
    }

    #[test]
    fn test_multiple_params() {
        let tokens = &[
            Token::Int,
            Token::Ident(Rc::new("multiple_args".to_string())),
            Token::LParen,
            Token::Int,
            Token::Ident(Rc::new("x".to_string())),
            Token::Comma,
            Token::Int,
            Token::Comma,
            Token::Int,
            Token::Ident(Rc::new("z".to_string())),
            Token::RParen,
            Token::LSquirly,
            Token::Return,
            Token::Literal(Rc::new("2".to_string())),
            Token::Semi,
            Token::RSquirly,
        ];
        let (function, _) = FunDecl::consume(tokens).unwrap();
        let expected = FunDecl {
            ret_t: Type::Int,
            name: Rc::new("multiple_args".to_string()),
            signature: vec![
                (Type::Int, Some(Rc::new("x".to_string()))),
                (Type::Int, None),
                (Type::Int, Some(Rc::new("z".to_string()))),
            ],
            block: Some(Block(vec![BlockItem::Stmt(Stmt::Return(Some(
                Expr::Literal(Literal::Int(2)),
            )))])),
        };
        assert_eq!(expected, function);
    }

    #[test]
    fn rejects_trailing_comma() {
        let tokens = &[
            Token::Int,
            Token::Ident(Rc::new("trailing_comma".to_string())),
            Token::LParen,
            Token::Int,
            Token::Ident(Rc::new("x".to_string())),
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
            Token::Ident(Rc::new("main".to_string())),
            Token::LParen,
            Token::Void,
            Token::RParen,
            Token::LSquirly,
            Token::RSquirly,
        ];
        let program = parse(tokens).unwrap();
        let expected = Program {
            functions: vec![FunDecl {
                name: Rc::new("main".to_string()),
                block: Some(Block(vec![])),
                signature: vec![],
                ret_t: Type::Int,
            }],
        };
        assert_eq!(expected, program);
    }

    #[test]
    fn parse_unary_bitnot_empty() {
        let tokens = &[Token::BitNot, Token::Literal(Rc::new("1".to_string()))];

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
        let tokens = &[
            Token::BitNot,
            Token::Literal(Rc::new("1".to_string())),
            Token::Semi,
        ];

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
        let tokens = &[Token::Minus, Token::Literal(Rc::new("1".to_string()))];

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
        let tokens = &[
            Token::Minus,
            Token::Literal(Rc::new("1".to_string())),
            Token::Semi,
        ];

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
            Token::Literal(Rc::new("1".to_string())),
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
            Token::Literal(Rc::new("2".to_string())),
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
            Token::Ident(Rc::new("main".to_string())),
            Token::LParen,
            Token::Void,
            Token::RParen,
            Token::LSquirly,
            Token::Return,
            Token::Minus,
            Token::LParen,
            Token::Minus,
            Token::Literal(Rc::new("2".to_string())),
            Token::RParen,
            Token::Semi,
            Token::RSquirly,
        ];
        let program = parse(tokens).unwrap();
        let expected = Program {
            functions: vec![FunDecl {
                ret_t: Type::Int,
                name: Rc::new("main".to_string()),
                signature: vec![],
                block: Some(Block(vec![BlockItem::Stmt(Stmt::Return(Some(
                    Expr::Unary {
                        op: UnaryOp::Negate,
                        expr: Box::new(Expr::Unary {
                            op: UnaryOp::Negate,
                            expr: Box::new(Expr::Literal(Literal::Int(2))),
                        }),
                    },
                )))])),
            }],
        };
        assert_eq!(expected, program);
    }

    #[test]
    fn parse_expr_thing() {
        let tokens = &[
            Token::Minus,
            Token::LParen,
            Token::Minus,
            Token::Literal(Rc::new("2".to_string())),
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
            Token::Literal(Rc::new("2".to_string())),
            Token::RParen,
            Token::Semi,
        ];

        let (_stmt, tokens) = Stmt::consume(tokens).unwrap();

        assert!(tokens.is_empty());
    }
}
