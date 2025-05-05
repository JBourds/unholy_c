use crate::lexer::{ConstantSuffix, Token};

use anyhow::{Context, Error, Result, bail, ensure};
use num::bigint::BigUint;
use std::str::FromStr;
use std::{
    num::{NonZeroU8, NonZeroUsize},
    rc::Rc,
};

pub fn parse(tokens: &[Token]) -> Result<Program> {
    let (prog, _) = Program::consume(tokens)?;
    Ok(prog)
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
    pub declarations: Vec<Declaration>,
}
impl AstNode for Program {
    fn consume(tokens: &[Token]) -> Result<(Program, &[Token])> {
        let mut declarations = vec![];
        let mut remaining = tokens;
        // This will change again very soon with file scope variables
        while !remaining.is_empty() {
            let (declaration, tokens) = Declaration::consume(remaining)?;
            declarations.push(declaration);
            remaining = tokens;
        }
        Ok((Program { declarations }, remaining))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunDecl {
    pub ret_t: Type,
    pub name: Rc<String>,
    pub signature: Vec<(Type, Option<Rc<String>>)>,
    pub block: Option<Block>,
    pub storage_class: Option<StorageClass>,
}

impl From<&FunDecl> for Type {
    fn from(decl: &FunDecl) -> Self {
        let param_types = decl
            .signature
            .iter()
            .map(|(param_type, _)| param_type.clone())
            .collect::<Vec<Type>>();
        let base = BaseType::Fun {
            ret_t: Box::new(decl.ret_t.clone()),
            param_types,
        };
        Self {
            alignment: base.default_alignment(),
            base,
            ptr: None,
            storage: decl.storage_class,
            is_const: false,
        }
    }
}

type ParameterList = Vec<(Type, Option<Rc<String>>)>;

impl FunDecl {
    /// Parses [ <type> [name] ]*.
    /// Does not consume opening or closing parentheses
    fn parse_parameter_list(tokens: &[Token]) -> Result<(ParameterList, &[Token])> {
        let mut signature = vec![];
        let remaining = match tokens {
            [Token::RParen, ..] => tokens,
            [Token::Void, Token::RParen, ..] => &tokens[1..],
            [Token::Void, t, ..] => {
                bail!("Expected closing parentheses but found \"{}\"", t)
            }
            _ => {
                let mut keep_going = true;
                let mut remaining = tokens;
                while keep_going {
                    let (typ, tokens) = Type::consume(remaining)?;
                    ensure!(
                        typ.storage.is_none(),
                        "Cannot have storage specifier in parameter type."
                    );
                    let (name, tokens) = parse_ident(tokens)
                        .map(|(name, tokens)| (Some(name), tokens))
                        .unwrap_or((None, tokens));
                    signature.push((typ, name));
                    if let Some(Token::Comma) = tokens.first() {
                        remaining = &tokens[1..];
                    } else {
                        keep_going = false;
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
        // Kind of a hack, but use the return type parsing to get the storage
        // class for the function, since the class can be intermixed with
        // potentially many specifiers in the return type
        let (mut ret_t, tokens) = Type::consume(tokens).context("Failed to parse function type")?;
        let storage_class = ret_t.storage.take();

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
                    storage_class,
                },
                tokens,
            ))
        } else {
            bail!("Expected start of function parameter list.")
        }
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
            _ => bail!("Unable to parse valid variable declaration."),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Declaration {
    FunDecl(FunDecl),
    VarDecl(VarDecl),
}

impl Declaration {
    pub fn defining(&self) -> bool {
        match self {
            Declaration::FunDecl(decl) => decl.block.is_some(),
            Declaration::VarDecl(decl) => decl.init.is_some(),
        }
    }
}

impl AstNode for Declaration {
    fn consume(tokens: &[Token]) -> Result<(Self, &[Token])> {
        if let Ok((decl, tokens)) = FunDecl::consume(tokens) {
            Ok((Self::FunDecl(decl), tokens))
        } else if let Ok((decl, tokens)) = VarDecl::consume(tokens) {
            Ok((Self::VarDecl(decl), tokens))
        } else {
            bail!(
                "Unable to parse valid declaration form from tokens {:#?}",
                &tokens[..std::cmp::min(tokens.len(), 25)]
            )
        }
    }
}

impl From<&Declaration> for Type {
    fn from(value: &Declaration) -> Self {
        match value {
            Declaration::FunDecl(decl) => Type::from(decl),
            Declaration::VarDecl(decl) => decl.typ.clone(),
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
        init: Box<ForInit>,
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
        cases: Option<Vec<(Constant, Rc<String>)>>,
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
                        init: Box::new(init),
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
    Cast {
        target: Type,
        exp: Box<Expr>,
    },
    Constant(Constant),
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
    pub fn is_lvalue(&self) -> bool {
        matches!(self, Self::Var(_))
    }

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
                    let mut keep_going = true;
                    while keep_going {
                        let (arg, tokens) = Expr::parse(remaining, 0)?;
                        args.push(arg);
                        match tokens {
                            [Token::Comma, tokens @ ..] => {
                                remaining = tokens;
                            }
                            [Token::RParen, tokens @ ..] => {
                                keep_going = false;
                                remaining = tokens;
                            }
                            t => bail!(
                                "Expected a \",\" or \")\" in function parameter list but found {t:?}"
                            ),
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
                [Token::Constant { .. }, ..] => {
                    let (lit, tokens) = Constant::consume(tokens)?;
                    Ok((Expr::Constant(lit), tokens))
                }
                [Token::Ident(s), tokens @ ..] => {
                    let (expr, tokens) = Self::check_for_postfix(Expr::Var(Rc::clone(s)), tokens);
                    Self::check_for_call(expr, tokens)
                }
                // Could be parentheses for a type cast or expression precedence
                [Token::LParen, tokens @ ..] => {
                    if let Ok((typ, tokens)) = Type::consume(tokens) {
                        ensure!(
                            matches!(tokens.first(), Some(Token::RParen)),
                            "Expected closing parentheses in type cast."
                        );
                        ensure!(
                            typ.storage.is_none(),
                            "Cannot have storage specifier in type cast."
                        );
                        let tokens = &tokens[1..];
                        let (expr, tokens) = Factor::parse(tokens)
                            .context("Parsing grammer rule: \"(\" <exp> \")\" failed")?;
                        Self::check_for_call(
                            Expr::Cast {
                                target: typ,
                                exp: Box::new(expr),
                            },
                            tokens,
                        )
                    } else {
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
                }
                _ => bail!("Could not match valid grammar rule."),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum BaseType {
    Int {
        nbytes: usize,
        signed: Option<bool>,
    },
    Float(usize),
    Double(usize),
    Char,
    Fun {
        ret_t: Box<Type>,
        param_types: Vec<Type>,
    },
    // TODO: Implement later and make this a non unit variant
    Struct,
    Void,
}

// Need to implement this since the optional signed parameter should
// be considered signed by default or use the actual value
impl PartialEq for BaseType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Int {
                    nbytes: l_nbytes,
                    signed: l_signed,
                },
                Self::Int {
                    nbytes: r_nbytes,
                    signed: r_signed,
                },
            ) => {
                let l_signed = l_signed.is_none_or(|signed| signed);
                let r_signed = r_signed.is_none_or(|signed| signed);
                l_nbytes == r_nbytes && l_signed == r_signed
            }
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::Double(l0), Self::Double(r0)) => l0 == r0,
            (
                Self::Fun {
                    ret_t: l_ret_t,
                    param_types: l_param_types,
                },
                Self::Fun {
                    ret_t: r_ret_t,
                    param_types: r_param_types,
                },
            ) => l_ret_t == r_ret_t && l_param_types == r_param_types,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl BaseType {
    pub fn nbytes(&self) -> usize {
        match self {
            BaseType::Int { nbytes, .. } => *nbytes,
            BaseType::Float(nbytes) => *nbytes,
            BaseType::Double(nbytes) => *nbytes,
            BaseType::Char => 1,
            BaseType::Fun { .. } => unreachable!(),
            BaseType::Struct => unreachable!(),
            BaseType::Void => unreachable!(),
        }
    }

    pub fn int(nbytes: usize, signed: Option<bool>) -> Self {
        Self::Int { nbytes, signed }
    }

    pub fn float(double: bool) -> Self {
        if double {
            Self::Float(4)
        } else {
            Self::Double(8)
        }
    }

    // Promotion rules

    fn rank(&self) -> Option<usize> {
        // Completely arbitrary numbers but this ensures that rank favors size,
        // and that floating point representations will always win out
        match self {
            Self::Int { nbytes, signed } => Some(
                *nbytes * 10
                    + 1
                    // Hack to make sure unsigned ints rank above signed
                    + if signed.is_some_and(|signed| !signed) {
                        1
                    } else {
                        0
                    },
            ),
            Self::Float(nbytes) => Some(*nbytes * 20),
            Self::Double(nbytes) => Some(*nbytes * 30),
            _ => None,
        }
    }

    pub fn can_assign_to(&self, other: &Self) -> bool {
        match (self.rank(), other.rank()) {
            (Some(_), Some(_)) => true,
            (Some(_), None) | (None, Some(_)) => false,
            (None, None) => self == other,
        }
    }

    pub fn default_alignment(&self) -> NonZeroUsize {
        match self {
            Self::Int { nbytes, .. } => NonZeroUsize::new(*nbytes).unwrap(),
            Self::Float(nbytes) => NonZeroUsize::new(*nbytes).unwrap(),
            Self::Double(nbytes) => NonZeroUsize::new(*nbytes).unwrap(),
            Self::Char => NonZeroUsize::new(1).unwrap(),
            Self::Fun { .. } => NonZeroUsize::new(8).unwrap(), // This is for when we have function pointers
            Self::Struct => todo!(),
            Self::Void => todo!(),
        }
    }

    pub fn lift(lhs: Self, rhs: Self) -> Result<(Self, Self)> {
        let left = lhs.default_promote();
        let right = rhs
            .promote(&left)
            .context("Unable to promote lefthand side to match righthand side.")?;
        let left = left
            .promote(&right)
            .context("Unable to promote righthand side to match lefthand side.")?;
        Ok((left, right))
    }

    fn default_promote(self) -> Self {
        match self {
            // Integer types get promoted to a basic signed int by default
            Self::Int { .. } | Self::Char => {
                let int = Self::default();
                let int_rank = int.rank().expect("Integer does not have a rank?");
                if self.rank().expect("Integer does not have a rank?") >= int_rank {
                    self
                } else {
                    self.promote(&int)
                        .expect("We can always promote an integer to another integer type.")
                }
            }
            _ => self,
        }
    }

    fn promote(self, other: &Self) -> Result<Self> {
        let other_rank = other
            .rank()
            .context("Could not establish a rank for type {other:#?}.")?;
        if self
            .rank()
            .context("Could not establish a rank for type {self:#?}")?
            >= other_rank
        {
            Ok(self)
        } else {
            Ok(other.clone())
        }
    }
}

impl From<&Constant> for BaseType {
    fn from(value: &Constant) -> Self {
        match value {
            Constant::I8(_) => Self::int(core::mem::size_of::<i8>(), None),
            Constant::I16(_) => Self::int(core::mem::size_of::<i16>(), None),
            Constant::I32(_) => Self::int(core::mem::size_of::<i32>(), None),
            Constant::I64(_) => Self::int(core::mem::size_of::<i64>(), None),
            Constant::U8(_) => Self::int(core::mem::size_of::<u8>(), None),
            Constant::U16(_) => Self::int(core::mem::size_of::<u16>(), None),
            Constant::U32(_) => Self::int(core::mem::size_of::<u32>(), None),
            Constant::U64(_) => Self::int(core::mem::size_of::<u64>(), None),
            Constant::F32(_) => Self::int(core::mem::size_of::<f32>(), None),
            Constant::F64(_) => Self::int(core::mem::size_of::<f64>(), None),
        }
    }
}

impl Default for BaseType {
    fn default() -> Self {
        Self::int(4, None)
    }
}

impl AstNode for BaseType {
    fn consume(tokens: &[Token]) -> Result<(BaseType, &[Token])> {
        if let Some(t) = tokens.first() {
            // TODO: Add parsing for function type signatures here too:
            // <ret_t> (*<optional name>)(<arg>*)
            // - Name and asterisk can have parens around them
            // - Types cannot have parens
            // - No trailing commas
            match t {
                Token::Void => Ok((Self::Void, &tokens[1..])),
                // Use this as a default, fix it up where this gets called
                // if there are specifiers which change this
                Token::Int => Ok((
                    Self::Int {
                        nbytes: std::mem::size_of::<i32>(),
                        signed: None,
                    },
                    &tokens[1..],
                )),
                Token::Float => Ok((Self::Float(std::mem::size_of::<f32>()), &tokens[1..])),
                Token::Double => Ok((Self::Double(std::mem::size_of::<f64>()), &tokens[1..])),
                Token::Char => Ok((Self::Char, &tokens[1..])),
                // TODO: Recursive parsing logic for structs
                Token::Struct => Ok((Self::Struct, &tokens[1..])),
                _ => bail!("Could not parse base type."),
            }
        } else {
            bail!("No more tokens to parse a base type from.")
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypePtr {
    // Hardcoded to prevent >255 pointer depths (sorry pointer fans)
    depth: NonZeroU8,
    // Bit vectors for whether each depth of pointer can alias/is const
    is_restrict: [u8; Self::BITVEC_LENGTH],
    is_const: [u8; Self::BITVEC_LENGTH],
}

impl TypePtr {
    const BITVEC_LENGTH: usize = (u8::MAX / 8) as usize;

    #[allow(dead_code)]
    fn get(arr: &[u8], depth: NonZeroU8, max_depth: NonZeroU8) -> Result<bool> {
        if depth.gt(&max_depth) {
            bail!("Requested depth exceeds maximum pointer depth.")
        } else {
            let byte_index: usize = ((depth.get() - 1) / 8).into();
            let bit_index = Into::<usize>::into(depth.get() - 1) - (byte_index * 8);
            let mask = 1 << bit_index;
            Ok(arr[byte_index] & mask > 0)
        }
    }

    #[allow(dead_code)]
    fn get_const(&self, depth: NonZeroU8) -> Result<bool> {
        Self::get(&self.is_const, depth, self.depth)
    }

    #[allow(dead_code)]
    fn get_restrict(&self, depth: NonZeroU8) -> Result<bool> {
        Self::get(&self.is_restrict, depth, self.depth)
    }

    fn set(arr: &mut [u8], depth: NonZeroU8, max_depth: NonZeroU8) -> Result<()> {
        if depth.gt(&max_depth) {
            bail!("Requested depth exceeds maximum pointer depth.")
        } else {
            let byte_index: usize = ((depth.get() - 1) / 8).into();
            let bit_index = Into::<usize>::into(depth.get() - 1) - (byte_index * 8);
            let mask = 1 << bit_index;
            arr[byte_index] |= mask;
            Ok(())
        }
    }

    fn set_const(&mut self, depth: NonZeroU8) -> Result<()> {
        Self::set(&mut self.is_const, depth, self.depth)
    }

    fn set_restrict(&mut self, depth: NonZeroU8) -> Result<()> {
        Self::set(&mut self.is_restrict, depth, self.depth)
    }
}

impl Default for TypePtr {
    fn default() -> Self {
        Self {
            depth: NonZeroU8::new(1).unwrap(),
            is_restrict: Default::default(),
            is_const: Default::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Type {
    pub base: BaseType,
    pub alignment: NonZeroUsize,
    pub ptr: Option<TypePtr>,
    pub storage: Option<StorageClass>,
    pub is_const: bool,
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.base == other.base && self.alignment == other.alignment && self.ptr == other.ptr
    }
}

impl Type {
    pub fn bool() -> Self {
        Self::int(4, None)
    }

    pub fn int(nbytes: usize, signed: Option<bool>) -> Self {
        Self {
            base: BaseType::int(nbytes, signed),
            alignment: NonZeroUsize::new(nbytes).unwrap(),
            ptr: None,
            storage: None,
            is_const: true,
        }
    }

    pub fn float(nbytes: usize) -> Self {
        Self {
            base: BaseType::float(nbytes == BaseType::Double(0).nbytes()),
            alignment: NonZeroUsize::new(nbytes).unwrap(),
            ptr: None,
            storage: None,
            is_const: true,
        }
    }

    pub fn size_of(&self) -> usize {
        match self.ptr {
            Some(..) => 8, // FIXME: maybe this shouldn't be a magic constant
            None => self.base.nbytes(),
        }
    }
}

impl AstNode for Type {
    fn consume<'a>(tokens: &'a [Token]) -> Result<(Self, &'a [Token])> {
        let mut remaining = tokens;

        // Integers have these specifiers attached to them, and double can
        // have up to a single "long" in its specifier
        let mut n_longs = 0;
        let mut is_signed = None;
        let mut is_short = false;
        // See if there is a specifier, ok result is boolean for if a token was found
        let mut check_for_specifier = |t: &Token| match t {
            Token::Short => {
                is_short = true;
                if is_short && n_longs > 0 {
                    bail!("Found \"short\" and \"long\" in same type declaration.");
                }
                Ok(true)
            }
            Token::Long => {
                n_longs += 1;
                if is_short && n_longs > 0 {
                    bail!("Found \"short\" and \"long\" in same type declaration.");
                }
                Ok(true)
            }
            Token::Unsigned => {
                if is_signed.is_some_and(|signed| signed) {
                    bail!("Type cannot be both signed and unsigned.");
                } else if is_signed.is_some() {
                    bail!("Error: duplicate unsigned");
                }
                is_signed = Some(false);
                Ok(true)
            }
            Token::Signed => {
                if is_signed.is_some_and(|signed| !signed) {
                    bail!("Type cannot be both signed and unsigned.");
                } else if is_signed.is_some() {
                    bail!("Error: duplicate nsigned");
                }
                is_signed = Some(true);
                Ok(true)
            }
            _ => Ok(false),
        };

        // Anything can be const
        let mut is_const = false;
        let mut check_for_const = |t: &Token| {
            if *t == Token::Const {
                // We can have multiple constants
                is_const = true;
                true
            } else {
                false
            }
        };

        let mut storage = None;
        let mut check_for_storage = |t: &Token| {
            if storage.is_some() {
                bail!("Error: Multiple storage class specifiers.");
            }
            storage = match t {
                Token::Static => Some(StorageClass::Static),
                Token::Extern => Some(StorageClass::Extern),
                Token::Auto => Some(StorageClass::Auto),
                Token::Register => Some(StorageClass::Register),
                _ => None,
            };
            Ok(storage.is_some())
        };

        let mut base: Option<BaseType> = None;
        let mut check_for_base_type = |tokens| {
            if let Ok((r#type, tokens)) = BaseType::consume(tokens) {
                if base.is_some() {
                    bail!("Error: Found two conflicting types.");
                }
                base = Some(r#type);
                Ok((true, tokens))
            } else {
                Ok((false, tokens))
            }
        };

        // Only a pointer can be marked as "restrict"
        let mut is_restrict = false;
        let mut check_for_restrict = |t: &Token| {
            if *t == Token::Restrict {
                // We can have multiple restricts
                is_restrict = true;
                true
            } else {
                false
            }
        };

        let mut ptr: Option<TypePtr> = None;
        let mut check_for_ptr = |ptr: Option<TypePtr>, tokens: &'a [Token]| {
            let mut remaining = tokens;
            if let Some(Token::Star) = remaining.first() {
                // Entering a new depth, reset const and restrict flags
                let mut inner_ptr = if let Some(p) = ptr {
                    let _ = p.depth.checked_add(1);
                    p
                } else {
                    TypePtr::default()
                };
                // Eat up any const or restrict keywords
                // Ignore the result type from set_const since we are using
                // the type's own depth to set it
                while let Some(t) = remaining.first() {
                    if check_for_const(t) {
                        let _ = inner_ptr.set_const(inner_ptr.depth);
                        remaining = &remaining[1..];
                    } else if check_for_restrict(t) {
                        let _ = inner_ptr.set_restrict(inner_ptr.depth);
                        remaining = &remaining[1..];
                    } else {
                        break;
                    }
                    let _ = inner_ptr.depth.checked_add(1);
                }
                (true, Some(inner_ptr), remaining)
            } else {
                (false, ptr, remaining)
            }
        };

        // Continue eating tokens with aditional type specifiers, updating
        // flags and enforcing type specifier rules along the way
        // Requirements:
        //  1. Needs to find a valid "base type" (defined as primitives or
        //  a user defined struct). As part of this, specifiers for an integer
        //  must be kept track of.
        //  2. Optionally attach a storage class and const specification.
        //  3. Parse up to some maximum pointer depth (256) and for each
        //  pointer check whether it has a restrict or const keyword.
        while let Some(t) = remaining.first() {
            if check_for_specifier(t).is_ok_and(|found| found)
                || check_for_storage(t).is_ok_and(|found| found)
            {
                remaining = &remaining[1..];
            } else if let Ok((true, tokens)) = check_for_base_type(remaining) {
                remaining = tokens;
            } else {
                // We could not find anything else, but the type could be
                // a pointer. Parse as many levels of pointer depth as possible
                // until we no longer find another level or hit the maximum.
                while let (true, new_ptr, tokens) = check_for_ptr(ptr.take(), remaining) {
                    ptr = new_ptr;
                    remaining = tokens;
                }
                // Once we have exhausted all the pointer indirections exit
                // the loop
                break;
            }
        }

        // Integer checks
        // 1. If any of the integer flags changed, the base type either has
        // to be explicitly declared as an integer or have been elided
        if n_longs > 0 && is_short {
            bail!("Integer cannot be both a long and a short.");
        }
        if n_longs > 0 && matches!(base, Some(BaseType::Double { .. })) {
            base.replace(BaseType::Double(std::mem::size_of::<f64>()));
        } else if n_longs > 0 || is_signed.is_some() || is_short {
            match base {
                Some(BaseType::Int { .. }) | None => {
                    let nbytes = if n_longs > 0 {
                        std::mem::size_of::<i64>()
                    } else if is_short {
                        std::mem::size_of::<i16>()
                    } else {
                        std::mem::size_of::<i32>()
                    };
                    base.replace(BaseType::Int {
                        nbytes,
                        signed: is_signed,
                    });
                }
                _ => bail!(
                    "Cannot provide type specifiers specific to integers for non integer type."
                ),
            }
        }

        if let Some(base) = base {
            Ok((
                Self {
                    alignment: base.default_alignment(),
                    base,
                    ptr,
                    storage,
                    is_const,
                },
                remaining,
            ))
        } else {
            bail!("Did not find any base type.");
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(storage) = self.storage {
            write!(f, "{storage} ")?;
        }
        if self.is_const {
            write!(f, "const ")?;
        }
        self.base.fmt(f)?;

        if let Some(ref ptr) = self.ptr {
            for depth in 1..=ptr.depth.into() {
                let byte_index: usize = (depth / 8).into();
                let bit_index = Into::<usize>::into(depth) - byte_index * 8;
                let mask = 1 << bit_index;
                if ptr.is_const[byte_index] & mask > 0 {
                    write!(f, "const ")?;
                }
                if ptr.is_restrict[byte_index] & mask > 0 {
                    write!(f, "restrict ")?;
                }
                if depth == ptr.depth.into() {
                    write!(f, "*")?;
                } else {
                    write!(f, "* ")?;
                }
            }
        }

        Ok(())
    }
}

impl std::fmt::Display for BaseType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int { nbytes, signed } => {
                let mut sign = 'i';
                if let Some(signed) = signed {
                    if *signed {
                        write!(f, "signed ")?;
                    } else {
                        write!(f, "unsigned ")?;
                        sign = 'u';
                    }
                }
                let nbits = nbytes * 8;
                // When pretty printing, use the number of bytes rather than
                // bending the knee to the wobbly-sized integers
                write!(f, "{sign}{nbits}")
            }
            Self::Float(_) => write!(f, "float"),
            Self::Double(_) => write!(f, "double"),
            Self::Char => write!(f, "char"),
            Self::Struct => todo!(),
            Self::Fun { ret_t, param_types } => {
                write!(f, "(")?;
                for (index, t) in param_types.iter().enumerate() {
                    write!(f, "{t}")?;
                    if index < param_types.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") -> ")?;
                ret_t.fmt(f)
            }
            Self::Void => write!(f, "void"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Constant {
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

impl std::cmp::Eq for Constant {}

impl Constant {
    pub fn is_int(&self) -> bool {
        true
    }

    pub fn fits_in<T>(&self) -> bool
    where
        T: Sized,
    {
        match &self {
            Constant::I8(_) => core::mem::size_of::<T>() <= core::mem::size_of::<i8>(),
            Constant::I16(_) => core::mem::size_of::<T>() <= core::mem::size_of::<i16>(),
            Constant::I32(_) => core::mem::size_of::<T>() <= core::mem::size_of::<i32>(),
            Constant::I64(_) => core::mem::size_of::<T>() <= core::mem::size_of::<i64>(),
            Constant::U8(_) => core::mem::size_of::<T>() <= core::mem::size_of::<u8>(),
            Constant::U16(_) => core::mem::size_of::<T>() <= core::mem::size_of::<u16>(),
            Constant::U32(_) => core::mem::size_of::<T>() <= core::mem::size_of::<u32>(),
            Constant::U64(_) => core::mem::size_of::<T>() <= core::mem::size_of::<u64>(),
            Constant::F32(_) => core::mem::size_of::<T>() <= core::mem::size_of::<f32>(),
            Constant::F64(_) => core::mem::size_of::<T>() <= core::mem::size_of::<f64>(),
        }
    }

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

    pub fn get_type(&self) -> Type {
        match self {
            Self::U8(_) | Self::U16(_) | Self::U32(_) | Self::U64(_) => {
                Type::int(self.size_bytes(), Some(false))
            }
            Self::I8(_) | Self::I16(_) | Self::I32(_) | Self::I64(_) => {
                Type::int(self.size_bytes(), Some(true))
            }
            Self::F32(_) => Type::float(self.size_bytes()),
            Self::F64(_) => Type::float(self.size_bytes()),
        }
    }
}

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::I8(v) => write!(f, "{v}"),
            Constant::I16(v) => write!(f, "{v}"),
            Constant::I32(v) => write!(f, "{v}"),
            Constant::I64(v) => write!(f, "{v}"),
            Constant::U8(v) => write!(f, "{v}"),
            Constant::U16(v) => write!(f, "{v}"),
            Constant::U32(v) => write!(f, "{v}"),
            Constant::U64(v) => write!(f, "{v}"),
            Constant::F32(v) => write!(f, "{v}"),
            Constant::F64(v) => write!(f, "{v}"),
        }
    }
}

impl AstNode for Constant {
    fn consume(tokens: &[Token]) -> Result<(Constant, &[Token])> {
        // FIXME: Is this how we would like to handle integer overflow?
        if let Some(token) = tokens.first() {
            match token {
                // NOTE: The text in these nodes does not include the negative
                // sign so we don't need to worry about absolute value sign
                Token::Constant { text, suffix: None } => {
                    if let Ok(val) = text.parse::<i32>() {
                        Ok((Self::I32(val), &tokens[1..]))
                    } else if let Ok(val) = text.parse::<i64>() {
                        Ok((Self::I64(val), &tokens[1..]))
                    } else if let Ok(val) = text.parse::<u32>() {
                        Ok((Self::U32(val), &tokens[1..]))
                    } else if let Ok(val) = text.parse::<u64>() {
                        Ok((Self::U64(val), &tokens[1..]))
                    } else {
                        eprintln!("Warning: Integer constant is being truncated to fit.");
                        let mut bytes = BigUint::from_str(text)
                            .context(format!("Unable to parse BigUint from text: \"{text}\""))?
                            .to_bytes_le();
                        bytes.resize(core::mem::size_of::<i64>(), 0);
                        let val = Self::I64(i64::from_le_bytes(
                            bytes.into_boxed_slice()[..core::mem::size_of::<i64>()]
                                .try_into()
                                .unwrap(),
                        ));
                        Ok((val, &tokens[1..]))
                    }
                }
                Token::Constant { text, suffix } => match suffix {
                    Some(ConstantSuffix::Unsigned) => {
                        let val = if let Ok(val) = text.parse::<u32>() {
                            Self::U32(val)
                        } else if let Ok(val) = text.parse::<u64>() {
                            Self::U64(val)
                        } else {
                            eprintln!("Warning: Integer constant is being truncated to fit.");
                            let mut bytes = BigUint::from_str(text)
                                .context(format!("Unable to parse BigUint from text: \"{text}\""))?
                                .to_bytes_le();
                            bytes.resize(core::mem::size_of::<u64>(), 0);
                            Self::U64(u64::from_le_bytes(
                                bytes.into_boxed_slice()[..core::mem::size_of::<u64>()]
                                    .try_into()
                                    .unwrap(),
                            ))
                        };
                        Ok((val, &tokens[1..]))
                    }
                    Some(ConstantSuffix::UnsignedLong) => {
                        let val = if let Ok(val) = text.parse::<u64>() {
                            Self::U64(val)
                        } else {
                            eprintln!("Warning: Integer constant is being truncated to fit .");
                            let mut bytes = BigUint::from_str(text)
                                .context(format!("Unable to parse BigUint from text: \"{text}\""))?
                                .to_bytes_le();
                            bytes.resize(core::mem::size_of::<u64>(), 0);
                            Self::U64(u64::from_le_bytes(
                                bytes.into_boxed_slice()[..core::mem::size_of::<u64>()]
                                    .try_into()
                                    .unwrap(),
                            ))
                        };

                        Ok((val, &tokens[1..]))
                    }
                    Some(ConstantSuffix::Long) => {
                        let val = if let Ok(val) = text.parse::<i64>() {
                            Self::I64(val)
                        } else if let Ok(val) = text.parse::<u64>() {
                            Self::U64(val)
                        } else {
                            eprintln!("Warning: Integer constant is being truncated to fit .");
                            let mut bytes = BigUint::from_str(text)
                                .context(format!("Unable to parse BigUint from text: \"{text}\""))?
                                .to_bytes_le();
                            bytes.resize(core::mem::size_of::<u64>(), 0);
                            Self::U64(u64::from_le_bytes(
                                bytes.into_boxed_slice()[..core::mem::size_of::<u64>()]
                                    .try_into()
                                    .unwrap(),
                            ))
                        };
                        Ok((val, &tokens[1..]))
                    }
                    _ => bail!("Could not parse token into constant."),
                },
                _ => bail!("Could not parse token into constant."),
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
    pub fn is_logical(&self) -> bool {
        *self == Self::Not
    }

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
