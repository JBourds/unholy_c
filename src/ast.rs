use crate::lexer::{ConstantFlag, Token};

use anyhow::{Context, Result, bail, ensure};
use num::{NumCast, bigint::BigUint};
use std::str::FromStr;
use std::{num::NonZeroUsize, rc::Rc};

pub fn parse(tokens: &[Token]) -> Result<Program> {
    let (prog, _) = Program::consume(tokens)?;
    Ok(prog)
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

#[derive(Clone, Debug, PartialEq)]
pub struct FunDecl {
    pub r#type: Type,
    pub name: Rc<String>,
    pub params: Vec<Option<Rc<String>>>,
    pub block: Option<Block>,
    pub storage_class: Option<StorageClass>,
}

impl From<&FunDecl> for Type {
    fn from(value: &FunDecl) -> Self {
        value.r#type.clone()
    }
}

impl FunDecl {
    #[allow(clippy::type_complexity)]
    pub fn signature(&self) -> Result<Vec<(&Type, Option<&Rc<String>>)>> {
        if let Type {
            base: BaseType::Fun {
                ref param_types, ..
            },
            ..
        } = self.r#type
        {
            ensure!(
                param_types.len() == self.params.len(),
                "ast.FunDecl.signature(): Parameter type and name vectors should be the same length."
            );
            Ok(param_types
                .iter()
                .zip(self.params.iter())
                .map(|(r#type, name)| (r#type, name.as_ref()))
                .collect())
        } else {
            bail!("ast.FunDecl.signature(): How did we get here? This match should always work!");
        }
    }

    fn new_uninit(
        r#type: Type,
        storage_class: Option<StorageClass>,
        name: Rc<String>,
        params: Vec<Option<Rc<String>>>,
    ) -> Self {
        Self {
            r#type,
            name,
            params,
            block: None,
            storage_class,
        }
    }

    fn check_for_definition(mut self, tokens: &[Token]) -> Result<(Self, &[Token])> {
        ensure!(
            self.block.is_none(),
            "ast.FunDecl.check_for_definition: Attempting to check for definition on function which already has one."
        );

        let (block, tokens) = match tokens {
            [Token::Semi, tokens @ ..] => (None, tokens),
            [Token::LSquirly, ..] => {
                let (block, tokens) = Block::consume(tokens)
                    .context("Failed to parse block within function definition.")?;
                (Some(block), tokens)
            }
            [token, ..] => bail!(
                "Expected \";\" or \"{{\" after function signature but found token: {}.",
                token
            ),
            [] => {
                bail!("Expected \";\" or \"{{\" after function signature but found no more tokens.")
            }
        };
        self.block = block;
        Ok((self, tokens))
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
pub struct VarDecl {
    pub r#type: Type,
    pub name: Rc<String>,
    pub init: Option<Expr>,
    pub storage_class: Option<StorageClass>,
}

impl VarDecl {
    fn check_for_definition(mut self, tokens: &[Token]) -> Result<(Self, &[Token])> {
        match tokens {
            [Token::Assign, tokens @ ..] => {
                let (expr, tokens) = Expr::parse(tokens, 0)?;
                if tokens.first().is_some_and(|x| *x != Token::Semi) {
                    bail!("Semicolon required after expression in variable declaration.")
                } else {
                    self.init = Some(expr);
                    Ok((self, &tokens[1..]))
                }
            }
            [Token::Semi, tokens @ ..] => Ok((self, tokens)),
            _ => bail!("Unable to parse valid variable declaration."),
        }
    }

    fn new_uninit(r#type: Type, storage_class: Option<StorageClass>, name: Rc<String>) -> Self {
        Self {
            r#type,
            name,
            init: None,
            storage_class,
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

    pub fn storage_class(&self) -> Option<&StorageClass> {
        match self {
            Declaration::FunDecl(decl) => decl.storage_class.as_ref(),
            Declaration::VarDecl(decl) => decl.storage_class.as_ref(),
        }
    }

    fn consume(tokens: &[Token]) -> Result<(Self, &[Token])> {
        let (stream_offset, base, storage_class) = TypeBuilder::new()
            .get_base(tokens)
            .and_then(|b| b.into_type())
            .context("Error building base type from token stream.")?;
        let (declarator, tokens) = Declarator::consume(&tokens[stream_offset..])
            .context("ast.Declaration.consume(): Error while parsing declarator.")?;
        let (name, decl_type, params) = Declarator::process(declarator, base)
            .context("ast.Declaration.consume(): Error while processing declarator.")?;
        let name =
            name.expect("ast.Declaration.consume(): Declaration must have name being declared.");

        match decl_type {
            Type {
                base: BaseType::Fun { .. },
                ..
            } => {
                let (decl, tokens) = FunDecl::new_uninit(
                    decl_type,
                    storage_class,
                    Rc::clone(&name),
                    params,
                )
                .check_for_definition(tokens)
                .context(format!(
                    "ast.Declaration.consume(): Error parsing function declaration \"{name}\"."
                ))?;
                Ok((Declaration::FunDecl(decl), tokens))
            }
            _ => {
                let (decl, tokens) = VarDecl::new_uninit(
                    decl_type,
                    storage_class,
                    Rc::clone(&name),
                )
                .check_for_definition(tokens)
                .context(
                    "ast.Declaration.consume(): Error parsing variable declaration for \"{name}\".",
                )?;
                Ok((Declaration::VarDecl(decl), tokens))
            }
        }
    }
}

impl From<&Declaration> for Type {
    fn from(value: &Declaration) -> Self {
        match value {
            Declaration::FunDecl(decl) => decl.r#type.clone(),
            Declaration::VarDecl(decl) => decl.r#type.clone(),
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

impl Stmt {
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
    Subscript {
        expr: Box<Expr>,
        index: Box<Expr>,
    },
}

impl Expr {
    pub fn is_lvalue(&self) -> bool {
        matches!(
            self,
            Self::Var(_)
                | Self::Unary {
                    op: UnaryOp::Deref,
                    ..
                }
        )
    }

    pub fn has_compound(&self) -> bool {
        match self {
            Expr::Cast { target: _, exp } => exp.has_compound(),
            Expr::Binary { op, .. } => op.compound_op().is_some(),
            _ => false,
        }
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
                if operator.compound_op().is_some() {
                    left = Expr::Binary {
                        op: operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    };
                } else {
                    left = Expr::Assignment {
                        lvalue: Box::new(left),
                        rvalue: Box::new(right),
                    };
                }
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
                    Self::check_for_call(Expr::Var(Rc::clone(s)), tokens)
                }
                // Could be parentheses for a type cast or expression precedence
                [Token::LParen, tokens @ ..] => {
                    if let Ok((stream_offset, r#type, storage_class)) = TypeBuilder::new()
                        .get_base(tokens)
                        .and_then(|b| b.into_type())
                    {
                        let tokens = &tokens[stream_offset..];
                        let (r#type, tokens) =
                            if let Ok((decl, tokens)) = AbstractDeclarator::consume(tokens) {
                                (AbstractDeclarator::process(decl, r#type)?, tokens)
                            } else {
                                (r#type, tokens)
                            };

                        ensure!(
                            matches!(tokens.first(), Some(Token::RParen)),
                            "Expected closing parentheses in type cast."
                        );
                        ensure!(
                            storage_class.is_none(),
                            "Cannot have storage specifier in type cast."
                        );
                        let tokens = &tokens[1..];
                        let (expr, tokens) = Factor::parse(tokens)
                            .context("Parsing grammer rule: \"(\" <exp> \")\" failed")?;
                        Self::check_for_call(
                            Expr::Cast {
                                target: r#type,
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
            }
            .map(|(expr, tokens)| Self::check_for_postfix(expr, tokens)),
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
    Fun {
        ret_t: Box<Type>,
        param_types: Vec<Type>,
    },
    Ptr {
        to: Box<Type>,
        is_restrict: bool,
    },
    Array {
        element: Box<Type>,
        size: usize,
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
            (
                Self::Ptr {
                    to: l_to,
                    is_restrict: l_is_restrict,
                },
                Self::Ptr {
                    to: r_to,
                    is_restrict: r_is_restrict,
                },
            ) => *l_to == *r_to && l_is_restrict == r_is_restrict,
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
            BaseType::Ptr { .. } => core::mem::size_of::<usize>(),
            BaseType::Array { .. } => unimplemented!(),
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
            Self::Double(core::mem::size_of::<f64>())
        } else {
            Self::Float(core::mem::size_of::<f32>())
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
            Self::Fun { .. } | Self::Ptr { .. } => {
                NonZeroUsize::new(core::mem::size_of::<usize>()).unwrap()
            }
            Self::Array { element, .. } => element.base.default_alignment(),
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
            Self::Int { .. } => {
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
                // TODO: Recursive parsing logic for structs
                Token::Struct => Ok((Self::Struct, &tokens[1..])),
                _ => bail!("Could not parse base type."),
            }
        } else {
            bail!("No more tokens to parse a base type from.")
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
            Constant::U32(_) => Self::int(core::mem::size_of::<u32>(), Some(false)),
            Constant::U64(_) => Self::int(core::mem::size_of::<u64>(), Some(false)),
            Constant::F32(_) => Self::float(false),
            Constant::F64(_) => Self::float(true),
        }
    }
}

impl Default for BaseType {
    fn default() -> Self {
        Self::int(core::mem::size_of::<i32>(), None)
    }
}

#[derive(Debug, Clone)]
pub struct Type {
    pub base: BaseType,
    pub alignment: NonZeroUsize,
    pub is_const: bool,
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.base == other.base && self.alignment == other.alignment
    }
}

impl Type {
    pub const PTR_ALIGNMENT: NonZeroUsize =
        NonZeroUsize::new(core::mem::size_of::<usize>()).unwrap();
    pub fn bool() -> Self {
        Self::int(core::mem::size_of::<i32>(), None)
    }

    pub fn int(nbytes: usize, signed: Option<bool>) -> Self {
        Self {
            base: BaseType::int(nbytes, signed),
            alignment: NonZeroUsize::new(nbytes).unwrap(),
            is_const: true,
        }
    }

    pub fn float(nbytes: usize) -> Self {
        Self {
            base: BaseType::float(nbytes == core::mem::size_of::<f64>()),
            alignment: NonZeroUsize::new(nbytes).unwrap(),
            is_const: true,
        }
    }

    pub fn size_of(&self) -> usize {
        self.base.nbytes()
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self.base, BaseType::Ptr { .. })
    }

    pub fn is_arithmetic(&self) -> bool {
        matches!(
            self.base,
            BaseType::Int { .. } | BaseType::Float(_) | BaseType::Double(_)
        )
    }

    pub fn is_float(&self) -> bool {
        matches!(self.base, BaseType::Float(_) | BaseType::Double(_))
    }

    pub fn is_function(&self) -> bool {
        matches!(self.base, BaseType::Fun { .. })
    }

    pub fn deref(self) -> Self {
        assert!(self.is_pointer(), "Cannot derefrence non-pointer type");
        let BaseType::Ptr { to, is_restrict: _ } = self.base else {
            unreachable!()
        };
        *to
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_const {
            write!(f, "const ")?;
        }
        self.base.fmt(f)?;

        Ok(())
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
struct TypeBuilder {
    stream_offset: usize,
    n_longs: usize,
    is_signed: Option<bool>,
    is_short: bool,
    is_const: bool,
    alignment: Option<NonZeroUsize>,
    storage: Option<StorageClass>,
    base: Option<BaseType>,
}

impl TypeBuilder {
    fn new() -> Self {
        Self::default()
    }

    fn get_base(mut self, tokens: &[Token]) -> Result<Self> {
        let mut remaining = tokens;
        while let Some(t) = remaining.first() {
            if self.check_for_specifier(t)? || self.check_for_storage(t)? || self.check_for_const(t)
            {
                remaining = &remaining[1..];
            } else if let (true, tokens) = self.check_for_base_type(remaining)? {
                remaining = tokens;
            } else {
                break;
            }
        }

        // Integer checks
        // 1. If any of the integer flags changed, the base type either has
        // to be explicitly declared as an integer or have been elided
        if self.n_longs > 0 && self.is_short {
            bail!("Integer cannot be both a long and a short.");
        }
        if self.n_longs > 0 && matches!(&self.base, Some(BaseType::Double { .. })) {
            self.base
                .replace(BaseType::Double(std::mem::size_of::<f64>()));
        } else if self.n_longs > 0 || self.is_signed.is_some() || self.is_short {
            match self.base {
                Some(BaseType::Int { .. }) | None => {
                    let nbytes = if self.n_longs > 0 {
                        std::mem::size_of::<i64>()
                    } else if self.is_short {
                        std::mem::size_of::<i16>()
                    } else {
                        std::mem::size_of::<i32>()
                    };
                    self.base.replace(BaseType::Int {
                        nbytes,
                        signed: self.is_signed,
                    });
                }
                _ => bail!(
                    "Cannot provide type specifiers specific to integers for non integer type."
                ),
            }
        }

        // SAFETY: `remaining` is constructed from within `tokens` slice of
        // memory. This will always be valid, and was a whole lot easier to do
        // than reworking all our parsing logic :^)
        self.stream_offset =
            unsafe { remaining.as_ptr().offset_from(tokens.as_ptr()) }.try_into()?;

        Ok(self)
    }

    fn check_for_base_type<'a>(&mut self, tokens: &'a [Token]) -> Result<(bool, &'a [Token])> {
        if let Ok((r#type, tokens)) = BaseType::consume(tokens) {
            if self.base.is_some() {
                bail!("Error: Found two conflicting types.");
            }
            self.base = Some(r#type);
            Ok((true, tokens))
        } else {
            Ok((false, tokens))
        }
    }

    fn check_for_const(&mut self, t: &Token) -> bool {
        if *t == Token::Const {
            // More `const` won't make it more constant but still allow it
            self.is_const = true;
            true
        } else {
            false
        }
    }

    fn check_for_specifier(&mut self, t: &Token) -> Result<bool> {
        match t {
            Token::Short => {
                self.is_short = true;
                if self.is_short && self.n_longs > 0 {
                    bail!("Found \"short\" and \"long\" in same type declaration.");
                }
                Ok(true)
            }
            Token::Long => {
                self.n_longs += 1;
                if self.is_short && self.n_longs > 0 {
                    bail!("Found \"short\" and \"long\" in same type declaration.");
                }
                Ok(true)
            }
            Token::Unsigned => {
                if self.is_signed.is_some_and(|signed| signed) {
                    bail!("Type cannot be both signed and unsigned.");
                } else if self.is_signed.is_some() {
                    bail!("Error: duplicate unsigned");
                }
                self.is_signed = Some(false);
                Ok(true)
            }
            Token::Signed => {
                if self.is_signed.is_some_and(|signed| !signed) {
                    bail!("Type cannot be both signed and unsigned.");
                } else if self.is_signed.is_some() {
                    bail!("Error: duplicate nsigned");
                }
                self.is_signed = Some(true);
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn check_for_storage(&mut self, t: &Token) -> Result<bool> {
        let storage = match t {
            Token::Static => Some(StorageClass::Static),
            Token::Extern => Some(StorageClass::Extern),
            Token::Auto => Some(StorageClass::Auto),
            Token::Register => Some(StorageClass::Register),
            _ => None,
        };
        match (self.storage.is_some(), storage.is_some()) {
            (true, true) => bail!("Error: Multiple storage class specifiers."),
            (false, true) => {
                self.storage = storage;
                Ok(true)
            }
            (_, false) => Ok(false),
        }
    }

    fn into_type(self) -> Result<(usize, Type, Option<StorageClass>)> {
        if let Some(base) = self.base {
            let alignment = self.alignment.unwrap_or(base.default_alignment());
            if !alignment.is_power_of_two() {
                bail!("Alignment must be a power of 2");
            }

            Ok((
                self.stream_offset,
                Type {
                    base,
                    alignment,
                    is_const: self.is_const,
                },
                self.storage,
            ))
        } else {
            bail!("Type builder has not parsed a base type yet");
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum AbstractDeclarator {
    Pointer {
        decl: Option<Box<Self>>,
        is_const: bool,
    },
}

impl AbstractDeclarator {
    fn consume(tokens: &[Token]) -> Result<(Self, &[Token])> {
        match tokens {
            [Token::LParen, tokens @ ..] => {
                let (decl, tokens) =
                    Self::consume(tokens).context("Failed to parse abstract declarator.")?;
                ensure!(
                    tokens.first().is_some_and(|t| *t == Token::RParen),
                    "Expected closing \")\" in abstract declarator."
                );
                Ok((decl, &tokens[1..]))
            }
            [Token::Star, tokens @ ..] => {
                let (is_const, is_restrict, tokens) = consume_pointer_modifiers(tokens);
                ensure!(
                    !is_restrict,
                    "Cannot use \"restrict\" keyword in abstract declarator."
                );
                if let Ok((decl, tokens)) = Self::consume(tokens) {
                    Ok((
                        Self::Pointer {
                            decl: Some(Box::new(decl)),
                            is_const,
                        },
                        tokens,
                    ))
                } else {
                    Ok((
                        Self::Pointer {
                            decl: None,
                            is_const,
                        },
                        tokens,
                    ))
                }
            }
            _ => bail!("Failed to parse abstract declarator"),
        }
    }

    fn process(declarator: Self, base: Type) -> Result<Type> {
        match declarator {
            Self::Pointer { decl, is_const } => {
                let derived_type = Type {
                    base: BaseType::Ptr {
                        to: Box::new(base),
                        is_restrict: false,
                    },
                    alignment: NonZeroUsize::new(core::mem::size_of::<usize>()).expect(
                        "ast.Declarator.process(): Pointers always have word-sized alignment.",
                    ),
                    is_const,
                };
                if let Some(decl) = decl {
                    Self::process(*decl, derived_type)
                } else {
                    Ok(derived_type)
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Declarator {
    Ident(Option<Rc<String>>),
    Pointer {
        decl: Box<Self>,
        is_restrict: bool,
        is_const: bool,
    },
    Fun {
        decl: Box<Self>,
        params: RawParameterList,
    },
}

fn consume_pointer_modifiers(tokens: &[Token]) -> (bool, bool, &[Token]) {
    let mut remaining = tokens;
    let mut is_restrict = false;
    let mut is_const = false;
    while match remaining.first() {
        Some(Token::Restrict) => {
            is_restrict = true;
            true
        }
        Some(Token::Const) => {
            is_const = true;
            true
        }
        _ => false,
    } {
        remaining = &remaining[1..];
    }
    (is_const, is_restrict, remaining)
}

impl Declarator {
    fn consume(tokens: &[Token]) -> Result<(Self, &[Token])> {
        let (mut decl, mut tokens) = match tokens {
            [Token::Ident(s), tokens @ ..] => Ok::<(Declarator, &[Token]), anyhow::Error>((
                Self::Ident(Some(Rc::clone(s))),
                tokens,
            )),
            [Token::LParen, tokens @ ..] => {
                let (decl, tokens) = Self::consume(tokens)?;
                ensure!(
                    tokens.first().is_some_and(|t| *t == Token::RParen),
                    "Expected closing parentheses in declarator."
                );
                Ok((decl, &tokens[1..]))
            }
            [Token::Star, tokens @ ..] => {
                let (is_const, is_restrict, remaining) = consume_pointer_modifiers(tokens);
                let (decl, tokens) = Self::consume(remaining)?;
                Ok((
                    Self::Pointer {
                        decl: Box::new(decl),
                        is_restrict,
                        is_const,
                    },
                    tokens,
                ))
            }
            _ => bail!("ast.Declarator.consume(): Error parsing declarator."),
        }?;

        while let Some(t) = tokens.first() {
            match t {
                // Disallow returning a function
                Token::LParen if !matches!(decl, Self::Fun { .. }) => {
                    let (params, left) = RawParameterList::consume(tokens)?;
                    decl = Self::Fun {
                        decl: Box::new(decl),
                        params,
                    };
                    tokens = left;
                }
                Token::LBracket => {
                    unimplemented!("Implement this when we get to arrays!");
                }
                _ => {
                    break;
                }
            }
        }
        Ok((decl, tokens))
    }
}

impl Declarator {
    fn name(&self) -> Option<Rc<String>> {
        match self {
            Declarator::Ident(name) => name.clone(),
            Declarator::Pointer { decl, .. } => decl.name(),
            Declarator::Fun { decl, .. } => decl.name(),
        }
    }

    #[allow(clippy::type_complexity)]
    fn process(
        declarator: Self,
        base: Type,
    ) -> Result<(Option<Rc<String>>, Type, Vec<Option<Rc<String>>>)> {
        match declarator {
            Declarator::Ident(name) => Ok((name, base, vec![])),
            Declarator::Pointer {
                decl,
                is_const,
                is_restrict,
            } => {
                let derived_type = Type {
                    base: BaseType::Ptr {
                        to: Box::new(base),
                        is_restrict,
                    },
                    alignment: NonZeroUsize::new(core::mem::size_of::<usize>()).expect(
                        "ast.Declarator.process(): Pointers always have word-sized alignment.",
                    ),
                    is_const,
                };
                Declarator::process(*decl, derived_type)
            }
            Declarator::Fun { decl, params } => {
                let mut param_types = vec![];
                let mut param_names = vec![];
                for (param_base_type, param_declarator) in params.0.into_iter() {
                    let (param_name, param_type, _) =
                        Self::process(param_declarator, param_base_type).context(
                            "ast.Declarator.process(): Error processing function signature.",
                        )?;
                    if matches!(param_type.base, BaseType::Fun { .. }) {
                        bail!(
                            "ast.Declarator.process(): Function pointers as parameters is not yet supported."
                        );
                    }
                    param_types.push(param_type);
                    param_names.push(param_name);
                }
                let name = decl.name();
                ensure!(
                    name.is_some(),
                    "ast.Declarator.process(): Function must have identifier in declarator."
                );
                let derived_type = Type {
                    base: BaseType::Fun {
                        ret_t: Box::new(base),
                        param_types,
                    },
                    alignment: NonZeroUsize::new(core::mem::size_of::<usize>()).expect(
                        "ast.Declarator.process(): Pointers always have word-sized alignment.",
                    ),
                    is_const: true,
                };
                Ok((name, derived_type, param_names))
            }
        }
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
            Self::Ptr { to, is_restrict } => {
                write!(f, "{to}")?;
                if *is_restrict {
                    write!(f, " restrict")?;
                }
                write!(f, "*")
            }
            Self::Array { element, size } => write!(f, "{element}[{size}]"),
            Self::Float(_) => write!(f, "float"),
            Self::Double(_) => write!(f, "double"),
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
        !matches!(self, Self::F32(_) | Self::F64(_))
    }

    pub fn fits_in<T>(&self) -> bool
    where
        T: TryFrom<i8>,
        T: TryFrom<i16>,
        T: TryFrom<i32>,
        T: TryFrom<i64>,
        T: TryFrom<u8>,
        T: TryFrom<u16>,
        T: TryFrom<u32>,
        T: TryFrom<u64>,
        T: NumCast,
    {
        match &self {
            Constant::I8(v) => <T as TryFrom<i8>>::try_from(*v).is_ok(),
            Constant::I16(v) => <T as TryFrom<i16>>::try_from(*v).is_ok(),
            Constant::I32(v) => <T as TryFrom<i32>>::try_from(*v).is_ok(),
            Constant::I64(v) => <T as TryFrom<i64>>::try_from(*v).is_ok(),
            Constant::U8(v) => <T as TryFrom<u8>>::try_from(*v).is_ok(),
            Constant::U16(v) => <T as TryFrom<u16>>::try_from(*v).is_ok(),
            Constant::U32(v) => <T as TryFrom<u32>>::try_from(*v).is_ok(),
            Constant::U64(v) => <T as TryFrom<u64>>::try_from(*v).is_ok(),
            Constant::F32(v) => num::cast::<f32, T>(*v).is_some(),
            Constant::F64(v) => num::cast::<f64, T>(*v).is_some(),
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

    pub fn const_from_type(r#type: &Type, value: u8) -> Result<Self> {
        match r#type {
            _ if *r#type == Constant::I8(value as i8).get_type() => Ok(Constant::I8(value as i8)),
            _ if *r#type == Constant::I16(value as i16).get_type() => {
                Ok(Constant::I16(value as i16))
            }
            _ if *r#type == Constant::I32(value as i32).get_type() => {
                Ok(Constant::I32(value as i32))
            }
            _ if *r#type == Constant::I64(value as i64).get_type() => {
                Ok(Constant::I64(value as i64))
            }
            _ if *r#type == Constant::U8(value).get_type() => Ok(Constant::U8(value)),
            _ if *r#type == Constant::U16(value as u16).get_type() => {
                Ok(Constant::U16(value as u16))
            }
            _ if *r#type == Constant::U32(value as u32).get_type() => {
                Ok(Constant::U32(value as u32))
            }
            _ if *r#type == Constant::U64(value as u64).get_type() => {
                Ok(Constant::U64(value as u64))
            }
            _ if *r#type == Constant::F32(value as f32).get_type() => {
                Ok(Constant::F32(value as f32))
            }
            _ if *r#type == Constant::F64(value as f64).get_type() => {
                Ok(Constant::F64(value as f64))
            }
            _ => bail!("Could not create a constant with type {type} and value {value}"),
        }
    }

    fn consume(tokens: &[Token]) -> Result<(Constant, &[Token])> {
        // FIXME: Is this how we would like to handle integer overflow?
        if let Some(token) = tokens.first() {
            match token {
                // NOTE: The text in these nodes does not include the negative
                // sign so we don't need to worry about absolute value sign
                Token::Constant { text, flag: None } => {
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
                Token::Constant { text, flag } => match flag {
                    // FIXME: More intelligent scheme to know what this value
                    // will end up as (e.g., float instead of double) to avoid
                    // the double roundoff error
                    Some(ConstantFlag::Float) => {
                        let val = text
                            .parse::<f64>()
                            .context("Unable to parse double from text: \"{text}\".")?;
                        Ok((Self::F64(val), &tokens[1..]))
                    }
                    Some(ConstantFlag::Unsigned) => {
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
                    Some(ConstantFlag::UnsignedLong) => {
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
                    Some(ConstantFlag::Long) => {
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
        matches!(*self, |Self::Equal| Self::NotEqual
            | Self::LessThan
            | Self::LessOrEqual
            | Self::GreaterThan
            | Self::GreaterOrEqual)
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
