pub mod declaration;
pub mod statements;
pub mod types;

use crate::lexer::{ConstantFlag, Token};

use anyhow::{Context, Result, bail, ensure};
use num::{NumCast, bigint::BigUint};
use std::str::FromStr;
use std::{num::NonZeroUsize, rc::Rc};

// Re-export these types for convenience
pub use declaration::{Declaration, FunDecl, VarDecl};
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
    pub fn is_modifiable_lvalue(&self, t: &Type) -> bool {
        let disallowed = [Type::is_array, Type::is_function];
        self.is_lvalue() && !disallowed.iter().any(|f| f(t))
    }

    pub fn is_lvalue(&self) -> bool {
        matches!(
            self,
            Self::Var(_)
                | Self::Unary {
                    op: UnaryOp::Deref,
                    ..
                }
                | Self::Subscript { .. }
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
    fn check_for_postfix(expr: Expr, tokens: &[Token]) -> Result<(Expr, &[Token])> {
        match tokens {
            [Token::LBracket, tokens @ ..] => {
                let (rhs, tokens) = Expr::parse(tokens, 0)?;
                ensure!(
                    tokens.first() == Some(&Token::RBracket),
                    "ast.Factor.check_for_postfix(): subscript expression missing closing bracket"
                );
                Self::check_for_postfix(
                    Expr::Subscript {
                        expr: expr.into(),
                        index: rhs.into(),
                    },
                    &tokens[1..],
                )
            }
            _ => match UnaryOp::consume_postfix(tokens) {
                Ok((op, tokens)) => Self::check_for_postfix(
                    Expr::Unary {
                        op,
                        expr: Box::new(expr),
                    },
                    tokens,
                ),
                _ => Ok((expr, tokens)),
            },
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
                                let (expr, tokens) = Self::check_for_postfix(expr, tokens)?;
                                Self::check_for_call(expr, tokens)
                            }
                            _ => bail!("Could not find matching right parenthesis"),
                        }
                    }
                }
                _ => bail!("Could not match valid grammar rule."),
            }
            .and_then(|(expr, tokens)| Self::check_for_postfix(expr, tokens)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum AbstractDeclarator {
    Pointer {
        decl: Option<Box<Self>>,
        is_const: bool,
    },
    Array {
        decl: Option<Box<Self>>,
        size: usize,
    },
}

impl AbstractDeclarator {
    fn consume(tokens: &[Token]) -> Result<(Self, &[Token])> {
        match tokens {
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
            _ => {
                Self::consume_direct_abstract(tokens).context("Failed to parse abstract declarator")
            }
        }
    }

    fn consume_direct_abstract(tokens: &[Token]) -> Result<(Self, &[Token])> {
        match tokens {
            [Token::LParen, tokens @ ..] => {
                let (decl, tokens) =
                    Self::consume(tokens).context("Failed to parse abstract declarator.")?;
                ensure!(
                    tokens.first().is_some_and(|t| *t == Token::RParen),
                    "Expected closing \")\" in abstract declarator."
                );

                let (decl, tokens, _) = Self::consume_subscript(Some(decl), &tokens[1..])?;

                Ok((decl, tokens))
            }
            [Token::LBracket, ..] => {
                let (decl, tokens, consumed) = Self::consume_subscript(None, tokens)?;
                ensure!(
                    consumed,
                    "ast.AbstractDeclarator.consume_direct_abstract() requires atleast one subscript in the non-recursive case"
                );
                Ok((decl, tokens))
            }
            _ => bail!("Failed to parse abstract declarator"),
        }
    }

    fn consume_subscript(
        mut decl: Option<Self>,
        mut tokens: &[Token],
    ) -> Result<(Self, &[Token], bool)> {
        const EMPTY_DECL: AbstractDeclarator = AbstractDeclarator::Array {
            decl: None,
            size: 0,
        };
        let mut consumed = false;
        while let Some(t) = tokens.first() {
            match t {
                Token::LBracket => {
                    let (constant, left) = Constant::consume(&tokens[1..])?;
                    if left.first() != Some(&Token::RBracket) {
                        bail!(
                            "ast.AbstractDeclarator.consume_subscript(): Did not find matching right bracket when parsing direct declarator"
                        );
                    }
                    consumed = true;
                    tokens = &left[1..];
                    decl = Some(Self::Array {
                        decl: decl.map(Box::new),
                        size: constant.as_array_size()?,
                    });
                }
                _ => break,
            }
        }
        Ok((decl.unwrap_or(EMPTY_DECL), tokens, consumed))
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
            Self::Array { decl, size } => {
                let derived_type = Type {
                    alignment: base.alignment,
                    base: BaseType::Array {
                        element: Box::new(base),
                        size,
                    },
                    is_const: false, // FIXME: Don't know whats supposed to go here
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
    Array {
        decl: Box<Self>,
        size: usize,
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
            // declarator
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
            _ => Self::consume_direct_declarator(tokens)
                .context("ast.Declarator.consume(): Error parsing declarator."),
        }?;

        while let Some(t) = tokens.first() {
            match t {
                // Disallow returning a function
                Token::LParen
                    if !matches!(decl, Self::Fun { .. }) && !matches!(decl, Self::Array { .. }) =>
                {
                    let (params, left) = RawParameterList::consume(tokens)?;
                    decl = Self::Fun {
                        decl: Box::new(decl),
                        params,
                    };
                    tokens = left;
                }
                _ => {
                    break;
                }
            }
        }
        Ok((decl, tokens))
    }

    fn consume_direct_declarator(tokens: &[Token]) -> Result<(Self, &[Token])> {
        // Simple declarator
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
            _ => bail!(
                "ast.Declarator.consume_direct_declarator(): Error parsing direct declarator."
            ),
        }?;

        // Optional declarator suffix
        while let Some(t) = tokens.first() {
            match t {
                // Disallow returning a function
                Token::LParen
                    if !matches!(decl, Self::Fun { .. }) && !matches!(decl, Self::Array { .. }) =>
                {
                    let (params, left) = RawParameterList::consume(tokens)?;
                    decl = Self::Fun {
                        decl: Box::new(decl),
                        params,
                    };
                    tokens = left;
                }
                Token::LBracket => {
                    let (constant, left) = Constant::consume(&tokens[1..])?;
                    if left.first() != Some(&Token::RBracket) {
                        bail!(
                            "ast.Declarator.consume_direct_declarator(): Did not find matching right bracket when parsing direct declarator"
                        );
                    }
                    tokens = &left[1..];
                    decl = Self::Array {
                        decl: Box::new(decl),
                        size: constant.as_array_size()?,
                    };
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
            Declarator::Array { decl, .. } => decl.name(),
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
            Declarator::Array { decl, size } => {
                ensure!(!base.is_function(), "Array of functions not allowed");
                let derived_type = Type {
                    alignment: base.alignment,
                    base: BaseType::Array {
                        element: Box::new(base),
                        size,
                    },
                    is_const: false, // FIXME: not sure whats supposed to go here
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

    pub fn as_array_size(&self) -> Result<usize> {
        match self {
            Constant::I8(v) => {
                usize::try_from(*v).context("failed to convert {v} to positive size")
            }
            Constant::I16(v) => {
                usize::try_from(*v).context("failed to convert {v} to positive size")
            }
            Constant::I32(v) => {
                usize::try_from(*v).context("failed to convert {v} to positive size")
            }
            Constant::I64(v) => {
                usize::try_from(*v).context("failed to convert {v} to positive size")
            }
            Constant::U8(v) => Ok(*v as usize),
            Constant::U16(v) => Ok(*v as usize),
            Constant::U32(v) => Ok(*v as usize),
            Constant::U64(v) => Ok(*v as usize),
            Constant::F32(..) => bail!("Floats cannot be used as constants in array declaration"),
            Constant::F64(..) => bail!("Floats cannot be used as constants in array declaration"),
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
