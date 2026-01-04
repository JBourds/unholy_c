use anyhow::{Context, Result, bail, ensure};
use std::num::NonZeroUsize;
use std::rc::Rc;

use super::{BaseType, Constant, RawParameterList, Type};
use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum AbstractDeclarator {
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
    pub fn consume(tokens: &[Token]) -> Result<(Self, &[Token])> {
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

    pub fn process(declarator: Self, base: Type) -> Result<Type> {
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
pub enum Declarator {
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
    pub fn consume(tokens: &[Token]) -> Result<(Self, &[Token])> {
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
    pub fn process(
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
