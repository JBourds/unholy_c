use crate::ast;
use crate::sema;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub functions: Vec<Function>,
}

impl From<sema::SemaStage<sema::Final>> for Program {
    fn from(stage: sema::SemaStage<sema::Final>) -> Self {
        let valid_functions = stage
            .program
            .functions
            .into_iter()
            .map(Option::<Function>::from)
            .collect::<Vec<Option<Function>>>();

        let valid_function_definitions = valid_functions.into_iter().flatten().collect();
        Self {
            functions: valid_function_definitions,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: Rc<String>,
    pub params: Vec<Option<Rc<String>>>,
    pub instructions: Vec<Instruction>,
}

impl Function {
    fn make_temp_var(name: Rc<String>, counter: &'_ mut usize) -> impl FnMut() -> String + use<'_> {
        move || {
            let n = *counter;
            *counter += 1;
            format!("{name}.{n}")
        }
    }
}

impl From<ast::FunDecl> for Option<Function> {
    fn from(node: ast::FunDecl) -> Self {
        let ast::FunDecl {
            name,
            signature,
            block: Some(block),
            ..
        } = node
        else {
            return None;
        };
        let mut temp_var_counter = 0;
        let mut make_temp_var =
            Function::make_temp_var(Rc::new(name.to_string()), &mut temp_var_counter);
        let mut instructions = Instruction::parse_block_with(block, &mut make_temp_var);

        // Temporary fix suggested by the book for the case where a function
        // is supposed to return something but does not.
        instructions.push(Instruction::Return(Some(Val::Constant(0))));

        let params = signature
            .into_iter()
            .map(|x| x.1)
            .collect::<Vec<Option<Rc<String>>>>();

        Some(Function {
            name: Rc::new(name.to_string()),
            params,
            instructions,
        })
    }
}

#[derive(Debug, PartialEq)]
pub enum Instruction {
    Return(Option<Val>),
    Unary {
        op: UnaryOp,
        src: Val,
        dst: Val,
    },
    Binary {
        op: BinaryOp,
        src1: Val,
        src2: Val,
        dst: Val,
    },
    Copy {
        src: Val,
        dst: Val,
    },
    Jump(Rc<String>),
    JumpIfZero {
        condition: Val,
        target: Rc<String>,
    },
    JumpIfNotZero {
        condition: Val,
        target: Rc<String>,
    },
    Label(Rc<String>),
    FunCall {
        name: Rc<String>,
        args: Vec<Val>,
        dst: Val,
    },
}

impl Instruction {
    fn parse_decl_with(
        decl: ast::Declaration,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> Vec<Self> {
        match decl {
            ast::Declaration::VarDecl(decl) => Self::parse_var_decl_with(decl, make_temp_var),
            ast::Declaration::FunDecl(decl) => Self::parse_fun_decl_with(decl, make_temp_var),
        }
    }

    fn parse_fun_decl_with(
        decl: ast::FunDecl,
        _make_temp_var: &mut impl FnMut() -> String,
    ) -> Vec<Self> {
        if Option::<Function>::from(decl).is_some() {
            unreachable!("Function declerations inside statements should be caught in sema");
        }
        vec![]
    }

    fn parse_var_decl_with(
        decl: ast::VarDecl,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> Vec<Self> {
        if let Some(init) = decl.init {
            let Expr {
                mut instructions,
                val: src,
            } = Expr::parse_with(init, make_temp_var);
            let dst = Val::Var(Rc::clone(&decl.name));
            instructions.push(Instruction::Copy {
                src,
                dst: dst.clone(),
            });
            instructions
        } else {
            vec![]
        }
    }
    fn parse_stmt_with(stmt: ast::Stmt, make_temp_var: &mut impl FnMut() -> String) -> Vec<Self> {
        let mut block_instructions = vec![];
        match stmt {
            ast::Stmt::Null => {}
            ast::Stmt::Return(Some(expr)) => {
                let Expr {
                    mut instructions,
                    val,
                } = Expr::parse_with(expr, make_temp_var);
                instructions.push(Instruction::Return(Some(val)));
                block_instructions.extend(instructions);
            }
            ast::Stmt::Return(None) => {
                block_instructions.push(Instruction::Return(None));
            }
            ast::Stmt::Expr(expr) => {
                let Expr { instructions, .. } = Expr::parse_with(expr, make_temp_var);
                block_instructions.extend(instructions);
            }
            ast::Stmt::Compound(block) => {
                block_instructions.extend(Self::parse_block_with(block, make_temp_var));
            }
            ast::Stmt::Goto(label) => {
                block_instructions.push(Instruction::Jump(label));
            }
            ast::Stmt::Label { name, stmt } => {
                block_instructions.push(Instruction::Label(name));
                block_instructions.extend(Self::parse_stmt_with(*stmt, make_temp_var));
            }
            ast::Stmt::Break(label) => {
                let label = Rc::new(format!("{}.break", label.unwrap()));
                block_instructions.push(Instruction::Jump(label));
            }
            ast::Stmt::Continue(label) => {
                let label = Rc::new(format!("{}.continue", label.unwrap()));
                block_instructions.push(Instruction::Jump(label));
            }
            ast::Stmt::While {
                condition,
                body,
                label,
            } => {
                let label = label.unwrap();
                let label_continue = Rc::new(format!("{label}.continue"));
                let label_break = Rc::new(format!("{label}.break"));

                // Label(continue_label)
                block_instructions.push(Instruction::Label(Rc::clone(&label_continue)));
                // <instructions for condition>
                // v = <result of condition>
                let Expr { instructions, val } = Expr::parse_with(condition, make_temp_var);
                block_instructions.extend(instructions);
                // JumpIfZero(v, break_label)
                block_instructions.push(Self::JumpIfZero {
                    condition: val,
                    target: Rc::clone(&label_break),
                });
                // <instructions for body>
                block_instructions.extend(Instruction::parse_stmt_with(*body, make_temp_var));
                // Jump(continue_label)
                block_instructions.push(Instruction::Jump(label_continue));
                // Label(break_label)
                block_instructions.push(Instruction::Label(label_break));
            }
            ast::Stmt::DoWhile {
                body,
                condition,
                label,
            } => {
                let label = label.unwrap();
                let label_start = Rc::new(format!("{label}.start"));
                let label_continue = Rc::new(format!("{label}.continue"));
                let label_break = Rc::new(format!("{label}.break"));

                // Label(start)
                block_instructions.push(Instruction::Label(Rc::clone(&label_start)));
                // <instructions for body>
                block_instructions.extend(Instruction::parse_stmt_with(*body, make_temp_var));
                // Label(continue_label)
                block_instructions.push(Instruction::Label(label_continue));
                // <instructions for condition>
                // v = <result of condition>
                let Expr { instructions, val } = Expr::parse_with(condition, make_temp_var);
                block_instructions.extend(instructions);
                // JumpIfNotZero(v, start)
                block_instructions.push(Instruction::JumpIfNotZero {
                    condition: val,
                    target: label_start,
                });
                // Label(break_label)
                block_instructions.push(Instruction::Label(label_break));
            }
            ast::Stmt::For {
                init,
                condition,
                post,
                body,
                label,
            } => {
                let label = label.unwrap();
                let label_start = Rc::new(format!("{label}.start"));
                let label_continue = Rc::new(format!("{label}.continue"));
                let label_break = Rc::new(format!("{label}.break"));

                // <instructions for init>
                match init {
                    ast::ForInit::Decl(decl) => {
                        block_instructions
                            .extend(Instruction::parse_var_decl_with(decl, make_temp_var));
                    }
                    ast::ForInit::Expr(Some(expr)) => {
                        let Expr { instructions, .. } = Expr::parse_with(expr, make_temp_var);
                        block_instructions.extend(instructions);
                    }
                    ast::ForInit::Expr(None) => {}
                }
                // Label(start)
                block_instructions.push(Instruction::Label(Rc::clone(&label_start)));
                // <instructions for condition>
                // v = <result of condition>
                if let Some(cond) = condition {
                    let Expr {
                        instructions: instructions_condition,
                        val: val_condition,
                    } = Expr::parse_with(cond, make_temp_var);
                    block_instructions.extend(instructions_condition);
                    // JumpIfZero(v, break_label)
                    block_instructions.push(Instruction::JumpIfZero {
                        condition: val_condition,
                        target: Rc::clone(&label_break),
                    });
                }
                // <instructions for body>
                block_instructions.extend(Instruction::parse_stmt_with(*body, make_temp_var));
                // Label(continue_label)
                block_instructions.push(Instruction::Label(label_continue));
                // <instructions for post>
                if let Some(post) = post {
                    let Expr {
                        instructions: instructions_post,
                        ..
                    } = Expr::parse_with(post, make_temp_var);
                    block_instructions.extend(instructions_post);
                }
                // Jump(Start)
                block_instructions.push(Instruction::Jump(label_start));
                // Label(break_label)
                block_instructions.push(Instruction::Label(label_break));
            }
            ast::Stmt::If {
                condition,
                then,
                r#else,
            } => {
                let (else_label, end_label) = {
                    let label = make_temp_var();
                    // This isn't needed and can be simplified... To Bad!
                    let Some((name, count)) = label.as_str().split_once('.') else {
                        unreachable!("label should always be name.count");
                    };
                    let else_label = format!("{name}.{count}.if_else");
                    let end_label = format!("{name}.{count}.if_end");
                    (Rc::new(else_label), Rc::new(end_label))
                };
                let Expr {
                    mut instructions,
                    val,
                } = Expr::parse_with(condition, make_temp_var);

                instructions.push(Self::JumpIfZero {
                    condition: val,
                    target: Rc::clone(match r#else {
                        Some(_) => &else_label,
                        None => &end_label,
                    }),
                });

                instructions.extend(Instruction::parse_block_with(
                    ast::Block(vec![ast::BlockItem::Stmt(*then)]),
                    make_temp_var,
                ));

                if let Some(r#else) = r#else {
                    instructions.push(Instruction::Jump(Rc::clone(&end_label)));
                    instructions.push(Instruction::Label(Rc::clone(&else_label)));
                    instructions.extend(Instruction::parse_block_with(
                        ast::Block(vec![ast::BlockItem::Stmt(*r#else)]),
                        make_temp_var,
                    ));
                }

                instructions.push(Instruction::Label(Rc::clone(&end_label)));
                block_instructions.extend(instructions);
            }
            ast::Stmt::Case {
                value: _,
                stmt,
                label,
            } => {
                let label = label.expect("Case must have label");
                block_instructions.push(Instruction::Label(Rc::clone(&label)));
                block_instructions.extend(Self::parse_stmt_with(*stmt, make_temp_var));
            }
            ast::Stmt::Default { label, stmt } => {
                let label = label.expect("Default must have label");
                block_instructions.push(Instruction::Label(Rc::clone(&label)));
                block_instructions.extend(Self::parse_stmt_with(*stmt, make_temp_var));
            }
            ast::Stmt::Switch {
                condition,
                body,
                label,
                cases,
                default,
            } => {
                let label = label.expect("Switch statement must be labelled.");
                let cases = cases.expect("Cases must have been populated.");
                let break_label = Rc::new(format!("{label}.break"));
                // Cases:
                // 1. There are no cases: Still evaluate the condition and make
                //  instructions in case a goto jumps to a label in them.
                // 2. The switch value is a literal (or has been const-evaled):
                //  Perform a compile time comparison to all the cases and make
                //  a single jump to the right location.
                // 3. Perform a linear comparison for each case and jump if the
                //  value matches.
                if cases.is_empty() {
                    let Expr { instructions, .. } = Expr::parse_with(condition, make_temp_var);
                    block_instructions.extend(instructions);
                    block_instructions.push(Instruction::Jump(
                        default.unwrap_or(Rc::clone(&break_label)),
                    ));
                    block_instructions.extend(Self::parse_stmt_with(*body, make_temp_var));
                } else if let ast::Expr::Literal(cond) = condition {
                    let jump_label = cases
                        .iter()
                        .find(|&&(case, _)| case == cond)
                        .map_or(default, |(_, label)| Some(Rc::clone(label)));
                    let has_jump_label = jump_label.is_some();
                    let jump_label = jump_label.unwrap_or(break_label.clone());
                    block_instructions.push(Instruction::Jump(jump_label.clone()));
                    block_instructions.extend(Self::parse_stmt_with(*body, make_temp_var));
                    if !has_jump_label {
                        block_instructions.push(Instruction::Label(jump_label));
                    }
                } else {
                    let Expr {
                        instructions,
                        val: switch_val,
                    } = Expr::parse_with(condition, make_temp_var);
                    block_instructions.extend(instructions);
                    for (case, label) in cases.iter() {
                        let Expr {
                            instructions,
                            val: case_val,
                        } = Expr::parse_with(ast::Expr::Literal(*case), make_temp_var);
                        block_instructions.extend(instructions);
                        let dst = Val::Var(Rc::new(make_temp_var()));
                        block_instructions.push(Self::Binary {
                            op: BinaryOp::Equal,
                            src1: switch_val.clone(),
                            src2: case_val,
                            dst: dst.clone(),
                        });
                        block_instructions.push(Self::JumpIfNotZero {
                            condition: dst,
                            target: Rc::clone(label),
                        });
                    }
                    block_instructions.push(Instruction::Jump(
                        default.unwrap_or(Rc::clone(&break_label)),
                    ));
                    block_instructions.extend(Self::parse_stmt_with(*body, make_temp_var));
                }
                // Break label always goes after all instructions
                block_instructions.push(Instruction::Label(break_label));
            }
        }
        block_instructions
    }
    fn parse_block_with(node: ast::Block, make_temp_var: &mut impl FnMut() -> String) -> Vec<Self> {
        let mut block_instructions = vec![];
        for item in node.into_items().into_iter() {
            match item {
                ast::BlockItem::Decl(decl) => {
                    block_instructions.extend(Self::parse_decl_with(decl, make_temp_var));
                }
                ast::BlockItem::Stmt(stmt) => {
                    block_instructions.extend(Self::parse_stmt_with(stmt, make_temp_var));
                }
            }
        }
        block_instructions
    }
}

#[derive(Debug, PartialEq)]
pub struct Expr {
    instructions: Vec<Instruction>,
    val: Val,
}

impl Expr {
    fn parse_with(node: ast::Expr, make_temp_var: &mut impl FnMut() -> String) -> Expr {
        match node {
            ast::Expr::Literal(v) => Self {
                instructions: vec![],
                val: Val::from(v),
            },
            ast::Expr::Unary { op, expr } => {
                let Self {
                    mut instructions,
                    val,
                } = Expr::parse_with(*expr, make_temp_var);
                let dst = match op {
                    ast::UnaryOp::PreInc => {
                        instructions.push(Instruction::Binary {
                            op: BinaryOp::Add,
                            src1: val.clone(),
                            src2: Val::Constant(1),
                            dst: val.clone(),
                        });
                        val.clone()
                    }
                    ast::UnaryOp::PostInc => {
                        let dst = Val::Var(make_temp_var().into());
                        instructions.push(Instruction::Copy {
                            src: val.clone(),
                            dst: dst.clone(),
                        });
                        instructions.push(Instruction::Binary {
                            op: BinaryOp::Add,
                            src1: val.clone(),
                            src2: Val::Constant(1),
                            dst: val.clone(),
                        });
                        dst
                    }
                    ast::UnaryOp::PreDec => {
                        instructions.push(Instruction::Binary {
                            op: BinaryOp::Subtract,
                            src1: val.clone(),
                            src2: Val::Constant(1),
                            dst: val.clone(),
                        });
                        val.clone()
                    }
                    ast::UnaryOp::PostDec => {
                        let dst = Val::Var(make_temp_var().into());
                        instructions.push(Instruction::Copy {
                            src: val.clone(),
                            dst: dst.clone(),
                        });
                        instructions.push(Instruction::Binary {
                            op: BinaryOp::Subtract,
                            src1: val.clone(),
                            src2: Val::Constant(1),
                            dst: val.clone(),
                        });
                        dst
                    }
                    // Other operations have tacky unary op equivalents
                    _ => {
                        let dst = Val::Var(make_temp_var().into());
                        instructions.push(Instruction::Unary {
                            op: UnaryOp::from(op),
                            src: val,
                            dst: dst.clone(),
                        });
                        dst
                    }
                };
                Expr {
                    instructions,
                    val: dst,
                }
            }
            ast::Expr::Binary { op, left, right } => {
                if op.is_logical() {
                    // <instructions for e1>
                    // v1 = <result of e1>
                    let Self {
                        mut instructions,
                        val: left_val,
                    } = Self::parse_with(*left, make_temp_var);

                    let label = {
                        let mut label = make_temp_var();
                        // FIXME: make_temp_var() should support making label names
                        label.push_str(match op {
                            ast::BinaryOp::And => ".binary_false_label",
                            ast::BinaryOp::Or => ".binary_true_label",
                            _ => unreachable!(),
                        });
                        label.into()
                    };

                    let make_jmp_instruction = |val, target| match op {
                        ast::BinaryOp::And => Instruction::JumpIfZero {
                            condition: val,
                            target,
                        },
                        ast::BinaryOp::Or => Instruction::JumpIfNotZero {
                            condition: val,
                            target,
                        },
                        _ => unreachable!(),
                    };

                    // JumpIfZero(v1, label)
                    instructions.push(make_jmp_instruction(left_val.clone(), Rc::clone(&label)));

                    // <instructions for e2>
                    // v2 = <result of e2>
                    let Self {
                        instructions: right_instructions,
                        val: right_val,
                    } = Self::parse_with(*right, make_temp_var);
                    instructions.extend(right_instructions);

                    let dst = Val::Var(make_temp_var().into());
                    let end = {
                        // FIXME: Support label use case
                        let mut end = make_temp_var();
                        end.push_str(".binary_end_label");
                        end.into()
                    };

                    let (result_nojmp, result_jmp) = match op {
                        ast::BinaryOp::And => (1, 0),
                        ast::BinaryOp::Or => (0, 1),
                        _ => unreachable!(),
                    };

                    // JumpIfZero(v2, false_label)
                    instructions.push(make_jmp_instruction(right_val.clone(), Rc::clone(&label)));

                    // result = 1
                    instructions.push(Instruction::Copy {
                        src: Val::Constant(result_nojmp),
                        dst: dst.clone(),
                    });

                    // Jump(end)
                    instructions.push(Instruction::Jump(Rc::clone(&end)));

                    // Label(false_label)
                    instructions.push(Instruction::Label(Rc::clone(&label)));

                    // result = 0
                    instructions.push(Instruction::Copy {
                        src: Val::Constant(result_jmp),
                        dst: dst.clone(),
                    });

                    // Label(end)
                    instructions.push(Instruction::Label(end));

                    Self {
                        instructions,
                        val: dst,
                    }
                } else if let Some(op) = op.compound_op() {
                    if let ast::Expr::Var(dst) = left.as_ref() {
                        let binary = ast::Expr::Binary {
                            op,
                            left: left.clone(),
                            right: right.clone(),
                        };
                        let Self {
                            mut instructions,
                            val: src,
                        } = Self::parse_with(binary, make_temp_var);

                        instructions.push(Instruction::Copy {
                            src,
                            dst: Val::Var(Rc::clone(dst)),
                        });
                        Self {
                            instructions,
                            val: Val::Var(Rc::clone(dst)),
                        }
                    } else {
                        panic!("Cannot use compound assignment on non-variable value.")
                    }
                } else {
                    let Self {
                        mut instructions,
                        val: left_val,
                    } = Self::parse_with(*left, make_temp_var);
                    let Self {
                        instructions: right_instructions,
                        val: right_val,
                    } = Self::parse_with(*right, make_temp_var);
                    instructions.extend(right_instructions);

                    let dst = Val::Var(make_temp_var().into());

                    instructions.push(Instruction::Binary {
                        op: op.into(),
                        src1: left_val,
                        src2: right_val,
                        dst: dst.clone(),
                    });
                    Self {
                        instructions,
                        val: dst,
                    }
                }
            }
            ast::Expr::Var(name) => Self {
                instructions: vec![],
                val: Val::Var(name),
            },
            ast::Expr::Assignment { lvalue, rvalue } => {
                if let ast::Expr::Var(name) = lvalue.as_ref() {
                    let Self {
                        mut instructions,
                        val: src,
                    } = Self::parse_with(*rvalue, make_temp_var);
                    let dst = Val::Var(Rc::clone(name));
                    instructions.push(Instruction::Copy {
                        src,
                        dst: dst.clone(),
                    });
                    Self {
                        instructions,
                        val: dst,
                    }
                } else {
                    panic!("Error: Cannot assign to rvalue.")
                }
            }
            ast::Expr::Conditional {
                condition,
                then,
                r#else,
            } => {
                let (result, e2_label, end_label) = {
                    let label = make_temp_var();
                    // This isn't needed and can be simplified... To Bad!
                    let Some((name, count)) = label.as_str().split_once('.') else {
                        unreachable!("label should always be name.count");
                    };
                    let e2_label = format!("{name}.{count}.cond_e2");
                    let end_label = format!("{name}.{count}.cond_end");
                    (Rc::new(label), Rc::new(e2_label), Rc::new(end_label))
                };
                let Expr {
                    mut instructions,
                    val,
                } = Expr::parse_with(*condition, make_temp_var);

                instructions.push(Instruction::JumpIfZero {
                    condition: val,
                    target: Rc::clone(&e2_label),
                });

                let Expr {
                    instructions: e1_instructions,
                    val: e1_val,
                } = Expr::parse_with(*then, make_temp_var);

                instructions.extend(e1_instructions);
                instructions.push(Instruction::Copy {
                    src: e1_val,
                    dst: Val::Var(Rc::clone(&result)),
                });

                instructions.push(Instruction::Jump(Rc::clone(&end_label)));
                instructions.push(Instruction::Label(Rc::clone(&e2_label)));

                let Expr {
                    instructions: e2_instructions,
                    val: e2_val,
                } = Expr::parse_with(*r#else, make_temp_var);

                instructions.extend(e2_instructions);

                instructions.push(Instruction::Copy {
                    src: e2_val,
                    dst: Val::Var(Rc::clone(&result)),
                });

                instructions.push(Instruction::Label(end_label));

                Self {
                    instructions,
                    val: Val::Var(Rc::clone(&result)),
                }
            }
            ast::Expr::FunCall { name, args } => {
                let label = Rc::new(make_temp_var());
                let (mut instructions, args) =
                    args.into_iter()
                        .fold((vec![], vec![]), |(mut instrs, mut args), arg| {
                            let Expr { instructions, val } = Expr::parse_with(arg, make_temp_var);
                            instrs.extend(instructions);
                            args.push(val);
                            (instrs, args)
                        });
                let dst = Val::Var(label);
                instructions.push(Instruction::FunCall {
                    name,
                    args,
                    dst: dst.clone(),
                });
                Self {
                    instructions,
                    val: dst,
                }
            }
        }
    }
}

// TODO: Other types
#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    Constant(i32),
    Var(Rc<String>),
}
impl From<ast::Literal> for Val {
    fn from(node: ast::Literal) -> Self {
        match node {
            ast::Literal::Int(i) => Self::Constant(i),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Negate,
    Complement,
    Not,
}

impl From<ast::UnaryOp> for UnaryOp {
    fn from(node: ast::UnaryOp) -> Self {
        match node {
            ast::UnaryOp::Complement => Self::Complement,
            ast::UnaryOp::Negate => Self::Negate,
            ast::UnaryOp::Not => Self::Not,
            _ => unreachable!(),
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
}

impl From<ast::BinaryOp> for BinaryOp {
    fn from(node: ast::BinaryOp) -> Self {
        match node {
            ast::BinaryOp::Add => Self::Add,
            ast::BinaryOp::Subtract => Self::Subtract,
            ast::BinaryOp::Multiply => Self::Multiply,
            ast::BinaryOp::Divide => Self::Divide,
            ast::BinaryOp::Remainder => Self::Remainder,
            ast::BinaryOp::BitAnd => Self::BitAnd,
            ast::BinaryOp::BitOr => Self::BitOr,
            ast::BinaryOp::Xor => Self::Xor,
            ast::BinaryOp::LShift => Self::LShift,
            ast::BinaryOp::RShift => Self::RShift,
            ast::BinaryOp::And => unreachable!("Logical operands are not Binary operands"),
            ast::BinaryOp::Or => unreachable!("Logical operands are not Binary operands"),
            ast::BinaryOp::Equal => Self::Equal,
            ast::BinaryOp::NotEqual => Self::NotEqual,
            ast::BinaryOp::LessThan => Self::LessThan,
            ast::BinaryOp::LessOrEqual => Self::LessOrEqual,
            ast::BinaryOp::GreaterThan => Self::GreaterThan,
            ast::BinaryOp::GreaterOrEqual => Self::GreaterOrEqual,
            ast::BinaryOp::Assign => Self::Assign,
            ast::BinaryOp::AddAssign => Self::AddAssign,
            ast::BinaryOp::SubAssign => Self::SubAssign,
            ast::BinaryOp::MultAssign => Self::MultAssign,
            ast::BinaryOp::DivAssign => Self::DivAssign,
            ast::BinaryOp::ModAssign => Self::ModAssign,
            ast::BinaryOp::AndAssign => Self::AndAssign,
            ast::BinaryOp::OrAssign => Self::OrAssign,
            ast::BinaryOp::XorAssign => Self::XorAssign,
            ast::BinaryOp::LShiftAssign => Self::LShiftAssign,
            ast::BinaryOp::RShiftAssign => Self::RShiftAssign,
            ast::BinaryOp::Ternary => {
                unreachable!("Ternary expressions are not true Binary operands")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_return_literal() {
        let ast = ast::Block(vec![ast::BlockItem::Stmt(ast::Stmt::Return(Some(
            ast::Expr::Literal(ast::Literal::Int(2)),
        )))]);
        let mut counter = 0;
        let mut make_temp_var = Function::make_temp_var(Rc::new("test".to_string()), &mut counter);
        let actual = Instruction::parse_block_with(ast, &mut make_temp_var);
        let expected = vec![Instruction::Return(Some(Val::Constant(2)))];
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_return_unary() {
        let ast = ast::Block(vec![ast::BlockItem::Stmt(ast::Stmt::Return(Some(
            ast::Expr::Unary {
                op: ast::UnaryOp::Complement,
                expr: Box::new(ast::Expr::Literal(ast::Literal::Int(2))),
            },
        )))]);
        let mut counter = 0;
        let mut make_temp_var = Function::make_temp_var(Rc::new("test".to_string()), &mut counter);
        let actual = Instruction::parse_block_with(ast, &mut make_temp_var);
        let expected = vec![
            Instruction::Unary {
                op: UnaryOp::Complement,
                src: Val::Constant(2),
                dst: Val::Var("test.0".to_string().into()),
            },
            Instruction::Return(Some(Val::Var("test.0".to_string().into()))),
        ];
        assert_eq!(actual, expected);
    }
    #[test]
    fn test_return_nested_unary() {
        let ast = ast::Block(vec![ast::BlockItem::Stmt(ast::Stmt::Return(Some(
            ast::Expr::Unary {
                op: ast::UnaryOp::Negate,
                expr: Box::new(ast::Expr::Unary {
                    op: ast::UnaryOp::Complement,
                    expr: Box::new(ast::Expr::Unary {
                        op: ast::UnaryOp::Negate,
                        expr: Box::new(ast::Expr::Literal(ast::Literal::Int(2))),
                    }),
                }),
            },
        )))]);
        let mut counter = 0;
        let mut make_temp_var = Function::make_temp_var(Rc::new("test".to_string()), &mut counter);
        let actual = Instruction::parse_block_with(ast, &mut make_temp_var);
        let expected = vec![
            Instruction::Unary {
                op: UnaryOp::Negate,
                src: Val::Constant(2),
                dst: Val::Var("test.0".to_string().into()),
            },
            Instruction::Unary {
                op: UnaryOp::Complement,
                src: Val::Var("test.0".to_string().into()),
                dst: Val::Var("test.1".to_string().into()),
            },
            Instruction::Unary {
                op: UnaryOp::Negate,
                src: Val::Var("test.1".to_string().into()),
                dst: Val::Var("test.2".to_string().into()),
            },
            Instruction::Return(Some(Val::Var("test.2".to_string().into()))),
        ];
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_binary_expr() {
        let ast_binary_expr = ast::Expr::Binary {
            op: ast::BinaryOp::Subtract,
            left: Box::new(ast::Expr::Binary {
                op: ast::BinaryOp::Multiply,
                left: Box::new(ast::Expr::Literal(ast::Literal::Int(1))),
                right: Box::new(ast::Expr::Literal(ast::Literal::Int(2))),
            }),
            right: Box::new(ast::Expr::Binary {
                op: ast::BinaryOp::Multiply,
                left: Box::new(ast::Expr::Literal(ast::Literal::Int(3))),
                right: Box::new(ast::Expr::Binary {
                    op: ast::BinaryOp::Add,
                    left: Box::new(ast::Expr::Literal(ast::Literal::Int(4))),
                    right: Box::new(ast::Expr::Literal(ast::Literal::Int(5))),
                }),
            }),
        };
        let mut counter = 0;
        let mut make_temp_var = Function::make_temp_var(Rc::new("test".to_string()), &mut counter);
        let tacky_expr = Expr::parse_with(ast_binary_expr, &mut make_temp_var);
        let expected = Expr {
            instructions: vec![
                Instruction::Binary {
                    op: BinaryOp::Multiply,
                    src1: Val::Constant(1),
                    src2: Val::Constant(2),
                    dst: Val::Var(Rc::new("test.0".to_string())),
                },
                Instruction::Binary {
                    op: BinaryOp::Add,
                    src1: Val::Constant(4),
                    src2: Val::Constant(5),
                    dst: Val::Var(Rc::new("test.1".to_string())),
                },
                Instruction::Binary {
                    op: BinaryOp::Multiply,
                    src1: Val::Constant(3),
                    src2: Val::Var(Rc::new("test.1".to_string())),
                    dst: Val::Var(Rc::new("test.2".to_string())),
                },
                Instruction::Binary {
                    op: BinaryOp::Subtract,
                    src1: Val::Var(Rc::new("test.0".to_string())),
                    src2: Val::Var(Rc::new("test.2".to_string())),
                    dst: Val::Var(Rc::new("test.3".to_string())),
                },
            ],
            val: Val::Var(Rc::new("test.3".to_string())),
        };
        assert_eq!(expected, tacky_expr);
    }
}
