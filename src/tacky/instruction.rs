use super::*;

#[derive(Debug, PartialEq)]
pub enum Instruction {
    Return(Option<Val>),
    SignExtend {
        src: Val,
        dst: Val,
    },
    ZeroExtend {
        src: Val,
        dst: Val,
    },
    DoubleToInt {
        src: Val,
        dst: Val,
    },
    IntToDouble {
        src: Val,
        dst: Val,
    },
    DoubleToUInt {
        src: Val,
        dst: Val,
    },
    UIntToDouble {
        src: Val,
        dst: Val,
    },
    Truncate {
        src: Val,
        dst: Val,
    },
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
    GetAddress {
        src: Val,
        dst: Val,
    },
    Load {
        src_ptr: Val,
        dst: Val,
    },
    Store {
        src: Val,
        dst_ptr: Val,
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
    AddPtr {
        ptr: Val,
        index: Val,
        scale: usize,
        dst: Val,
    },
    CopyToOffset {
        src: Val,
        dst: Rc<String>,
        offset: isize,
    },
}

impl Instruction {
    pub(crate) fn parse_decl_with(
        decl: ast::Declaration,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> Vec<Self> {
        match decl {
            ast::Declaration::VarDecl(decl) => {
                Self::parse_var_decl_with(decl, symbols, make_temp_var)
            }
            ast::Declaration::FunDecl(decl) => Self::parse_fun_decl_with(decl, make_temp_var),
        }
    }

    pub(crate) fn parse_fun_decl_with(
        decl: ast::FunDecl,
        _make_temp_var: &mut impl FnMut() -> String,
    ) -> Vec<Self> {
        assert!(decl.block.is_none());
        vec![]
    }

    pub(crate) fn process_initializer_rec(
        base: usize,
        in_array: bool,
        name: Rc<String>,
        init: ast::Initializer,
        r#type: &ast::Type,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> Vec<Self> {
        match init {
            ast::Initializer::SingleInit(init) => {
                let Expr {
                    mut instructions,
                    val: src,
                } = Expr::parse_with_and_convert(*init, symbols, make_temp_var);
                if in_array {
                    instructions.push(Instruction::CopyToOffset {
                        src,
                        dst: Rc::clone(&name),
                        offset: base.try_into().unwrap(),
                    });
                } else {
                    let dst = Val::Var(name);
                    instructions.push(Instruction::Copy {
                        src,
                        dst: dst.clone(),
                    });
                }
                instructions
            }
            ast::Initializer::CompundInit(inits) => {
                let mut instructions = vec![];
                let per_element_size = r#type.base.size_of_base_type();
                let mut current_base = base;
                for init in inits {
                    instructions.extend(Self::process_initializer_rec(
                        current_base,
                        true,
                        name.clone(),
                        init,
                        r#type,
                        symbols,
                        make_temp_var,
                    ));
                    current_base += per_element_size;
                }
                instructions
            }
        }
    }

    pub(crate) fn process_initializer(
        name: Rc<String>,
        init: ast::Initializer,
        r#type: &ast::Type,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> Vec<Self> {
        Self::process_initializer_rec(0, false, name, init, r#type, symbols, make_temp_var)
    }

    pub(crate) fn parse_var_decl_with(
        decl: ast::VarDecl,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> Vec<Self> {
        if decl.storage_class != Some(ast::StorageClass::Extern) {
            symbols.new_entry(Rc::clone(&decl.name), decl.r#type.clone());
        }
        match decl.init {
            Some(init) => Self::process_initializer(
                Rc::clone(&decl.name),
                init,
                &decl.r#type,
                symbols,
                make_temp_var,
            ),
            _ => vec![],
        }
    }

    pub(crate) fn parse_stmt_with(
        stmt: ast::Stmt,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> Vec<Self> {
        let mut block_instructions = vec![];
        match stmt {
            ast::Stmt::Null => {}
            ast::Stmt::Return(Some(expr)) => {
                let Expr {
                    mut instructions,
                    val,
                } = Expr::parse_with_and_convert(expr, symbols, make_temp_var);
                instructions.push(Instruction::Return(Some(val)));
                block_instructions.extend(instructions);
            }
            ast::Stmt::Return(None) => {
                block_instructions.push(Instruction::Return(None));
            }
            ast::Stmt::Expr(expr) => {
                let Expr { instructions, .. } =
                    Expr::parse_with_and_convert(expr, symbols, make_temp_var);
                block_instructions.extend(instructions);
            }
            ast::Stmt::Compound(block) => {
                block_instructions.extend(Self::parse_block_with(block, symbols, make_temp_var));
            }
            ast::Stmt::Goto(label) => {
                block_instructions.push(Instruction::Jump(label));
            }
            ast::Stmt::Label { name, stmt } => {
                block_instructions.push(Instruction::Label(name));
                block_instructions.extend(Self::parse_stmt_with(*stmt, symbols, make_temp_var));
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
                let Expr { instructions, val } =
                    Expr::parse_with_and_convert(condition, symbols, make_temp_var);
                block_instructions.extend(instructions);
                // JumpIfZero(v, break_label)
                block_instructions.push(Self::JumpIfZero {
                    condition: val,
                    target: Rc::clone(&label_break),
                });
                // <instructions for body>
                block_instructions.extend(Instruction::parse_stmt_with(
                    *body,
                    symbols,
                    make_temp_var,
                ));
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
                block_instructions.extend(Instruction::parse_stmt_with(
                    *body,
                    symbols,
                    make_temp_var,
                ));
                // Label(continue_label)
                block_instructions.push(Instruction::Label(label_continue));
                // <instructions for condition>
                // v = <result of condition>
                let Expr { instructions, val } =
                    Expr::parse_with_and_convert(condition, symbols, make_temp_var);
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
                match *init {
                    ast::ForInit::Decl(decl) => {
                        block_instructions.extend(Instruction::parse_var_decl_with(
                            decl,
                            symbols,
                            make_temp_var,
                        ));
                    }
                    ast::ForInit::Expr(Some(expr)) => {
                        let Expr { instructions, .. } =
                            Expr::parse_with_and_convert(expr, symbols, make_temp_var);
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
                    } = Expr::parse_with_and_convert(cond, symbols, make_temp_var);
                    block_instructions.extend(instructions_condition);
                    // JumpIfZero(v, break_label)
                    block_instructions.push(Instruction::JumpIfZero {
                        condition: val_condition,
                        target: Rc::clone(&label_break),
                    });
                }
                // <instructions for body>
                block_instructions.extend(Instruction::parse_stmt_with(
                    *body,
                    symbols,
                    make_temp_var,
                ));
                // Label(continue_label)
                block_instructions.push(Instruction::Label(label_continue));
                // <instructions for post>
                if let Some(post) = post {
                    let Expr {
                        instructions: instructions_post,
                        ..
                    } = Expr::parse_with_and_convert(post, symbols, make_temp_var);
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
                } = Expr::parse_with_and_convert(condition, symbols, make_temp_var);

                instructions.push(Self::JumpIfZero {
                    condition: val,
                    target: Rc::clone(match r#else {
                        Some(_) => &else_label,
                        None => &end_label,
                    }),
                });

                instructions.extend(Instruction::parse_block_with(
                    ast::Block(vec![ast::BlockItem::Stmt(*then)]),
                    symbols,
                    make_temp_var,
                ));

                if let Some(r#else) = r#else {
                    instructions.push(Instruction::Jump(Rc::clone(&end_label)));
                    instructions.push(Instruction::Label(Rc::clone(&else_label)));
                    instructions.extend(Instruction::parse_block_with(
                        ast::Block(vec![ast::BlockItem::Stmt(*r#else)]),
                        symbols,
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
                block_instructions.extend(Self::parse_stmt_with(*stmt, symbols, make_temp_var));
            }
            ast::Stmt::Default { label, stmt } => {
                let label = label.expect("Default must have label");
                block_instructions.push(Instruction::Label(Rc::clone(&label)));
                block_instructions.extend(Self::parse_stmt_with(*stmt, symbols, make_temp_var));
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
                    let Expr { instructions, .. } =
                        Expr::parse_with_and_convert(condition, symbols, make_temp_var);
                    block_instructions.extend(instructions);
                    block_instructions.push(Instruction::Jump(
                        default.unwrap_or(Rc::clone(&break_label)),
                    ));
                    block_instructions.extend(Self::parse_stmt_with(*body, symbols, make_temp_var));
                } else if let ast::Expr::Constant(cond) = condition {
                    let jump_label = cases
                        .iter()
                        .find(|&&(case, _)| case == cond)
                        .map_or(default, |(_, label)| Some(Rc::clone(label)));
                    let has_jump_label = jump_label.is_some();
                    let jump_label = jump_label.unwrap_or(break_label.clone());
                    block_instructions.push(Instruction::Jump(jump_label.clone()));
                    block_instructions.extend(Self::parse_stmt_with(*body, symbols, make_temp_var));
                    if !has_jump_label {
                        block_instructions.push(Instruction::Label(jump_label));
                    }
                } else {
                    let Expr {
                        instructions,
                        val: switch_val,
                    } = Expr::parse_with_and_convert(condition, symbols, make_temp_var);
                    block_instructions.extend(instructions);
                    for (case, label) in cases.iter() {
                        let Expr {
                            instructions,
                            val: case_val,
                        } = Expr::parse_with_and_convert(
                            ast::Expr::Constant(*case),
                            symbols,
                            make_temp_var,
                        );
                        block_instructions.extend(instructions);
                        let dst = Function::make_tacky_temp_var(
                            ast::Type::bool(),
                            symbols,
                            make_temp_var,
                        );
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
                    block_instructions.extend(Self::parse_stmt_with(*body, symbols, make_temp_var));
                }
                // Break label always goes after all instructions
                block_instructions.push(Instruction::Label(break_label));
            }
        }
        block_instructions
    }

    pub(crate) fn parse_block_with(
        node: ast::Block,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> Vec<Self> {
        let mut block_instructions = vec![];
        for item in node.into_items().into_iter() {
            match item {
                // Statics already get initialized at the top level.
                // If we reinitialized them here they would act like local
                // variables (suboptimal)
                ast::BlockItem::Decl(ast::Declaration::VarDecl(ast::VarDecl {
                    storage_class: Some(ast::StorageClass::Static),
                    ..
                })) => {}
                ast::BlockItem::Decl(decl) => {
                    block_instructions.extend(Self::parse_decl_with(decl, symbols, make_temp_var));
                }
                ast::BlockItem::Stmt(stmt) => {
                    block_instructions.extend(Self::parse_stmt_with(stmt, symbols, make_temp_var));
                }
            }
        }
        block_instructions
    }
}
