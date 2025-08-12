use crate::ast;
use crate::sema;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct Program {
    pub top_level: Vec<TopLevel>,
    pub symbols: SymbolTable,
}

#[derive(Debug, PartialEq)]
pub enum TopLevel {
    Fun(Function),
    Static(StaticVariable),
}

#[derive(Debug, Default)]
pub struct SymbolTable {
    pub table: HashMap<Rc<String>, SymbolEntry>,
}

impl SymbolTable {
    pub fn get(&self, key: &Rc<String>) -> Option<&SymbolEntry> {
        self.table.get(key)
    }

    pub fn new_entry(&mut self, key: Rc<String>, r#type: ast::Type) {
        let old_key = self.table.insert(
            Rc::clone(&key),
            SymbolEntry {
                r#type,
                attribute: sema::tc::Attribute::Local,
            },
        );

        assert!(
            old_key.is_none(),
            "Every new entry into SymbolTable should have a unique name!, but {key} did not!"
        );
    }
}

#[derive(Clone, Debug)]
pub struct SymbolEntry {
    pub r#type: ast::Type,
    pub attribute: sema::tc::Attribute,
}

impl From<sema::tc::SymbolTable> for SymbolTable {
    fn from(value: sema::tc::SymbolTable) -> Self {
        Self {
            table: value
                .global
                .into_iter()
                .map(|(k, v)| {
                    (
                        k,
                        SymbolEntry {
                            r#type: v.r#type,
                            attribute: v.attribute,
                        },
                    )
                })
                .collect(),
        }
    }
}

impl StaticVariable {
    fn from_symbol_with_name(name: Rc<String>, symbol: &sema::tc::SymbolEntry) -> Option<Self> {
        match &symbol.attribute {
            sema::tc::Attribute::Fun { .. } => None,
            sema::tc::Attribute::Static {
                initial_value,
                external_linkage,
            } => match initial_value {
                sema::tc::InitialValue::Initial(i) => Some(StaticVariable {
                    identifier: name,
                    external_linkage: *external_linkage,
                    init: Some(Rc::clone(i)),
                }),
                sema::tc::InitialValue::Tentative => Some(StaticVariable {
                    identifier: name,
                    external_linkage: *external_linkage,
                    init: Some(vec![0; symbol.r#type.base.nbytes()].into()),
                }),
                sema::tc::InitialValue::None => None,
            },
            sema::tc::Attribute::Local => None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct StaticVariable {
    pub identifier: Rc<String>,
    pub external_linkage: bool,
    pub init: Option<Rc<[u8]>>,
}

impl From<sema::ValidAst> for Program {
    fn from(ast: sema::ValidAst) -> Self {
        let sema::ValidAst { program, symbols } = ast;
        let mut statics = vec![];
        for (name, symbol) in symbols.global.iter() {
            if let Some(r#static) = StaticVariable::from_symbol_with_name(Rc::clone(name), symbol) {
                statics.push(r#static);
            }
        }
        let mut symbols = SymbolTable::from(symbols);
        let mut valid_functions = vec![];
        for decl in program.declarations.into_iter() {
            match decl {
                // Only declarations with bodies will be returned here.
                // We need to do some fixup so that if the definition for a
                // function was not marked static but the first declaration was
                // that the function gets defined as static.
                ast::Declaration::FunDecl(f) => {
                    if let Some(f) = Function::from_symbol(f, &mut symbols) {
                        valid_functions.push(f);
                    }
                }
                ast::Declaration::VarDecl(_) => {}
            };
        }
        let top_level = valid_functions
            .into_iter()
            .map(TopLevel::Fun)
            .chain(statics.into_iter().map(TopLevel::Static))
            .collect::<Vec<TopLevel>>();
        Self { top_level, symbols }
    }
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: Rc<String>,
    pub external_linkage: bool,
    pub signature: Vec<(ast::Type, Option<Rc<String>>)>,
    pub instructions: Vec<Instruction>,
}

impl Function {
    fn make_temp_var(name: Rc<String>, counter: &'_ mut usize) -> impl FnMut() -> String + use<'_> {
        move || {
            let n = *counter;
            *counter += 1;
            format!("tacky.{name}.{n}")
        }
    }

    fn make_tacky_temp_var(
        r#type: ast::Type,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> Val {
        let name = Rc::new(make_temp_var());
        symbols.new_entry(Rc::clone(&name), r#type);
        Val::Var(name)
    }
}

impl Function {
    fn from_symbol(decl: ast::FunDecl, symbols: &mut SymbolTable) -> Option<Self> {
        // Insert function parameter types for type inference
        // FIXME: Make sure that parameter names are unique!!!!
        let mut signature = vec![];
        for (r#type, name) in decl
            .signature()
            .expect("tacky.Function.from_symbol(): Error getting function declaration signature.")
            .into_iter()
        {
            if let Some(name) = name {
                symbols.new_entry(Rc::clone(name), r#type.clone());
                signature.push((r#type.clone(), Some(Rc::clone(name))));
            } else {
                signature.push((r#type.clone(), None));
            }
        }

        let ast::FunDecl {
            name,
            block: Some(block),
            ..
        } = decl
        else {
            return None;
        };

        let mut temp_var_counter = 0;
        let mut make_temp_var =
            Function::make_temp_var(Rc::new(name.to_string()), &mut temp_var_counter);
        let mut instructions = Instruction::parse_block_with(block, symbols, &mut make_temp_var);

        // Temporary fix suggested by the book for the case where a function
        // is supposed to return something but does not.
        instructions.push(Instruction::Return(Some(Val::Constant(
            ast::Constant::I32(0),
        ))));

        let name = Rc::new(name.to_string());

        // Check symbol table to get external linkage since the function
        // declaration could be static but the definition can elide it.
        // ```
        // static int foo();
        // int foo() {
        //      ...
        // }
        // ```

        let external_linkage = {
            if let Some(SymbolEntry {
                r#type:
                    ast::Type {
                        base: ast::BaseType::Fun { .. },
                        ..
                    },
                attribute,
            }) = symbols.get(&name)
            {
                attribute.has_external_linkage()
            } else {
                unreachable!()
            }
        };

        Some(Function {
            name,
            signature,
            external_linkage,
            instructions,
        })
    }
}

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
}

impl Instruction {
    fn parse_decl_with(
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

    fn parse_fun_decl_with(
        decl: ast::FunDecl,
        _make_temp_var: &mut impl FnMut() -> String,
    ) -> Vec<Self> {
        assert!(decl.block.is_none());
        vec![]
    }

    fn parse_var_decl_with(
        decl: ast::VarDecl,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> Vec<Self> {
        if decl.storage_class != Some(ast::StorageClass::Extern) {
            symbols.new_entry(Rc::clone(&decl.name), decl.r#type.clone());
        }
        if let Some(init) = decl.init {
            let Expr {
                mut instructions,
                val: src,
            } = Expr::parse_with_and_convert(init, symbols, make_temp_var);
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
    fn parse_stmt_with(
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
    fn parse_block_with(
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

#[derive(Debug, PartialEq)]
pub struct Expr {
    instructions: Vec<Instruction>,
    val: Val,
}

impl Expr {
    fn parse_with(
        node: ast::Expr,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> ExprResult {
        match node {
            ast::Expr::Constant(v) => ExprResult::PlainOperand(Self {
                instructions: vec![],
                val: Val::from(v),
            }),
            ast::Expr::Unary {
                op: ast::UnaryOp::Deref,
                expr,
            } => ExprResult::DerefrencedPointer(Self::parse_with_and_convert(
                *expr,
                symbols,
                make_temp_var,
            )),
            ast::Expr::Unary {
                op: ast::UnaryOp::AddrOf,
                expr,
            } => {
                let result_expr = Self::parse_with(*expr, symbols, make_temp_var);
                match result_expr {
                    ExprResult::PlainOperand(expr) => {
                        let Self {
                            mut instructions,
                            val,
                        } = expr;
                        let dst = Function::make_tacky_temp_var(
                            ast::Type {
                                base: ast::BaseType::Ptr {
                                    to: Box::new(val.get_type(symbols)),
                                    is_restrict: false,
                                },
                                alignment: ast::Type::PTR_ALIGNMENT,
                                is_const: false,
                            },
                            symbols,
                            make_temp_var,
                        );
                        instructions.push(Instruction::GetAddress {
                            src: val,
                            dst: dst.clone(),
                        });
                        ExprResult::PlainOperand(Expr {
                            instructions,
                            val: dst,
                        })
                    }
                    ExprResult::DerefrencedPointer(expr) => ExprResult::PlainOperand(expr),
                }
            }
            ast::Expr::Unary { op, expr } => {
                let Self {
                    mut instructions,
                    val,
                } = Expr::parse_with_and_convert(*expr, symbols, make_temp_var);
                let dst = match op {
                    ast::UnaryOp::PreInc => {
                        instructions.push(Instruction::Binary {
                            op: BinaryOp::Add,
                            src1: val.clone(),
                            src2: Val::Constant(
                                ast::Constant::const_from_type(&val.get_type(symbols), 1)
                                    .expect("UnaryOp type has an ast::Constant equivilent"),
                            ),
                            dst: val.clone(),
                        });
                        val.clone()
                    }
                    ast::UnaryOp::PostInc => {
                        let dst = Function::make_tacky_temp_var(
                            val.get_type(symbols),
                            symbols,
                            make_temp_var,
                        );
                        instructions.push(Instruction::Copy {
                            src: val.clone(),
                            dst: dst.clone(),
                        });
                        instructions.push(Instruction::Binary {
                            op: BinaryOp::Add,
                            src1: val.clone(),
                            src2: Val::Constant(
                                ast::Constant::const_from_type(&val.get_type(symbols), 1)
                                    .expect("UnaryOp type has an ast::Constant equivilent"),
                            ),
                            dst: val.clone(),
                        });
                        dst
                    }
                    ast::UnaryOp::PreDec => {
                        instructions.push(Instruction::Binary {
                            op: BinaryOp::Subtract,
                            src1: val.clone(),
                            src2: Val::Constant(
                                ast::Constant::const_from_type(&val.get_type(symbols), 1)
                                    .expect("UnaryOp type has an ast::Constant equivilent"),
                            ),
                            dst: val.clone(),
                        });
                        val.clone()
                    }
                    ast::UnaryOp::PostDec => {
                        let dst = Function::make_tacky_temp_var(
                            val.get_type(symbols),
                            symbols,
                            make_temp_var,
                        );
                        instructions.push(Instruction::Copy {
                            src: val.clone(),
                            dst: dst.clone(),
                        });
                        instructions.push(Instruction::Binary {
                            op: BinaryOp::Subtract,
                            src1: val.clone(),
                            src2: Val::Constant(
                                ast::Constant::const_from_type(&val.get_type(symbols), 1)
                                    .expect("UnaryOp type has an ast::Constant equivilent"),
                            ),
                            dst: val.clone(),
                        });
                        dst
                    }
                    ast::UnaryOp::Not => {
                        let dst = Function::make_tacky_temp_var(
                            ast::Type::int(4, None),
                            symbols,
                            make_temp_var,
                        );
                        instructions.push(Instruction::Unary {
                            op: UnaryOp::from(op),
                            src: val,
                            dst: dst.clone(),
                        });
                        dst
                    }
                    // Other operations have tacky unary op equivalents
                    _ => {
                        let dst = Function::make_tacky_temp_var(
                            val.get_type(symbols),
                            symbols,
                            make_temp_var,
                        );
                        instructions.push(Instruction::Unary {
                            op: UnaryOp::from(op),
                            src: val,
                            dst: dst.clone(),
                        });
                        dst
                    }
                };
                ExprResult::PlainOperand(Expr {
                    instructions,
                    val: dst,
                })
            }
            ast::Expr::Binary { op, left, right } => {
                if op.is_logical() {
                    // <instructions for e1>
                    // v1 = <result of e1>
                    let Self {
                        mut instructions,
                        val: left_val,
                    } = Self::parse_with_and_convert(*left, symbols, make_temp_var);

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
                    } = Self::parse_with_and_convert(*right, symbols, make_temp_var);
                    instructions.extend(right_instructions);

                    let dst =
                        Function::make_tacky_temp_var(ast::Type::bool(), symbols, make_temp_var);
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
                        src: Val::Constant(ast::Constant::I32(result_nojmp)),
                        dst: dst.clone(),
                    });

                    // Jump(end)
                    instructions.push(Instruction::Jump(Rc::clone(&end)));

                    // Label(false_label)
                    instructions.push(Instruction::Label(Rc::clone(&label)));

                    // result = 0
                    instructions.push(Instruction::Copy {
                        src: Val::Constant(ast::Constant::I32(result_jmp)),
                        dst: dst.clone(),
                    });

                    // Label(end)
                    instructions.push(Instruction::Label(end));

                    ExprResult::PlainOperand(Self {
                        instructions,
                        val: dst,
                    })
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
                        } = Self::parse_with_and_convert(binary, symbols, make_temp_var);

                        instructions.push(Instruction::Copy {
                            src,
                            dst: Val::Var(Rc::clone(dst)),
                        });
                        ExprResult::PlainOperand(Self {
                            instructions,
                            val: Val::Var(Rc::clone(dst)),
                        })
                    } else {
                        panic!("Cannot use compound assignment on non-variable value.")
                    }
                } else {
                    let Self {
                        mut instructions,
                        val: left_val,
                    } = Self::parse_with_and_convert(*left, symbols, make_temp_var);
                    let Self {
                        instructions: right_instructions,
                        val: right_val,
                    } = Self::parse_with_and_convert(*right, symbols, make_temp_var);
                    instructions.extend(right_instructions);

                    let dst_type = if op.is_relational() {
                        ast::Type::bool()
                    } else {
                        left_val.get_type(symbols)
                    };

                    // FIXME: Same as above, not exactly sure where the type casting happens
                    let dst = Function::make_tacky_temp_var(dst_type, symbols, make_temp_var);

                    instructions.push(Instruction::Binary {
                        op: op.into(),
                        src1: left_val,
                        src2: right_val,
                        dst: dst.clone(),
                    });
                    ExprResult::PlainOperand(Self {
                        instructions,
                        val: dst,
                    })
                }
            }
            ast::Expr::Var(name) => ExprResult::PlainOperand(Self {
                instructions: vec![],
                val: Val::Var(name),
            }),
            ast::Expr::Assignment { lvalue, rvalue } => {
                let lval = Self::parse_with(*lvalue, symbols, make_temp_var);
                let rval = Self::parse_with_and_convert(*rvalue, symbols, make_temp_var);
                match lval {
                    ExprResult::PlainOperand(Expr {
                        mut instructions,
                        val,
                    }) => {
                        instructions.extend(rval.instructions.into_iter());

                        instructions.push(Instruction::Copy {
                            src: rval.val,
                            dst: val.clone(),
                        });
                        ExprResult::PlainOperand(Self { instructions, val })
                    }
                    ExprResult::DerefrencedPointer(Expr {
                        mut instructions,
                        val,
                    }) => {
                        instructions.extend(rval.instructions.into_iter());

                        instructions.push(Instruction::Store {
                            src: rval.val.clone(),
                            dst_ptr: val,
                        });

                        ExprResult::PlainOperand(Expr {
                            instructions,
                            val: rval.val,
                        })
                    }
                }
            }
            ast::Expr::Conditional {
                condition,
                then,
                r#else,
            } => {
                let (e2_label, end_label) = {
                    let label = make_temp_var();
                    let e2_label = format!("{label}.cond_e2");
                    let end_label = format!("{label}.cond_end");
                    (Rc::new(e2_label), Rc::new(end_label))
                };
                let Expr {
                    mut instructions,
                    val,
                } = Expr::parse_with_and_convert(*condition, symbols, make_temp_var);

                instructions.push(Instruction::JumpIfZero {
                    condition: val,
                    target: Rc::clone(&e2_label),
                });

                let Expr {
                    instructions: e1_instructions,
                    val: e1_val,
                } = Expr::parse_with_and_convert(*then, symbols, make_temp_var);

                let result = Function::make_tacky_temp_var(
                    e1_val.get_type(symbols).clone(),
                    symbols,
                    make_temp_var,
                );

                instructions.extend(e1_instructions);
                instructions.push(Instruction::Copy {
                    src: e1_val,
                    dst: result.clone(),
                });

                instructions.push(Instruction::Jump(Rc::clone(&end_label)));
                instructions.push(Instruction::Label(Rc::clone(&e2_label)));

                let Expr {
                    instructions: e2_instructions,
                    val: e2_val,
                } = Expr::parse_with_and_convert(*r#else, symbols, make_temp_var);

                instructions.extend(e2_instructions);

                instructions.push(Instruction::Copy {
                    src: e2_val,
                    dst: result.clone(),
                });

                instructions.push(Instruction::Label(end_label));

                ExprResult::PlainOperand(Self {
                    instructions,
                    val: result,
                })
            }
            ast::Expr::FunCall { name, args } => {
                let SymbolEntry {
                    r#type:
                        ast::Type {
                            base: ast::BaseType::Fun { ret_t, .. },
                            ..
                        },
                    ..
                } = symbols
                    .get(&name)
                    .expect("Function '{name}' should already be in symbol table, but it was not!")
                else {
                    unreachable!(
                        "Function name '{name}' resulted in non-function type in symbol table"
                    );
                };
                let dst = Function::make_tacky_temp_var(*ret_t.clone(), symbols, make_temp_var);
                let (mut instructions, args) =
                    args.into_iter()
                        .fold((vec![], vec![]), |(mut instrs, mut args), arg| {
                            let Expr { instructions, val } =
                                Expr::parse_with_and_convert(arg, symbols, make_temp_var);
                            instrs.extend(instructions);
                            args.push(val);
                            (instrs, args)
                        });
                instructions.push(Instruction::FunCall {
                    name,
                    args,
                    dst: dst.clone(),
                });
                ExprResult::PlainOperand(Self {
                    instructions,
                    val: dst,
                })
            }
            ast::Expr::Cast { target, exp: expr } => {
                let Expr {
                    mut instructions,
                    val,
                } = Expr::parse_with_and_convert(*expr, symbols, make_temp_var);

                // I do this cause get_type currently clones on vars
                let val_type = val.get_type(symbols);
                if target == val_type {
                    return ExprResult::PlainOperand(Self { instructions, val });
                }
                let dst = Function::make_tacky_temp_var(target.clone(), symbols, make_temp_var);

                let is_float = |t: &ast::Type| {
                    matches!(
                        t,
                        ast::Type {
                            base: ast::BaseType::Float(_) | ast::BaseType::Double(_),
                            ..
                        }
                    )
                };

                // Double -> Integer
                if is_float(&val_type) {
                    match target {
                        ast::Type {
                            base:
                                ast::BaseType::Int {
                                    nbytes: _,
                                    signed: Some(false),
                                },
                            ..
                        } => {
                            instructions.push(Instruction::DoubleToUInt {
                                src: val,
                                dst: dst.clone(),
                            });
                        }
                        ast::Type {
                            base:
                                ast::BaseType::Int {
                                    nbytes: _,
                                    signed: _,
                                },
                            ..
                        } => {
                            instructions.push(Instruction::DoubleToInt {
                                src: val,
                                dst: dst.clone(),
                            });
                        }
                        // FIXME: Add chars here
                        // We should not ever be trying to cast a double to
                        // anything other than an int
                        _ => unreachable!("Casting float type to {target:?}"),
                    }
                } else if is_float(&target) {
                    match val_type {
                        ast::Type {
                            base:
                                ast::BaseType::Int {
                                    nbytes: _,
                                    signed: Some(false),
                                },
                            ..
                        } => {
                            instructions.push(Instruction::UIntToDouble {
                                src: val,
                                dst: dst.clone(),
                            });
                        }
                        ast::Type {
                            base:
                                ast::BaseType::Int {
                                    nbytes: _,
                                    signed: _,
                                },
                            ..
                        } => {
                            instructions.push(Instruction::IntToDouble {
                                src: val,
                                dst: dst.clone(),
                            });
                        }
                        // FIXME: Add chars here
                        // We should not ever be trying to cast a double to
                        // anything other than an int
                        _ => unreachable!("Casting float type to {target:?}"),
                    }
                } else {
                    // Integer ops
                    // FIXME: This needs to use PartialEq/Eq
                    match target.base.nbytes().cmp(&val_type.base.nbytes()) {
                        std::cmp::Ordering::Equal => {
                            instructions.push(Instruction::Copy {
                                src: val,
                                dst: dst.clone(),
                            });
                        }
                        std::cmp::Ordering::Less => {
                            instructions.push(Instruction::Truncate {
                                src: val,
                                dst: dst.clone(),
                            });
                        }
                        _ => match val_type {
                            ast::Type {
                                base: ast::BaseType::Int { signed, .. },
                                ..
                            } => {
                                if signed.is_none_or(|signed| signed) {
                                    instructions.push(Instruction::SignExtend {
                                        src: val,
                                        dst: dst.clone(),
                                    });
                                } else {
                                    instructions.push(Instruction::ZeroExtend {
                                        src: val,
                                        dst: dst.clone(),
                                    });
                                }
                            }
                            _ => unimplemented!(),
                        },
                    }
                }

                ExprResult::PlainOperand(Self {
                    instructions,
                    val: dst,
                })
            }
        }
    }

    fn parse_with_and_convert(
        node: ast::Expr,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> Self {
        match Self::parse_with(node, symbols, make_temp_var) {
            ExprResult::PlainOperand(expr) => expr,
            ExprResult::DerefrencedPointer(expr) => {
                let Self {
                    mut instructions,
                    val,
                } = expr;
                let dst = Function::make_tacky_temp_var(
                    val.get_type(symbols).deref(),
                    symbols,
                    make_temp_var,
                );
                instructions.push(Instruction::Load {
                    src_ptr: val,
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

enum ExprResult {
    PlainOperand(Expr),
    DerefrencedPointer(Expr),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    Constant(ast::Constant),
    Var(Rc<String>),
}

impl Val {
    pub fn get_type(&self, symbols: &SymbolTable) -> ast::Type {
        match self {
            Self::Constant(c) => c.get_type(),
            Self::Var(name) => {
                let Some(entry) = symbols.get(name) else {
                    unreachable!("Variable name '{name}' not found in symbol table");
                };
                entry.r#type.clone()
            }
        }
    }
}

impl From<ast::Constant> for Val {
    fn from(node: ast::Constant) -> Self {
        Self::Constant(node)
    }
}

#[derive(Clone, Debug, PartialEq)]
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
        let mut symbols = SymbolTable::default();
        let ast = ast::Block(vec![ast::BlockItem::Stmt(ast::Stmt::Return(Some(
            ast::Expr::Constant(ast::Constant::I32(2)),
        )))]);
        let mut counter = 0;
        let mut make_temp_var = Function::make_temp_var(Rc::new("test".to_string()), &mut counter);
        let actual = Instruction::parse_block_with(ast, &mut symbols, &mut make_temp_var);
        let expected = vec![Instruction::Return(Some(Val::Constant(
            ast::Constant::I32(2),
        )))];
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_return_unary() {
        let mut symbols = SymbolTable::default();
        let ast = ast::Block(vec![ast::BlockItem::Stmt(ast::Stmt::Return(Some(
            ast::Expr::Unary {
                op: ast::UnaryOp::Complement,
                expr: Box::new(ast::Expr::Constant(ast::Constant::I32(2))),
            },
        )))]);
        let mut counter = 0;
        let mut make_temp_var = Function::make_temp_var(Rc::new("test".to_string()), &mut counter);
        let actual = Instruction::parse_block_with(ast, &mut symbols, &mut make_temp_var);
        let expected = vec![
            Instruction::Unary {
                op: UnaryOp::Complement,
                src: Val::Constant(ast::Constant::I32(2)),
                dst: Val::Var("tacky.test.0".to_string().into()),
            },
            Instruction::Return(Some(Val::Var("tacky.test.0".to_string().into()))),
        ];
        assert_eq!(actual, expected);
    }
    #[test]
    fn test_return_nested_unary() {
        let mut symbols = SymbolTable::default();
        let ast = ast::Block(vec![ast::BlockItem::Stmt(ast::Stmt::Return(Some(
            ast::Expr::Unary {
                op: ast::UnaryOp::Negate,
                expr: Box::new(ast::Expr::Unary {
                    op: ast::UnaryOp::Complement,
                    expr: Box::new(ast::Expr::Unary {
                        op: ast::UnaryOp::Negate,
                        expr: Box::new(ast::Expr::Constant(ast::Constant::I32(2))),
                    }),
                }),
            },
        )))]);
        let mut counter = 0;
        let mut make_temp_var = Function::make_temp_var(Rc::new("test".to_string()), &mut counter);
        let actual = Instruction::parse_block_with(ast, &mut symbols, &mut make_temp_var);
        let expected = vec![
            Instruction::Unary {
                op: UnaryOp::Negate,
                src: Val::Constant(ast::Constant::I32(2)),
                dst: Val::Var("tacky.test.0".to_string().into()),
            },
            Instruction::Unary {
                op: UnaryOp::Complement,
                src: Val::Var("tacky.test.0".to_string().into()),
                dst: Val::Var("tacky.test.1".to_string().into()),
            },
            Instruction::Unary {
                op: UnaryOp::Negate,
                src: Val::Var("tacky.test.1".to_string().into()),
                dst: Val::Var("tacky.test.2".to_string().into()),
            },
            Instruction::Return(Some(Val::Var("tacky.test.2".to_string().into()))),
        ];
        assert_eq!(actual, expected);
    }
}
