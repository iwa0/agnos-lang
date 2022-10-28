use std::{collections::HashMap, mem};

use data_structures::interner::Interner;

use crate::syntax::{ast, token};

use super::{il, ildecl};

pub struct Ast2IL<'a> {
    ast: &'a ast::Ast,
    int: &'a mut Interner,
    il: &'a mut ildecl::IL,
    cf: il::ControlFlow,
}

impl Ast2IL<'_> {
    pub fn insert_symbol(
        &mut self,
        id: token::Ident,
        kind: ildecl::SymbolKind,
    ) -> ildecl::SymbolId {
        self.il.insert_symbol(id, kind, self.current_scope_id())
    }
    pub fn current_scope_id(&self) -> ildecl::ScopeId {
        todo!()
    }
    pub fn current_scope(&self) -> &ildecl::Scope {
        self.il.scope(self.current_scope_id())
    }

    fn mk_context(&mut self) -> Self {
        Self {
            cf: il::ControlFlow::new(),
            ..*self
        }
    }

    fn emit(&mut self, kind: il::InstructionKind) -> il::Operand {
        let ty = self.il.mk_tyvar();
        self.cf.emit(il::Instruction { kind, ty })
    }
    fn emit_terminator(&mut self, kind: il::TerminatorKind) {
        self.cf.emit_terminator(il::Terminator { kind })
    }

    pub fn find_exit(&self, which: ildecl::ScopeId) -> il::OpId {
        let mut cursor = self.current_scope_id();
        while cursor != which {
            let scope = self.il.scope(cursor);
            if let Some(_) = scope.enclosing.defer {
                // todo: diagnostic
                return self.cf.op_unreachable();
            }
            cursor = scope.enclosing.block.unwrap();
        }
        let scope = self.il.scope(cursor);
        if let Some(defer) = scope.enclosing.defer {
            self.il.scope(defer).op.unwrap()
        } else {
            scope.op.unwrap()
        }
    }

    pub fn emit_break(&self, which: ildecl::ScopeId) {
        let exit_point = self.find_exit(which);
    }

    fn emit_linear(&mut self, kind: il::LinearTerminatorKind, target: il::Branch) {
        self.emit_terminator(il::TerminatorKind::Linear(il::LinearTerminator {
            kind,
            target,
        }));
    }

    fn emit_branch(&mut self, kind: il::ExitTerminatorKind, target: il::OpId) {
        let fall = self.cf.reserve_op();
        self.emit_terminator(il::TerminatorKind::Exit(il::ExitTerminator {
            kind,
            target: il::Branch::new(target),
            fall: il::Branch::new(fall),
        }));
        let fall_bb = self.cf.emit_bb([]);
        self.cf.set_reserved_op(fall, il::Operand::block(fall_bb));
    }

    fn delete_inherit_current_scope(
        &self,
        overriding: ildecl::ScopeEnclosing,
    ) -> ildecl::ScopeEnclosing {
        let id = self.current_scope_id();
        self.il.scope(id).inherit(id)
    }

    /*
    scope kind
    entity name, kind
    entity name, kind

    */

    fn emit_file(&mut self, id: ast::SourceFileId) {
        let file = self.ast.files[id];
        let file_scope = self.insert_scope(ildecl::ScopeKind::File);
        //self.emit(il::InstructionKind::scope file);
        match file.kind {
            ast::SourceFileKind::Dir(files) => {
                let entity = self.insert_symbol(file.name, ildecl::SymbolKind::File);
                // emit entity name, file (use ty as symbol)
                let mut ctx = self.mk_context();
                ctx.cf.emit_bb([]);
                let bb = ctx.emit_enter(scope);
                {
                    let symbol = ctx.il.symbol_mut(entity);
                    symbol.scope = Some(file_scope);
                    let scope = ctx.il.scope_mut(file_scope);
                    scope.entity = Some(entity);
                }
                for file in files.iter() {
                    ctx.emit_file(file);
                }
            }
            ast::SourceFileKind::File(blk) => {
                let entity = self.insert_symbol(file.name, ildecl::SymbolKind::File);
                let scope = self.il.insert_scope(ildecl::ScopeKind::Block, file_scope);
                let mut ctx = self.mk_context();
                ctx.cf.emit_bb([]);
                let bb = ctx.emit_enter(scope);
                let exit_fwd = ctx.cf.reserve_op();
                {
                    let scope = ctx.il.scope_mut(scope);
                    scope.op = Some(exit_fwd);
                    let scope = ctx.il.scope_mut(file_scope);
                    scope.entity = Some(entity);
                }
                ctx.emit_block(blk);
                let exit = self.emit_exit();
                self.cf.set_reserved_op(exit, il::Operand::block(post_exit));
                self.cf
                    .set_reserved_op(exit_fwd, il::Operand::block(post_exit));
                {
                    let cf = ctx.cf;
                    let sym = ctx.il.symbol_mut(entity);
                    sym.scope = Some(file_scope);
                    sym.cf = Some(Box::new(cf));
                }
                // ty,val?
            }
        }
    }

    fn emit_item(&mut self, id: ast::ItemId) {
        let item = self.ast.items[id];
        match item.kind {
            ast::ItemKind::Const(ast::ConstItem(ty_annot, val)) => {
                let entity = self.insert_symbol(item.name, ildecl::SymbolKind::Const);
                let scope = self.insert_scope(ildecl::ScopeKind::Block);

                let mut ctx = self.mk_context();

                ctx.cf.emit_bb([]);
                let ty_op = if let Some(ty_annot) = ty_annot {
                    let op = ctx.emit_expr_mk(ty_annot, false);
                    Some(op)
                } else {
                    None
                };
                if let Some(ty_op) = ty_op {
                    //ctx.emit(il::InstructionKind::Annot(val, ty_op));
                }

                let bb = ctx.emit_enter(scope);
                let exit_fwd = ctx.cf.reserve_op();
                {
                    let scope = ctx.il.scope_mut(scope);
                    scope.op = Some(exit_fwd);
                }

                ctx.emit_expr_mk(val, true);
                let exit = ctx.emit_exit();

                let cf = ctx.cf;
                let sym = self.il.symbol_mut(entity);
                sym.cf = Some(Box::new(cf));
                // ty,val
            }
            ast::ItemKind::Module(ast::ModuleItem(blk)) => {
                let entity = self.insert_symbol(item.name, ildecl::SymbolKind::Module);
                let module_scope = self.insert_scope(ildecl::ScopeKind::Module);
                let mut ctx = self.mk_context();

                let (entry, scope) = ctx.emit_entry(ildecl::ScopeKind::Block, module_scope, []);
                let exit_fwd = ctx.cf.reserve_op();
                {
                    let scope = ctx.il.scope_mut(scope);
                    scope.op = Some(exit_fwd);
                }
                ctx.emit_block(blk);
                let exit = self.emit_exit();
                let post_exit = ctx.emit_bb([]);
                ctx.emit_terminator(il::TerminatorKind::Exit(il::ExitTerminator {
                    kind: il::ExitTerminatorKind::Return(None),
                    target: self.cf.op_unreachable(),
                }));

                self.cf.set_reserved_op(exit, il::Operand::block(post_exit));
                self.cf
                    .set_reserved_op(exit_fwd, il::Operand::block(post_exit));

                {
                    let scope = ctx.il.scope_mut(module_scope);
                    scope.entity = Some(entity);
                }
                {
                    let cf = ctx.cf;
                    let sym = ctx.il.symbol_mut(entity);
                    sym.scope = Some(module_scope);
                    sym.cf = Some(Box::new(cf));
                }
            }
            ast::ItemKind::Struct(ast::StructItem(fields)) => {
                let entity = self.insert_symbol(item.name, ildecl::SymbolKind::DataType);
                let scope = self.insert_scope(ildecl::ScopeKind::DataType);
                {
                    let scope = self.il.scope_mut(scope);
                    scope.entity = Some(entity);
                }
                for field in fields.iter() {
                    let field = self.ast.fields[field];
                    let sym = self
                        .il
                        .insert_symbol(field.name, ildecl::SymbolKind::Field, scope);
                    // types and stuff...
                }
            }
            ast::ItemKind::Union(_) => todo!(),
            ast::ItemKind::Enum(ast::EnumItem(enumerators)) => todo!(),
            ast::ItemKind::Func(ast::FuncItem(params, ret_ty, body)) => {
                let entity = self.insert_symbol(item.name, ildecl::SymbolKind::Function);
                let function_scope = self.insert_scope(ildecl::ScopeKind::Function);

                let mut ctx = self.mk_context();

                let mut param_types = Vec::with_capacity(params.len());
                for _ in params.iter() {
                    let ty = ctx.il.mk_tyvar();
                    param_types.push(ty);
                }

                let (entry, scope) = ctx.emit_entry(
                    ildecl::ScopeKind::Block,
                    function_scope,
                    param_types.iter().copied(),
                );
                let exit_fwd = ctx.cf.reserve_op();
                {
                    let scope = ctx.il.scope_mut(scope);
                    scope.entity = Some(entity);
                    scope.op = Some(exit_fwd);
                }

                for (idx, pd) in params.iter().enumerate() {
                    let pd = self.ast.patdecls[pd];

                    let op = il::Operand::param(entry, idx as u32);
                    let op = ctx.cf.mk_op(op);
                    if let Some(ty_annot) = pd.ty_annot {
                        let ty = ctx.emit_expr_mk(ty_annot, false);
                        self.emit(il::InstructionKind::Annot(op, ty));
                    }
                    self.emit_pat_irrefutable(ildecl::SymbolKind::Param, pd.pat, op, scope);
                }

                let ret_ty_op = if let Some(ret_ty) = ret_ty {
                    ctx.emit_rhs(ret_ty)
                } else {
                    let ident = token::mk_ident(self.int, "unit");
                    ctx.emit(il::InstructionKind::Lookup(
                        il::LookupKind::AssociatedScope,
                        ident,
                    ))
                };
                let ret_ty_op = self.cf.mk_op(ret_ty_op);

                let op = self.emit_rhs(body);
                let exit = self.emit_exit();

                let post_exit = self.emit_bb([]);
                let op = if let Some(&op) = op.as_non_unreachable() {
                    let op = self.cf.mk_op(op);
                    self.emit(il::InstructionKind::Annot(op, ret_ty_op));
                    Some(op)
                } else {
                    None
                };
                ctx.emit_terminator(il::TerminatorKind::Exit(il::ExitTerminator {
                    kind: il::ExitTerminatorKind::Return(None),
                    target: self.cf.op_unreachable(),
                }));

                self.cf.set_reserved_op(exit, il::Operand::block(post_exit));
                self.cf
                    .set_reserved_op(exit_fwd, il::Operand::block(post_exit));

                {
                    let cf = ctx.cf;
                    let sym = self.il.symbol_mut(entity);
                    sym.scope = Some(function_scope);
                    sym.cf = Some(Box::new(cf));
                }
            }
        }
    }

    pub fn emit_block(&mut self, blk: ast::Block) -> il::Operand {
        for stmt in blk.stmts.iter() {
            self.emit_stmt(stmt);
        }
        let res = match blk.kind {
            ast::BlockKind::Void => il::Operand::Unreachable,
            ast::BlockKind::Trailing(e) => self.emit_rhs(e),
        };
        res
    }

    fn emit_pm(
        &mut self,
        kind: ildecl::SymbolKind,
        pat: ast::PatId,
        op: il::OpId,
        pat_decl_scope: ildecl::ScopeId,
        otherwise: il::OpId,
    ) -> il::Info {
        let pat = &self.ast.pats[pat];
        match pat.kind {
            ast::PatternKind::Wildcard | ast::PatternKind::Rest => il::Info::none(),
            ast::PatternKind::Rename(pat) => {
                let info = self.emit_pm(kind, pat, op, pat_decl_scope, otherwise);
                // attach info to sigma insn
                let sigma = self.emit(il::InstructionKind::LValue(il::LValue::Rename(op)));
                let sigma = self.cf.mk_op(sigma);
                self.emit(il::InstructionKind::Assign(op, sigma)); // should work even if immut
                info
            }
            ast::PatternKind::Name(qual, name, sub) => {
                let ssi = if let Some(sub) = sub {
                    self.emit_pm(kind, sub, op, pat_decl_scope, otherwise)
                } else {
                    il::Info::none()
                };

                let ssi_op = if sub.is_some() {
                    // attach info to sigma insn
                    let sigma = self.emit(il::InstructionKind::LValue(il::LValue::Rename(op)));
                    self.cf.mk_op(sigma)
                } else {
                    op
                };

                let symbol = self.il.insert_symbol(name, kind, pat_decl_scope);
                self.emit(il::InstructionKind::Let(
                    qual,
                    il::WithSymbol {
                        t: name,
                        symbol: Some(symbol),
                    },
                    ssi_op,
                ));

                let ty = self.il.mk_tyvar();
                let sym = self.il.symbol_mut(symbol);
                sym.ty = Some(ty);

                ssi
            }
            ast::PatternKind::Group(pat) => self.emit_pm(kind, pat, op, pat_decl_scope, otherwise),
            ast::PatternKind::Cmp(kind, e) => {
                todo!()
            }
            ast::PatternKind::Compound(pats) => {
                let mut info = il::Info::none();
                for (idx, pat) in pats.iter().enumerate() {
                    let op = self.emit(il::InstructionKind::LValue(il::LValue::Extract(
                        op, idx as u32,
                    )));
                    let op = self.cf.mk_op(op);
                    let sub_info = self.emit_pm(kind, pat, op, pat_decl_scope, otherwise);
                    // propagate info upwards?
                }
                info
            }
        }
    }
    fn emit_pat_irrefutable(
        &mut self,
        kind: ildecl::SymbolKind,
        pat: ast::PatId,
        op: il::OpId,
        pat_decl_scope: ildecl::ScopeId,
    ) {
        let otherwise = self.cf.op_unreachable();
        let res = self.emit_pm(kind, pat, op, pat_decl_scope, otherwise);
    }

    // =======

    pub fn emit_assoc_call(&mut self, name: token::Ident, args: il::OpListId) -> il::Operand {
        let method = self.emit(il::InstructionKind::Lookup(
            il::LookupKind::AssociatedScope,
            name,
        ));
        let method = self.cf.mk_op(method);
        let succ_op = self.cf.reserve_op();
        self.emit_terminator(il::TerminatorKind::Call(
            method,
            args,
            il::Branch::with_args(succ_op, []),
        ));

        let ty = self.il.mk_tyvar();
        let succ = self.cf.emit_bb_special(self.current_scope_id(), [ty]);
        self.cf.set_reserved_op(succ_op, il::Operand::block(succ));

        il::Operand::param(succ, 0)
    }

    pub fn emit_stmt(&mut self, id: ast::StmtId) {
        let stmt = self.ast.stmts[id];
        match stmt.kind {
            ast::StatementKind::Item(item) => {
                self.emit_item(item);
            }
            ast::StatementKind::Local(pd) => {
                let pd = self.ast.patdecls[pd];
                let ty_op = if let Some(ty_annot) = pd.ty_annot {
                    let op = self.emit_expr_mk(ty_annot, false);
                    Some(op)
                } else {
                    None
                };
                let val = self.emit_expr_mk(pd.expr.unwrap(), true);
                if let Some(ty_op) = ty_op {
                    self.emit(il::InstructionKind::Annot(val, ty_op));
                }
                let scope = self.insert_scope(ildecl::ScopeKind::Let);
                self.emit_pat_irrefutable(ildecl::SymbolKind::Local, pd.pat, val, scope);
            }
            ast::StatementKind::Assign(left, right) => {
                let left = self.emit_expr_mk(left, false);
                let right = self.emit_expr_mk(right, true);
                self.emit(il::InstructionKind::Assign(left, right));
            }
            ast::StatementKind::Defer(body) => {
                let defer_op = self.cf.reserve_op();
                self.emit(il::InstructionKind::Defer(defer_op));
                let scope = self.insert_scope(ildecl::ScopeKind::Block);
                let succ = self.emit_linear_reserved(il::LinearTerminatorKind::Enter(scope));

                let a = self.emit_enter(scope);
                self.emit_expr(body);
                let b = self.emit_exit();

                let sucb = self.cf.emit_bb([]);
                self.cf.set_reserved_op(succ, il::Operand::block(sucb));
            }
            ast::StatementKind::Break(label, val) => {
                let op = val.map(|val| self.emit_expr_mk(val, true));

                let scope = self.current_scope().enclosing.r#break.unwrap();
                let exit = self.find_exit(scope);

                let succ_op = self.cf.reserve_op();
                //how to pass "op" to block exit?

                self.emit_terminator(il::TerminatorKind::BrFall(
                    il::Branch::with_args(exit, op),
                    il::Branch::with_args(succ_op, []),
                ));
                let succ = self.emit_bb([]);
                self.cf.set_reserved_op(succ_op, il::Operand::block(succ));
            }
            ast::StatementKind::Continue(label, val) => {
                let op = val.map(|val| self.emit_expr_mk(val, true));

                let scope = self.current_scope().enclosing.r#continue.unwrap();
                let exit = self.find_exit(scope);

                let succ_op = self.cf.reserve_op();

                self.emit_terminator(il::TerminatorKind::BrFall(
                    il::Branch::with_args(exit, op),
                    il::Branch::with_args(succ_op, []),
                ));
                let succ = self.emit_bb([]);
                self.cf.set_reserved_op(succ_op, il::Operand::block(succ));
            }
            ast::StatementKind::Return(val) => {
                let op = val.map(|val| self.emit_expr_mk(val, true));

                let scope = self.current_scope().enclosing.func.unwrap();
                let exit = self.find_exit(scope);

                let succ_op = self.cf.reserve_op();

                self.emit_terminator(il::TerminatorKind::BrFall(
                    il::Branch::with_args(exit, op),
                    il::Branch::with_args(succ_op, []),
                ));
                let succ = self.emit_bb([]);
                self.cf.set_reserved_op(succ_op, il::Operand::block(succ));
            }
            ast::StatementKind::Expr(e) => {
                let op = self.emit_rhs(e);
            }
        }
    }

    pub fn emit_path(&mut self, path: ast::IdListId) -> il::Operand {
        let mut path = path.iter();
        let (first, rest) = (path.next().unwrap(), path);

        let id = self.ast.ids[first];
        let op = self.emit(il::InstructionKind::Id(id.id));
        let op = self.emit_inst_if_some(op, id.apply);

        let mut left = op;
        for id in rest {
            let id = self.ast.ids[id];
            let left_op = self.cf.mk_op(left);
            let op = self.emit(il::InstructionKind::ScopeRes(left_op, id.id));
            let op = self.emit_inst_if_some(op, id.apply);
            left = op;
        }
        left
    }
    fn emit_inst_if_some(&mut self, op: il::Operand, apply: Option<ast::Apply>) -> il::Operand {
        match apply {
            Some(ast::Apply::Juxtaposition(_)) => todo!(),
            Some(ast::Apply::AngleBrackets(args)) => {
                let op = self.cf.mk_op(op);
                let ops = self.cf.reserve_ops(args.len());
                for (e, op) in args.iter().zip(ops.iter()) {
                    let rhs = self.emit_rhs(e);
                    self.cf.set_reserved_op(op, rhs);
                }
                self.emit(il::InstructionKind::Inst(op, ops))
            }
            None => op,
        }
    }
    fn emit_expr_mk(&mut self, id: ast::ExprId, rhs: bool) -> il::OpId {
        let op = if rhs {
            self.emit_rhs(id)
        } else {
            self.emit_expr(id)
        };
        self.cf.mk_op(op)
    }

    pub fn emit_rhs(&mut self, id: ast::ExprId) -> il::Operand {
        let op = self.emit_expr(id);
        match op {
            il::Operand::Unreachable => unreachable!(),
            il::Operand::Undef => op,
            il::Operand::Const(_) => op,
            il::Operand::Ref(il::OperandRef(_, il::OperandRefKind::Block)) => unreachable!(),
            il::Operand::Ref(il::OperandRef(_, il::OperandRefKind::Param(_))) => op,
            il::Operand::Ref(il::OperandRef(_, il::OperandRefKind::TypeOf(_))) => unreachable!(),
            il::Operand::Ref(il::OperandRef(_, il::OperandRefKind::ResultOf(insn))) => {
                let insn = &self.cf.insns[insn];
                match insn.kind {
                    il::InstructionKind::AmbiValue(_) => {
                        let op = self.cf.mk_op(op);
                        self.emit(il::InstructionKind::Resolve(op))
                    }
                    il::InstructionKind::RValue(_) => op,
                    il::InstructionKind::LValue(_) => {
                        let op = self.cf.mk_op(op);
                        self.emit(il::InstructionKind::RValue(il::RValue::Val(op)))
                    }
                    _ => todo!(),
                }
            }
        }
    }
    pub fn emit_lhs(&mut self, id: ast::ExprId) -> il::Operand {
        todo!()
    }
    pub fn emit_x(&mut self, id: ast::ExprId) -> il::Operand {
        todo!()
    }
    fn emit_exprr(&mut self, id: ast::ExprId) -> il::Operand {
        todo!()
    }
    /*

    */

    fn emit_expr_impl(&mut self, expr: ast::Expression) -> il::InstructionKind {
        match expr.kind {
            ast::ExpressionKind::Use(path) => {
                let op = self.emit_path(path);
                let op = self.cf.mk_op(op);
                self.emit(il::InstructionKind::Resolve(op))
            }
            _ => todo!(),
        }
    }

    pub fn emit_expr(&mut self, id: ast::ExprId) -> il::Operand {
        let expr = self.ast.exprs[id];
        match expr.kind {
            ast::ExpressionKind::Use(path) => {
                let op = self.emit_path(path);
                let op = self.cf.mk_op(op);
                self.emit(il::InstructionKind::Resolve(op))
            }
            ast::ExpressionKind::Literal(lit) => match lit {
                ast::Literal::Number(n) => {
                    let n: i64 = n.try_into().unwrap();
                    self.emit(il::InstructionKind::IntLiteral(n))
                }
                ast::Literal::Char(_) => todo!(),
                ast::Literal::Quote(_) => todo!(),
                ast::Literal::DotId(id, list) => {
                    let id = self.ast.ids[id];
                    let ops = if let Some(list) = list {
                        let ops = self.cf.reserve_ops(list.len());
                        for (e, op) in list.iter().zip(ops.iter()) {
                            let rhs = self.emit_rhs(e);
                            self.cf.set_reserved_op(op, rhs);
                        }
                        Some(ops)
                    } else {
                        None
                    };
                    let op = self.emit(il::InstructionKind::DotId(id.id, ops));
                    self.emit_inst_if_some(op, id.apply)
                }
                ast::Literal::Undef => il::Operand::Undef,
            },
            ast::ExpressionKind::Group(e) => {
                let op = self.emit_expr_mk(e, false);
                self.emit(il::InstructionKind::Paren(op))
            }
            ast::ExpressionKind::Compound(list) => {
                let ops = self.cf.reserve_ops(list.len());
                for (e, op) in list.iter().zip(ops.iter()) {
                    let rhs = self.emit_rhs(e);
                    self.cf.set_reserved_op(op, rhs);
                }
                self.emit(il::InstructionKind::Compound(ops))
            }
            ast::ExpressionKind::Unary(kind, op) => {
                let op = self.emit_expr_mk(op, true);
                match kind {
                    ast::UnaryOpKind::Not => self.emit(il::InstructionKind::RValue(
                        il::RValue::UnArith(il::UnBitArithOpKind::Not, op),
                    )),
                    ast::UnaryOpKind::Neg => {
                        let id = token::mk_ident(self.int, "__neg");
                        self.emit_assoc_call(id, op.as_slice())
                    }
                }
            }
            ast::ExpressionKind::Binary(kind, left, right) => {
                match kind {
                    ast::BinaryOpKind::Add => {
                        let left = self.emit_rhs(left);
                        let right = self.emit_rhs(right);
                        let name = token::mk_ident(self.int, "__add");
                        let ops = self.cf.mk_ops([left, right]);
                        self.emit_assoc_call(name, ops)
                    }
                    ast::BinaryOpKind::Sub => todo!(),
                    ast::BinaryOpKind::Mul => todo!(),
                    ast::BinaryOpKind::Div => {
                        let left = self.emit_expr_mk(left, true);
                        let right = self.emit_expr_mk(right, true);
                        let succ_op = self.cf.reserve_op();
                        let fail_br = il::Branch::with_args(self.cf.op_unreachable(), []);
                        self.emit_terminator(il::TerminatorKind::CheckedBin(
                            il::BinArithOpKind::Div,
                            left,
                            right,
                            il::Branch::with_args(succ_op, []),
                            fail_br,
                        ));
                        let ty = self.il.mk_tyvar();
                        let succ_bb = self.cf.emit_bb_special(self.current_scope_id(), [ty]);
                        self.cf
                            .set_reserved_op(succ_op, il::Operand::block(succ_bb));
                        il::Operand::param(succ_bb, 0)
                    }
                    ast::BinaryOpKind::Mod => todo!(),
                    ast::BinaryOpKind::BitAnd
                    | ast::BinaryOpKind::BitOr
                    | ast::BinaryOpKind::BitXor => {
                        let left = self.emit_expr_mk(left, true);
                        let right = self.emit_expr_mk(right, true);
                        self.emit(il::InstructionKind::RValue(il::RValue::BinArith(
                            match kind {
                                ast::BinaryOpKind::BitAnd => il::BinBitArithOpKind::And,
                                ast::BinaryOpKind::BitOr => il::BinBitArithOpKind::Or,
                                ast::BinaryOpKind::BitXor => il::BinBitArithOpKind::Xor,
                                _ => unreachable!(),
                            },
                            left,
                            right,
                        )))
                    }
                    ast::BinaryOpKind::Shl | ast::BinaryOpKind::Shr => {
                        let left = self.emit_expr_mk(left, true);
                        let right = self.emit_expr_mk(right, true);
                        self.emit(il::InstructionKind::RValue(il::RValue::Shift(
                            match kind {
                                ast::BinaryOpKind::Shl => il::ShiftOpKind::Shl,
                                ast::BinaryOpKind::Shr => il::ShiftOpKind::Shr,
                                // sar rotl rotr
                                _ => unreachable!(),
                            },
                            left,
                            right,
                        )))
                    }
                    ast::BinaryOpKind::Lt
                    | ast::BinaryOpKind::Le
                    | ast::BinaryOpKind::Gt
                    | ast::BinaryOpKind::Ge
                    | ast::BinaryOpKind::Eq
                    | ast::BinaryOpKind::Ne => {
                        let left = self.emit_expr_mk(left, true);
                        let right = self.emit_expr_mk(right, true);
                        //let ty = self.gen.ilgen.mk_type(ildecl::TypeKind::BInt(1));
                        self.emit(
                            il::InstructionKind::RValue(il::RValue::Cmp(
                                match kind {
                                    ast::BinaryOpKind::Lt => il::CmpKind::Lt,
                                    ast::BinaryOpKind::Ge => il::CmpKind::Ge,
                                    ast::BinaryOpKind::Le => il::CmpKind::Le,
                                    ast::BinaryOpKind::Gt => il::CmpKind::Gt,
                                    ast::BinaryOpKind::Eq => il::CmpKind::Eq,
                                    ast::BinaryOpKind::Ne => il::CmpKind::Ne,
                                    _ => unreachable!(),
                                },
                                left,
                                right,
                            )),
                            //  ty,
                        )
                    }
                    ast::BinaryOpKind::BrAnd => {
                        /*
                        condbr left, also, tail[0]
                        also:
                        ..
                        condbr right, tail[1], tail[0]
                        tail:
                        */
                        let ty = self.il.builtin_bint(1);
                        let zero = self.il.const_bint(1, 0);
                        let zero = self.cf.mk_op(il::Operand::Const(zero));
                        let one = self.il.const_bint(1, 1);
                        let one = self.cf.mk_op(il::Operand::Const(one));

                        let tail_op = self.cf.reserve_op();
                        let also_op = self.cf.reserve_op();
                        let false_br = il::Branch::with_args(tail_op, [zero]);

                        let left = self.emit_expr_mk(left, true);
                        let true_br = il::Branch::with_args(tail_op, [one]);
                        self.emit_terminator(il::TerminatorKind::CondBr(
                            left,
                            il::Branch::with_args(also_op, []),
                            false_br.clone(),
                        ));

                        let also = self.emit_bb([]);
                        let right = self.emit_expr_mk(right, true);
                        self.emit_terminator(il::TerminatorKind::CondBr(right, true_br, false_br));

                        let tail = self.emit_bb([ty]);
                        self.cf.set_reserved_op(also_op, il::Operand::block(also));
                        self.cf.set_reserved_op(tail_op, il::Operand::block(tail));

                        il::Operand::param(tail, 0)
                    }
                    ast::BinaryOpKind::BrOr => {
                        /*
                        condbr left, tail[1], or
                        or:
                        ..
                        condbr right, tail[1], tail[0]
                        tail:
                        */
                        let ty = self.il.builtin_bint(1);
                        let zero = self.il.const_bint(1, 0);
                        let zero = self.cf.mk_op(il::Operand::Const(zero));
                        let one = self.il.const_bint(1, 1);
                        let one = self.cf.mk_op(il::Operand::Const(one));

                        let tail_op = self.cf.reserve_op();
                        let true_br = il::Branch::with_args(tail_op, [one]);
                        let false_br = il::Branch::with_args(tail_op, [zero]);

                        let left = self.emit_expr_mk(left, true);
                        let orelse_op = self.cf.reserve_op();
                        self.emit_terminator(il::TerminatorKind::CondBr(
                            left,
                            true_br.clone(),
                            il::Branch::with_args(orelse_op, []),
                        ));

                        let orelse = self.emit_bb([]);
                        let right = self.emit_expr_mk(right, true);
                        self.emit_terminator(il::TerminatorKind::CondBr(right, true_br, false_br));

                        let tail = self.emit_bb([ty]);
                        self.cf
                            .set_reserved_op(orelse_op, il::Operand::block(orelse));
                        self.cf.set_reserved_op(tail_op, il::Operand::block(tail));

                        il::Operand::param(tail, 0)
                    }
                    ast::BinaryOpKind::Index => {
                        let left = self.emit_expr_mk(left, false);
                        let right = self.emit_expr_mk(right, true);
                        self.emit(il::InstructionKind::Subscript(left, right))
                    }
                }
            }
            ast::ExpressionKind::Try(_) => todo!(),
            ast::ExpressionKind::Yield(op) => {
                let op = self.emit_expr_mk(op, true);

                let succ0 = self.cf.reserve_op();
                self.emit_linear(il::LinearTerminatorKind::Yield(op), il::Branch::new(succ0));

                let ty = self.il.mk_tyvar();
                let succ0_bb = self.cf.emit_bb([ty]);
                let succ1 = self.emit_linear_reserved(il::LinearTerminatorKind::Fall);
                let succ1_bb = self.cf.emit_bb([]);

                self.cf.set_reserved_op(succ0, il::Operand::block(succ0_bb));
                self.cf.set_reserved_op(succ1, il::Operand::block(succ1_bb));

                il::Operand::param(succ0_bb, 0)
            }
            ast::ExpressionKind::Await(_, _) => todo!(),
            ast::ExpressionKind::Call(callee, args) => {
                let callee = self.emit_expr_mk(callee, true);
                let ops = self.cf.reserve_ops(args.len());
                for (e, op) in args.iter().zip(ops.iter()) {
                    let rhs = self.emit_rhs(e);
                    self.cf.set_reserved_op(op, rhs);
                }

                let succ0 = self.cf.reserve_op();
                self.emit_linear(
                    il::LinearTerminatorKind::Call(callee, ops),
                    il::Branch::new(succ0),
                );

                let ty = self.il.mk_tyvar();
                let succ0_bb = self.cf.emit_bb([ty]);
                let succ1 = self.emit_linear_reserved(il::LinearTerminatorKind::Fall);
                let succ1_bb = self.cf.emit_bb([]);

                self.cf.set_reserved_op(succ0, il::Operand::block(succ0_bb));
                self.cf.set_reserved_op(succ1, il::Operand::block(succ1_bb));

                il::Operand::param(succ0_bb, 0)
            }
            ast::ExpressionKind::Field(op, field) => {
                let op = self.emit_expr_mk(op, false);
                self.emit(il::InstructionKind::Field(
                    op,
                    il::WithSymbol {
                        t: field,
                        symbol: None,
                    },
                ))
            }
            ast::ExpressionKind::MethodCall(rx, callee, args) => {
                todo!()
            }
            ast::ExpressionKind::Block(blk) => self.emit_block(blk),
            ast::ExpressionKind::IfLet(pat, expr, then, els) => {
                todo!()
            }
            ast::ExpressionKind::If(cond, then, els) => {
                /*
                'if {
                    {
                        flag=<cond>
                        if flag {
                            { <then> }
                            <break 'if>
                            exit
                        }
                        exit
                    }
                    <else>
                    exit
                }
                */

                let bb = self.emit_enter();
                let (if_then_bb, if_then_scope) = self.emit_enter(ildecl::ScopeKind::Block);
                let cond = self.emit_expr_mk(cond, true);
                let then_header = self.cf.reserve_op();
                let exit_point = self.find_exit(if_then_scope);
                self.emit_terminator(il::TerminatorKind::If(
                    cond,
                    il::Branch::with_args(then_header, []),
                ));

                let (then_header_bb, then_scope) = self.emit_enter(ildecl::ScopeKind::Block);
                let then_val = self.emit_expr_mk(then, true);
                let then_exit = self.emit_exit();
                let if_then_exit = self.cf.emit_bb(if_then_scope, []);

                if els.is_some() {
                    let exit_point = self.find_exit(if_scope);
                    let img = self.cf.reserve_op();
                    self.emit_terminator(il::TerminatorKind::BrFall(
                        il::Branch::with_args(exit_point, []),
                        il::Branch::with_args(img, []),
                    ));
                    let bb = self.emit_bb([]);
                    self.cf.set_reserved_op(img, il::Operand::block(bb));
                }

                let if_then_post = self.emit_exit();

                self.cf
                    .set_reserved_op(then_header, il::Operand::block(then_header_bb));
                self.cf
                    .set_reserved_op(then_exit, il::Operand::block(if_then_exit));

                if let Some(els) = els {
                    let else_header = self.cf.emit_bb(if_scope, []);
                    let if_then_exit = self.emit_exit();
                    let else_val = self.emit_expr_mk(els, true);

                    self.cf
                        .set_reserved_op(if_then_post, il::Operand::block(else_header));
                }

                let if_exit = self.emit_exit();
                let exit = self.cf.emit_bb(scobe, []);
                self.cf.set_reserved_op(if_exit, il::Operand::block(exit));

                il::Operand::Unreachable
            }
            ast::ExpressionKind::Match(op, cases, bodies) => {
                assert!(cases.len() == bodies.len());
                let op = self.emit_expr_mk(op, true);
                let mut casesvec = Vec::with_capacity(cases.len());
                let opss = self.cf.reserve_ops(cases.len() + bodies.len());
                for i in 0..opss.len() / 2 {
                    let case = opss.nth(i * 2);
                    let body = opss.nth(i * 2 + 1);
                    casesvec.push((case, il::Branch::with_args(body, [])));
                }
                self.emit_terminator(il::TerminatorKind::Switch(op, casesvec));

                let tail = self.cf.reserve_op();

                for (case, body) in cases.iter().zip(bodies.iter()) {
                    let bb = self.cf.emit_bb([]);
                    // .....?
                    //self.emit_pat_irrefutable(ildecl::SymbolKind::Local, case, op);
                    let body = self.emit_expr_mk(body, true);
                    self.emit_terminator(il::TerminatorKind::Br(il::Branch::with_args(
                        tail,
                        [body],
                    )));
                }

                let ty = self.il.mk_tyvar();
                let bb = self.cf.emit_bb([ty]);

                il::Operand::param(bb, 0)
            }
            ast::ExpressionKind::Do(body) => {
                todo!()
            }
            ast::ExpressionKind::While(cond, body) => il::Operand::Unreachable,
            ast::ExpressionKind::For(_, _, _, _) => todo!(),
        }
    }

    fn emit_linear_reserved(&mut self, kind: il::LinearTerminatorKind) -> il::OpId {
        let op = self.cf.reserve_op();
        self.emit_linear(kind, il::Branch::new(op));
        op
    }

    fn emit_enter(&mut self) -> il::BbId {
        let target = self.emit_linear_reserved(il::LinearTerminatorKind::Enter);
        let bb = self.cf.emit_bb([]);
        self.cf.set_reserved_op(target, il::Operand::block(bb));
        bb
    }
    fn emit_exit(&mut self) -> il::BbId {
        self.emit(il::InstructionKind::ExecDefers);
        let target = self.emit_linear_reserved(il::LinearTerminatorKind::Exit);
        let bb = self.cf.emit_bb([]);
        self.cf.set_reserved_op(target, il::Operand::block(bb));
        bb
    }
    fn emit_early_exit(&mut self) -> il::BbId {
        let target = self.emit_linear_reserved(il::LinearTerminatorKind::Exit);
        let bb = self.cf.emit_bb([]);
        self.cf.set_reserved_op(target, il::Operand::block(bb));
        bb
    }
}
