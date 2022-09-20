use data_structures::{indexvec::ElementIndex, interner::Interner};

use crate::syntax::{
    ast::{
        Ast, EnumItem, ExprId, Expression, ExpressionKind, Field, FieldId, IdId, IdListId, ItemId,
        ItemKind, ModuleItem, PatDeclId, PatId, Pattern, PatternDecl, PatternKind, SourceFileId,
        StmtId, StructItem, UnionItem,
    },
    ast_visitor::AstVisitor,
    token::Ident,
};

use super::{
    sema::{Sema},
    symbol_table::{AstNodeId},
};

pub struct NameAnalyzer<'a> {
    sema: &'a mut Sema,
    scope_stack: Vec<ScopeId>,
    enclosing_scope_stack: Vec<ScopeId>,
}

/*
struct ScopeStack {
    stack: Vec<ScopeId>,
    enclosing_stack: Vec<ScopeId>,
}

impl ScopeStack {
    fn mk_scope(&self, kind: ScopeKind) -> Scope {
        let enclosing = self.stack.last().copied();
        let pred = self.enclosing_stack.last().copied();
        Scope {
            pred,
            enclosing,
            kind,
        }
    }
    fn next_scope(&mut self, kind: ScopeKind, sema: &mut Sema) {
        let scope = self.mk_scope(kind);
        let scope_id = sema.scopes.alloc(scope);
        self.stack.push(scope_id);
    }
    fn push_scope(&mut self, kind: ScopeKind, sema: &mut Sema) {
        let scope = self.mk_scope(kind);
        let scope_id = sema.scopes.alloc(scope);
        self.stack.push(scope_id);
        self.enclosing_stack.push(scope_id);
    }
    fn pop_scope(&mut self) {
        let enclosing = self.enclosing_stack.pop().unwrap();
        loop {
            let id = self.stack.pop().unwrap();
            if enclosing == id {
                break;
            }
        }
    }
}
*/

pub type ScopeId = ElementIndex<Scope>;

pub struct Scope {
    pub kind: ScopeKind,
    pub pred: Option<ScopeId>,
    pub enclosing: Option<ScopeId>,
}

#[derive(Clone, Copy, Hash, Debug, PartialEq, Eq)]
pub enum ScopeKind {
    Root,
    SourceFile(SourceFileId),
    Item(ItemId), // module + types?
    Block(ExprId),
    Loop(ExprId),
    Pat(PatId), // replace with block
}

// enclosing_file
// enclosing_scope

impl<'a> NameAnalyzer<'a> {
    pub fn analyze(sema: &'a mut Sema) {
        let mut this = Self {
            sema,
            scope_stack: Vec::new(),
            enclosing_scope_stack: Vec::new(),
        };
        this.visit_root();
    }
    fn mk_scope(&mut self, kind: ScopeKind) -> Scope {
        Scope {
            pred: self.scope_stack.last().copied(),
            enclosing: self.enclosing_scope_stack.last().copied(),
            kind,
        }
    }
    fn next_scope(&mut self, kind: ScopeKind) -> ScopeId {
        let scope = self.mk_scope(kind);
        let scope_id = self.sema.scopes.alloc(scope);
        self.scope_stack.push(scope_id);
        scope_id
    }
    fn push_scope(&mut self, kind: ScopeKind) -> ScopeId {
        let scope_id = self.next_scope(kind);
        self.enclosing_scope_stack.push(scope_id);
        scope_id
    }
    fn pop_scope(&mut self) {
        // let old_len =
        // self.scope_stack.truncate(old_len);
        let enclosing = self.enclosing_scope_stack.pop().unwrap();
        loop {
            let id = self.scope_stack.pop().unwrap();
            if enclosing == id {
                break;
            }
        }
    }

    fn visit_patates(&mut self, node: AstNodeId) {
        let id = match node {
            AstNodeId::Null
            | AstNodeId::File(_)
            | AstNodeId::Item(_)
            | AstNodeId::Variant(_)
            | AstNodeId::Field(_) => panic!(),
            AstNodeId::Local(id)
            | AstNodeId::Param(id)
            | AstNodeId::GenericParam(id)
            | AstNodeId::Enumerator(id) => id,
        };
        fn mk_another(id: PatId, kind: AstNodeId) -> AstNodeId {
            match kind {
                AstNodeId::Null
                | AstNodeId::File(_)
                | AstNodeId::Item(_)
                | AstNodeId::Variant(_)
                | AstNodeId::Field(_) => panic!(),
                AstNodeId::Local(_) => AstNodeId::Local(id),
                AstNodeId::Param(_) => AstNodeId::Param(id),
                AstNodeId::GenericParam(_) => AstNodeId::GenericParam(id),
                AstNodeId::Enumerator(_) => AstNodeId::Enumerator(id),
            }
        }
        let Pattern { kind, span: _ } = &mut self.sema.ast.pats[id];
        match kind {
            PatternKind::Wildcard | PatternKind::Rest => {}
            PatternKind::Bind(_, name) => {
                name.symbol =
                    self.sema
                        .symbol_table
                        .declare(name.t, *self.scope_stack.last().unwrap(), node);
            }
            PatternKind::DotId(name, pat) => {
                name.symbol =
                    self.sema
                        .symbol_table
                        .declare(name.t, *self.scope_stack.last().unwrap(), node);
                if let &mut Some(pat) = pat {
                    self.visit_patates(mk_another(pat, node));
                }
            }
            &mut PatternKind::Group(pat) => self.visit_patates(mk_another(pat, node)),
            &mut PatternKind::Constant(e) => {
                self.visit_expr(e);
            }
            PatternKind::Compound(pats) => {
                for pat in pats.iter() {
                    self.visit_patates(mk_another(pat, node));
                }
            }
        }
    }
}

impl AstVisitor<()> for NameAnalyzer<'_> {
    fn ast(&self) -> &Ast {
        &self.sema.ast
    }
    fn int(&self) -> &Interner {
        &self.sema.interner
    }

    fn visit_root(&mut self) {
        self.push_scope(ScopeKind::Root);
        self.traverse().visit_root();
        self.pop_scope();
    }

    fn visit_ident(&mut self, id: Ident) {
        self.traverse().visit_ident(id)
    }

    fn visit_path(&mut self, id: IdListId) {
        self.traverse().visit_path(id)
    }

    fn visit_id(&mut self, id: IdId) {
        self.traverse().visit_id(id)
    }

    fn visit_source_file(&mut self, id: SourceFileId) {
        let file = &mut self.sema.ast.files[id];
        let sub_scope_fwd = self.sema.scopes.next_id();
        file.name.symbol = self.sema.symbol_table.declare(
            file.name.t,
            *self.enclosing_scope_stack.last().unwrap(),
            AstNodeId::File(id),
        );
        file.sub_scope = sub_scope_fwd;

        let file = *file;
        self.visit_ident(file.name.t);
        let sub_scope = self.push_scope(ScopeKind::SourceFile(id));
        assert!(sub_scope == sub_scope_fwd);
        self.traverse()
            .helper_user_visit_source_file_kind(file.kind);
        self.pop_scope();
    }

    fn visit_item(&mut self, id: ItemId) {
        let item = &mut self.sema.ast.items[id];
        let sub_scope_fwd = self.sema.scopes.next_id();
        item.name.symbol = self.sema.symbol_table.declare(
            item.name.t,
            *self.enclosing_scope_stack.last().unwrap(),
            AstNodeId::Item(id),
        );
        match &mut item.kind {
            ItemKind::Const(_) => {}
            ItemKind::Module(ModuleItem(_, scope))
            | ItemKind::Struct(StructItem(_, scope))
            | ItemKind::Union(UnionItem(_, scope))
            | ItemKind::Enum(EnumItem(_, scope)) => {
                *scope = sub_scope_fwd;
            }
            ItemKind::Func(_) => {}
        }

        let item = *item;
        self.visit_ident(item.name.t);
        let sub_scope = self.push_scope(ScopeKind::Item(id));
        assert!(sub_scope == sub_scope_fwd);
        self.traverse()
            .helper_visit_opt(item.generics, |usr, list| {
                usr.traverse()
                    .helper_visit_list(list, Self::visit_generic_param)
            });
        self.traverse().helper_user_visit_item_body(item.kind);
        self.pop_scope();
    }

    fn visit_stmt(&mut self, id: StmtId) {
        self.traverse().visit_stmt(id);
    }

    fn visit_expr(&mut self, id: ExprId) {
        let Expression {
            kind,
            span: _,
        } = self.ast().exprs[id];
        match kind {
            ExpressionKind::Use(id_id, _) => {
                let scope = self.scope_stack.last().copied().unwrap();
                self.sema.ast.exprs[id].kind = ExpressionKind::Use(id_id, scope);
                self.traverse().visit_expr(id);
            }
            ExpressionKind::Do(_) => {
                self.push_scope(ScopeKind::Block(id));
                self.traverse().visit_expr(id);
                self.pop_scope();
            }
            ExpressionKind::Block(_) => {
                self.push_scope(ScopeKind::Block(id));
                self.traverse().visit_expr(id);
                self.pop_scope();
            }
            ExpressionKind::If(_, _, _) => {
                self.push_scope(ScopeKind::Block(id));
                self.traverse().visit_expr(id);
                self.pop_scope();
            }
            ExpressionKind::While(_, _) => {
                self.push_scope(ScopeKind::Loop(id));
                self.traverse().visit_expr(id);
                self.pop_scope();
            }
            ExpressionKind::For(pat, in_, with, body) => {
                self.push_scope(ScopeKind::Loop(id));
                self.visit_expr(in_);
                self.traverse().helper_visit_opt(with, Self::visit_expr);
                self.next_scope(ScopeKind::Pat(pat));
                self.visit_patates(AstNodeId::Local(pat));
                self.visit_expr(body);
                self.pop_scope();
            }
            ExpressionKind::Match(op, cases, bodies) => {
                self.push_scope(ScopeKind::Block(id));
                self.visit_expr(op);
                for (case, body) in cases.iter().zip(bodies.iter()) {
                    self.push_scope(ScopeKind::Pat(case));
                    self.visit_patates(AstNodeId::Local(case));
                    self.visit_expr(body);
                    self.pop_scope();
                }
                self.pop_scope();
            }
            ExpressionKind::Case(expr, pat) => {
                self.visit_expr(expr);
                self.next_scope(ScopeKind::Pat(pat));
                // FIXME: pat is not always a local, it depends on expr
                self.visit_patates(AstNodeId::Local(pat));
            }
            ExpressionKind::Literal(_)
            | ExpressionKind::Compound(_)
            | ExpressionKind::Group(_)
            | ExpressionKind::Unary(_, _)
            | ExpressionKind::Binary(_, _, _)
            | ExpressionKind::Try(_)
            | ExpressionKind::Yield(_)
            | ExpressionKind::Await(_, _)
            | ExpressionKind::Call(_, _)
            | ExpressionKind::Field(_, _)
            | ExpressionKind::MethodCall(_, _, _) => self.traverse().visit_expr(id),
        }
    }

    fn visit_pat(&mut self, _: PatId) {
        panic!("visit_pat may not be called")
    }

    fn visit_local(&mut self, id: PatDeclId) {
        let PatternDecl {
            pat,
            ty_annot,
            expr,
        } = self.ast().patdecls[id];
        self.traverse().helper_visit_opt(ty_annot, Self::visit_expr);
        self.traverse().helper_visit_opt(expr, Self::visit_expr);
        self.next_scope(ScopeKind::Pat(pat));
        self.visit_patates(AstNodeId::Local(pat));
    }

    fn visit_param(&mut self, id: PatDeclId) {
        let PatternDecl {
            pat,
            ty_annot,
            expr,
        } = self.ast().patdecls[id];
        self.traverse().helper_visit_opt(ty_annot, Self::visit_expr);
        self.traverse().helper_visit_opt(expr, Self::visit_expr);
        self.next_scope(ScopeKind::Pat(pat));
        self.visit_patates(AstNodeId::Param(pat));
    }

    fn visit_generic_param(&mut self, id: PatDeclId) {
        let PatternDecl {
            pat,
            ty_annot: ty,
            expr,
        } = self.ast().patdecls[id];
        self.traverse().helper_visit_opt(ty, Self::visit_expr);
        self.traverse().helper_visit_opt(expr, Self::visit_expr);
        self.visit_patates(AstNodeId::GenericParam(pat));
    }

    fn visit_enumerator(&mut self, id: PatDeclId) {
        let PatternDecl {
            pat,
            ty_annot,
            expr,
        } = self.ast().patdecls[id];
        self.traverse().helper_visit_opt(ty_annot, Self::visit_expr);
        self.traverse().helper_visit_opt(expr, Self::visit_expr);
        self.visit_patates(AstNodeId::Enumerator(pat));
    }

    fn visit_variant(&mut self, id: FieldId) {
        let Field { name, ty: _ } = &mut self.sema.ast.fields[id];
        name.symbol = self.sema.symbol_table.declare(
            name.t,
            *self.enclosing_scope_stack.last().unwrap(),
            AstNodeId::Variant(id),
        );
        self.traverse().visit_variant(id);
    }

    fn visit_field(&mut self, id: FieldId) {
        let Field { name, ty: _ } = &mut self.sema.ast.fields[id];
        name.symbol = self.sema.symbol_table.declare(
            name.t,
            *self.enclosing_scope_stack.last().unwrap(),
            AstNodeId::Field(id),
        );
        self.traverse().visit_field(id);
    }
}
