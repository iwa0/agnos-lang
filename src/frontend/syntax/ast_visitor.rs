use data_structures::{
    indexvec::{ElementIndex, SliceIndex},
    interner::Interner,
};

use crate::syntax::ast::{
    Apply, ExprId, ExpressionKind, FieldId, IdId, IdListId, ItemBody, ItemId, Literal, PatDeclId,
    PatId, PatternKind, SourceFileId, SourceFileKind, StatementKind, StmtId,
};

use super::{
    ast::{Ast, Block, Expression, Field, Id, Item, Pattern, PatternDecl, SourceFile, Statement, SourceFileListId},
    token::Ident,
};

pub trait AstVisitor<T> {
    fn ast(&self) -> &Ast;
    fn int(&self) -> &Interner;

    fn traverse(&mut self) -> AstTraverser<'_, Self>
    where
        Self: AstVisitor<()>,
    {
        AstTraverser { user: self }
    }

    fn visit_opt<U, F>(&mut self, elem: Option<U>, apply: F)
    where
        F: Fn(&mut Self, U) -> T,
    {
        if let Some(elem) = elem {
            apply(self, elem);
        }
    }

    fn visit_list<U, F>(&mut self, list: SliceIndex<U>, apply: F)
    where
        F: Fn(&mut Self, ElementIndex<U>) -> T,
    {
        for elem in list.iter() {
            apply(self, elem);
        }
    }

    fn visit_root(&mut self, id: SliceIndex<SourceFile>);

    fn visit_ident(&mut self, id: Ident) -> T;
    fn visit_path(&mut self, id: IdListId) -> T;
    fn visit_id(&mut self, id: IdId) -> T;
    fn visit_source_file(&mut self, id: SourceFileId) -> T;
    fn visit_item(&mut self, id: ItemId) -> T;
    fn visit_stmt(&mut self, id: StmtId) -> T;
    fn visit_expr(&mut self, id: ExprId) -> T;
    fn visit_pat(&mut self, id: PatId) -> T;
    fn visit_local(&mut self, id: PatDeclId) -> T;
    fn visit_param(&mut self, id: PatDeclId) -> T;
    fn visit_generic_param(&mut self, id: PatDeclId) -> T;
    fn visit_enum_field(&mut self, id: PatDeclId) -> T;
    fn visit_field(&mut self, id: FieldId) -> T;
}

pub struct AstTraverser<'a, User: ?Sized + AstVisitor<()>> {
    user: &'a mut User,
}

impl<'a, User: ?Sized + AstVisitor<()>> AstTraverser<'_, User> {
    pub fn helper_user_visit_patdecl(&mut self, id: PatDeclId) {
        let PatternDecl {
            pat,
            mutable: _,
            ty,
            expr,
        } = self.ast().patdecls[id];
        self.user.visit_pat(pat);
        self.user.visit_opt(ty, User::visit_expr);
        self.user.visit_opt(expr, User::visit_expr);
    }
    pub fn helper_user_visit_source_file_kind(&mut self, kind: SourceFileKind) {
        match kind {
            SourceFileKind::Dir(dirs) => self.user.visit_list(dirs, User::visit_source_file),
            SourceFileKind::File(Block { stmts, kind: _ }) => {
                self.user.visit_list(stmts, User::visit_stmt)
            }
        }
    }
    pub fn helper_user_visit_item_body(&mut self, body: ItemBody) {
        match body {
            ItemBody::Const(expr) => self.user.visit_expr(expr),
            ItemBody::Module(body) => self.user.visit_expr(body),
            ItemBody::Struct(body) => self.user.visit_list(body, User::visit_field),
            ItemBody::Union(body) => self.user.visit_list(body, User::visit_field),
            ItemBody::Enum(body) => self.user.visit_list(body, User::visit_enum_field),
            ItemBody::Func {
                params,
                ret_ty,
                body,
            } => {
                self.user.visit_list(params, User::visit_param);
                self.user.visit_opt(ret_ty, User::visit_expr);
                self.user.visit_expr(body);
            }
        }
    }
}

impl<User: ?Sized + AstVisitor<()>> AstVisitor<()> for AstTraverser<'_, User> {
    fn ast(&self) -> &Ast {
        self.user.ast()
    }
    fn int(&self) -> &Interner {
        self.user.int()
    }

    fn visit_root(&mut self, id: SourceFileListId) {
        self.user.visit_list(id, User::visit_source_file);
    }

    fn visit_ident(&mut self, _id: Ident) {}

    fn visit_path(&mut self, id: IdListId) {
        self.user.visit_list(id, User::visit_id)
    }

    fn visit_id(&mut self, id: IdId) {
        let Id { id, apply } = self.ast().ids[id];
        self.user.visit_ident(id.id);
        self.user.visit_opt(apply, |usr, apply| match apply {
            Apply::Juxtaposition(arg) => usr.visit_expr(arg),
            Apply::AngleBrackets(args) => usr.visit_list(args, User::visit_expr),
            Apply::Infer => {}
        });
    }

    fn visit_source_file(&mut self, id: SourceFileId) {
        let SourceFile { name, kind } = self.ast().src_files[id];
        self.user.visit_ident(name);
        self.helper_user_visit_source_file_kind(kind)
    }

    fn visit_item(&mut self, id: ItemId) {
        let Item {
            vis: _,
            name,
            generics,
            body,
        } = self.ast().items[id];
        self.user.visit_ident(name);
        self.user.visit_opt(generics, |usr, list| {
            usr.visit_list(list, User::visit_generic_param)
        });
        self.helper_user_visit_item_body(body);
    }

    fn visit_stmt(&mut self, id: StmtId) {
        let Statement { kind, span: _ } = self.ast().stmts[id];
        match kind {
            StatementKind::Item(item) => self.user.visit_item(item),
            StatementKind::Local(patdecl) => self.user.visit_local(patdecl),
            StatementKind::Defer(body) => self.user.visit_expr(body),
            StatementKind::Break(label, val) => {
                self.user.visit_opt(label.id, User::visit_ident);
                self.user.visit_opt(val, User::visit_expr);
            }
            StatementKind::Continue(label, val) => {
                self.user.visit_opt(label.id, User::visit_ident);
                self.user.visit_opt(val, User::visit_expr);
            }
            StatementKind::Return(val) => self.user.visit_opt(val, User::visit_expr),
            StatementKind::Assign(left, right) => {
                self.user.visit_expr(left);
                self.user.visit_expr(right);
            }
            StatementKind::Expr(expr) => self.user.visit_expr(expr),
        }
    }

    fn visit_expr(&mut self, id: ExprId) {
        let Expression { kind, span: _ } = self.ast().exprs[id];
        match kind {
            ExpressionKind::Use(path) => self.user.visit_path(path),
            ExpressionKind::Literal(lit) => match lit {
                Literal::Number(_) => {}
                Literal::Char(_) => {}
                Literal::Quote(_) => {}
                Literal::DotId(path, exprs) => {
                    self.user.visit_path(path);
                    self.user
                        .visit_opt(exprs, |usr, list| usr.visit_list(list, User::visit_expr));
                }
                Literal::Undef => {}
            },
            ExpressionKind::Group(sub) => self.user.visit_expr(sub),
            ExpressionKind::Compound(list) => self.user.visit_list(list, User::visit_expr),
            ExpressionKind::Unary(_, op) => self.user.visit_expr(op),
            ExpressionKind::Binary(_, left, right) => {
                self.user.visit_expr(left);
                self.user.visit_expr(right);
            }
            ExpressionKind::Call(callee, args) => {
                self.user.visit_expr(callee);
                self.user.visit_list(args, User::visit_expr);
            }
            ExpressionKind::Field(left, right) => {
                self.user.visit_expr(left);
                self.user.visit_ident(right.id);
            }
            ExpressionKind::MethodCall(receiver, method, args) => {
                self.user.visit_expr(receiver);
                self.user.visit_id(method);
                self.user.visit_list(args, User::visit_expr);
            }
            ExpressionKind::Case(expr, pat) => {
                self.user.visit_expr(expr);
                self.user.visit_pat(pat);
            }
            ExpressionKind::Block(Block { stmts, kind: _ }) => {
                self.user.visit_list(stmts, User::visit_stmt);
            }
            ExpressionKind::If(cond, then, els) => {
                self.user.visit_expr(cond);
                self.user.visit_expr(then);
                self.user.visit_opt(els, User::visit_expr);
            }
            ExpressionKind::Match(op, cases, bodies) => {
                self.user.visit_expr(op);
                for (case, body) in cases.iter().zip(bodies.iter()) {
                    self.user.visit_pat(case);
                    self.user.visit_expr(body);
                }
            }
            ExpressionKind::Do(body) => self.user.visit_expr(body),
            ExpressionKind::While(cond, body) => {
                self.user.visit_expr(cond);
                self.user.visit_expr(body);
            }
            ExpressionKind::For(pat, in_, with, body) => {
                self.user.visit_pat(pat);
                self.user.visit_expr(in_);
                self.user.visit_opt(with, User::visit_expr);
                self.user.visit_expr(body);
            }
        }
    }

    fn visit_pat(&mut self, id: PatId) {
        let Pattern { kind, span: _ } = self.ast().pats[id];
        match kind {
            PatternKind::Wildcard => {}
            PatternKind::Rest => {}
            PatternKind::Constant(expr) => self.user.visit_expr(expr),
            PatternKind::Group(pat) => self.user.visit_pat(pat),
            PatternKind::Bind(id) => self.user.visit_ident(id),
            PatternKind::DotId(id, pat) => {
                self.user.visit_ident(id);
                self.user.visit_opt(pat, User::visit_pat);
            }
            PatternKind::Compound(pats) => self.user.visit_list(pats, User::visit_pat),
        }
    }

    fn visit_local(&mut self, id: PatDeclId) {
        self.helper_user_visit_patdecl(id)
    }

    fn visit_param(&mut self, id: PatDeclId) {
        self.helper_user_visit_patdecl(id)
    }

    fn visit_generic_param(&mut self, id: PatDeclId) {
        self.helper_user_visit_patdecl(id)
    }

    fn visit_enum_field(&mut self, id: PatDeclId) {
        self.helper_user_visit_patdecl(id)
    }

    fn visit_field(&mut self, id: FieldId) {
        let Field { name, ty } = self.ast().fields[id];
        self.user.visit_ident(name);
        self.user.visit_expr(ty);
    }
}
