use data_structures::{
    indexvec::{ElementIndex, SliceIndex},
    interner::Interner,
};

use crate::syntax::ast::{
    Apply, ExprId, ExpressionKind, FieldId, IdId, IdListId, ItemId, ItemKind, Literal, PatDeclId,
    PatId, PatternKind, SourceFileId, SourceFileKind, StatementKind, StmtId,
};

use super::{
    ast::{
        Ast, Block, ConstItem, EnumItem, Expression, Field, FuncItem, Id, Item, ModuleItem,
        Pattern, PatternDecl, SourceFile, Statement, StructItem, UnionItem,
    },
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

    fn visit_root(&mut self);
    fn visit_ident(&mut self, id: Ident);
    fn visit_path(&mut self, id: IdListId) -> T;
    fn visit_id(&mut self, id: IdId) -> T;
    fn visit_source_file(&mut self, id: SourceFileId);
    fn visit_item(&mut self, id: ItemId);
    fn visit_stmt(&mut self, id: StmtId);
    fn visit_expr(&mut self, id: ExprId) -> T;
    fn visit_pat(&mut self, id: PatId);
    fn visit_local(&mut self, id: PatDeclId);
    fn visit_param(&mut self, id: PatDeclId);
    fn visit_generic_param(&mut self, id: PatDeclId);
    fn visit_enumerator(&mut self, id: PatDeclId);
    fn visit_variant(&mut self, id: FieldId);
    fn visit_field(&mut self, id: FieldId);
}

pub struct AstTraverser<'a, User: ?Sized + AstVisitor<()>> {
    user: &'a mut User,
}

impl<'a, User: ?Sized + AstVisitor<()>> AstTraverser<'_, User> {
    pub fn helper_visit_opt<U, F>(&mut self, elem: Option<U>, apply: F)
    where
        F: Fn(&mut User, U) -> (),
    {
        if let Some(elem) = elem {
            apply(&mut self.user, elem);
        }
    }
    pub fn helper_visit_list<U, F>(&mut self, list: SliceIndex<U>, apply: F)
    where
        F: Fn(&mut User, ElementIndex<U>) -> (),
    {
        for elem in list.iter() {
            apply(&mut self.user, elem);
        }
    }
    pub fn helper_user_visit_patdecl(&mut self, id: PatDeclId) {
        let PatternDecl {
            pat,
            ty_annot: ty,
            expr,
        } = self.ast().patdecls[id];
        self.user.visit_pat(pat);
        self.user.traverse().helper_visit_opt(ty, User::visit_expr);
        self.user
            .traverse()
            .helper_visit_opt(expr, User::visit_expr);
    }
    pub fn helper_user_visit_source_file_kind(&mut self, kind: SourceFileKind) {
        match kind {
            SourceFileKind::Dir(dirs) => self
                .user
                .traverse()
                .helper_visit_list(dirs, User::visit_source_file),
            SourceFileKind::File(Block { stmts, kind: _ }) => self
                .user
                .traverse()
                .helper_visit_list(stmts, User::visit_stmt),
        }
    }
    pub fn helper_user_visit_item_body(&mut self, body: ItemKind) {
        match body {
            ItemKind::Const(ConstItem(annot_expr, expr)) => {
                self.user
                    .traverse()
                    .helper_visit_opt(annot_expr, User::visit_expr);
                self.user.visit_expr(expr);
            }
            ItemKind::Module(ModuleItem(body, _)) => self
                .user
                .traverse()
                .helper_visit_list(body.stmts, User::visit_stmt),
            ItemKind::Struct(StructItem(body, _)) => self
                .user
                .traverse()
                .helper_visit_list(body, User::visit_field),
            ItemKind::Union(UnionItem(body, _)) => self
                .user
                .traverse()
                .helper_visit_list(body, User::visit_variant),
            ItemKind::Enum(EnumItem(body, _)) => self
                .user
                .traverse()
                .helper_visit_list(body, User::visit_enumerator),
            ItemKind::Func(FuncItem(params, ret_ty, body)) => {
                self.user
                    .traverse()
                    .helper_visit_list(params, User::visit_param);
                self.user
                    .traverse()
                    .helper_visit_opt(ret_ty, User::visit_expr);
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

    fn visit_root(&mut self) {
        let root = self.ast().root;
        self.user
            .traverse()
            .helper_visit_list(root, User::visit_source_file);
    }

    fn visit_ident(&mut self, _id: Ident) {}

    fn visit_path(&mut self, id: IdListId) {
        self.user.traverse().helper_visit_list(id, User::visit_id)
    }

    fn visit_id(&mut self, id: IdId) {
        let Id { id, apply } = self.ast().ids[id];
        self.user.visit_ident(id.t);
        self.user
            .traverse()
            .helper_visit_opt(apply, |usr, apply| match apply {
                Apply::Juxtaposition(arg) => usr.visit_expr(arg),
                Apply::AngleBrackets(args) => {
                    usr.traverse().helper_visit_list(args, User::visit_expr)
                }
            });
    }

    fn visit_source_file(&mut self, id: SourceFileId) {
        let SourceFile {
            name,
            kind,
            sub_scope: _,
        } = self.ast().files[id];
        self.user.visit_ident(name.t);
        self.helper_user_visit_source_file_kind(kind)
    }

    fn visit_item(&mut self, id: ItemId) {
        let Item {
            vis: _,
            name,
            generics,
            kind: body,
        } = self.ast().items[id];
        self.user.visit_ident(name.t);
        self.user
            .traverse()
            .helper_visit_opt(generics, |usr, list| {
                usr.traverse()
                    .helper_visit_list(list, User::visit_generic_param)
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
                self.user
                    .traverse()
                    .helper_visit_opt(label, User::visit_ident);
                self.user.traverse().helper_visit_opt(val, User::visit_expr);
            }
            StatementKind::Continue(label, val) => {
                self.user
                    .traverse()
                    .helper_visit_opt(label, User::visit_ident);
                self.user.traverse().helper_visit_opt(val, User::visit_expr);
            }
            StatementKind::Return(val) => {
                self.user.traverse().helper_visit_opt(val, User::visit_expr)
            }
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
            ExpressionKind::Use(path, _) => self.user.visit_path(path),
            ExpressionKind::Literal(lit) => match lit {
                Literal::Number(_) => {}
                Literal::Char(_) => {}
                Literal::Quote(_) => {}
                Literal::DotId(id, exprs) => {
                    self.user.visit_id(id);
                    self.user.traverse().helper_visit_opt(exprs, |usr, list| {
                        usr.traverse().helper_visit_list(list, User::visit_expr)
                    });
                }
                Literal::Undef => {}
            },
            ExpressionKind::Group(sub) => self.user.visit_expr(sub),
            ExpressionKind::Compound(list) => self
                .user
                .traverse()
                .helper_visit_list(list, User::visit_expr),
            ExpressionKind::Unary(_, op) => self.user.visit_expr(op),
            ExpressionKind::Binary(_, left, right) => {
                self.user.visit_expr(left);
                self.user.visit_expr(right);
            }
            ExpressionKind::Try(op) => self.user.visit_expr(op),
            ExpressionKind::Yield(op) => self.user.visit_expr(op),
            ExpressionKind::Await(op, with) => {
                self.user.visit_expr(op);
                self.user
                    .traverse()
                    .helper_visit_opt(with, User::visit_expr);
            }
            ExpressionKind::Call(callee, args) => {
                self.user.visit_expr(callee);
                self.user
                    .traverse()
                    .helper_visit_list(args, User::visit_expr);
            }
            ExpressionKind::Field(left, right) => {
                self.user.visit_expr(left);
                self.user.visit_ident(right.t);
            }
            ExpressionKind::MethodCall(receiver, method, args) => {
                self.user.visit_expr(receiver);
                self.user.visit_id(method);
                self.user
                    .traverse()
                    .helper_visit_list(args, User::visit_expr);
            }
            ExpressionKind::Case(expr, pat) => {
                self.user.visit_expr(expr);
                self.user.visit_pat(pat);
            }
            ExpressionKind::Block(Block { stmts, kind: _ }) => {
                self.user
                    .traverse()
                    .helper_visit_list(stmts, User::visit_stmt);
            }
            ExpressionKind::If(cond, then, els) => {
                self.user.visit_expr(cond);
                self.user.visit_expr(then);
                self.user.traverse().helper_visit_opt(els, User::visit_expr);
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
                self.user
                    .traverse()
                    .helper_visit_opt(with, User::visit_expr);
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
            PatternKind::Bind(_, id) => self.user.visit_ident(id.t),
            PatternKind::DotId(id, pat) => {
                self.user.visit_ident(id.t);
                self.user.traverse().helper_visit_opt(pat, User::visit_pat);
            }
            PatternKind::Compound(pats) => self
                .user
                .traverse()
                .helper_visit_list(pats, User::visit_pat),
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

    fn visit_enumerator(&mut self, id: PatDeclId) {
        self.helper_user_visit_patdecl(id)
    }

    fn visit_variant(&mut self, id: FieldId) {
        let Field { name, ty } = self.ast().fields[id];
        self.user.visit_ident(name.t);
        self.user.visit_expr(ty);
    }

    fn visit_field(&mut self, id: FieldId) {
        let Field { name, ty } = self.ast().fields[id];
        self.user.visit_ident(name.t);
        self.user.visit_expr(ty);
    }
}
