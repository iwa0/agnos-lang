use super::{
    ast::{PatternDecl, Statement, StatementKind},
    parser::Parser,
    token::{Keyword, Token},
};

impl Parser<'_, '_> {
    pub(crate) fn statement(&mut self) -> Option<Statement> {
        let start = self.peek_ex().span.lo;
        let kind = match self.peek() {
            Token::Keyword(
                Keyword::Const
                | Keyword::Module
                | Keyword::Struct
                | Keyword::Union
                | Keyword::Enum
                | Keyword::Fn,
            ) => {
                self.bump();
                let item = self.item()?;
                let item = self.sema.ast.items.alloc(item);
                StatementKind::Item(item)
            }
            Token::Keyword(kw @ (Keyword::Let | Keyword::Mut)) => {
                if let Keyword::Let = kw {
                    self.bump();
                }
                let pat = self.bind_pattern()?;
                let pat = self.sema.ast.pats.alloc(pat);
                let ty = if self.accept(Token::Colon) {
                    let ty = self.type_expr()?;
                    let ty = self.sema.ast.exprs.alloc(ty);
                    Some(ty)
                } else {
                    None
                };
                let expr = if self.accept(Token::Eq) {
                    let expr = self.expression()?;
                    let expr = self.sema.ast.exprs.alloc(expr);
                    Some(expr)
                } else {
                    None
                };
                let decl = PatternDecl {
                    pat,
                    ty_annot: ty,
                    expr,
                };
                let decl = self.sema.ast.patdecls.alloc(decl);
                StatementKind::Local(decl)
            }
            Token::Keyword(Keyword::Defer) => {
                self.bump();
                let block = self.expr_or_line_block()?;
                let block = self.sema.ast.exprs.alloc(block);
                StatementKind::Defer(block)
            }
            Token::Keyword(kw @ (Keyword::Break | Keyword::Continue)) => {
                self.bump();
                let id = if self.accept(Token::Apostrophe) {
                    let id = self.expect_ident()?;
                    Some(id)
                } else {
                    None
                };
                let expr = if let Token::NewLine = self.peek() {
                    None
                } else {
                    let expr = self.expression()?;
                    let expr = self.sema.ast.exprs.alloc(expr);
                    Some(expr)
                };
                let stmt = match kw {
                    Keyword::Break => StatementKind::Break(id, expr),
                    Keyword::Continue => StatementKind::Continue(id, expr),
                    _ => unreachable!(),
                };
                stmt
            }
            Token::Keyword(Keyword::Return) => {
                self.bump();
                let expr = if let Token::NewLine = self.peek() {
                    None
                } else {
                    let expr = self.expression()?;
                    let expr = self.sema.ast.exprs.alloc(expr);
                    Some(expr)
                };
                StatementKind::Return(expr)
            }
            _ => {
                let expr = self.expression()?;
                if self.accept(Token::Eq) {
                    let right = self.expression()?;
                    let left = self.sema.ast.exprs.alloc(expr);
                    let right = self.sema.ast.exprs.alloc(right);
                    StatementKind::Assign(left, right)
                } else {
                    let expr = self.sema.ast.exprs.alloc(expr);
                    StatementKind::Expr(expr)
                }
            }
        };
        let span = start.to(self.peek_ex_prev().span.hi);
        Some(Statement { kind, span })
    }
}
