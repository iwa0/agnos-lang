use super::{
    ast::{Name, PatternDecl, Resolvable, Statement, StatementKind},
    parser::Parser,
    token::{Keyword, Token},
};

impl Parser<'_, '_, '_> {
    pub(crate) fn statement(&mut self) -> Option<Statement> {
        let start = self.peek_ex().span.lo;
        let kind = match self.peek() {
            Token::Keyword(
                Keyword::Const
                | Keyword::Module
                | Keyword::Struct
                | Keyword::Union
                | Keyword::Enum
                | Keyword::Func,
            ) => {
                self.bump();
                let item = self.item()?;
                let item = self.pool.items.alloc(item);
                StatementKind::Item(item)
            }
            Token::Keyword(kw @ (Keyword::Let | Keyword::Var)) => {
                self.bump();
                let pat = self.bind_pattern()?;
                let pat = self.pool.pats.alloc(pat);
                let mutable = match kw {
                    Keyword::Let => false,
                    Keyword::Var => true,
                    _ => unreachable!(),
                };
                let ty = if self.accept(Token::Colon) {
                    let ty = self.type_expr()?;
                    let ty = self.pool.exprs.alloc(ty);
                    Some(ty)
                } else {
                    None
                };
                let expr = if self.accept(Token::Eq) {
                    let expr = self.expression()?;
                    let expr = self.pool.exprs.alloc(expr);
                    Some(expr)
                } else {
                    None
                };
                let decl = PatternDecl {
                    pat,
                    mutable: Some(mutable),
                    ty,
                    expr,
                };
                let decl = self.pool.patdecls.alloc(decl);
                StatementKind::Local(decl)
            }
            Token::Keyword(Keyword::Defer) => {
                self.bump();
                let block = self.expr_or_block()?;
                let block = self.pool.exprs.alloc(block);
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
                    let expr = self.pool.exprs.alloc(expr);
                    Some(expr)
                };
                let r = Resolvable {
                    id,
                    name: Name::Unresolved,
                };
                let stmt = match kw {
                    Keyword::Break => StatementKind::Break(r, expr),
                    Keyword::Continue => StatementKind::Continue(r, expr),
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
                    let expr = self.pool.exprs.alloc(expr);
                    Some(expr)
                };
                StatementKind::Return(expr)
            }
            _ => {
                let expr = self.expression()?;
                if self.accept(Token::Eq) {
                    let right = self.expression()?;
                    let left = self.pool.exprs.alloc(expr);
                    let right = self.pool.exprs.alloc(right);
                    StatementKind::Assign(left, right)
                } else {
                    let expr = self.pool.exprs.alloc(expr);
                    StatementKind::Expr(expr)
                }
            }
        };
        let span = start.to(self.peek_ex_prev().span.hi);
        Some(Statement { kind, span })
    }
}
