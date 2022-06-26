use crate::{
    ast::{Item, ItemBody, PatternDecl, Statement, StatementKind, Visibility},
    parser::Parser,
    token::{DelimKind, Keyword, Token},
};

impl Parser<'_, '_> {
    pub(crate) fn statement(&mut self) -> Option<Statement> {
        let start = self.peek_ex().span.lo;
        let kind = match self.peek() {
            Token::Keyword(
                kw @ (Keyword::Const
                | Keyword::Module
                | Keyword::Struct
                | Keyword::Union
                | Keyword::Enum
                | Keyword::Func),
            ) => {
                self.bump();
                let vis = if self.accept(Token::Star) {
                    Visibility::Public
                } else {
                    Visibility::Private
                };
                let name = self.expect_ident().unwrap();
                let generics = if self.accept(Token::Lt) {
                    let mut params = Vec::new();
                    while !matches!(self.peek(), Token::Gt) {
                        let param = self.param_generic().unwrap();
                        params.push(param);
                        if !self.accept(Token::Comma) {
                            break;
                        }
                    }
                    self.expect(Token::Gt);
                    let params = self.pool.patdecls.alloc_with(params.into_iter());
                    Some(params)
                } else {
                    None
                };
                let body = match kw {
                    Keyword::Const => {
                        self.expect(Token::Eq);
                        let expr = self.expression()?;
                        let expr = self.pool.exprs.alloc(expr);
                        ItemBody::Const(expr)
                    }
                    Keyword::Module => {
                        let body = self.line_block()?;
                        let body = self.pool.exprs.alloc(body);
                        ItemBody::Module(body)
                    }
                    Keyword::Struct => {
                        self.expect(Token::NewLine);
                        let fields = self.field_list();
                        let fields = self.pool.fields.alloc_with(fields.into_iter());
                        ItemBody::Struct(fields)
                    }
                    Keyword::Union => {
                        self.expect(Token::NewLine);
                        let fields = self.field_list();
                        let fields = self.pool.fields.alloc_with(fields.into_iter());
                        ItemBody::Union(fields)
                    }
                    Keyword::Enum => {
                        self.expect(Token::NewLine);
                        let decls = self.enum_member_list();
                        let decls = self.pool.patdecls.alloc_with(decls.into_iter());
                        ItemBody::Enum(decls)
                    }
                    Keyword::Func => {
                        let params = self.parse_comma(DelimKind::Paren, Self::param_func);
                        let ret_ty = if !matches!(self.peek(), Token::NewLine) {
                            let ty = self.type_expr()?;
                            let ty = self.pool.exprs.alloc(ty);
                            Some(ty)
                        } else {
                            None
                        };
                        let body = self.line_block()?;
                        let params = self.pool.patdecls.alloc_with(params.into_iter());
                        let body = self.pool.exprs.alloc(body);
                        ItemBody::Func {
                            params,
                            ret_ty,
                            body,
                        }
                    }
                    _ => unreachable!(),
                };
                let item = Item {
                    vis,
                    name,
                    generics,
                    body,
                };
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
                let name = if self.accept(Token::Apostrophe) {
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
                let stmt = match kw {
                    Keyword::Break => StatementKind::Break(name, expr),
                    Keyword::Continue => StatementKind::Continue(name, expr),
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
