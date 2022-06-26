use crate::{
    ast::{
        Apply, BinaryOpKind, BlockValueKind, ExprListId, Expression, ExpressionKind, Id, IdListId,
        Literal, Statement, StatementKind, UnaryOpKind,
    },
    parser::Parser,
    token::{DelimKind, Keyword, Token},
};

impl Parser<'_, '_> {
    fn mk_unop(&mut self, kind: UnaryOpKind, op: Expression) -> ExpressionKind {
        let op = self.pool.exprs.alloc(op);
        ExpressionKind::Unary(kind, op)
    }
    fn mk_binop(
        &mut self,
        kind: BinaryOpKind,
        left: Expression,
        right: Expression,
    ) -> ExpressionKind {
        let left_id = self.pool.exprs.alloc(left);
        let right_id = self.pool.exprs.alloc(right);
        ExpressionKind::Binary(kind, left_id, right_id)
    }

    pub(crate) fn expression(&mut self) -> Option<Expression> {
        self.suffix_expr(0)
    }

    pub(crate) fn type_expr(&mut self) -> Option<Expression> {
        let start = self.peek_ex().span.lo;
        let kind = match self.peek() {
            Token::Ident(_) => {
                let ids = self.id_list_and_alloc(false)?;
                ExpressionKind::Use(ids)
            }
            Token::Dot => {
                self.bump();
                let (ids, exprs) = self.dot_id_list_and_alloc(false)?;
                ExpressionKind::Literal(Literal::DotId(ids, exprs))
            }
            _ => {
                self.report("unknown type expression token".to_string());
                return None;
            }
        };
        let span = start.to(self.peek_ex_prev().span.hi);
        Some(Expression { kind, span })
    }

    pub(crate) fn id_list(&mut self, disambiguate: bool) -> Vec<Id> {
        assert!(matches!(self.peek(), Token::Ident(_)));
        let mut list = Vec::new();
        while let Some(id) = self.expect_ident() {
            if let Token::Ident(_) = self.peek() {
                if let Some(arg) = self.id_list_and_alloc(disambiguate) {
                    list.push(Id {
                        id,
                        apply: Some(Apply::Juxtaposition(arg)),
                    });
                }
                break;
            }
            let excl_mark = self.accept(Token::ExclMark);
            let id = if (excl_mark || !disambiguate) && self.accept(Token::Lt) {
                let mut args = Vec::new();
                while !matches!(self.peek(), Token::Gt) {
                    if let Some(expr) = self.expr_no_lt_gt() {
                        args.push(expr);
                    } else {
                        break;
                    }
                    if !self.accept(Token::Comma) {
                        break;
                    }
                }
                self.expect(Token::Gt);
                let args = self.pool.exprs.alloc_with(args.into_iter());
                Id {
                    id,
                    apply: Some(Apply::AngleBrackets(args)),
                }
            } else if excl_mark {
                Id {
                    id,
                    apply: Some(Apply::Infer),
                }
            } else {
                Id { id, apply: None }
            };
            list.push(id);

            if !self.accept(Token::Colon2) {
                break;
            }
        }
        list
    }

    pub(crate) fn id_list_and_alloc(&mut self, disambiguate: bool) -> Option<IdListId> {
        let ids = self.id_list(disambiguate);
        let ids = self.pool.ids.alloc_with(ids.into_iter());
        Some(ids)
    }

    pub(crate) fn dot_id_list_and_alloc(
        &mut self,
        disambiguate: bool,
    ) -> Option<(IdListId, Option<ExprListId>)> {
        assert!(matches!(self.peek_prev(), Token::Dot));
        let ids = self.id_list_and_alloc(disambiguate)?;
        if let Token::OpenDelim(DelimKind::Curly) = self.peek() {
            let list = self.parse_comma(DelimKind::Curly, Self::expression);
            let list = self.pool.exprs.alloc_with(list.into_iter());
            Some((ids, Some(list)))
        } else {
            Some((ids, None))
        }
    }

    fn atom_expr(&mut self) -> Option<Expression> {
        let start = self.peek_ex().span.lo;
        let kind = match self.peek() {
            Token::Ident(_) => {
                let ids = self.id_list_and_alloc(true)?;
                ExpressionKind::Use(ids)
            }
            Token::Number(n) => {
                self.bump();
                let bytes = self.pool.interner.get(n).split_array_ref::<8>().0;
                let n = u64::from_le_bytes(*bytes);
                ExpressionKind::Literal(Literal::Number(n))
            }
            Token::Quote(s) => {
                self.bump();
                ExpressionKind::Literal(Literal::Quote(s))
            }
            Token::Keyword(Keyword::Undef) => {
                self.bump();
                ExpressionKind::Literal(Literal::Undef)
            }
            Token::Dot => {
                self.bump();
                let (path, exprs) = self.dot_id_list_and_alloc(true)?;
                ExpressionKind::Literal(Literal::DotId(path, exprs))
            }
            Token::OpenDelim(DelimKind::Paren) => {
                self.bump();
                let expr = self.expression()?;
                self.expect(Token::CloseDelim(DelimKind::Paren));
                let expr = self.pool.exprs.alloc(expr);
                ExpressionKind::Group(expr)
            }
            Token::OpenDelim(DelimKind::Curly) => {
                let list = self.parse_comma(DelimKind::Curly, Self::expression);
                let list = self.pool.exprs.alloc_with(list.into_iter());
                ExpressionKind::Compound(list)
            }
            _ => {
                self.report(format!("unknown expression token {:?}", self.peek()));
                return None;
            }
        };
        let span = start.to(self.peek_ex_prev().span.hi);
        Some(Expression { kind, span })
    }

    fn primary_expr(&mut self) -> Option<Expression> {
        let start = self.peek_ex().span.lo;
        let kind = match self.peek() {
            Token::Keyword(Keyword::If) => {
                fn if_expr(this: &mut Parser) -> Option<ExpressionKind> {
                    this.bump();
                    let cond = this.expression()?;
                    let then = this.line_block()?;
                    let has_else = matches!(this.peek(), Token::NewLine)
                        && matches!(this.peek_ahead(1), Token::Keyword(Keyword::Else));
                    let els = if has_else {
                        this.bump();
                        this.bump();
                        match this.peek() {
                            Token::Keyword(Keyword::If) => {
                                let expr = this.primary_expr()?;
                                Some(expr)
                            }
                            Token::NewLine => {
                                let expr = this.line_block()?;
                                Some(expr)
                            }
                            _ => {
                                this.report("unknown else token".to_string());
                                return None;
                            }
                        }
                    } else {
                        None
                    };
                    let cond = this.pool.exprs.alloc(cond);
                    let then = this.pool.exprs.alloc(then);
                    let els = els.map(|e| this.pool.exprs.alloc(e));
                    let expr = ExpressionKind::If(cond, then, els);
                    Some(expr)
                }
                let expr = if_expr(self)?;
                expr
            }
            Token::Keyword(Keyword::Match) => {
                self.bump();
                let op = self.expression()?;
                let mut pats = Vec::new();
                let mut exprs = Vec::new();
                self.expect(Token::NewLine);
                self.expect(Token::OpenDelim(DelimKind::Indentation));
                while !matches!(self.peek(), Token::CloseDelim(DelimKind::Indentation)) {
                    let pat = self.pattern()?;
                    self.expect(Token::FatArrow);
                    let body = self.expr_or_block()?;
                    pats.push(pat);
                    exprs.push(body);
                    if !self.accept(Token::NewLine) {
                        break;
                    }
                }
                self.expect(Token::CloseDelim(DelimKind::Indentation));
                let op = self.pool.exprs.alloc(op);
                let pats = self.pool.pats.alloc_with(pats.into_iter());
                let exprs = self.pool.exprs.alloc_with(exprs.into_iter());
                ExpressionKind::Match(op, pats, exprs)
            }
            Token::Keyword(Keyword::Do) => {
                self.bump();
                let block = self.expr_or_block()?;
                let expr = self.pool.exprs.alloc(block);
                ExpressionKind::Do(expr)
            }
            Token::Keyword(Keyword::While) => {
                self.bump();
                let cond = self.expression()?;
                let block = self.line_block()?;
                let cond = self.pool.exprs.alloc(cond);
                let block = self.pool.exprs.alloc(block);
                ExpressionKind::While(cond, block)
            }
            Token::Keyword(Keyword::For) => {
                self.bump();
                let pat = self.bind_pattern()?;
                self.expect(Token::Keyword(Keyword::In));
                let gen = self.expression()?;
                let with = if self.accept(Token::Semi) {
                    let with = self.expression()?;
                    Some(with)
                } else {
                    None
                };
                let body = self.line_block()?;
                let pat = self.pool.pats.alloc(pat);
                let gen = self.pool.exprs.alloc(gen);
                let with = with.map(|e| self.pool.exprs.alloc(e));
                let body = self.pool.exprs.alloc(body);
                ExpressionKind::For(pat, gen, with, body)
            }
            _ => return self.atom_expr(),
        };
        let span = start.to(self.peek_ex_prev().span.hi);
        Some(Expression { kind, span })
    }

    /*
    x() x[] x.f() 20
    try await ~ - ! & 19
    * / % 17 18
    + - 15 16
    << >> 13 14
    & 11 12
    ^ 9 10
    | 7 8
    == != < <= > >= => 6 6
    && 4 5
    || 2 3
    yield 1
    */
    pub(crate) fn expr_no_lt_gt(&mut self) -> Option<Expression> {
        self.suffix_expr(15)
    }
    fn prefix_expr(&mut self) -> Option<Expression> {
        enum PrefixPrec {
            UnOp(UnaryOpKind, u32),
            Await(u32),
            Unmatched,
        }
        let prec = match self.peek() {
            Token::Keyword(Keyword::Yield) => PrefixPrec::UnOp(UnaryOpKind::Yield, 1),
            Token::Amp => PrefixPrec::UnOp(UnaryOpKind::RefTo, 19),
            Token::ExclMark => PrefixPrec::UnOp(UnaryOpKind::BrNot, 19),
            Token::Minus => PrefixPrec::UnOp(UnaryOpKind::Neg, 19),
            Token::Tilde => PrefixPrec::UnOp(UnaryOpKind::Not, 19),
            Token::Keyword(Keyword::Try) => PrefixPrec::UnOp(UnaryOpKind::Try, 19),
            Token::Keyword(Keyword::Await) => PrefixPrec::Await(19),
            _ => PrefixPrec::Unmatched,
        };
        let start = self.peek_ex().span.lo;
        let kind = match prec {
            PrefixPrec::UnOp(kind, op_prec) => {
                self.bump();
                let op = self.suffix_expr(op_prec)?;
                let op = self.pool.exprs.alloc(op);
                ExpressionKind::Unary(kind, op)
            }
            PrefixPrec::Await(op_prec) => {
                self.bump();
                let op = self.suffix_expr(op_prec)?;
                let expr = if self.accept(Token::Keyword(Keyword::For)) {
                    let with = self.suffix_expr(0)?;
                    self.mk_binop(BinaryOpKind::AwaitFor, op, with)
                } else {
                    self.mk_unop(UnaryOpKind::Await, op)
                };
                expr
            }
            PrefixPrec::Unmatched => return self.primary_expr(),
        };
        let span = start.to(self.peek_ex_prev().span.hi);
        Some(Expression { kind, span })
    }
    fn suffix_expr(&mut self, min_prec: u32) -> Option<Expression> {
        let start = self.peek_ex().span.lo;
        let mut op = self.prefix_expr()?;
        loop {
            enum BinOpPrec {
                Postfix(u32),
                Infix(BinaryOpKind, u32, u32),
                Unmatched,
            }
            use BinOpPrec::*;
            use BinaryOpKind::*;
            let prec = match self.peek() {
                Token::OpenDelim(DelimKind::Paren | DelimKind::Square) | Token::Dot => Postfix(20),
                Token::Star => Infix(Mul, 17, 18),
                Token::Slash => Infix(Div, 17, 18),
                Token::Percent => Infix(Mod, 17, 18),
                Token::Plus => Infix(Add, 15, 16),
                Token::Minus => Infix(Sub, 15, 16),
                Token::Lt
                    if matches!(self.peek_ahead(1), Token::Lt)
                        && self.peek_ex().span.consecutive(self.peek_ex_ahead(1).span) =>
                {
                    Infix(Shl, 13, 14)
                }
                Token::Gt
                    if matches!(self.peek_ahead(1), Token::Gt)
                        && self.peek_ex().span.consecutive(self.peek_ex_ahead(1).span) =>
                {
                    Infix(Shr, 13, 14)
                }
                Token::Amp => Infix(BitAnd, 11, 12),
                Token::Caret => Infix(BitXor, 9, 10),
                Token::Pipe => Infix(BitOr, 7, 8),
                Token::Eq2 => Infix(Eq, 6, 6),
                Token::NEq => Infix(Ne, 6, 6),
                Token::Le => Infix(Le, 6, 6),
                Token::Ge => Infix(Ge, 6, 6),
                Token::Lt => Infix(Lt, 6, 6),
                Token::Gt => Infix(Gt, 6, 6),
                Token::FatArrow => Postfix(6),
                Token::Amp2 => Infix(BrAnd, 4, 5),
                Token::Pipe2 => Infix(BrOr, 2, 3),
                _ => Unmatched,
            };
            let kind = match prec {
                Postfix(prec) if prec >= min_prec => match self.peek() {
                    Token::OpenDelim(DelimKind::Paren) => {
                        let callee = self.pool.exprs.alloc(op);
                        let args = self.parse_comma(DelimKind::Paren, Self::expression);
                        let args = self.pool.exprs.alloc_with(args.into_iter());
                        ExpressionKind::Call(callee, args)
                    }
                    Token::OpenDelim(DelimKind::Square) => {
                        self.bump();
                        let sub_op = self.suffix_expr(0)?;
                        self.expect(Token::CloseDelim(DelimKind::Square));
                        self.mk_binop(BinaryOpKind::Index, op, sub_op)
                    }
                    Token::Dot => {
                        self.bump();
                        match self.peek() {
                            Token::Ident(_) => {
                                let first_arg = self.pool.exprs.alloc(op);
                                let ids = self.id_list(true);
                                if ids.len() != 1 {
                                    self.report("expected single id".to_string());
                                } else {
                                    return None;
                                }
                                let name = self.pool.ids.alloc(ids[0]);
                                if let Token::OpenDelim(DelimKind::Paren) = self.peek() {
                                    let args = self.parse_comma(DelimKind::Paren, Self::expression);
                                    let args = self.pool.exprs.alloc_with(args.into_iter());
                                    ExpressionKind::MethodCall(first_arg, name, args)
                                } else {
                                    ExpressionKind::Field(first_arg, name)
                                }
                            }
                            _ => {
                                self.report("unknown postfix token".to_string());
                                return None;
                            }
                        }
                    }
                    Token::FatArrow => {
                        self.bump();
                        let pat = self.pattern()?;
                        let expr = self.pool.exprs.alloc(op);
                        let pat = self.pool.pats.alloc(pat);
                        ExpressionKind::Case(expr, pat)
                    }
                    _ => unreachable!(),
                },
                Infix(kind, left_prec, right_prec) if left_prec >= min_prec => {
                    self.bump();
                    if let BinaryOpKind::Shl | BinaryOpKind::Shr = kind {
                        self.bump();
                    }
                    let right = self.suffix_expr(right_prec)?;
                    self.mk_binop(kind, op, right)
                }
                _ => break Some(op),
            };
            let span = start.to(self.peek_ex_prev().span.hi);
            op = Expression { kind, span };
        }
    }

    pub(crate) fn expr_or_block(&mut self) -> Option<Expression> {
        if let Token::NewLine = self.peek() {
            self.bump();
            self.expect(Token::OpenDelim(DelimKind::Indentation));
            self.parse_block(Token::CloseDelim(DelimKind::Indentation))
        } else {
            self.expression()
        }
    }

    pub(crate) fn line_block(&mut self) -> Option<Expression> {
        let line = self.expect(Token::NewLine);
        if line.is_none() {
            loop {
                match self.peek() {
                    Token::NewLine => {
                        self.bump();
                        break;
                    }
                    Token::Eof => return None,
                    _ => self.bump(),
                }
            }
        }
        self.expect(Token::OpenDelim(DelimKind::Indentation));
        self.parse_block(Token::CloseDelim(DelimKind::Indentation))
    }

    pub(crate) fn parse_block(&mut self, terminator: Token) -> Option<Expression> {
        let start = self.peek_ex().span.lo;
        let mut last_semi_span = None;
        let mut trailing_semi = false;
        let mut stmts = Vec::new();
        'outer: while self.peek() != terminator {
            let begin_indent = self.indent_level();
            if let Some(stmt) = self.statement() {
                trailing_semi = self.accept(Token::Semi);
                if trailing_semi {
                    let old_semi = last_semi_span.replace(self.peek_ex_prev().span);
                    if let Some(span) = old_semi {
                        self.report_span(
                            "semi may only appear at the end of last statement".to_string(),
                            span,
                        );
                    }
                }
                stmts.push(stmt);
            } else {
                while begin_indent < self.indent_level() {
                    self.bump();
                }
                loop {
                    match self.peek() {
                        Token::NewLine => break,
                        Token::Eof => break 'outer,
                        _ => self.bump(),
                    }
                }
            }
            if !self.accept(Token::NewLine) {
                break;
            }
        }
        self.expect(terminator);
        let r = if trailing_semi {
            BlockValueKind::Trailing
        } else if let Some(&Statement {
            kind: StatementKind::Expr(expr),
            ..
        }) = stmts.last()
        {
            match self.pool.expression(expr).kind {
                ExpressionKind::Do(_)
                | ExpressionKind::For(..)
                | ExpressionKind::While(..)
                | ExpressionKind::If(..)
                | ExpressionKind::Match(..) => BlockValueKind::Trailing,
                _ => BlockValueKind::Void,
            }
        } else {
            BlockValueKind::Void
        };
        let stmts = self.pool.stmts.alloc_with(stmts.into_iter());
        let span = start.to(self.peek_ex_prev().span.hi);
        Some(Expression {
            kind: ExpressionKind::Block(stmts, r),
            span,
        })
    }
}
