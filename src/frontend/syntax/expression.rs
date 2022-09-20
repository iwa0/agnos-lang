use crate::semantic::sema::TypeKind;

use super::{
    ast::{
        Apply, BinaryOpKind, Block, BlockKind, Expression, ExpressionKind, Id, Literal, Statement,
        StatementKind, UnaryOpKind,
    },
    parser::Parser,
    token::{DelimKind, Keyword, Token},
};

impl Parser<'_, '_> {
    pub(crate) fn expression(&mut self) -> Option<Expression> {
        self.suffix_expr(0)
    }

    pub(crate) fn type_expr(&mut self) -> Option<Expression> {
        match self.peek() {
            Token::Ident(_) | Token::Dot => self.atom_expr(false),
            _ => {
                self.report(format!("unknown type expression token: '{:?}'", self.peek()));
                None
            }
        }
    }

    pub(crate) fn id_list(&mut self, disambiguate: bool) -> Vec<Id> {
        assert!(matches!(self.peek(), Token::Ident(_)));
        let mut list = Vec::new();
        while let Some(id) = self.expect_ident() {
            if let Token::Ident(_) = self.peek() {
                let arg = self.atom_expr(disambiguate);
                if let Some(arg) = arg {
                    let arg = self.sema.ast.exprs.alloc(arg);
                    let id = Id {
                        id: self.mk_with_unresolved_symbol(id),
                        apply: Some(Apply::Juxtaposition(arg)),
                    };
                    list.push(id);
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
                let args = self.sema.ast.exprs.alloc_with(args.into_iter());
                Id {
                    id: self.mk_with_unresolved_symbol(id),
                    apply: Some(Apply::AngleBrackets(args)),
                }
            } else {
                Id {
                    id: self.mk_with_unresolved_symbol(id),
                    apply: None,
                }
            };
            list.push(id);

            if !self.accept(Token::Colon2) {
                break;
            }
        }
        list
    }

    fn atom_expr(&mut self, disambiguate: bool) -> Option<Expression> {
        let start = self.peek_ex().span.lo;
        let kind = match self.peek() {
            Token::Ident(_) => {
                let ids = self.id_list(disambiguate);
                let ids = self.sema.ast.ids.alloc_with(ids.into_iter());
                ExpressionKind::Use(ids, self.sema.dummy_scope)
            }
            Token::Number(n) => {
                self.bump();
                let bytes = self.sema.interner.get(n).split_array_ref::<8>().0;
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
            Token::Dot if matches!(self.peek_ex_ahead(1).tok, Token::Ident(_)) => {
                self.bump();
                let span = self.peek_ex().span;
                let ids = self.id_list(disambiguate);
                let id = if ids.len() == 1 {
                    ids[0]
                } else {
                    self.report_span("expected single id".to_string(), span);
                    return None;
                };
                let id = self.sema.ast.ids.alloc(id);
                let lit = if let Token::OpenDelim(DelimKind::Curly) = self.peek() {
                    let list = self.parse_comma(DelimKind::Curly, Self::expression);
                    let list = self.sema.ast.exprs.alloc_with(list.into_iter());
                    Literal::DotId(id, Some(list))
                } else {
                    Literal::DotId(id, None)
                };
                ExpressionKind::Literal(lit)
            }
            Token::OpenDelim(DelimKind::Paren) => {
                self.bump();
                let expr = self.expression()?;
                self.expect(Token::CloseDelim(DelimKind::Paren));
                let expr = self.sema.ast.exprs.alloc(expr);
                ExpressionKind::Group(expr)
            }
            Token::OpenDelim(DelimKind::Curly) => {
                let list = self.parse_comma(DelimKind::Curly, Self::expression);
                let list = self.sema.ast.exprs.alloc_with(list.into_iter());
                ExpressionKind::Compound(list)
            }
            _ => {
                self.report(format!("unknown expression token: '{:?}'", self.peek()));
                return None;
            }
        };
        let span = start.to(self.peek_ex_prev().span.hi);
        Some(Expression {
            kind,
            span,
        })
    }

    fn primary_expr(&mut self) -> Option<Expression> {
        let start = self.peek_ex().span.lo;
        let kind = match self.peek() {
            Token::Keyword(Keyword::If) => {
                fn if_expr(this: &mut Parser) -> Option<ExpressionKind> {
                    this.bump();
                    let cond = this.expression()?;
                    let then = this.line_block_as_expr()?;
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
                                let expr = this.line_block_as_expr()?;
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
                    let cond = this.sema.ast.exprs.alloc(cond);
                    let then = this.sema.ast.exprs.alloc(then);
                    let els = els.map(|e| this.sema.ast.exprs.alloc(e));
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
                    let body = self.expr_or_line_block()?;
                    pats.push(pat);
                    exprs.push(body);
                    if !self.accept(Token::NewLine) {
                        break;
                    }
                }
                self.expect(Token::CloseDelim(DelimKind::Indentation));
                let op = self.sema.ast.exprs.alloc(op);
                let pats = self.sema.ast.pats.alloc_with(pats.into_iter());
                let exprs = self.sema.ast.exprs.alloc_with(exprs.into_iter());
                ExpressionKind::Match(op, pats, exprs)
            }
            Token::Keyword(Keyword::Do) => {
                self.bump();
                let block = self.expr_or_line_block()?;
                let expr = self.sema.ast.exprs.alloc(block);
                ExpressionKind::Do(expr)
            }
            Token::Keyword(Keyword::While) => {
                self.bump();
                let cond = self.expression()?;
                let block = self.line_block_as_expr()?;
                let cond = self.sema.ast.exprs.alloc(cond);
                let block = self.sema.ast.exprs.alloc(block);
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
                let body = self.line_block_as_expr()?;
                let pat = self.sema.ast.pats.alloc(pat);
                let gen = self.sema.ast.exprs.alloc(gen);
                let with = with.map(|e| self.sema.ast.exprs.alloc(e));
                let body = self.sema.ast.exprs.alloc(body);
                ExpressionKind::For(pat, gen, with, body)
            }
            _ => return self.atom_expr(true),
        };
        let span = start.to(self.peek_ex_prev().span.hi);
        Some(Expression {
            kind,
            span,
        })
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
            Try(u32),
            Yield(u32),
            Await(u32),
            Unmatched,
        }
        let prec = match self.peek() {
            Token::Keyword(Keyword::Yield) => PrefixPrec::Yield(1),
            Token::Amp => PrefixPrec::UnOp(UnaryOpKind::RefTo, 19),
            Token::ExclMark => PrefixPrec::UnOp(UnaryOpKind::BrNot, 19),
            Token::Minus => PrefixPrec::UnOp(UnaryOpKind::Neg, 19),
            Token::Tilde => PrefixPrec::UnOp(UnaryOpKind::Not, 19),
            Token::Keyword(Keyword::Try) => PrefixPrec::Try(19),
            Token::Keyword(Keyword::Await) => PrefixPrec::Await(19),
            _ => PrefixPrec::Unmatched,
        };
        let start = self.peek_ex().span.lo;
        let kind = match prec {
            PrefixPrec::UnOp(kind, op_prec) => {
                self.bump();
                let op = self.suffix_expr(op_prec)?;
                let op = self.sema.ast.exprs.alloc(op);
                ExpressionKind::Unary(self.mk_with_unresolved_symbol(kind), op)
            }
            PrefixPrec::Try(op_prec) => {
                self.bump();
                let op = self.suffix_expr(op_prec)?;
                let op = self.sema.ast.exprs.alloc(op);
                ExpressionKind::Try(op)
            }
            PrefixPrec::Yield(op_prec) => {
                self.bump();
                let op = self.suffix_expr(op_prec)?;
                let op = self.sema.ast.exprs.alloc(op);
                ExpressionKind::Yield(op)
            }
            PrefixPrec::Await(op_prec) => {
                self.bump();
                let op = self.suffix_expr(op_prec)?;
                let op = self.sema.ast.exprs.alloc(op);
                let with = if self.accept(Token::Keyword(Keyword::For)) {
                    let with = self.suffix_expr(0)?;
                    let with = self.sema.ast.exprs.alloc(with);
                    Some(with)
                } else {
                    None
                };
                ExpressionKind::Await(op, with)
            }
            PrefixPrec::Unmatched => return self.primary_expr(),
        };
        let span = start.to(self.peek_ex_prev().span.hi);
        Some(Expression {
            kind,
            span,
        })
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
                        let callee = self.sema.ast.exprs.alloc(op);
                        let args = self.parse_comma(DelimKind::Paren, Self::expression);
                        let args = self.sema.ast.exprs.alloc_with(args.into_iter());
                        ExpressionKind::Call(callee, args)
                    }
                    Token::OpenDelim(DelimKind::Square) => {
                        self.bump();
                        let sub_op = self.suffix_expr(0)?;
                        self.expect(Token::CloseDelim(DelimKind::Square));
                        let left = self.sema.ast.exprs.alloc(op);
                        let right = self.sema.ast.exprs.alloc(sub_op);
                        ExpressionKind::Binary(
                            self.mk_with_unresolved_symbol(BinaryOpKind::Index),
                            left,
                            right,
                        )
                    }
                    Token::Dot => {
                        self.bump();
                        match self.peek() {
                            Token::Ident(_) => {
                                let span = self.peek_ex().span;
                                let first_arg = self.sema.ast.exprs.alloc(op);
                                let ids = self.id_list(true);
                                let id = if ids.len() == 1 {
                                    ids[0]
                                } else {
                                    self.report_span("expected single id".to_string(), span);
                                    return None;
                                };

                                if let Token::OpenDelim(DelimKind::Paren) = self.peek() {
                                    let args = self.parse_comma(DelimKind::Paren, Self::expression);
                                    let args = self.sema.ast.exprs.alloc_with(args.into_iter());
                                    let method = self.sema.ast.ids.alloc(id);
                                    ExpressionKind::MethodCall(first_arg, method, args)
                                } else {
                                    if id.apply.is_some() {
                                        self.report_span(
                                            "field may not have generic parameters".to_string(),
                                            span,
                                        );
                                    }
                                    ExpressionKind::Field(first_arg, id.id)
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
                        let expr = self.sema.ast.exprs.alloc(op);
                        let pat = self.sema.ast.pats.alloc(pat);
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
                    let left = self.sema.ast.exprs.alloc(op);
                    let right = self.sema.ast.exprs.alloc(right);
                    ExpressionKind::Binary(self.mk_with_unresolved_symbol(kind), left, right)
                }
                _ => break Some(op),
            };
            let span = start.to(self.peek_ex_prev().span.hi);
            op = Expression {
                kind,
                span,
            };
        }
    }

    pub(crate) fn expr_or_line_block(&mut self) -> Option<Expression> {
        if let Token::NewLine = self.peek() {
            self.bump();
            self.expect(Token::OpenDelim(DelimKind::Indentation));
            self.block_as_expr(Token::CloseDelim(DelimKind::Indentation))
        } else {
            self.expression()
        }
    }

    pub(crate) fn line_block_as_expr(&mut self) -> Option<Expression> {
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
        self.block_as_expr(Token::CloseDelim(DelimKind::Indentation))
    }

    fn block_as_expr(&mut self, terminator: Token) -> Option<Expression> {
        let start = self.peek_ex().span.lo;
        let blk = self.block(terminator)?;
        let span = start.to(self.peek_ex_prev().span.hi);
        Some(Expression {
            kind: ExpressionKind::Block(blk),
            span,
        })
    }

    pub(crate) fn line_block(&mut self) -> Option<Block> {
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
        self.block(Token::CloseDelim(DelimKind::Indentation))
    }

    pub(crate) fn block(&mut self, terminator: Token) -> Option<Block> {
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
        let last_expr = if let Some(&Statement {
            kind: StatementKind::Expr(expr),
            ..
        }) = stmts.last() {
            Some(expr)
        } else {
            None
        };
        let kind = 
        if let Some(expr) = last_expr
            && (trailing_semi
                || matches!(
                    self.sema.ast.exprs[expr].kind,
                    ExpressionKind::Do(_)
                    | ExpressionKind::For(..)
                    | ExpressionKind::While(..)
                    | ExpressionKind::If(..)
                    | ExpressionKind::Match(..)
                    )
                )
        {
            BlockKind::Trailing(expr)
        } else {
            BlockKind::Void
        };
        let stmts = self.sema.ast.stmts.alloc_with(stmts.into_iter());
        Some(Block { stmts, kind })
    }
}
