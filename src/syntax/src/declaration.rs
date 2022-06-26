use crate::ast::{Field, Pattern, PatternDecl, PatternKind};
use crate::parser::Parser;
use crate::token::{DelimKind, Token};

impl Parser<'_, '_> {
    pub(crate) fn bind_pattern(&mut self) -> Option<Pattern> {
        let start = self.peek_ex().span.lo;
        let kind = match self.peek() {
            Token::Ident(id) => {
                self.bump();
                PatternKind::Bind(id)
            }
            _ => return self.pattern(),
        };
        let span = start.to(self.peek_ex_prev().span.hi);
        Some(Pattern { kind, span })
    }

    pub(crate) fn pattern(&mut self) -> Option<Pattern> {
        let start = self.peek_ex().span.lo;
        let kind = match self.peek() {
            Token::Underscore => {
                self.bump();
                PatternKind::Wildcard
            }
            Token::Dot
                if matches!(self.peek_ahead(1), Token::Dot)
                    && self.peek_ex().span.consecutive(self.peek_ex_ahead(1).span) =>
            {
                self.bump();
                self.bump();
                PatternKind::Rest
            }
            Token::Eq => {
                self.bump();
                let expr = self.expression()?;
                let expr = self.pool.exprs.alloc(expr);
                PatternKind::Constant(expr)
            }
            Token::Dot => {
                self.bump();
                let id = self.expect_ident()?;
                let sub = if let Token::OpenDelim(DelimKind::Paren | DelimKind::Curly) = self.peek()
                {
                    let pat = self.bind_pattern()?;
                    let pat = self.pool.pats.alloc(pat);
                    Some(pat)
                } else {
                    None
                };
                PatternKind::DotId(id, sub)
            }
            Token::OpenDelim(DelimKind::Paren) => {
                self.bump();
                let pat = self.bind_pattern()?;
                self.expect(Token::CloseDelim(DelimKind::Paren));
                let pat = self.pool.pats.alloc(pat);
                PatternKind::Group(pat)
            }
            Token::OpenDelim(DelimKind::Curly) => {
                let pats = self.parse_comma(DelimKind::Curly, Self::bind_pattern);
                let pats = self.pool.pats.alloc_with(pats.into_iter());
                PatternKind::Compound(pats)
            }
            _ => {
                self.report(format!("unknown pattern token {:?}", self.peek()));
                return None;
            }
        };
        let span = start.to(self.peek_ex_prev().span.hi);
        Some(Pattern { kind, span })
    }

    pub(crate) fn param_func(&mut self) -> Option<PatternDecl> {
        self.param_0(false)
    }
    pub(crate) fn param_generic(&mut self) -> Option<PatternDecl> {
        self.param_0(true)
    }

    fn param_0(&mut self, for_generics: bool) -> Option<PatternDecl> {
        let pat = self.bind_pattern()?;
        let pat = self.pool.pats.alloc(pat);
        let mut mutable = None;

        let ty = if self.accept(Token::Colon) {
            match self.peek() {
                Token::Amp => {
                    self.bump();
                    mutable = Some(false);
                }
                Token::Amp2 => {
                    self.bump();
                    mutable = Some(true);
                }
                _ => {}
            }
            let ty = self.type_expr()?;
            let ty = self.pool.exprs.alloc(ty);
            Some(ty)
        } else {
            None
        };
        let expr = if self.accept(Token::Eq) {
            let expr = if for_generics {
                self.expr_no_lt_gt()
            } else {
                self.expression()
            };
            let expr = expr?;
            let expr = self.pool.exprs.alloc(expr);
            Some(expr)
        } else {
            None
        };
        Some(PatternDecl  {
                pat,
                mutable,
                ty,
                expr,
            }
        )
    }

    pub(crate) fn field_list(&mut self) -> Vec<Field> {
        self.expect(Token::OpenDelim(DelimKind::Indentation));
        let mut fields = Vec::with_capacity(4);
        while !matches!(self.peek(), Token::CloseDelim(DelimKind::Indentation)) {
            let name = self.expect_ident().unwrap();
            self.expect(Token::Colon);
            let ty = self.type_expr().unwrap();
            let ty = self.pool.exprs.alloc(ty);
            fields.push(Field { name, ty });
            if !self.accept(Token::NewLine) {
                break;
            }
        }
        self.expect(Token::CloseDelim(DelimKind::Indentation));
        fields
    }
    pub(crate) fn enum_member_list(&mut self) -> Vec<PatternDecl> {
        self.expect(Token::OpenDelim(DelimKind::Indentation));
        let mut list = Vec::with_capacity(4);
        while !matches!(self.peek(), Token::CloseDelim(DelimKind::Indentation)) {
            let decl = self.param_func().unwrap();
            list.push(decl);
            if !self.accept(Token::NewLine) {
                break;
            }
        }
        self.expect(Token::CloseDelim(DelimKind::Indentation));
        list
    }
}
