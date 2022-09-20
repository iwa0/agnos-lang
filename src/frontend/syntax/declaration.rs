use crate::semantic::sema::TypeKind;

use super::ast::{
    BindQualifier, ConstItem, EnumItem, Field, FuncItem, Item, ItemKind, ModuleItem, Pattern,
    PatternDecl, PatternKind, StructItem, UnionItem, Visibility,
};
use super::parser::Parser;
use super::token::{DelimKind, Keyword, Token};

impl Parser<'_, '_> {
    pub(crate) fn item(&mut self) -> Option<Item> {
        let kw = match self.peek_prev() {
            Token::Keyword(
                kw @ (Keyword::Const
                | Keyword::Module
                | Keyword::Struct
                | Keyword::Union
                | Keyword::Enum
                | Keyword::Fn),
            ) => kw,
            _ => panic!(),
        };
        let vis = if self.accept(Token::Star) {
            Visibility::Public
        } else {
            Visibility::Private
        };
        let name = self.expect_ident()?;
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
            let params = self.sema.ast.patdecls.alloc_with(params.into_iter());
            Some(params)
        } else {
            None
        };
        let body = match kw {
            Keyword::Const => {
                let ty = if self.accept(Token::Colon) {
                    let expr = self.type_expr()?;
                    let expr = self.sema.ast.exprs.alloc(expr);
                    Some(expr)
                } else {
                    None
                };
                self.expect(Token::Eq);
                let expr = self.expression()?;
                let expr = self.sema.ast.exprs.alloc(expr);
                ItemKind::Const(ConstItem(ty, expr))
            }
            Keyword::Module => {
                let body = self.line_block()?;
                ItemKind::Module(ModuleItem(body, self.sema.dummy_scope))
            }
            Keyword::Struct => {
                self.expect(Token::NewLine);
                let fields = self.field_list();
                let fields = self.sema.ast.fields.alloc_with(fields.into_iter());
                ItemKind::Struct(StructItem(fields, self.sema.dummy_scope))
            }
            Keyword::Union => {
                self.expect(Token::NewLine);
                let fields = self.field_list();
                let fields = self.sema.ast.fields.alloc_with(fields.into_iter());
                ItemKind::Union(UnionItem(fields, self.sema.dummy_scope))
            }
            Keyword::Enum => {
                self.expect(Token::NewLine);
                let decls = self.enum_member_list();
                let decls = self.sema.ast.patdecls.alloc_with(decls.into_iter());
                ItemKind::Enum(EnumItem(decls, self.sema.dummy_scope))
            }
            Keyword::Fn => {
                let params = self.parse_comma(DelimKind::Paren, Self::param_func);
                let ret_ty = if !matches!(self.peek(), Token::NewLine) {
                    let ty = self.type_expr()?;
                    let ty = self.sema.ast.exprs.alloc(ty);
                    Some(ty)
                } else {
                    None
                };
                let body = self.line_block_as_expr()?;
                let params = self.sema.ast.patdecls.alloc_with(params.into_iter());
                let body = self.sema.ast.exprs.alloc(body);
                ItemKind::Func(FuncItem(params, ret_ty, body))
            }
            _ => unreachable!(),
        };
        Some(Item {
            vis,
            name: self.mk_with_unresolved_symbol(name),
            generics,
            kind: body,
        })
    }

    pub(crate) fn bind_pattern(&mut self) -> Option<Pattern> {
        let start = self.peek_ex().span.lo;
        let bind_kind = match self.peek() {
            Token::Amp => {
                self.bump();
                Some(BindQualifier::Ref(false))
            }
            Token::Amp2 => {
                self.bump();
                Some(BindQualifier::Ref(true))
            }
            Token::Keyword(Keyword::Mut) => {
                self.bump();
                Some(BindQualifier::Mut)
            }
            _ => None,
        };
        let kind = match self.peek() {
            Token::Ident(id) => {
                self.bump();
                PatternKind::Bind(bind_kind, self.mk_with_unresolved_symbol(id))
            }
            _ => return self.pattern(),
        };
        let span = start.to(self.peek_ex_prev().span.hi);
        Some(Pattern {
            kind,
            span,
        })
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
                let expr = self.sema.ast.exprs.alloc(expr);
                PatternKind::Constant(expr)
            }
            Token::Dot => {
                self.bump();
                let id = self.expect_ident()?;
                let sub = if let Token::OpenDelim(DelimKind::Paren | DelimKind::Curly) = self.peek()
                {
                    let pat = self.bind_pattern()?;
                    let pat = self.sema.ast.pats.alloc(pat);
                    Some(pat)
                } else {
                    None
                };
                PatternKind::DotId(self.mk_with_unresolved_symbol(id), sub)
            }
            Token::OpenDelim(DelimKind::Paren) => {
                self.bump();
                let pat = self.bind_pattern()?;
                self.expect(Token::CloseDelim(DelimKind::Paren));
                let pat = self.sema.ast.pats.alloc(pat);
                PatternKind::Group(pat)
            }
            Token::OpenDelim(DelimKind::Curly) => {
                let pats = self.parse_comma(DelimKind::Curly, Self::bind_pattern);
                let pats = self.sema.ast.pats.alloc_with(pats.into_iter());
                PatternKind::Compound(pats)
            }
            _ => {
                self.report(format!("unknown pattern token {:?}", self.peek()));
                return None;
            }
        };
        let span = start.to(self.peek_ex_prev().span.hi);
        Some(Pattern {
            kind,
            span,
        })
    }

    pub(crate) fn param_func(&mut self) -> Option<PatternDecl> {
        self.param_0(false)
    }
    pub(crate) fn param_generic(&mut self) -> Option<PatternDecl> {
        self.param_0(true)
    }

    fn param_0(&mut self, for_generics: bool) -> Option<PatternDecl> {
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
            let expr = if for_generics {
                self.expr_no_lt_gt()
            } else {
                self.expression()
            };
            let expr = expr?;
            let expr = self.sema.ast.exprs.alloc(expr);
            Some(expr)
        } else {
            None
        };
        Some(PatternDecl {
            pat,
            ty_annot: ty,
            expr,
        })
    }

    pub(crate) fn field_list(&mut self) -> Vec<Field> {
        self.expect(Token::OpenDelim(DelimKind::Indentation));
        let mut fields = Vec::with_capacity(4);
        while !matches!(self.peek(), Token::CloseDelim(DelimKind::Indentation)) {
            let name = self.expect_ident().unwrap();
            self.expect(Token::Colon);
            let ty = self.type_expr().unwrap();
            let ty = self.sema.ast.exprs.alloc(ty);
            fields.push(Field {
                name: self.mk_with_unresolved_symbol(name),
                ty,
            });
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
