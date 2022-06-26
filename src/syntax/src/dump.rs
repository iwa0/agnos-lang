use crate::{
    ast::{
        Apply, Ast, AstKind, AstPool, BlockValueKind, Expression, ExpressionKind, Field, Id, Item,
        ItemBody, Literal, Pattern, PatternDecl, PatternKind, Program, Statement, StatementKind,
    },
    token::{Ident, Span},
};

struct AstDump<'a> {
    pool: &'a AstPool,
    path: Vec<u32>,
    dump: String,
}

impl Program {
    pub fn dump(&self) -> String {
        let mut dumper = AstDump {
            pool: self,
            path: Vec::new(),
            dump: String::new(),
        };
        dumper
            .push(1)
            .desc("Program")
            .sub_node_list(self.root, AstPool::ast_list, AstDump::ast)
            .pop();
        dumper.dump
    }
}

impl AstDump<'_> {
    fn push(&mut self, count: usize) -> &mut Self {
        self.path.push(count as u32);
        self
    }
    fn pop(&mut self) -> &mut Self {
        assert!(self.path.pop().unwrap() == 0);
        self
    }
    fn write_str(&mut self, s: &str) -> &mut Self {
        for &cnt in &self.path[0..self.path.len() - 1] {
            match cnt {
                0 => self.dump.push(' '),
                _ => self.dump.push('|'),
            }
        }
        match self.path.last().unwrap() {
            0 => unreachable!(),
            //1 => self.dump.push('-'),
            _ => self.dump.push('+'),
        }
        *self.path.last_mut().unwrap() -= 1;
        self.dump.push_str(s);
        self.dump.push('\n');
        self
    }

    fn node<T, F, G, U>(&mut self, id: T, accessor: F, dumper: G) -> &mut Self
    where
        F: Fn(&AstPool, T) -> &U,
        G: Fn(&mut Self, &U),
    {
        dumper(self, accessor(&self.pool, id));
        self
    }
    fn node_list<T, F, G, U>(&mut self, id: T, accessor: F, dumper: G) -> &mut Self
    where
        F: Fn(&AstPool, T) -> &[U],
        G: Fn(&mut Self, &U),
    {
        for val in accessor(&self.pool, id) {
            dumper(self, val);
        }
        self
    }
    fn sub_node<T, F, G, U>(&mut self, id: T, accessor: F, dumper: G) -> &mut Self
    where
        F: Fn(&AstPool, T) -> &U,
        G: Fn(&mut Self, &U),
    {
        self.push(1).node(id, accessor, dumper).pop()
    }
    fn sub_node_list<T, F, G, U>(&mut self, id: T, accessor: F, dumper: G) -> &mut Self
    where
        F: Fn(&AstPool, T) -> &[U],
        G: Fn(&mut Self, &U),
    {
        let vals = accessor(&self.pool, id);
        self.push(vals.len());
        for val in vals {
            dumper(self, val);
        }
        self.pop()
    }

    fn desc(&mut self, s: &str) -> &mut Self {
        self.write_str(s)
    }
    fn named_span(&mut self, s: &str, span: Span) -> &mut Self {
        self.write_str(
            format!(
                "{} <{}:{} ~ {}:{}>",
                s, span.lo.line, span.lo.col, span.hi.line, span.hi.col
            )
            .as_str(),
        )
    }
    fn ident(&mut self, elem: &Ident) -> &mut Self {
        let s = self.pool.get_intern(elem.0);
        let s = std::str::from_utf8(s).unwrap();
        self.write_str(format!("Ident({s})").as_str())
    }
    fn id(&mut self, elem: &Id) {
        self.ident(&elem.id);
        if let Some(apply) = elem.apply {
            match apply {
                Apply::Juxtaposition(arg) => self.sub_node_list(arg, AstPool::id_list, Self::id),
                Apply::AngleBrackets(args) => {
                    self.sub_node_list(args, AstPool::expression_list, Self::expr)
                }
                Apply::Infer => self.push(1).desc("Infer").pop(),
            };
        }
    }
    fn literal(&mut self, elem: &Literal) {
        match elem {
            &Literal::Number(n) => {
                self.write_str(format!("Number({n})").as_str());
            }
            &Literal::Char(c) => {
                self.write_str(format!("Char '{c}'").as_str());
            }
            &Literal::Quote(s) => {
                let s = self.pool.get_intern(s);
                let s = std::str::from_utf8(s).unwrap();
                self.write_str(format!("Quote '{s}'").as_str());
            }
            &Literal::DotId(id, exprs) => {
                assert!(id.len() > 0);
                self.desc("DotId").push(id.len() + exprs.iter().len());
                self.node_list(id, AstPool::id_list, Self::id);
                if let Some(exprs) = exprs {
                    self.desc("AssociatedExprs").sub_node_list(
                        exprs,
                        AstPool::expression_list,
                        Self::expr,
                    );
                }
                self.pop();
            }
            Literal::Undef => {
                self.desc("Undef");
            }
        }
    }

    fn ast(&mut self, ast: &Ast) {
        self.desc("Ast").push(2).ident(&ast.name);
        match &ast.kind {
            &AstKind::Dir(asts) => {
                self.desc("Dir")
                    .sub_node_list(asts, AstPool::ast_list, Self::ast);
            }
            &AstKind::File(body, ref errs) => {
                self.desc("File")
                    .push(1 + errs.len())
                    .node(body, AstPool::expression, Self::expr);
                for err in errs {
                    self.write_str("err")
                        .push(1)
                        .write_str(
                            format!(
                                "{} <{}:{} ~ {}:{}>",
                                err.msg.as_str(),
                                err.span.lo.line,
                                err.span.lo.col,
                                err.span.hi.line,
                                err.span.hi.col
                            )
                            .as_str(),
                        )
                        .pop();
                }
                self.pop();
            }
        }
        self.pop();
    }
    fn item(&mut self, elem: &Item) {
        self.desc("Item")
            .push(3)
            .write_str(format!("{:?}", &elem.vis).as_str())
            .ident(&elem.name);
        if let Some(generics) = elem.generics {
            self.sub_node_list(generics, AstPool::pattern_decl_list, Self::patdecl);
        }
        match &elem.body {
            &ItemBody::Const(expr) => {
                self.desc("Const")
                    .sub_node(expr, AstPool::expression, Self::expr);
            }
            &ItemBody::Module(body) => {
                self.desc("Module")
                    .sub_node(body, AstPool::expression, Self::expr);
            }
            &ItemBody::Struct(body) => {
                self.desc("Struct")
                    .sub_node_list(body, AstPool::field_list, Self::field);
            }
            &ItemBody::Union(body) => {
                self.desc("Union")
                    .sub_node_list(body, AstPool::field_list, Self::field);
            }
            &ItemBody::Enum(body) => {
                self.desc("Enum")
                    .sub_node_list(body, AstPool::pattern_decl_list, Self::patdecl);
            }
            &ItemBody::Func {
                params,
                ret_ty,
                body,
            } => {
                self.desc("Func")
                    .push(2 + ret_ty.iter().len())
                    .desc("params")
                    .sub_node_list(params, AstPool::pattern_decl_list, Self::patdecl);
                if let Some(ret_ty) = ret_ty {
                    self.desc("ret_ty")
                        .sub_node(ret_ty, AstPool::expression, Self::expr);
                }
                self.desc("body")
                    .sub_node(body, AstPool::expression, Self::expr)
                    .pop();
            }
        }
        self.pop();
    }
    fn stmt(&mut self, elem: &Statement) {
        self.named_span("Statement", elem.span).push(1);
        match &elem.kind {
            &StatementKind::Item(item) => {
                self.desc("Item").sub_node(item, AstPool::item, Self::item);
            }
            &StatementKind::Local(pd) => {
                self.desc("Local")
                    .sub_node(pd, AstPool::pattern_decl, Self::patdecl);
            }
            &StatementKind::Defer(body) => {
                self.desc("Defer")
                    .sub_node(body, AstPool::expression, Self::expr);
            }
            &StatementKind::Break(ref label, expr) => {
                self.desc("Break")
                    .push(label.iter().len() + expr.iter().len());
                if let Some(id) = label {
                    self.ident(id);
                }
                if let Some(expr) = expr {
                    self.node(expr, AstPool::expression, Self::expr);
                }
                self.pop();
            }
            &StatementKind::Continue(ref label, expr) => {
                self.desc("Continue")
                    .push(label.iter().len() + expr.iter().len());
                if let Some(id) = label {
                    self.ident(id);
                }
                if let Some(expr) = expr {
                    self.node(expr, AstPool::expression, Self::expr);
                }
                self.pop();
            }
            &StatementKind::Return(expr) => {
                self.desc("Return").push(expr.iter().len());
                if let Some(expr) = expr {
                    self.node(expr, AstPool::expression, Self::expr);
                }
                self.pop();
            }
            &StatementKind::Assign(left, right) => {
                self.desc("Assign")
                    .push(2)
                    .node(left, AstPool::expression, Self::expr)
                    .node(right, AstPool::expression, Self::expr)
                    .pop();
            }
            &StatementKind::Expr(expr) => {
                self.desc("Expr")
                    .sub_node(expr, AstPool::expression, Self::expr);
            }
        }
        self.pop();
    }
    fn expr(&mut self, elem: &Expression) {
        self.named_span("Expression", elem.span).push(1);
        match &elem.kind {
            &ExpressionKind::Use(id) => {
                self.desc("Use")
                    .sub_node_list(id, AstPool::id_list, Self::id);
            }
            ExpressionKind::Literal(lit) => {
                self.desc("Literal").push(1).literal(lit);
                self.pop();
            }
            &ExpressionKind::Group(expr) => {
                self.desc("Group")
                    .sub_node(expr, AstPool::expression, Self::expr);
            }
            &ExpressionKind::Compound(exprs) => {
                self.desc("Compound")
                    .sub_node_list(exprs, AstPool::expression_list, Self::expr);
            }
            &ExpressionKind::Unary(op, expr) => {
                self.write_str(format!("Unary({op:?})").as_str()).sub_node(
                    expr,
                    AstPool::expression,
                    Self::expr,
                );
            }
            &ExpressionKind::Binary(op, left, right) => {
                self.write_str(format!("Binary({op:?})").as_str())
                    .push(2)
                    .node(left, AstPool::expression, Self::expr)
                    .node(right, AstPool::expression, Self::expr)
                    .pop();
            }
            &ExpressionKind::Call(callee, args) => {
                self.desc("Call")
                    .push(1 + args.len())
                    .node(callee, AstPool::expression, Self::expr)
                    .node_list(args, AstPool::expression_list, Self::expr)
                    .pop();
            }
            &ExpressionKind::Field(left, name) => {
                self.desc("Field")
                    .push(2)
                    .node(left, AstPool::expression, Self::expr)
                    .node(name, AstPool::id, Self::id)
                    .pop();
            }
            &ExpressionKind::MethodCall(left, method, args) => {
                self.desc("MethodCall")
                    .push(2 + args.len())
                    .node(left, AstPool::expression, Self::expr)
                    .node(method, AstPool::id, Self::id)
                    .node_list(args, AstPool::expression_list, Self::expr)
                    .pop();
            }
            &ExpressionKind::Case(expr, pat) => {
                self.desc("Case")
                    .push(2)
                    .node(expr, AstPool::expression, Self::expr)
                    .node(pat, AstPool::pattern, Self::pat)
                    .pop();
            }
            &ExpressionKind::Block(body, trailing) => {
                match trailing {
                    BlockValueKind::Void => {
                        self.desc("BlockVoid");
                    }
                    BlockValueKind::Trailing => {
                        self.desc("BlockTrailingValue");
                    }
                }
                self.sub_node_list(body, AstPool::statement_list, Self::stmt);
            }
            &ExpressionKind::If(cond, then, els) => {
                self.desc("If")
                    .push(2 + els.iter().len())
                    .node(cond, AstPool::expression, Self::expr)
                    .node(then, AstPool::expression, Self::expr);
                if let Some(els) = els {
                    self.desc("else")
                        .sub_node(els, AstPool::expression, Self::expr);
                }
                self.pop();
            }
            &ExpressionKind::Match(op, cases, bodies) => {
                let pats = self.pool.pattern_list(cases);
                let exprs = self.pool.expression_list(bodies);
                self.desc("Match").push(1 + pats.len());
                self.node(op, AstPool::expression, Self::expr);
                for (pat, expr) in pats.iter().zip(exprs.iter()) {
                    self.desc("Case").push(2);
                    self.pat(pat);
                    self.expr(expr);
                    self.pop();
                }
                self.pop();
            }
            &ExpressionKind::Do(body) => {
                self.desc("Do")
                    .sub_node(body, AstPool::expression, Self::expr);
            }
            &ExpressionKind::While(cond, body) => {
                self.desc("While")
                    .push(2)
                    .node(cond, AstPool::expression, Self::expr)
                    .node(body, AstPool::expression, Self::expr)
                    .pop();
            }
            &ExpressionKind::For(pat, in_, with, body) => {
                self.desc("For")
                    .push(3 + with.iter().len())
                    .node(pat, AstPool::pattern, Self::pat)
                    .node(in_, AstPool::expression, Self::expr);
                if let Some(with) = with {
                    self.node(with, AstPool::expression, Self::expr);
                }
                self.node(body, AstPool::expression, Self::expr).pop();
            }
        }
        self.pop();
    }
    fn pat(&mut self, elem: &Pattern) {
        self.named_span("Pattern", elem.span).push(1);
        match &elem.kind {
            PatternKind::Wildcard => {
                self.desc("Wildcard");
            }
            PatternKind::Rest => {
                self.desc("Rest");
            }
            &PatternKind::Constant(expr) => {
                self.desc("Constant")
                    .sub_node(expr, AstPool::expression, Self::expr);
            }
            &PatternKind::Group(pat) => {
                self.desc("Group")
                    .sub_node(pat, AstPool::pattern, Self::pat);
            }
            PatternKind::Bind(id) => {
                self.desc("Bind").push(1).ident(id).pop();
            }
            &PatternKind::DotId(ref id, pat) => {
                self.desc("DotId").push(1 + pat.iter().len()).ident(id);
                if let Some(pat) = pat {
                    self.node(pat, AstPool::pattern, Self::pat);
                }
                self.pop();
            }
            &PatternKind::Compound(pats) => {
                self.desc("Compound")
                    .sub_node_list(pats, AstPool::pattern_list, Self::pat);
            }
        }
        self.pop();
    }
    fn patdecl(&mut self, elem: &PatternDecl) {
        self.desc("PatternDecl");
        self.push(1 + elem.mutable.iter().len() + elem.ty.iter().len() + elem.expr.iter().len())
            .node(elem.pat, AstPool::pattern, Self::pat);
        if let Some(mutable) = elem.mutable {
            self.write_str(format!("Mutability({mutable})").as_str());
        }
        if let Some(ty) = elem.ty {
            self.desc("ty");
            self.sub_node(ty, AstPool::expression, Self::expr);
        }
        if let Some(expr) = elem.expr {
            self.node(expr, AstPool::expression, Self::expr);
        }
        self.pop();
    }
    fn field(&mut self, elem: &Field) {
        self.desc("Field")
            .push(2)
            .ident(&elem.name)
            .desc("ty")
            .sub_node(elem.ty, AstPool::expression, Self::expr)
            .pop();
    }
}
