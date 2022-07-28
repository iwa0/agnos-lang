use std::collections::HashMap;

use data_structures::{
    indexvec::ElementIndex,
    interner::Interner,
    tree::{Node, TreeBuilder, TreeDumper},
};

use crate::{
    syntax::ast::ErrorInfo,
};

use super::{
    ast::{
        Apply, Ast, AstNodeId, AstPool, AstWithInterner, Block, ExprId, ExpressionKind, FieldId,
        IdId, IdListId, ItemBody, ItemId, Literal, PatDeclId, PatId, PatternKind, SourceFileId,
        SourceFileListId, StatementKind, StmtId,
    },
    ast_visitor::AstVisitor,
    token::{Ident, Span},
};


impl Ast {
    pub fn dump(&self, int: &Interner) -> String {
        let mut ast_dump_tree = AstDumpTree {
            ast_int: AstWithInterner { ast: self, int },
            tree: TreeBuilder::new(),
        };
        ast_dump_tree.visit_root(self.root);
        let ast_root = ast_dump_tree.tree.as_root();

        fn err2str(dump_tree: &mut TreeBuilder<String>, tree: &[Node<Option<ErrorInfo>>]) {
            dump_tree.push("ErrorInfo".to_string());
            for err in tree {
                match &err.data {
                    Some(err) => {
                        dump_tree.push(err.msg.clone());
                        dump_tree.add(format!(
                            "Span({}:{} - {}:{}",
                            err.span.lo.line, err.span.lo.col, err.span.hi.line, err.span.hi.col
                        ));
                        dump_tree.pop();
                    }
                    None => {
                        err2str(dump_tree, &err.children);
                    }
                }
            }
            dump_tree.pop();
        }

        let mut dumped_err = TreeBuilder::new();
        err2str(&mut dumped_err, &self.errs);
        let err_root = dumped_err.into_root("Errors".to_string());

        let mut root = TreeBuilder::new();
        root.add_node(ast_root);
        root.add_node(err_root);
        let root = root.into_root("Root".to_string());
        TreeDumper::dump(&root)
    }
}

fn dump_ast_node_id(tree: &mut TreeBuilder<String>, _: &AstPool, id: AstNodeId) {
    let ref_desc = match id {
        AstNodeId::Root => format!("Root"),
        AstNodeId::SourceFile(id) => format!("SourceFile({})", id.index()),
        AstNodeId::Item(id) => format!("Item({})", id.index()),
        AstNodeId::Stmt(id) => format!("Statement({})", id.index()),
        AstNodeId::Expr(id) => format!("Expression({})", id.index()),
        AstNodeId::Pat(id) => format!("Pattern({})", id.index()),
        //AstNodeId::Local(id) => format!("Local({})", id.index()),
        //AstNodeId::Param(id) => format!("Param({})", id.index()),
        //AstNodeId::EnumField(id) => format!("EnumField({})", id.index()),
        AstNodeId::Field(id) => format!("Field({})", id.index()),
    };
    tree.add(format!("AstRef <{}>", ref_desc));
}

struct AstDumpTree<'a, 'b> {
    ast_int: AstWithInterner<'a, 'b>,
    tree: TreeBuilder<String>,
}

impl AstDumpTree<'_, '_> {
    fn push_name_str(&mut self, name: &str) -> &mut Self {
        self.push_name(name.to_string())
    }
    fn push_name(&mut self, name: String) -> &mut Self {
        self.tree.push(name.to_string());
        self
    }

    fn push_name_id<T>(&mut self, name: &str, id: ElementIndex<T>) -> &mut Self {
        self.push_name_id_impl(name, id.index())
    }

    fn push_name_id_span<T>(&mut self, name: &str, id: ElementIndex<T>, span: Span) -> &mut Self {
        self.push_name_id_span_impl(name, id.index(), span)
    }

    fn push_name_id_impl(&mut self, name: &str, id: usize) -> &mut Self {
        self.tree.push(format!("{} <Id={}>", name, id));
        self
    }
    fn push_name_id_span_impl(&mut self, name: &str, id: usize, span: Span) -> &mut Self {
        self.tree.push(format!(
            "{} <Id={}, Span={}:{} ~ {}:{}>",
            name, id, span.lo.line, span.lo.col, span.hi.line, span.hi.col
        ));
        self
    }

    fn pop_name(&mut self) -> &mut Self {
        self.tree.pop();
        self
    }

    fn name(&mut self, name: String) -> &mut Self {
        self.tree.add(name);
        self
    }
    fn _name_str(&mut self, name: &str) -> &mut Self {
        self.name(name.to_string())
    }
}

impl AstVisitor<()> for AstDumpTree<'_, '_> {
    fn ast(&self) -> &Ast {
        self.ast_int.ast
    }
    fn int(&self) -> &Interner {
        self.ast_int.int
    }

    fn visit_root(&mut self, id: SourceFileListId) {
        self.push_name_str("Ast");
        self.traverse().visit_root(id);
        self.pop_name();
    }

    fn visit_ident(&mut self, id: Ident) {
        let s = self.int().get(id.0);
        let s = std::str::from_utf8(s).unwrap();
        self.tree.add(format!("Ident({})", s));
        self.traverse().visit_ident(id)
    }

    fn visit_path(&mut self, id: IdListId) {
        self.push_name_str("Path");
        self.traverse().visit_path(id);
        self.pop_name();
    }

    fn visit_id(&mut self, id: IdId) {
        let elem = self.ast().ids[id];
        match elem.apply {
            Some(Apply::Juxtaposition(_)) => self.push_name_id("Id::Juxtaposition", id),
            Some(Apply::AngleBrackets(_)) => self.push_name_id("Id::AngleBrackets", id),
            Some(Apply::Infer) => self.push_name_id("Id::Infer", id),
            None => self.push_name_id("Id", id),
        };
        self.traverse().visit_id(id);
        self.pop_name();
    }

    fn visit_source_file(&mut self, id: SourceFileId) {
        self.push_name_id("SourceFile", id);
        self.traverse().visit_source_file(id);
        self.pop_name();
    }

    fn visit_item(&mut self, id: ItemId) {
        let elem = self.ast().items[id];
        match elem.body {
            ItemBody::Const(_) => self.push_name_id("Const", id),
            ItemBody::Module(_) => self.push_name_id("Module", id),
            ItemBody::Struct(_) => self.push_name_id("Struct", id),
            ItemBody::Union(_) => self.push_name_id("Union", id),
            ItemBody::Enum(_) => self.push_name_id("Enum", id),
            ItemBody::Func { .. } => self.push_name_id("Func", id),
        };
        self.name(format!("{:?}", elem.vis));
        self.traverse().visit_item(id);
        self.pop_name();
    }

    fn visit_stmt(&mut self, id: StmtId) {
        let elem = self.ast().stmts[id];
        self.push_name_id_span("Statement", id, elem.span);
        match elem.kind {
            StatementKind::Item(_) => self.push_name_str("Item"),
            StatementKind::Local(_) => self.push_name_str("Local"),
            StatementKind::Defer(_) => self.push_name_str("Defer"),
            StatementKind::Break(_, _) => self.push_name_str("Break"),
            StatementKind::Continue(_, _) => self.push_name_str("Continue"),
            StatementKind::Return(_) => self.push_name_str("Return"),
            StatementKind::Assign(_, _) => self.push_name_str("Assign"),
            StatementKind::Expr(_) => self.push_name_str("Expr"),
        };
        self.traverse().visit_stmt(id);
        self.pop_name();
        self.pop_name();
    }

    fn visit_expr(&mut self, id: ExprId) {
        let elem = self.ast().exprs[id];
        self.push_name_id_span("Expression", id, elem.span);
        let mut pops = 2;
        let _ = match elem.kind {
            ExpressionKind::Use(_) => self.push_name_str("Use"),
            ExpressionKind::Literal(lit) => {
                self.push_name_str("Literal");
                match lit {
                    Literal::Number(n) => self.push_name(format!("Number({})", n)),
                    Literal::Char(c) => self.push_name(format!("Char('{}')", c)),
                    Literal::Quote(s) => {
                        let s = self.int().get(s);
                        let s = std::str::from_utf8(s).unwrap();
                        self.push_name(format!("Quote(\"{}\")", s))
                    }
                    Literal::DotId(..) => self.push_name_str("DotId"),
                    Literal::Undef => self.push_name_str("Undef"),
                };
                pops += 1;
                &mut *self // "self" syntax says self is moved ..?
            }
            ExpressionKind::Group(_) => self.push_name_str("Group"),
            ExpressionKind::Compound(_) => self.push_name_str("Compound"),
            ExpressionKind::Unary(kind, _) => self.push_name(format!("Unary({:?})", kind)),
            ExpressionKind::Binary(kind, _, _) => self.push_name(format!("Binary({:?})", kind)),
            ExpressionKind::Call(_, _) => self.push_name_str("Call"),
            ExpressionKind::Field(_, _) => self.push_name_str("Field"),
            ExpressionKind::MethodCall(_, _, _) => self.push_name_str("MethodCall"),
            ExpressionKind::Case(_, _) => self.push_name_str("Case"),
            ExpressionKind::Block(Block { stmts: _, kind }) => {
                self.push_name(format!("Block({:?})", kind))
            }
            ExpressionKind::If(_, _, _) => self.push_name_str("If"),
            ExpressionKind::Match(_, _, _) => self.push_name_str("Match"),
            ExpressionKind::Do(_) => self.push_name_str("Do"),
            ExpressionKind::While(_, _) => self.push_name_str("While"),
            ExpressionKind::For(_, _, _, _) => self.push_name_str("For"),
        };
        self.traverse().visit_expr(id);
        for _ in 0..pops {
            self.pop_name();
        }
    }

    fn visit_pat(&mut self, id: PatId) {
        let elem = self.ast().pats[id];
        self.push_name_id_span("Pattern", id, elem.span);
        match elem.kind {
            PatternKind::Wildcard => self.push_name_str("Wildcard"),
            PatternKind::Rest => self.push_name_str("Rest"),
            PatternKind::Constant(_) => self.push_name_str("Constant"),
            PatternKind::Group(_) => self.push_name_str("Group"),
            PatternKind::Bind(_) => self.push_name_str("Bind"),
            PatternKind::DotId(_, _) => self.push_name_str("DotId"),
            PatternKind::Compound(_) => self.push_name_str("Compound"),
        };
        self.traverse().visit_pat(id);
        self.pop_name();
        self.pop_name();
    }

    fn visit_local(&mut self, id: PatDeclId) -> () {
        self.push_name_id("Local", id);
        self.traverse().visit_local(id);
        self.pop_name();
    }

    fn visit_param(&mut self, id: PatDeclId) -> () {
        self.push_name_id("Param", id);
        self.traverse().visit_param(id);
        self.pop_name();
    }

    fn visit_generic_param(&mut self, id: PatDeclId) -> () {
        self.push_name_id("GenericParam", id);
        self.traverse().visit_generic_param(id);
        self.pop_name();
    }

    fn visit_enum_field(&mut self, id: PatDeclId) -> () {
        self.push_name_id("EnumField", id);
        self.traverse().visit_enum_field(id);
        self.pop_name();
    }

    fn visit_field(&mut self, id: FieldId) {
        self.push_name_id("Field", id);
        self.traverse().visit_field(id);
        self.pop_name();
    }
}
