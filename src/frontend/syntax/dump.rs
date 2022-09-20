use data_structures::{
    indexvec::ElementIndex,
    interner::Interner,
    tree::{TreeBuilder, TreeDumper},
};

use crate::semantic::{name_analysis::ScopeId, sema::Sema, symbol_table::AstNodeId};

use super::{
    ast::{
        Apply, Ast, AstPool, Block, ConstItem, EnumItem, ExprId, ExpressionKind, FieldId, FuncItem,
        IdId, IdListId, ItemId, ItemKind, Literal, ModuleItem, PatDeclId, PatId, PatternKind,
        SourceFileId, StatementKind, StmtId, StructItem, UnionItem,
    },
    ast_visitor::AstVisitor,
    token::{rd_ident, Ident, Span},
};

/*impl SymbolTable {
    pub fn dump(&self, ast_int: AstWithInterner) -> String {
        let mut dump_tree = TreeBuilder::new();

        let mut map_by_scope = HashMap::new();
        for (&id, entries) in &self.table {
            for e in entries {
                map_by_scope
                    .entry(self.symbol(*e).scope_no)
                    .or_insert_with(Vec::new)
                    .push((id, self.symbol(*e).backref));
            }
        }

        dump_tree.push("MapByScope".to_string());
        for (&scope, entries) in &map_by_scope {
            dump_tree.push(format!("Scope({})", scope));
            for &(id, node) in entries {
                let s_id = ast_int.int.get(id.0);
                let s_id = std::str::from_utf8(s_id).unwrap();
                dump_tree.push(format!("Symbol({})", s_id));
                dump_ast_node_id(&mut dump_tree, &ast_int, node);
                dump_tree.pop();
            }
            dump_tree.pop();
        }
        dump_tree.pop();

        dump_tree.push("MapBySymbol".to_string());
        for (&id, entries) in &self.table {
            let s_id = ast_int.int.get(id.0);
            let s_id = std::str::from_utf8(s_id).unwrap();
            dump_tree.push(format!("Symbol({})", s_id));
            for e in entries {
                dump_tree.push("Entry".to_string());
                dump_ast_node_id(&mut dump_tree, &ast_int, self.symbol(*e).backref);
                dump_tree.add(format!("Scope({})", self.symbol(*e).scope_no));
                dump_tree.pop();
            }
            dump_tree.pop();
        }
        dump_tree.pop();

        let root = dump_tree.into_root("SymbolTable".to_string());
        TreeDumper::dump(&root)
    }
}*/

impl Sema {
    pub fn dump(&self) -> String {
        let mut ast_dump_tree = AstDumpTree {
            sema: self,
            tree: TreeBuilder::new(),
        };
        ast_dump_tree.visit_root();
        let ast_root = ast_dump_tree.tree.as_root();

        /*
        let mut sym_tbl_dump = TreeBuilder::new();
        for a in self.symbol_table {
            todo!()
        }

        let mut scope_dump = TreeBuilder::new();
        for a in self.scopes /*??*/ {
            todo!()
        }
        */

        let mut err_dump = TreeBuilder::new();
        for &(id, ref errs) in &self.errors {
            let file = self.ast.files[id];
            let name = rd_ident(&self.interner, file.name.t);
            err_dump.push(format!("FileErrors({} - {})", name, id.index()));
            for err in errs {
                err_dump.push(err.msg.clone());
                err_dump.add(format!(
                    "Span({}:{} ~ {}:{})",
                    err.span.lo.line, err.span.lo.col, err.span.hi.line, err.span.hi.col
                ));
                err_dump.pop();
            }
            err_dump.pop();
        }
        let err_root = err_dump.into_root("Errors".to_string());

        let mut root = TreeBuilder::new();
        root.add_node(ast_root);
        root.add_node(err_root);
        let root = root.into_root("Root".to_string());
        TreeDumper::dump(&root)
    }
}

fn dump_ast_node_id(tree: &mut TreeBuilder<String>, _: &AstPool, id: AstNodeId) {
    let ref_desc = match id {
        AstNodeId::Null => format!("Unresolved"),
        AstNodeId::File(id) => format!("SourceFile({})", id.index()),
        AstNodeId::Item(id) => format!("Item({})", id.index()),
        //AstNodeId::Stmt(id) => format!("Statement({})", id.index()),
        //AstNodeId::Expr(id) => format!("Expression({})", id.index()),
        AstNodeId::Local(id) => format!("Local({})", id.index()),
        AstNodeId::Param(id) => format!("Param({})", id.index()),
        AstNodeId::GenericParam(id) => format!("GenericParam({})", id.index()),
        AstNodeId::Enumerator(id) => format!("Enumerator({})", id.index()),
        AstNodeId::Variant(id) => format!("Variant({})", id.index()),
        AstNodeId::Field(id) => format!("Field({})", id.index()),
    };
    tree.add(format!("AstRef <{}>", ref_desc));
}

struct AstDumpTree<'a> {
    sema: &'a Sema,
    tree: TreeBuilder<String>,
}

impl AstDumpTree<'_> {
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
}

impl AstVisitor<()> for AstDumpTree<'_> {
    fn ast(&self) -> &Ast {
        &self.sema.ast
    }
    fn int(&self) -> &Interner {
        &self.sema.interner
    }

    fn visit_root(&mut self) {
        self.push_name_str("Ast");
        self.traverse().visit_root();
        self.pop_name();
    }

    fn visit_ident(&mut self, id: Ident) {
        let s = rd_ident(self.int(), id);
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
        match elem.kind {
            ItemKind::Const(ConstItem(_, _)) => self.push_name_id("Const", id),
            ItemKind::Module(ModuleItem(_, sub)) => self.push_name_id("Module", id),
            ItemKind::Struct(StructItem(_, sub)) => self.push_name_id("Struct", id),
            ItemKind::Union(UnionItem(_, sub)) => self.push_name_id("Union", id),
            ItemKind::Enum(EnumItem(_, sub)) => self.push_name_id("Enum", id),
            ItemKind::Func(FuncItem(_, _, _)) => self.push_name_id("Func", id),
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
            ExpressionKind::Use(_, scope) => self.push_name(format!("Use(in {:?})", scope.index())),
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
            ExpressionKind::Unary(kind, _) => self.push_name(format!("Unary({:?})", kind.t)),
            ExpressionKind::Binary(kind, _, _) => self.push_name(format!("Binary({:?})", kind.t)),
            ExpressionKind::Try(_) => self.push_name_str("Try"),
            ExpressionKind::Yield(_) => self.push_name_str("Yield"),
            ExpressionKind::Await(_, _) => self.push_name_str("Await"),
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
            PatternKind::Bind(kind, _) => {
                self.push_name_str("Bind");
                if let Some(kind) = kind {
                    self.name(format!("Qualifier({:?})", kind))
                } else {
                    &mut *self
                }
            }
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

    fn visit_enumerator(&mut self, id: PatDeclId) -> () {
        self.push_name_id("EnumField", id);
        self.traverse().visit_enumerator(id);
        self.pop_name();
    }

    fn visit_variant(&mut self, id: FieldId) {
        self.push_name_id("Variant", id);
        self.traverse().visit_variant(id);
        self.pop_name();
    }

    fn visit_field(&mut self, id: FieldId) {
        self.push_name_id("Field", id);
        self.traverse().visit_field(id);
        self.pop_name();
    }
}
