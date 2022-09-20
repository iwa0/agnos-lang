use std::ops::{Deref, DerefMut};

use crate::{
    semantic::{
        name_analysis::ScopeId,
        sema::{ErrorInfo, Sema},
        symbol_table::SymbolId,
    },
    syntax::{
        parser::Parser,
        token::{Ident, Span},
    },
};
use data_structures::{
    indexvec::{ElementIndex, IndexVec, SliceIndex},
    interner::Intern,
    tree::{Node, TreeBuilder},
};

use super::token::mk_ident;

pub type IdId = ElementIndex<Id>;
pub type SourceFileId = ElementIndex<SourceFile>;
pub type ItemId = ElementIndex<Item>;
pub type StmtId = ElementIndex<Statement>;
pub type ExprId = ElementIndex<Expression>;
pub type PatId = ElementIndex<Pattern>;
pub type PatDeclId = ElementIndex<PatternDecl>;
pub type FieldId = ElementIndex<Field>;

pub type IdListId = SliceIndex<Id>;
pub type SourceFileListId = SliceIndex<SourceFile>;
pub type ItemListId = SliceIndex<Item>;
pub type StmtListId = SliceIndex<Statement>;
pub type ExprListId = SliceIndex<Expression>;
pub type PatListId = SliceIndex<Pattern>;
pub type PatDeclListId = SliceIndex<PatternDecl>;
pub type FieldListId = SliceIndex<Field>;

pub type Name = WithSymbol<Ident>;

#[derive(Clone, Copy)]
pub struct WithSymbol<T> {
    pub t: T,
    pub symbol: SymbolId,
}

pub struct Ast {
    pub(crate) pool: AstPool,
    pub root: SourceFileListId,
}

#[derive(Clone, Copy)]
pub struct Id {
    pub id: Name,
    pub apply: Option<Apply>,
}

#[derive(Clone, Copy)]
pub enum Apply {
    Juxtaposition(ExprId),
    AngleBrackets(ExprListId),
}

#[derive(Clone, Copy)]
pub struct SourceFile {
    pub name: Name,
    pub kind: SourceFileKind,
    pub sub_scope: ScopeId,
}

#[derive(Clone, Copy)]
pub enum SourceFileKind {
    Dir(SourceFileListId),
    File(Block),
}

#[derive(Clone, Copy)]
pub struct Item {
    pub vis: Visibility,
    pub name: Name,
    pub generics: Option<PatDeclListId>,
    pub kind: ItemKind,
}

#[derive(Clone, Copy)]
pub enum ItemKind {
    Const(ConstItem),
    Module(ModuleItem),
    Struct(StructItem),
    Union(UnionItem),
    Enum(EnumItem),
    Func(FuncItem),
}

#[derive(Clone, Copy)]
pub struct ConstItem(pub Option<ExprId>, pub ExprId);
#[derive(Clone, Copy)]
pub struct ModuleItem(pub Block, pub ScopeId);
#[derive(Clone, Copy)]
pub struct StructItem(pub FieldListId, pub ScopeId);
#[derive(Clone, Copy)]
pub struct UnionItem(pub FieldListId, pub ScopeId);
#[derive(Clone, Copy)]
pub struct EnumItem(pub PatDeclListId, pub ScopeId);
#[derive(Clone, Copy)]
pub struct FuncItem(pub PatDeclListId, pub Option<ExprId>, pub ExprId);

#[derive(Clone, Copy)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

#[derive(Clone, Copy)]
pub enum StatementKind {
    Item(ItemId),
    Local(PatDeclId),
    Defer(ExprId),
    Break(Option<Ident>, Option<ExprId>),
    Continue(Option<Ident>, Option<ExprId>),
    Return(Option<ExprId>),
    Assign(ExprId, ExprId),
    //CompoundAssign(BinOpKind, ExprId, ExprId),
    Expr(ExprId),
}

#[derive(Clone, Copy)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

#[derive(Clone, Copy)]
pub enum ExpressionKind {
    Use(IdListId, ScopeId),
    Literal(Literal),
    Group(ExprId),
    Compound(ExprListId),
    Unary(WithSymbol<UnaryOpKind>, ExprId),
    Binary(WithSymbol<BinaryOpKind>, ExprId, ExprId),
    Try(ExprId),
    Yield(ExprId),
    Await(ExprId, Option<ExprId>),
    Call(ExprId, ExprListId),
    Field(ExprId, Name),
    MethodCall(ExprId, IdId, ExprListId),
    Case(ExprId, PatId),
    Block(Block), // intermediate node
    If(ExprId, ExprId, Option<ExprId>),
    Match(ExprId, PatListId, ExprListId),
    Do(ExprId),
    While(ExprId, ExprId),
    For(PatId, ExprId, Option<ExprId>, ExprId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOpKind {
    BrNot,
    Not,
    Neg,
    RefTo, // move
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,

    Lt,
    Le,
    Gt,
    Ge,

    Eq,
    Ne,

    BrAnd,
    BrOr,

    Index,
}

#[derive(Clone, Copy)]
pub enum Literal {
    Number(u64),
    //Float
    Char(char),
    Quote(Intern),
    DotId(IdId, Option<ExprListId>),
    Undef,
}

#[derive(Clone, Copy)]
pub struct Block {
    pub stmts: StmtListId,
    pub kind: BlockKind,
}

#[derive(Clone, Copy, Debug)]
pub enum BlockKind {
    Void,
    Trailing(ExprId),
}

#[derive(Clone, Copy)]
pub struct Pattern {
    pub kind: PatternKind,
    pub span: Span,
}

#[derive(Clone, Copy)]
pub enum PatternKind {
    //Or(PatId, PatId),
    Wildcard,
    Rest,
    Constant(ExprId),
    Group(PatId),
    Bind(Option<BindQualifier>, Name),
    DotId(Name, Option<PatId>),
    Compound(PatListId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BindQualifier {
    Ref(bool),
    Mut,
}

#[derive(Clone, Copy)]
pub struct PatternDecl {
    pub pat: PatId,
    pub ty_annot: Option<ExprId>,
    pub expr: Option<ExprId>,
}

#[derive(Clone, Copy, Debug)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Clone, Copy)]
pub struct Field {
    pub name: Name,
    pub ty: ExprId,
}

pub struct AstPool {
    pub(crate) ids: IndexVec<Id>,
    pub(crate) files: IndexVec<SourceFile>,
    pub(crate) stmts: IndexVec<Statement>,
    pub(crate) exprs: IndexVec<Expression>,
    pub(crate) pats: IndexVec<Pattern>,
    pub(crate) items: IndexVec<Item>,
    pub(crate) patdecls: IndexVec<PatternDecl>,
    pub(crate) fields: IndexVec<Field>,
}

impl Item {
    pub fn as_const(&self) -> Option<&ConstItem> {
        match &self.kind {
            ItemKind::Const(v) => Some(v),
            _ => None,
        }
    }
}

pub struct SemaBuilder {
    pub(crate) sema: Sema,
    files: TreeBuilder<(SourceFile, Vec<ErrorInfo>)>,
}

impl SemaBuilder {
    pub fn new() -> Self {
        Self {
            sema: Sema::new(),
            files: TreeBuilder::new(),
        }
    }
    pub fn push_dir(&mut self, name: &str) {
        self.files.push((
            SourceFile {
                name: Name {
                    t: mk_ident(&mut self.sema.interner, name),
                    symbol: self.sema.unresolved_sym(),
                },
                kind: SourceFileKind::Dir(SliceIndex::empty()),
                sub_scope: self.sema.dummy_scope,
            },
            Vec::new(),
        ));
    }
    pub fn pop_dir(&mut self) {
        self.files.pop();
    }
    pub fn add_file(&mut self, name: &str, input: &str) {
        let (block, errs) = Parser::parse(self, input);
        self.files.add((
            SourceFile {
                name: Name {
                    t: mk_ident(&mut self.sema.interner, name),
                    symbol: self.sema.unresolved_sym(),
                },
                kind: SourceFileKind::File(block),
                sub_scope: self.sema.dummy_scope,
            },
            errs,
        ));
    }
    pub fn into_sema(mut self) -> Sema {
        fn populate_dirs(
            sema: &mut Sema,
            nodes: &mut [Node<(SourceFile, Vec<ErrorInfo>)>],
        ) -> SourceFileListId {
            for node in nodes.iter_mut() {
                let (file, err) = &mut node.data;
                match &mut file.kind {
                    SourceFileKind::Dir(dirs) => {
                        assert!(err.is_empty());
                        *dirs = populate_dirs(sema, &mut node.children);
                    }
                    SourceFileKind::File(_) => {
                        assert!(node.children.is_empty());
                    }
                }
            }
            let it = nodes.iter().map(|a| a.data.0);
            let id = sema.ast.files.alloc_with(it);
            for (id, node) in id.iter().zip(nodes.iter()) {
                let (file, err) = &node.data;
                match file.kind {
                    SourceFileKind::Dir(_) => {}
                    SourceFileKind::File(_) => {
                        if !err.is_empty() {
                            sema.errors.push((id, err.clone()));
                        }
                    }
                }
            }
            id
        }
        let mut tree = self.files.into_nodes();
        let root = populate_dirs(&mut self.sema, &mut tree);
        self.sema.ast.root = root;

        self.sema
    }
}

impl Deref for Ast {
    type Target = AstPool;
    fn deref(&self) -> &Self::Target {
        &self.pool
    }
}

impl DerefMut for Ast {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.pool
    }
}
