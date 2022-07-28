use std::ops::Deref;

use crate::syntax::{
    parser::Parser,
    token::{Ident, Span},
};
use data_structures::{
    indexvec::{ElementIndex, IndexVec, SliceIndex},
    interner::{Intern, Interner},
    tree::{Node, TreeBuilder},
};

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

#[derive(Clone, Copy, Hash, Debug, PartialEq, Eq)]
pub enum AstNodeId {
    Root,
    SourceFile(SourceFileId),
    Item(ItemId),
    Stmt(StmtId),
    Expr(ExprId),
    Pat(PatId),
    //PatDecl(PatDeclId),
    //Local(PatDeclId),
    //Param(PatDeclId),
    //EnumField(PatDeclId),
    Field(FieldId),
}

#[derive(Clone, Copy)]
pub enum Name<T = AstNodeId> {
    Unresolved,
    Resolved(T),
    Err,
}

#[derive(Clone, Copy)]
pub struct Resolvable<T, U = Ident> {
    pub name: Name<T>,
    pub id: U,
}

#[derive(Clone, Copy)]
pub struct Id {
    pub id: Resolvable<AstNodeId>,
    pub apply: Option<Apply>,
}

#[derive(Clone, Copy)]
pub enum Apply {
    Juxtaposition(ExprId),
    AngleBrackets(ExprListId),
    Infer,
}

#[derive(Clone, Copy)]
pub struct Block {
    pub stmts: StmtListId,
    pub kind: BlockKind,
}

#[derive(Clone, Copy, Debug)]
pub enum BlockKind {
    Void,
    Trailing,
}

pub struct AstWithInterner<'a, 'b> {
    pub(crate) ast: &'a Ast,
    pub(crate) int: &'b Interner,
}

pub struct AstWithMutInterner<'a, 'b> {
    pub(crate) ast: &'a Ast,
    pub(crate) int: &'b mut Interner,
}

pub struct MutAstWithMutInterner<'a, 'b> {
    pub(crate) ast: &'a mut Ast,
    pub(crate) int: &'b mut Interner,
}

pub struct Ast {
    pub(crate) pool: AstPool,
    pub root: SourceFileListId,
    pub errs: Vec<Node<Option<ErrorInfo>>>,
}

#[derive(Clone, Copy)]
pub struct SourceFile {
    pub name: Ident,
    pub kind: SourceFileKind,
}

#[derive(Clone, Copy)]
pub enum SourceFileKind {
    Dir(SourceFileListId),
    File(Block),
}

#[derive(Clone, Copy)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

#[derive(Clone, Copy)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

#[derive(Clone, Copy)]
pub struct Pattern {
    pub kind: PatternKind,
    pub span: Span,
}

#[derive(Clone, Copy)]
pub enum StatementKind {
    Item(ItemId),
    Local(PatDeclId),
    Defer(ExprId),
    Break(Resolvable<ExprId, Option<Ident>>, Option<ExprId>),
    Continue(Resolvable<ExprId, Option<Ident>>, Option<ExprId>),
    Return(Option<ExprId>),
    Assign(ExprId, ExprId),
    //CompoundAssign
    Expr(ExprId),
}

#[derive(Clone, Copy)]
pub enum ExpressionKind {
    Use(IdListId),
    Literal(Literal),
    Group(ExprId),
    Compound(ExprListId),
    Unary(UnaryOpKind, ExprId),
    Binary(BinaryOpKind, ExprId, ExprId),
    Call(ExprId, ExprListId),
    Field(ExprId, Resolvable<FieldId>),
    MethodCall(ExprId, IdId, ExprListId),
    Case(ExprId, PatId),
    Block(Block), // intermediate node
    If(ExprId, ExprId, Option<ExprId>),
    Match(ExprId, PatListId, ExprListId),
    Do(ExprId),
    While(ExprId, ExprId),
    For(PatId, ExprId, Option<ExprId>, ExprId),
}

#[derive(Clone, Copy)]
pub enum PatternKind {
    //Or(PatId, PatId),
    Wildcard,
    Rest,
    Constant(ExprId),
    Group(PatId),
    Bind(Ident),
    DotId(Ident, Option<PatId>),
    Compound(PatListId),
}

#[derive(Clone, Copy)]
pub struct PatternDecl {
    pub pat: PatId,
    pub mutable: Option<bool>,
    pub ty: Option<ExprId>,
    pub expr: Option<ExprId>,
}

#[derive(Clone, Copy)]
pub struct Item {
    pub vis: Visibility,
    pub name: Ident,
    pub generics: Option<PatDeclListId>,
    pub body: ItemBody,
}

#[derive(Clone, Copy)]
pub enum ItemBody {
    Const(ExprId),
    Module(ExprId),
    Struct(FieldListId),
    Union(FieldListId),
    Enum(PatDeclListId),
    Func {
        params: PatDeclListId,
        ret_ty: Option<ExprId>,
        body: ExprId,
    },
}

#[derive(Clone, Copy, Debug)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Clone, Copy)]
pub enum Literal {
    Number(u64),
    //Float
    Char(char),
    Quote(Intern),
    DotId(IdListId, Option<ExprListId>),
    Undef,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOpKind {
    BrNot,
    Not,
    Neg,
    RefTo,
    Try,
    Yield,
    Await,
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

    AwaitFor,
    Index,
}

#[derive(Clone, Copy)]
pub struct Field {
    pub name: Ident,
    pub ty: ExprId,
}

#[derive(Clone, Debug)]
pub struct ErrorInfo {
    pub msg: String,
    pub span: Span,
}

pub struct AstPool {
    pub(crate) ids: IndexVec<IdId>,
    pub(crate) src_files: IndexVec<SourceFileId>,
    pub(crate) stmts: IndexVec<StmtId>,
    pub(crate) exprs: IndexVec<ExprId>,
    pub(crate) pats: IndexVec<PatId>,
    pub(crate) items: IndexVec<ItemId>,
    pub(crate) patdecls: IndexVec<PatDeclId>,
    pub(crate) fields: IndexVec<FieldId>,
}

impl AstWithInterner<'_, '_> {
    pub fn get_ident(&self, id: Ident) -> &str {
        let s = self.int.get(id.0);
        let s = std::str::from_utf8(s).unwrap();
        s
    }
}

pub struct AstBuilder {
    pub(crate) int: Interner,
    pub(crate) pool: AstPool,
    files: TreeBuilder<SourceFile>,
    errs: TreeBuilder<Option<ErrorInfo>>,
}

impl AstBuilder {
    pub fn new() -> Self {
        Self {
            int: Interner::new(),
            pool: AstPool {
                ids: IndexVec::new(),
                src_files: IndexVec::new(),
                stmts: IndexVec::new(),
                exprs: IndexVec::new(),
                pats: IndexVec::new(),
                items: IndexVec::new(),
                patdecls: IndexVec::new(),
                fields: IndexVec::new(),
            },
            files: TreeBuilder::new(),
            errs: TreeBuilder::new(),
        }
    }
    pub fn interner(&self) -> &Interner {
        &self.int
    }
    pub fn push_dir(&mut self, name: &str) {
        let name = Ident(self.int.intern(name.as_bytes()));

        self.errs.push(None);
        self.files.push(SourceFile {
            name,
            kind: SourceFileKind::Dir(SliceIndex::empty()),
        });
    }
    pub fn pop_dir(&mut self) {
        self.errs.pop();
        self.files.pop();
    }
    pub fn add_file(&mut self, name: &str, input: &str) {
        let name = Ident(self.int.intern(name.as_bytes()));
        let (block, errs) = Parser::parse(self, input);

        for err in errs {
            self.errs.add(Some(err));
        }
        self.files.add(SourceFile {
            name,
            kind: SourceFileKind::File(block),
        });
    }
    pub fn into_ast_int(mut self) -> (Ast, Interner) {
        fn populate_dirs(pool: &mut AstPool, nodes: &mut [Node<SourceFile>]) -> SourceFileListId {
            for dir in nodes.iter_mut() {
                match &mut dir.data.kind {
                    SourceFileKind::Dir(dirs) => {
                        *dirs = populate_dirs(pool, &mut dir.children);
                    }
                    SourceFileKind::File(_) => {
                        assert!(dir.children.is_empty());
                    }
                }
            }
            let it = nodes.iter().map(|a| a.data);
            pool.src_files.alloc_with(it)
        }

        let mut tree = self.files.into_nodes();
        let root = populate_dirs(&mut self.pool, &mut tree);

        let errs = self.errs.into_nodes();

        let ast = Ast {
            pool: self.pool,
            root,
            errs,
        };
        let int = self.int;
        (ast, int)
    }
}

impl Deref for AstWithInterner<'_, '_> {
    type Target = Ast;
    fn deref(&self) -> &Self::Target {
        &self.ast
    }
}

impl Deref for AstWithMutInterner<'_, '_> {
    type Target = Ast;
    fn deref(&self) -> &Self::Target {
        &self.ast
    }
}

impl Deref for Ast {
    type Target = AstPool;
    fn deref(&self) -> &Self::Target {
        &self.pool
    }
}
