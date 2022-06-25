use std::ops::Deref;

use crate::{parser::Parser, token::{Span, Ident}};
use data_structures::{
    indexvec::IndexVec,
    interner::{Intern, Interner},
    new_index_type,
};

new_index_type!(IdId, IdListId);
new_index_type!(ItemId, ItemListId);

new_index_type!(AstId, AstListId);

new_index_type!(StmtId, StmtListId);
new_index_type!(ExprId, ExprListId);
new_index_type!(TypeId, TypeListId);
new_index_type!(PatId, PatListId);

new_index_type!(PatDeclId, PatternDeclListId);
new_index_type!(FieldId, FieldListId);

#[derive(Clone, Copy)]
pub struct Id {
    pub id: Ident,
    pub apply: Option<Apply>,
}

#[derive(Clone, Copy)]
pub enum Apply {
    Juxtaposition(IdListId),
    AngleBrackets(ExprListId),
    Infer,
}

pub struct Program {
    pool: AstPool,
    pub root: AstListId,
}

pub struct Ast {
    pub name: Ident,
    pub kind: AstKind,
}

pub enum AstKind {
    Dir(AstListId),
    File(ExprId, Vec<ErrorInfo>),
}

#[derive(Clone, Copy)]
pub enum Statement {
    Item(ItemId),
    Local(PatDeclId),
    Defer(ExprId),
    Break(Option<Ident>, Option<ExprId>),
    Continue(Option<Ident>, Option<ExprId>),
    Return(Option<ExprId>),
    Assign(ExprId, ExprId),
    //CompoundAssign
    Expr(ExprId),
}

#[derive(Clone, Copy, Debug)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Clone, Copy)]
pub struct Item {
    pub vis: Visibility,
    pub name: Ident,
    pub generics: Option<PatternDeclListId>,
    pub body: ItemBody,
}

#[derive(Clone, Copy)]
pub enum ItemBody {
    Const(ExprId),
    Module(ExprId),
    Struct(FieldListId),
    Union(FieldListId),
    Enum(PatternDeclListId),
    Func {
        params: PatternDeclListId,
        ret_ty: Option<ExprId>,
        body: ExprId,
    },
}

#[derive(Clone, Copy)]
pub enum Expression {
    Use(IdListId),
    Literal(Literal),
    Group(ExprId),
    Compound(ExprListId),
    Unary(UnaryOpKind, ExprId),
    Binary(BinaryOpKind, ExprId, ExprId),
    Call(ExprId, ExprListId),
    Field(ExprId, IdId),
    MethodCall(ExprId, IdId, ExprListId),
    Case(ExprId, PatId),
    Block(StmtListId, BlockValueKind), // intermediate node
    If(ExprId, ExprId, Option<ExprId>),
    Match(ExprId, PatListId, ExprListId),
    Do(ExprId),
    While(ExprId, ExprId),
    For(PatId, ExprId, Option<ExprId>, ExprId),
}

#[derive(Clone, Copy)]
pub enum Pattern {
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
pub struct PatternDecl {
    pub pat: PatId,
    pub mutable: Option<bool>,
    pub ty: Option<ExprId>,
    pub expr: Option<ExprId>,
}

#[derive(Clone, Copy, Debug)]
pub enum BlockValueKind {
    Void,
    Trailing,
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
    pub(crate) interner: Interner,

    pub(crate) ids: IndexVec<IdId, Id>,
    pub(crate) asts: IndexVec<AstId, Ast>,

    pub(crate) stmts: IndexVec<StmtId, Statement>,
    pub(crate) exprs: IndexVec<ExprId, Expression>,
    pub(crate) pats: IndexVec<PatId, Pattern>,

    pub(crate) items: IndexVec<ItemId, Item>,
    pub(crate) patdecls: IndexVec<PatDeclId, PatternDecl>,
    pub(crate) fields: IndexVec<FieldId, Field>,
}

pub struct ProgramBuilder {
    pub(crate) pool: AstPool,
    dirs: Vec<(Ident, usize)>,
    asts: Vec<Ast>,
}

impl ProgramBuilder {
    pub fn new() -> Self {
        Self {
            pool: AstPool {
                interner: Interner::new(),
                ids: IndexVec::new(),
                asts: IndexVec::new(),
                stmts: IndexVec::new(),
                exprs: IndexVec::new(),
                pats: IndexVec::new(),
                items: IndexVec::new(),
                patdecls: IndexVec::new(),
                fields: IndexVec::new(),
            },
            dirs: Vec::new(),
            asts: Vec::new(),
        }
    }
    pub fn push_dir(&mut self, name: &str) {
        let name = Ident(self.pool.interner.intern(name.as_bytes()));
        self.dirs.push((name, self.asts.len()));
    }
    pub fn pop_dir(&mut self) {
        let (name, start_index) = self.dirs.pop().unwrap();
        let it = self.asts.drain(start_index..);
        let asts = self.pool.asts.alloc_with(it);
        let ast = Ast {
            name,
            kind: AstKind::Dir(asts),
        };
        self.asts.push(ast);
    }
    pub fn add_file(&mut self, name: &str, input: &str) {
        let name = Ident(self.pool.interner.intern(name.as_bytes()));
        let (stmts, errs) = Parser::parse(self, input);
        let ast = Ast {
            name,
            kind: AstKind::File(stmts, errs),
        };
        self.asts.push(ast);
    }
    pub fn into_program(mut self) -> Program {
        assert!(self.dirs.is_empty());
        let root = self.pool.asts.alloc_with(self.asts.into_iter());
        Program {
            pool: self.pool,
            root,
        }
    }
}

impl Deref for Program {
    type Target = AstPool;
    fn deref(&self) -> &Self::Target {
        &self.pool
    }
}

impl AstPool {
    pub fn get_intern(&self, id: Intern) -> &[u8] {
        self.interner.get(id)
    }

    pub fn id(&self, id: IdId) -> &Id {
        self.ids.element(id)
    }
    pub fn ast(&self, id: AstId) -> &Ast {
        self.asts.element(id)
    }
    pub fn item(&self, id: ItemId) -> &Item {
        self.items.element(id)
    }
    pub fn statement(&self, id: StmtId) -> &Statement {
        self.stmts.element(id)
    }
    pub fn expression(&self, id: ExprId) -> &Expression {
        self.exprs.element(id)
    }
    pub fn pattern(&self, id: PatId) -> &Pattern {
        self.pats.element(id)
    }
    pub fn pattern_decl(&self, id: PatDeclId) -> &PatternDecl {
        self.patdecls.element(id)
    }
    pub fn field(&self, id: FieldId) -> &Field {
        self.fields.element(id)
    }

    pub fn id_list(&self, id: IdListId) -> &[Id] {
        self.ids.slice(id)
    }
    pub fn ast_list(&self, id: AstListId) -> &[Ast] {
        self.asts.slice(id)
    }
    pub fn item_list(&self, id: ItemListId) -> &[Item] {
        self.items.slice(id)
    }
    pub fn statement_list(&self, id: StmtListId) -> &[Statement] {
        self.stmts.slice(id)
    }
    pub fn expression_list(&self, id: ExprListId) -> &[Expression] {
        self.exprs.slice(id)
    }
    pub fn pattern_list(&self, id: PatListId) -> &[Pattern] {
        self.pats.slice(id)
    }
    pub fn pattern_decl_list(&self, id: PatternDeclListId) -> &[PatternDecl] {
        self.patdecls.slice(id)
    }
    pub fn field_list(&self, id: FieldListId) -> &[Field] {
        self.fields.slice(id)
    }
}
