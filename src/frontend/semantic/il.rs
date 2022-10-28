use bitvec::vec::BitVec;
use data_structures::indexvec::{ElementIndex, IndexVec, SliceIndex};
use smallvec::SmallVec;

use crate::syntax::{ast, token::Ident};

use super::ildecl;

pub type BbId = ElementIndex<BasicBlock>;
pub type BbListId = SliceIndex<BasicBlock>;

pub type InsnId = ElementIndex<Instruction>;
pub type InsnListId = SliceIndex<Instruction>;

pub type OpId = ElementIndex<Operand>;
pub type OpListId = SliceIndex<Operand>;

pub struct ControlFlow {
    pub(crate) bbs: IndexVec<BasicBlock>,
    pub(crate) insns: IndexVec<Instruction>,
    pub(crate) ops: IndexVec<Operand>,
}

pub struct BasicBlock {
    pub params: SmallVec<[ildecl::TypeId; 2]>,
    pub insn_list: InsnListId,
    pub terminator: Option<Terminator>,
}

#[derive(Clone, Copy)]
pub enum Operand {
    Unreachable,
    Undef,
    //Const(ildecl::ValueId),
    Ref(OperandRef),
}

#[derive(Clone, Copy)]
pub struct OperandRef(pub BbId, pub OperandRefKind);

#[derive(Clone, Copy)]
pub enum OperandRefKind {
    Block,
    SSAOf(InsnId),
    TypeOf(InsnId),
    ResultOf(InsnId),
    Param(u32),
}

pub type Name = WithSymbol<Ident>;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct WithSymbol<T> {
    pub t: T,
    pub symbol: Option<ildecl::SymbolId>,
}

pub struct Info {
    pub kind: InfoKind,
}

pub enum InfoKind {
    None,
    Array,
    Struct(Vec<Info>),
    Enum(BitVec),
    //variant
    Int(),
}

// insn -> op?

pub struct Instruction {
    pub kind: InstructionKind,
    pub ty: OpId,
}

#[derive(PartialEq, Eq)]
pub enum InstructionKind {
    Nop,

    Enter,
    ResultLoc,
    Defer(OpId),
    Annot(OpId, OpId),
    Symbol(Ident, ildecl::SymbolKind),
    Scope(ildecl::ScopeKind),

    DotId(Ident, Option<OpListId>),
    Id(Ident),
    ScopeRes(OpId, Ident),
    Inst(OpId, OpListId),
    //Paren(OpId),
    
    Resolve(OpId),
    Assign(OpId, OpId),
    Unpack(OpId, u32),
    
    Compound(OpListId),


    Inout(OpId), // .. delete?

    AutoCast(AutoCastKind, OpId), // autocast
    IntLiteral(i64),

    Builtin(BuiltinKind, OpListId),
}

#[derive(PartialEq, Eq)]
pub enum AutoCastKind {
    Resolve,
    Sel,
    Mat,
    Val,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BuiltinKind {
    Type(TypeKind),
    ObjectDesignator(ObjectDesignatorKind),
    Arithmetic(ArithmeticKind),
    Cmp(CmpKind),
    Bitwise(BitwiseKind),
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TypeKind {
    Unit,
    BInt,
    Int,
    UInt,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ObjectDesignatorKind {
    Access,
    Subscript,
    Field(Ident),
    Flow,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ArithmeticKind {
    Neg,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CmpKind {
    Eq,
    Ne,
    Lt,
    Ge,
    Le,
    Gt,
    Test,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BitwiseKind {
    And,
    Or,
    Not,
    Xor,
    Shift(ShiftDir),
    Sar,
    Extend,
    //Rotate(ShiftDir),
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ShiftDir {
    Left,
    Right,
}

pub struct Terminator {
    pub kind: TerminatorKind,
}

pub enum TerminatorKind {
    Linear(LinearTerminator),
    Exit(ExitTerminator),
    Loop(Branch),
    Br(OpId, Branch, Branch),
    Switch(OpId, Vec<(OpId, Branch)>),
}

#[derive(Clone)]
pub struct Branch {
    pub target: OpId,
    pub args: SmallVec<[OpId; 1]>,
}

pub struct LinearTerminator {
    pub kind: LinearTerminatorKind,
    pub target: Branch,
}

pub enum LinearTerminatorKind {
    Fall,
    Leave,
    Call(OpId, OpListId),
    Yield(OpId),
    //Await(OpId<'a>, Option<OpListId<'a>>, Branch<'a>),
}

pub struct ExitTerminator {
    pub kind: ExitTerminatorKind,
    pub target: Branch,
    pub fall: Branch,
}

pub enum ExitTerminatorKind {
    Break,
    Continue,
    Return,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ExpressionPosition {
    Generic,
    Left,
    Right,
    Branch,
}
