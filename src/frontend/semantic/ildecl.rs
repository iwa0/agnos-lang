use std::collections::HashMap;

use data_structures::indexvec::{ElementIndex, IndexVec};
use smallvec::SmallVec;

use crate::syntax::token::Ident;

use super::il;

pub type SymbolId = ElementIndex<Symbol>;
pub type ScopeId = ElementIndex<Scope>;
pub type TypeId = ElementIndex<Type>;
pub type ValueId = ElementIndex<Value>;

pub struct IL {
    pub(crate) symbol_table: HashMap<Ident, SmallVec<[SymbolId; 4]>>,
    pub(crate) symbols: IndexVec<Symbol>,
    pub(crate) scopes: IndexVec<Scope>,
    pub(crate) types: IndexVec<Type>,
    pub(crate) values: IndexVec<Value>,
    pub(crate) next_tyvar_id: u64,
}

pub struct Symbol {
    pub kind: SymbolKind,
    pub decl_scope: ScopeId,
    pub scope: Option<ScopeId>,
    pub ty: Option<TypeId>,
    pub val: Option<ValueId>,
    pub cf: Option<Box<il::ControlFlow>>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    //builtin, alias?
    File,
    Module,
    //TypeDecl + Const + Generic + GenericConst?
    GenericConst,
    GenericDataType,
    GenericFunction,
    Const,
    DataType,
    Function,
    Field,
    Variant,
    Enumerator,
    Local,
    Param,
    GenericParam,
}

pub struct Scope {
    pub kind: ScopeKind,
    pub entity: Option<SymbolId>,
    pub entries: SmallVec<[SymbolId; 4]>,
    pub op: Option<il::OpId>,
    pub enclosing: ScopeEnclosing,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    Package,
    File,
    Module,
    DataType,
    Function,
    ConstEval,
    //genericparam .. param ..?
    Let,
    Defer,
    Block,
    Control,
}

#[derive(Clone, Copy)]
pub struct ScopeEnclosing {
    pub pred: Option<ScopeId>,
    pub file: Option<ScopeId>,
    pub module: Option<ScopeId>,

    pub datatype: Option<ScopeId>,
    pub func: Option<ScopeId>,

    pub block: Option<ScopeId>,
    pub defer: Option<ScopeId>,

    pub r#try: Option<ScopeId>,
    pub r#break: Option<ScopeId>,
    pub r#continue: Option<ScopeId>,
}

pub struct Value {
    pub kind: ValueKind,
    pub ty: TypeId,
}

pub enum ValueKind {
    Infer,
    Err,
    Trap,
    //Void,
    Constant(SmallVec<[u64; 1]>),
}

pub struct Type {
    pub decl: Option<SymbolId>,
    pub kind: TypeKind,
    //pub obj: Option<Object>,
}

pub enum TypeKind {
    // place-holder types
    TypeVariable(u64, TypeVariableKind),
    TypeCons(TypeConsKind),
    Inst(TypeId, SmallVec<[ValueId; 4]>), // instantiation of unknown type
    // mono types --
    // builtins
    Err,
    Never,
    Void,
    //Metatype,
    Symbol(SymbolId),
    Nat(u64),
    EnumNat,
    Gep(TypeId, TypeId),
    // objects --
    Unit,
    BInt(u16),
    Int(u16),
    UInt(u16),
    Array(TypeId, u64),
    //
    Struct,
    Union,
    Enum(TypeId),
    Func(SmallVec<[TypeId; 2]>, TypeId),
}

pub enum TypeConsKind {
    Nat,
    BInt,
    Int,
    UInt,
    Array,
}

pub enum TypeVariableKind {
    Standard,
    Nat(u64),
    BitIntegral(u16),
    Integral(u16),
    UIntegral(u16),
}

impl Scope {
    pub fn inherit(&self, self_id: ScopeId) -> ScopeEnclosing {
        let pred = Some(self_id);
        let Scope {
            kind, enclosing, ..
        } = self;
        match kind {
            ScopeKind::Package => ScopeEnclosing {
                pred,
                ..ScopeEnclosing::ALL_NONE
            },
            ScopeKind::File => ScopeEnclosing {
                pred,
                file: pred,
                ..ScopeEnclosing::ALL_NONE
            },
            ScopeKind::Module => ScopeEnclosing {
                pred,
                file: enclosing.file,
                module: pred,
                ..ScopeEnclosing::ALL_NONE
            },
            ScopeKind::DataType => ScopeEnclosing {
                pred,
                file: enclosing.file,
                module: enclosing.module,
                datatype: pred,
                func: None, // enclosing.func,
                ..ScopeEnclosing::ALL_NONE
            },
            ScopeKind::Function => ScopeEnclosing {
                pred,
                file: enclosing.file,
                module: enclosing.module,
                datatype: None, // enclosing.datatype,
                func: pred,
                ..ScopeEnclosing::ALL_NONE
            },
            ScopeKind::ConstEval => ScopeEnclosing {
                pred,
                file: enclosing.file,
                module: enclosing.module,
                datatype: enclosing.datatype,
                func: enclosing.func,
                ..ScopeEnclosing::ALL_NONE
            },
            ScopeKind::Let => ScopeEnclosing {
                pred,
                file: enclosing.file,
                module: enclosing.module,
                datatype: None,
                func: enclosing.func,
                block: enclosing.block,
                defer: enclosing.defer,
                r#try: enclosing.r#try,
                r#break: enclosing.r#break,
                r#continue: enclosing.r#continue,
            },
            ScopeKind::Defer => ScopeEnclosing {
                pred,
                file: enclosing.file,
                module: enclosing.module,
                datatype: None,
                func: enclosing.func,
                block: enclosing.block,
                defer: pred,
                r#try: enclosing.r#try,
                r#break: enclosing.r#break,
                r#continue: enclosing.r#continue,
            },
            ScopeKind::Block => ScopeEnclosing {
                pred,
                file: enclosing.file,
                module: enclosing.module,
                datatype: enclosing.datatype,
                func: enclosing.func,
                block: pred,
                defer: None,
                r#try: enclosing.r#try,
                r#break: enclosing.r#break,
                r#continue: enclosing.r#continue,
            },
            ScopeKind::Control => ScopeEnclosing {
                pred,
                file: enclosing.file,
                module: enclosing.module,
                datatype: enclosing.datatype,
                func: enclosing.func,
                block: pred,
                defer: None,
                r#try: enclosing.r#try,
                r#break: enclosing.r#break,
                r#continue: enclosing.r#continue,
            },
            /*
            ban try break continue return inside defer body
            */
        }
    }
}

impl ScopeEnclosing {
    pub const ALL_NONE: Self = Self {
        pred: None,
        file: None,
        module: None,
        datatype: None,
        func: None,
        block: None,
        defer: None,
        r#try: None,
        r#break: None,
        r#continue: None,
    };
}
