use std::collections::HashMap;

use data_structures::{
    interner::Interner,
};

use crate::syntax::{
    ast::{Ast, FieldId, ItemId, PatId, SourceFileId},
    token::{Span},
};

pub struct Sema {
    pub(crate) interner: Interner,
    pub(crate) ast: Ast,
    pub(crate) errors: HashMap<SourceFileId, Vec<ErrorInfo>>,
}

#[derive(Clone, Copy, Hash, Debug, PartialEq, Eq)]
pub enum AstNodeId {
    Null,
    File(SourceFileId),
    Item(ItemId),
    //Stmt(StmtId),
    //Expr(ExprId),
    Local(PatId),
    Param(PatId),
    GenericParam(PatId),
    Enumerator(PatId),
    Variant(FieldId),
    Field(FieldId),
}

#[derive(Clone, Debug)]
pub struct ErrorInfo {
    pub msg: String,
    pub span: Span,
}
