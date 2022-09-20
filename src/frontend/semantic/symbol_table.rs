use std::collections::HashMap;

use data_structures::indexvec::{ElementIndex, IndexVec};

use crate::syntax::{
    ast::{FieldId, ItemId, PatId, SourceFileId},
    token::Ident,
};

use super::{
    name_analysis::ScopeId,
};

pub type SymbolId = ElementIndex<Symbol>;

pub struct SymbolTable {
    table: HashMap<Ident, Vec<SymbolId>>,
    pub(crate) symbols: IndexVec<Symbol>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Symbol {
    pub kind: SymbolKind,
    pub scope: ScopeId,
    pub backref: AstNodeId,
    //span
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SymbolKind {
    Unresolved,
    Builtin(BuiltinSymbolKind),
    //Alias(SymbolId),
    Source,
    //Decl,
    //Generic(Vec<Vec<ValueId>>),
    //Namespace,              // file module
    //Type(TypeId),           // struct union enum local param genericparam variant field
    //Const(ValueId, TypeId), // const func enumerator
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BuiltinSymbolKind {
    BInt,
    Int,
    UInt,
    Array,
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

impl Symbol {
    pub fn as_item(&self) -> Option<ItemId> {
        match self.backref {
            AstNodeId::Item(id) => Some(id),
            _ => None,
        }
    }
}

// check-out lookup alg. https://web.eecs.umich.edu/~weimerw/2015-4610/scottcd/3a_impsc.pdf

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
            symbols: IndexVec::new(),
        }
    }
    //remove
    pub fn symbol(&self, id: SymbolId) -> &Symbol {
        &self.symbols[id]
    }
    //remove
    pub fn symbol_mut(&mut self, id: SymbolId) -> &mut Symbol {
        &mut self.symbols[id]
    }
    pub fn declare(&mut self, id: Ident, scope: ScopeId, backref: AstNodeId) -> SymbolId {
        let entry = Symbol {
            kind: SymbolKind::Unresolved,
            scope,
            backref,
        };
        let entries = self
            .table
            .entry(id)
            .or_insert_with(|| Vec::with_capacity(1));
        let sym = self.symbols.alloc(entry);
        //assert!(!entries.contains(&entry));
        entries.push(sym);
        sym
    }
    pub fn lookup(
        &self,
        key: Ident,
        scope_stack: impl Iterator<Item = ScopeId> + Clone,
    ) -> Result<SymbolId, ()> {
        let entries = self.table.get(&key);
        let entries = entries.iter().flat_map(|a| a.iter());
        let mut matches = Vec::new();
        for scope_no in scope_stack {
            for a in entries
                .clone()
                .filter(|&&a| self.symbols[a].scope == scope_no)
            {
                matches.push(*a);
            }
            if !matches.is_empty() {
                break;
            }
        }
        match &*matches {
            [] => Err(()),
            [sym] => Ok(*sym),
            [..] => Err(()),
        }
    }
    pub fn lookup_exact(&self, key: Ident, scope: ScopeId) -> Result<SymbolId, ()> {
        self.lookup(key, std::iter::once(scope))
    }
    pub fn lookup_adjacent(&self, key: Ident, adj: SymbolId) -> Result<SymbolId, ()> {
        self.lookup_exact(key, self.symbol(adj).scope)
    }
}
