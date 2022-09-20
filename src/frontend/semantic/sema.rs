use std::{
    collections::{hash_map::Entry, HashMap},
    hash::Hash, cell::RefCell,
};

use data_structures::{
    indexvec::{ElementIndex, IndexVec, SliceIndex},
    interner::Interner,
};


use crate::{
    semantic::{name_analysis::ScopeKind, symbol_table::SymbolKind},
    syntax::{
        ast::{Ast, AstPool, ExprId, ItemKind, ModuleItem, PatId, SourceFileId, SourceFileKind},
        token::{mk_ident, Span},
    },
};

use super::{
    name_analysis::{NameAnalyzer, Scope, ScopeId},
    symbol_table::{AstNodeId, SymbolId, SymbolTable},
};


pub struct Sema {
    pub(crate) interner: Interner,
    pub(crate) ast: Ast,

    pub(crate) errors: Vec<(SourceFileId, Vec<ErrorInfo>)>, // HashMap<SourceFileId, ..>

    pub(crate) scopes: IndexVec<Scope>,
    pub(crate) symbol_table: SymbolTable,

    pub(crate) dummy_scope: ScopeId,
    builtin_scope: ScopeId,
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Value<'a> {
    pub kind: ValueKind<'a>,
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum ValueKind<'a> {
    Infer,
    Void,
    Constant(Box<[u64]>),
    Type(&'a Type<'a>),
    //TypeInline(Type<'a>), // ?
    Err,
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Type<'a> {
    pub decl: SymbolId,
    // align, size, stride()=(size + align - 1) & -align
    pub kind: TypeKind<'a>,
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum TypeKind<'a> {
    Unresolved,
    Err,
    TypeVariable(u64),
    Infer,
    Typeof(ExprId),
    GenericParam(PatId),
    Inst(SymbolId, &'a [Value<'a>]),
    Metatype,
    Never,
    Void,
    Unit,
    Func(&'a [Type<'a>], &'a Type<'a>, ExprId),
    //
    BInt(u16),
    Int(u16),
    UInt(u16),
    Array(&'a Type<'a>, u64),
    Struct(&'a [Type<'a>]),
    Union(&'a [Type<'a>]),
    Enum(&'a Type<'a>, u32),
}

#[derive(Clone, Debug)]
pub struct ErrorInfo {
    pub msg: String,
    pub span: Span,
}

impl Value<'_> {
    pub fn is_err(&self) -> bool {
        matches!(self.kind, ValueKind::Err)
    }
    pub fn as_type(&self) -> Option<&Type> {
        match self.kind {
            ValueKind::Type(ty) => Some(ty),
            _ => None,
        }
    }
    pub fn as_constant(&self) -> Option<&[u64]> {
        match &self.kind {
            ValueKind::Constant(c) => Some(c),
            _ => None,
        }
    }
}

impl Type<'_> {
    pub fn is_err(&self) -> bool {
        matches!(self.kind, TypeKind::Err)
    }
    pub fn is_infer(&self) -> bool {
        matches!(self.kind, TypeKind::Err)
    }
}

impl Sema {
    pub fn new() -> Self {
        let scopes = IndexVec::new();
        let dummy_scope = scopes.alloc(Scope {
            kind: ScopeKind::Root,
            pred: None,
            enclosing: None,
        });

        let mut this = Self {
            interner: Interner::new(),
            ast: Ast {
                pool: AstPool {
                    ids: IndexVec::new(),
                    files: IndexVec::new(),
                    stmts: IndexVec::new(),
                    exprs: IndexVec::new(),
                    pats: IndexVec::new(),
                    items: IndexVec::new(),
                    patdecls: IndexVec::new(),
                    fields: IndexVec::new(),
                },
                root: SliceIndex::empty(),
            },
            errors: Vec::new(),
            scopes,
            symbol_table: SymbolTable::new(),
            dummy_scope,
            builtin_scope: dummy_scope,
        };

        let unresolved_sym_fwd = this.symbol_table.symbols.next_id();
        let ty = this.intern_type(unresolved_sym_fwd, TypeKind::Unresolved);
        let unresolved_sym = this.symbol_table.declare(
            mk_ident(&mut this.interner, "__unresolved"),
            dummy_scope,
            AstNodeId::Null,
        );
        assert!(unresolved_sym == unresolved_sym_fwd);

        this
    }

    pub fn analyze(&mut self) {
        NameAnalyzer::analyze(self);

        let mut builtin_file = None;
        let builtin_ident = mk_ident(&mut self.interner, "builtin");
        for file in &self.ast.files[self.ast.root] {
            if file.name.t == builtin_ident {
                match file.kind {
                    SourceFileKind::Dir(_) => {}
                    SourceFileKind::File(_) => {
                        builtin_file = Some(file.name.symbol);
                        break;
                    }
                }
            }
        }

        let builtin_file = builtin_file.unwrap();
        let sym = self.symbol_table.symbol(builtin_file);
        match sym.backref {
            AstNodeId::Item(item) => {
                let item = &self.ast.items[item];
                match item.kind {
                    ItemKind::Module(ModuleItem(_, scope)) => {
                        self.builtin_scope = scope;
                    }
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }

        /*
        // make module global innards visible in global scope.
        let sym = this.builtin_symbol("global").unwrap();
        let sym_data = this.symbol_table.symbol_mut(sym);
        match sym_data.backref {
            AstNodeId::Root
            |AstNodeId::SourceFile(_)
            |AstNodeId::Stmt(_)
            |AstNodeId::Expr(_)
            |AstNodeId::Pat(_)
            |AstNodeId::Field(_) => panic!(),
            AstNodeId::Item(item) => {
                // re-run name analysis for "module global" with scope_stack.push(root)
                // or
                // store Vec<SymbolId> in Scope, and update scope=self.builtin_scope later here.
                todo!()
            }
        }
        */

        // bind built-in symbols

        let sym = self.lookup_builtin("type").unwrap();
        let metaty = self.intern_type(sym, TypeKind::Metatype);
        let sym_data = self.symbol_table.symbol_mut(sym);
        sym_data.kind = SymbolKind::Type(metaty);

        let sym = self.lookup_builtin("error").unwrap();
        let ty = self.intern_type(sym, TypeKind::Err);
        let sym_data = self.symbol_table.symbol_mut(sym);
        sym_data.kind = SymbolKind::Type(ty);

        let sym = self.lookup_builtin("infer").unwrap();
        let ty = self.intern_type(sym, TypeKind::Infer);
        let sym_data = self.symbol_table.symbol_mut(sym);
        sym_data.kind = SymbolKind::Type(ty);

        let sym = self.lookup_builtin("void").unwrap();
        let ty = self.intern_type(sym, TypeKind::Void);
        let sym_data = self.symbol_table.symbol_mut(sym);
        sym_data.kind = SymbolKind::Type(ty);

        let sym = self.lookup_builtin("never").unwrap();
        let ty = self.intern_type(sym, TypeKind::Void);
        let sym_data = self.symbol_table.symbol_mut(sym);
        sym_data.kind = SymbolKind::Type(ty);

        let sym = self.lookup_builtin("unit").unwrap();
        let ty = self.intern_type(sym, TypeKind::Unit);
        let sym_data = self.symbol_table.symbol_mut(sym);
        sym_data.kind = SymbolKind::Type(ty);

        //TypeResolver::resolve(&mut self);
    }

    fn lookup_builtin(&mut self, name: &str) -> Result<SymbolId, ()> {
        let id = mk_ident(&mut self.interner, name);
        let sym = self.symbol_table.lookup_exact(id, self.builtin_scope);
        match sym {
            Ok(id) => {
                let sym_data = self.symbol_table.symbol(id);
                // assert sym_data.backref is expr .builtin somewhere
                assert!(matches!(sym_data.kind, SymbolKind::Unresolved));
                Ok(id)
            }
            Err(e) => Err(e),
        }
    }
    pub fn unresolved_sym(&self) -> SymbolId {
        let ty = self.get_intern_type(TypeKind::Unresolved);
        self.types[ty].decl
    }
}
