use std::collections::HashMap;

use data_structures::{
    indexvec::{IndexVec, SliceIndex},
    interner::Interner,
};

use crate::syntax::{
    ast::{Ast, AstPool},
};

use super::sema_decl::{Sema};

/*
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
*/

impl Sema {
    pub fn new() -> Self {
        let this = Self {
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
            errors: HashMap::new(),
        };

        this
    }

    pub fn analyze(&mut self) {
        /*
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
                let sym = &self.symbols[builtin_file];
                match sym.backref {
                    AstNodeId::File(file) => {
                        let file = &self.ast.files[file];
                        match file.kind {
                            SourceFileKind::File(_) => {}
                            _ => panic!(),
                        }
                        self.builtin_scope = file.sub_scope;
                    }
                    _ => panic!(),
                }
        */
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
        /*
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
        */
        //TypeResolver::resolve(&mut self);
    }

    /*fn lookup_impl(
        &self,
        key: Ident,
        scope_stack: impl Iterator<Item = ScopeId> + Clone,
    ) -> Result<SymbolId, ()> {
        let entries = self.symbol_table.get(&key);
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
    pub fn lookup(&self, key: Ident, scope: ScopeId) -> Result<SymbolId, ()> {
        #[derive(Copy, Clone)]
        struct ScopePredIter<'a> {
            this: &'a Sema,
            curr_scope: Option<ScopeId>,
        }
        impl Iterator for ScopePredIter<'_> {
            type Item = ScopeId;
            fn next(&mut self) -> Option<Self::Item> {
                let curr = self.curr_scope?;
                let pred = self.this.scopes[curr].pred;
                self.curr_scope = pred;
                pred
            }
        }
        let iter = ScopePredIter {
            this: self,
            curr_scope: Some(scope),
        };
        self.lookup_impl(key, iter)
    }
    pub fn lookup_exact(&self, key: Ident, scope: ScopeId) -> Result<SymbolId, ()> {
        self.lookup_impl(key, std::iter::once(scope))
    }
    */
}
