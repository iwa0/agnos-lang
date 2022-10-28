use std::collections::hash_map::Entry;

use data_structures::indexvec::IndexVec;
use smallvec::SmallVec;

use crate::syntax::token::Ident;

use super::{il, ildecl};

impl ildecl::IL {
    fn insert_symbol_impl(
        &mut self,
        id: Ident,
        decl_scope: ildecl::ScopeId,
        sym: ildecl::SymbolId,
    ) {
        match self.symbol_table.entry(id) {
            Entry::Occupied(mut e) => {
                assert!(!e.get().contains(&sym));
                e.get_mut().push(sym);
            }
            Entry::Vacant(e) => {
                let mut vec = SmallVec::new();
                vec.push(sym);
                e.insert(vec);
            }
        }
        let enclosing_scope = &mut self.scopes[decl_scope];
        assert!(!enclosing_scope.entries.contains(&sym));
        enclosing_scope.entries.push(sym);
    }
    fn insert_scope_impl(
        &mut self,
        kind: ildecl::ScopeKind,
        pred: Option<ildecl::ScopeId>,
    ) -> ildecl::ScopeId {
        let enclosing = if let Some(pred) = pred {
            self.scope(pred).inherit(pred)
        } else {
            ildecl::ScopeEnclosing::ALL_NONE
        };
        let scope = ildecl::Scope {
            kind,
            entity: None,
            entries: SmallVec::new(),
            op: None,
            enclosing,
        };
        let scope_id = self.scopes.alloc(scope);
        scope_id
    }

    fn mk_tyvar_impl(&mut self, kind: ildecl::TypeVariableKind) -> ildecl::TypeId {
        let tyvar = self.next_tyvar_id;
        self.next_tyvar_id += 1;
        self.types.alloc(ildecl::Type {
            decl: None,
            kind: ildecl::TypeKind::TypeVariable(tyvar, kind),
        })
    }

    pub fn mk_tyvar(&mut self) -> ildecl::TypeId {
        self.mk_tyvar_impl(ildecl::TypeVariableKind::Standard)
    }

    pub fn builtin_bint(&mut self, width: u16) -> ildecl::TypeId {
        self.types.alloc(ildecl::Type {
            decl: None,
            kind: ildecl::TypeKind::BInt(width),
        })
    }
    pub fn const_bint(&mut self, width: u16, val: u64) -> ildecl::ValueId {
        let val = SmallVec::from_slice(std::slice::from_ref(&val));
        let ty = self.builtin_bint(width);
        self.mk_val(ildecl::ValueKind::Constant(val), ty)
    }

    pub fn mk_type(&mut self, decl: ildecl::SymbolId, kind: ildecl::TypeKind) -> ildecl::TypeId {
        let ty = ildecl::Type {
            decl: Some(decl),
            kind,
        };
        self.types.alloc(ty)
    }

    pub fn mk_val(&mut self, kind: ildecl::ValueKind, ty: ildecl::TypeId) -> ildecl::ValueId {
        let val = ildecl::Value { kind, ty };
        self.values.alloc(val)
    }

    // insert_scope_root
    pub fn insert_scope(
        &mut self,
        kind: ildecl::ScopeKind,
        pred: ildecl::ScopeId,
    ) -> ildecl::ScopeId {
        self.insert_scope_impl(kind, Some(pred))
    }

    pub fn insert_symbol(
        &mut self,
        id: Ident,
        kind: ildecl::SymbolKind,
        decl_scope: ildecl::ScopeId,
    ) -> ildecl::SymbolId {
        let sym = ildecl::Symbol {
            kind,
            decl_scope,
            scope: None,
            ty: None,
            val: None,
            cf: None,
        };
        let sym = self.symbols.alloc(sym);
        self.insert_symbol_impl(id, decl_scope, sym);
        sym
    }
    pub fn symbol_mut(&mut self, id: ildecl::SymbolId) -> &mut ildecl::Symbol {
        &mut self.symbols[id]
    }
    pub fn scope_mut(&mut self, id: ildecl::ScopeId) -> &mut ildecl::Scope {
        &mut self.scopes[id]
    }
    pub fn symbol(&self, id: ildecl::SymbolId) -> &ildecl::Symbol {
        &self.symbols[id]
    }
    pub fn scope(&self, id: ildecl::ScopeId) -> &ildecl::Scope {
        &self.scopes[id]
    }
}

impl il::ControlFlow {
    pub fn new() -> Self {
        let mut this = Self {
            bbs: IndexVec::new(),
            insns: IndexVec::new(),
            ops: IndexVec::new(),
        };
        let unreachable = this.ops.alloc(il::Operand::Unreachable);
        let undef = this.ops.alloc(il::Operand::Undef);
        this
    }

    pub fn op_unreachable(&self) -> il::OpId {
        self.ops.all_ids().nth(0)
    }
    pub fn op_undef(&self) -> il::OpId {
        self.ops.all_ids().nth(1)
    }

    pub fn mk_op(&mut self, op: il::Operand) -> il::OpId {
        //assert!(!matches!(op, il::Operand::Undef | il::Operand::Unreachable));
        self.ops.alloc(op)
    }

    pub fn mk_ops<I>(&mut self, i: I) -> il::OpListId
    where
        I: IntoIterator<Item = il::Operand>,
    {
        self.ops.alloc_with(i.into_iter())
    }

    pub fn reserve_op(&mut self) -> il::OpId {
        self.mk_op(il::Operand::Unreachable)
    }
    pub fn reserve_ops(&mut self, n: usize) -> il::OpListId {
        self.mk_ops(std::iter::repeat(il::Operand::Unreachable).take(n))
    }

    pub fn set_reserved_op(&mut self, op: il::OpId, new: il::Operand) {
        let op = &mut self.ops[op];
        assert!(matches!(op, il::Operand::Unreachable));
        *op = new;
    }
    pub fn set_nop(&mut self, old: il::InsnId, new: il::Instruction) {
        let insn = &mut self.insns[old];
        assert!(matches!(insn.kind, il::InstructionKind::Nop));
        *insn = new;
    }

    pub fn cursor(&self) -> il::BbId {
        self.bbs.last_id()
    }
    pub fn emit_bb<I>(&mut self, params: I) -> il::BbId
    where
        I: IntoIterator<Item = ildecl::TypeId>,
    {
        if self.bbs.len() > 0 {
            let cursor = self.cursor();
            assert!(self.bbs[cursor].terminator.is_some());
        }
        self.bbs.alloc(il::BasicBlock {
            params: SmallVec::from_iter(params),
            insn_list: il::InsnListId::empty(),
            terminator: None,
        })
    }
    pub fn emit(&mut self, insn: il::Instruction) -> il::Operand {
        let cursor = self.cursor();
        let id = self.emit_insn(insn);
        il::Operand::result_of(cursor, id)
    }
    pub fn emit_insn(&mut self, insn: il::Instruction) -> il::InsnId {
        let cursor = self.cursor();
        let bb = &mut self.bbs[cursor];
        assert!(bb.terminator.is_none());
        let id = self.insns.alloc(insn);
        bb.insn_list = self.insns.last_n_id(bb.insn_list.len() + 1);
        id
    }

    pub fn emit_terminator(&mut self, insn: il::Terminator) {
        let cursor = self.cursor();
        let bb = &mut self.bbs[cursor];
        assert!(bb.terminator.is_none());
        bb.terminator = Some(insn);
    }
}

impl il::Info {
    pub fn none() -> Self {
        Self {
            kind: il::InfoKind::None,
        }
    }
}

impl il::Operand {
    pub fn block(bb: il::BbId) -> Self {
        Self::Ref(il::OperandRef(bb, il::OperandRefKind::Block))
    }
    pub fn result_of(bb: il::BbId, insn: il::InsnId) -> Self {
        Self::Ref(il::OperandRef(bb, il::OperandRefKind::ResultOf(insn)))
    }
    pub fn param(bb: il::BbId, idx: u32) -> Self {
        Self::Ref(il::OperandRef(bb, il::OperandRefKind::Param(idx)))
    }
    pub fn as_non_unreachable(&self) -> Option<&Self> {
        match self {
            il::Operand::Unreachable => None,
            a => Some(a),
        }
    }
}

impl il::Branch {
    pub fn new(target: il::OpId) -> Self {
        Self {
            target,
            args: SmallVec::new(),
        }
    }
    pub fn with_args<I>(target: il::OpId, args: I) -> Self
    where
        I: IntoIterator<Item = il::OpId>,
    {
        Self {
            target,
            args: SmallVec::from_iter(args),
        }
    }
}
