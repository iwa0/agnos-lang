// sema.rs + symbol_table.rs -> sema_decl.rs + sema_impl.rs
// type_resolution ->? sema_impl.rs
// decl expr stmt -> rs

// name binding, type analysis, infer
// check...

/*pub struct TypeResolver<'a> {

}

pub struct EvaluatorRef<'a, 'b> {
    sema: &'a Sema,
    next_tyvar: Cell<u64>, // could be moved to upper ds
    //substs_env: HashMap<scopeid+ident?, &'b Value<'b>>, // generic params
    values: &'b mut Arena<Value<'b>>,
    types: &'b mut Arena<Type<'b>>,
    val_intern: &'b mut HashSet<&'b Value<'b>>, // HashSet<Value<'b>> ?
}

impl<'a, 'b> EvaluatorRef<'a, 'b> {
    pub fn resolve(sema: &'a mut Sema) {
        let mut val_arena = Arena::new();
        let mut type_arena = Arena::new();
        let mut val_intern = RefCell::default();
        let mut this = Self { sema, next_tyvar: 0, values: &mut val_arena, types: &mut type_arena, val_intern: &mut val_intern };
        this.visit_root();
    }

    fn intern_val(&mut self, val: Value<'b>) -> &'b Value {
        let set = &mut *self.val_intern.borrow_mut();
        if let Some(val) = set.get(&val) {
            val
        } else {
            let val = &*self.values.alloc(val);
            set.insert(&val);
            val
        }
    }

    fn intern_ty(&mut self, ty: Type<'b>) -> &'b Type {
        let ty_as_val = Value { kind:ValueKind::Type(&ty) };
        self.intern_val(ty_as_val).as_type().unwrap()
    }

    fn intern_ty_kind(&self, kind: TypeKind<'a>) -> &'b Type<'b> {
        let decl = self.sema.unresolved_sym();
        self.intern_ty(Type { decl, kind })
    }

    fn mk_tyvar(&self, decl: SymbolId) -> &'b Type {
        let tyvar = self.next_tyvar.replace(self.next_tyvar.get() + 1);
        self.types.alloc(Type { decl, kind: TypeKind::TypeVariable(tyvar) })
    }

    fn mk_tyvar_unresolved(&self) -> &'b Type {
        let decl = self.sema.unresolved_sym();
        self.mk_tyvar(decl)
    }

    fn infer_eval_expr(&mut self, id: ExprId, annot_ty: &'b Type) -> (&'b Value<'b>, &'b Type<'b>) {
        // eval
        todo!()
    }

    fn eval_item(&self, id: ItemId) -> &'b Type {
        let meta_ty = self.intern_ty_kind(TypeKind::Metatype);
        let infer_ty = self.intern_ty_kind(TypeKind::Infer);
        let item = &self.sema.ast.items[id];
        let decl = item.name.symbol;

        if let Some(generics) = item.generics {
            for param_id in generics.iter() {
                let param = self.sema.ast.patdecls[param_id];
                let ty = if let Some(annot_expr) = param.ty_annot {
                    self.infer(annot_expr, meta_ty)
                } else {
                    meta_ty
                };
                let (default_val, ty) = if let Some(init_expr) = param.expr {
                    let (val, ty) = self.infer_eval(init_expr, ty);
                    (Some(val), ty)
                } else {
                    (None, ty)
                };
                //self.infer_pat(param.pat, ty);
            }
        }

        let kind = match item.kind {
            ItemKind::Const(_) => todo!(),
            ItemKind::Module(_) => panic!(),
            ItemKind::Struct(StructItem(fields, _)) => {

                let tys = fields.iter().map(|id| self.sema.ast.fields[id]).map(|field| self.eval(field.ty).as_type().unwrap_or(err_ty)).collect();
                TypeKind::Struct(tys)
            }
            ItemKind::Union(UnionItem(variants, _)) => {
                let tys = variants.iter().map(|id| self.sema.ast.fields[id]).map(|variant| self.eval(variant.ty).as_type().unwrap_or(err_ty)).collect();
                TypeKind::Union(tys)
            }
            ItemKind::Enum(EnumItem(enumerators, _)) => {
                let vec =
                enumerators.iter().map(|id| self.sema.ast.patdecls[id])
                .map(|enumerator| enumerator );
                //let enumer_cnt: u32 = enumerators.len().try_into();
                //TypeKind::Enum(todo!(), enumer_cnt)
                todo!()
            }
            ItemKind::Func(FuncItem(params, ret_ty, body)) => {
                let mut param_tys = Vec::new();
                for param_id in params.iter() {
                    let param = self.sema.ast.patdecls[param_id];
                    let ty = if let Some(annot_expr) = param.ty_annot {
                        self.expr(annot_expr, meta_ty)
                    } else {
                        infer_ty
                    };
                    let (default_val, ty) = if let Some(init_expr) = param.expr {
                        let (val, ty) = self.infer_eval(init_expr, &ty);
                        (Some(val), self.sema.types[ty].clone())
                    } else {
                        (None, ty)
                    };
                    //self.infer_pat(param.pat, ty);
                    param_tys.push(ty);
                }
                let ret_ty = if let Some(annot_expr) = ret_ty {
                    self.infer_eval_as_type(annot_expr)
                } else {
                    self.sema.get_intern_type(TypeKind::Unit)
                };
                let body_ty = self.infer(body, &self.sema.types[ret_ty]);
                TypeKind::Func(param_tys, ret_ty, body_ty)
            }
        };

        self.types.alloc(Type { decl, kind })
    }

    fn resolve_item(&mut self, id: ItemId) {
        let meta_ty = self.sema.get_intern_type(TypeKind::Metatype);
        let infer_ty = self.sema.get_intern_type(TypeKind::Infer);
        let err_ty = self.sema.get_intern_type(TypeKind::Err);

        let item = self.sema.ast.items[id];
        let decl = item.name.symbol;
        let sym = self.sema.symbol_table.symbol(decl);

        assert!(sym.kind == SymbolKind::Unresolved);

        if let Some(params) = item.generics {
            //assert!(false, "generics are unimplemented");
            for param_id in params.iter() {
                let param = self.sema.ast.patdecls[param_id];
                let ty = if let Some(annot_expr) = param.ty_annot {
                    self.infer_eval_as_type(annot_expr)
                } else {
                    meta_ty
                };
                let (default_val, ty) = if let Some(init_expr) = param.expr {
                    let ty = &self.sema.types[ty];
                    let (val, ty) = self.infer_eval(init_expr, ty);
                    (Some(val), ty)
                } else {
                    (None, ty)
                };
                self.infer_pat(param.pat, ty);
            }
        }

        match item.kind {
            ItemKind::Const(ConstItem(annot_expr, val_expr)) => {
                // move out ..?
                let ty = if let Some(annot_expr) = annot_expr {
                    self.infer_eval_as_type(annot_expr)
                } else {
                    infer_ty
                };
                let ty = &self.sema.types[ty];
                let (val,ty) = self.infer_eval(val_expr, ty);
                let (val, ty) = match self.sema.values[val].kind {
                    ValueKind::Infer => {
                        // builtin symbols
                        let sym_name = rd_ident(&self.sema.interner, item.name.t);
                        match sym_name {
                            "int" => {
                            }
                        }
                        todo!()
                    }
                    _ => (val,ty),
                };
                // alias etc?
                let sym = self.sema.symbol_table.symbol_mut(decl);
                sym.kind = SymbolKind::Const(val, ty);
            }
            ItemKind::Module(ModuleItem(blk, _)) => {
                let sym = self.sema.symbol_table.symbol_mut(decl);
                sym.kind = SymbolKind::Namespace;
                self.traverse().helper_visit_list(blk.stmts, Self::visit_stmt);
            }
            ItemKind::Struct(StructItem(fields, _)) => {
                if item.generics.is_none() {
                    sym.kind = SymbolKind::Type(self.mk_tyvar(decl));
                } else {
                    sym.kind = SymbolKind::Generic(Vec::new());
                }
                for field_id in fields.iter() {
                    let field = self.sema.ast.fields[field_id];
                    let ty = self.infer_eval_as_type(field.ty);
                    let sym = self.sema.symbol_table.symbol_mut(field.name.symbol);
                    sym.kind = SymbolKind::Type(ty);
                }
            }
            ItemKind::Union(UnionItem(variants, _)) => {
                if item.generics.is_none() {
                    sym.kind = SymbolKind::Type(self.mk_tyvar(decl));
                } else {
                    sym.kind = SymbolKind::Generic(Vec::new());
                }
                for variant_id in variants.iter() {
                    let variant = self.sema.ast.fields[variant_id];
                    let meta_ty = &self.sema.types[meta_ty];
                    let ty = self.infer(variant.ty, meta_ty);
                    let sym = self.sema.symbol_table.symbol_mut(variant.name.symbol);
                    //sym.kind = SymbolKind::Type(ty);
                }
            }
            ItemKind::Enum(EnumItem(enumerators, _)) => {
                if item.generics.is_none() {
                    sym.kind = SymbolKind::Type(self.mk_tyvar(decl));
                } else {
                    sym.kind = SymbolKind::Generic(Vec::new());
                }
                for patdecl_id in enumerators.iter() {
                    let patdecl = self.sema.ast.patdecls[patdecl_id];
                    patdecl.ty_annot.ok_or(()).unwrap_err();
                    patdecl.expr.ok_or(()).unwrap_err();
                    //self.infer_pat(patdecl.pat, enum_ty);
                    match self.sema.ast.pats[patdecl.pat].kind {
                        PatternKind::Bind(kind, id) => {
                            assert!(kind.is_none());
                            let sym = self.sema.symbol_table.symbol_mut(id.symbol);
                            //sym.kind = SymbolKind::Type(enum_ty);
                            //symbols.push(id.symbol);
                        }
                        _ => unreachable!(),
                    }
                }
            }
            ItemKind::Func(FuncItem(params, ret_ty, body)) => {
                let fn_ty = self.mk_tyvar(decl);
                sym.kind = SymbolKind::Type(fn_ty);
                // fn f() { f() } ..?
                let mut param_tys = Vec::new();
                for param_id in params.iter() {
                    let param = self.sema.ast.patdecls[param_id];
                    let ty = if let Some(annot_expr) = param.ty_annot {
                        self.infer_eval_as_type(annot_expr)
                    } else {
                        infer_ty
                    };
                    let (default_val, ty) = if let Some(init_expr) = param.expr {
                        let (val, ty) = self.infer_eval(init_expr, ty);
                        (Some(val), ty)
                    } else {
                        (None, ty)
                    };
                    self.infer_pat(param.pat, ty);
                    param_tys.push(ty);
                }
                let ret_ty = if let Some(annot_expr) = ret_ty {
                    self.infer_eval_as_type(annot_expr)
                } else {
                    self.sema.get_intern_type(TypeKind::Unit)
                };
                self.infer(body, &self.sema.types[ret_ty]);
            }
        }

        // build struct union enum here, if they are not dependent on genericparam

    }

    fn infer_pat(&mut self, pat_id: PatId, ty_id: TypeId) {
        let err_ty = self.sema.get_intern_type(TypeKind::Err);
        let err_sym = self.sema.types[err_ty].decl;
        let pat = &mut self.sema.ast.pats[pat_id];
        pat.ty = ty_id;
        let pat = &*pat;
        let ty = &self.sema.types[ty_id];
        match pat.kind {
            PatternKind::Wildcard => {}
            PatternKind::Rest => todo!(),
            PatternKind::Constant(expr) => {
                //self.infer(expr, ty);
            }
            PatternKind::Group(pat) => {
                self.infer_pat(pat, ty_id);
            }
            PatternKind::Bind(kind, id) => {
                self.sema.symbol_table.symbols[id.symbol].kind = SymbolKind::Type(ty_id);
            }
            PatternKind::DotId(mut id, arg) => {
                match self.sema.symbol_table.symbol(ty.decl).backref {
                    AstNodeId::Item(item) => {
                        let item = self.sema.ast.items[item];
                        match item.kind {
                            ItemKind::Const(_) => todo!(),
                            ItemKind::Module(_) => todo!(),
                            ItemKind::Struct(_) => todo!(),
                            ItemKind::Union(UnionItem(_, scope)) => {
                                let variant = self.sema.symbol_table.lookup_exact(id.t, scope);
                                id.symbol = variant.unwrap_or(err_sym);
                                if let Some(arg) = arg {
                                    let sym = self.sema.symbol_table.symbol(id.symbol);
                                    let variant_ty = sym.as_type().unwrap_or(err_ty);
                                    self.infer_pat(arg, variant_ty);
                                }
                            }
                            ItemKind::Enum(EnumItem(_, scope)) => {
                                let enumerator = self.sema.symbol_table.lookup_exact(id.t, scope);
                                id.symbol = enumerator.unwrap_or(err_sym);
                                if let Some(arg) = arg {
                                    self.infer_pat(arg, err_ty)
                                }
                            }
                            ItemKind::Func(_) => todo!(),
                        }
                    }
                    _ => panic!(),
                }
                self.sema.ast.pats[pat_id].kind = PatternKind::DotId(id, arg);
            }
            PatternKind::Compound(pats) => match &ty.kind {
                &TypeKind::Array(elem_ty, sz) => {
                    for pat in pats.iter() {
                        self.infer_pat(pat, elem_ty);
                    }
                }
                TypeKind::Struct(fields) => {
                    for (pat, field_ty) in pats.iter().zip(fields.clone()) {
                        self.infer_pat(pat, field_ty);
                    }
                }
                _ => {
                    for pat in pats.iter() {
                        self.infer_pat(pat, err_ty);
                    }
                }
            },
        }
    }

    /*fn infer_dual(&mut self, outer_ty_id: &mut TypeId, expr: ExprId) -> TypeId {
        // let x: .array<u16, .infer> = {1,2,3}
        // let x: stru = { {1,2}, 3 }
        /*
        self.infer(expr, ty { inst { cons::uint } } )
        */
        let expr_ty_id = self.infer(expr, *outer_ty_id);

        *outer_ty_id
    }*/

    fn resolve_use(&mut self, sym_id: SymbolId, generic_args: &[ValueId]) -> TypeId {
        let err_ty = self.sema.get_intern_type(TypeKind::Err);
        let sym = self.sema.symbol_table.symbol(sym_id);
        // implement semi-inst
        match &sym.kind {
            SymbolKind::Unresolved => {
                match sym.backref {
                    AstNodeId::Null => unreachable!(),
                    AstNodeId::File(file) => {
                        assert!(generic_args.len() == 0);
                        let file = &self.sema.ast.files[file];

                        todo!()
                    }
                    AstNodeId::Item(item) => {

                        todo!()
                    }
                    AstNodeId::Local(_) => todo!(),
                    AstNodeId::Param(_) => todo!(),
                    AstNodeId::GenericParam(_) => todo!(),
                    AstNodeId::Enumerator(_)
                    | AstNodeId::Variant(_)
                    | AstNodeId::Field(_) => panic!(),
                }
            }
            SymbolKind::Builtin(
                kind @ (BuiltinSymbolKind::BInt | BuiltinSymbolKind::Int | BuiltinSymbolKind::UInt),
            ) => {
                assert!(generic_args.len() == 1);
                if let Some(&[size]) = self.sema.values[generic_args[0]].as_constant() {
                    let cons_fn = match kind {
                        BuiltinSymbolKind::BInt => TypeKind::BInt,
                        BuiltinSymbolKind::Int => TypeKind::Int,
                        BuiltinSymbolKind::UInt => TypeKind::UInt,
                        _ => unreachable!(),
                    };
                    let size: u16 = size.try_into().unwrap();
                    let ty = self.sema.intern_type(sym_id, cons_fn(size));
                    ty
                } else {
                    err_ty
                }
            }
            SymbolKind::Builtin(BuiltinSymbolKind::Array) => {
                assert!(generic_args.len() == 2);
                let elem_ty = self.sema.values[generic_args[0]].as_type().unwrap_or(err_ty);
                let size = self.sema.values[generic_args[1]].as_constant().map(|a| a[0]);
                let ty = self.sema
                    .intern_type(sym_id, TypeKind::Array(elem_ty, size.unwrap()));
                ty
            }
            &SymbolKind::Type(ty) => ty,
            SymbolKind::Const(_, _) => panic!(), // errty?
        }
    }

    // fn merge_mutating(&mut self, &mut a_ty_id, &mut b_ty_id) -> Result<(), ()>

    fn merge_ty(&mut self, ty1: &'b Type<'b>, ty2: &'b Type<'b>) -> &'b Type<'b> {
        let err_ty = self.intern_ty_kind(TypeKind::Err);
        match (&ty1.kind, &ty2.kind) {
            (TypeKind::Unresolved, _) | (_, TypeKind::Unresolved) => panic!(),
            (TypeKind::Infer, _) => ty2,
            (_, TypeKind::Infer) => ty1,
            (TypeKind::Inst(cons1, args1), TypeKind::Inst(cons2, args2)) if cons1 == cons2 => {
                let cons = *cons1;
                debug_assert!(args1.len() == args2.len());
                let mut merged_args = Vec::with_capacity(args1.len());
                let mut semi_inst = false;
                for (arg1, arg2) in args1
                    .iter()
                    .copied()
                    .zip(args2.iter().copied())
                {
                    let merged_arg = match (&arg1.kind, &arg2.kind) {
                        (&ValueKind::Type(a), &ValueKind::Type(b)) => {
                            let ty = self.merge_ty(a, b);
                            self.sema.intern_value(ValueKind::Type(ty))
                        }
                        (ValueKind::Infer, _) => { semi_inst= true; arg2 }
                        (_, ValueKind::Infer) => { semi_inst=true; arg1 }
                        _ => self.sema.get_intern_value(ValueKind::Err),
                    };
                    merged_args.push(merged_arg);
                }
                if semi_inst {
                }
                let sym = self.sema.symbol_table.symbol(cons);
                let item = sym.as_item().unwrap();
                //self.inst_item(item, &merged_args)
                todo!()
            }
            _ => err_ty,
        }
    }

    fn infer_call(&self, callee: TypeId, args: &[ExprId]) -> Type {
        // inst?
        let callee_ty = &self.sema.types[callee];
        match callee_ty.kind {
            TypeKind::Func(ref params, ret, body) => {
                let params = params.clone();

                //self.infer(body, ret);
                //ret_ty could be generic, we need to eval ret expr here
                // generic instantiation..?
                todo!()
            }
            _ => panic!(),
        }
    }

    fn expr(&self, id: ExprId, ty: &Type<'b>) -> Value {
        let err_ty = self.sema.get_intern_type(TypeKind::Err);
        let Expression { kind, span: _ } = &self.sema.ast.exprs[id];
        if matches!(
            ty.kind,
            TypeKind::Unresolved | TypeKind::Infer | TypeKind::Err
        ) {
            return Value {
                kind: ValueKind::Err,
            };
        }
        match kind {
            ExpressionKind::Use(_, _) => {
                // handle .infer value here? no
                todo!()
            }
            ExpressionKind::Literal(_) => todo!(),
            ExpressionKind::Group(e) => self.eval(e),
            ExpressionKind::Compound(_) => todo!(),
            ExpressionKind::Unary(kind, op) => {
                let callee = self.sema.symbol_table.symbol(kind.symbol);
                let callee = callee.as_type().unwrap_or(err_ty);
                self.eval_fn_call(callee, &[op])
            }
            ExpressionKind::Binary(kind, left, right) => {
                let callee = self.sema.symbol_table.symbol(kind.symbol);
                let callee = callee.as_type().unwrap_or(err_ty);
                self.eval_fn_call(callee, &[left, right])
            }
            ExpressionKind::Try(_) => todo!(),
            ExpressionKind::Yield(_) => todo!(),
            ExpressionKind::Await(_, _) => todo!(),
            ExpressionKind::Call(callee, args) => todo!(),
            ExpressionKind::Field(_, _) => todo!(),
            ExpressionKind::MethodCall(receiver, method, args) => todo!(),
            ExpressionKind::Case(_, _) => todo!(),
            ExpressionKind::Block(_) => todo!(),
            ExpressionKind::If(_, _, _) => todo!(),
            ExpressionKind::Match(_, _, _) => todo!(),
            ExpressionKind::Do(e) => self.eval(e),
            ExpressionKind::While(_, _) => todo!(),
            ExpressionKind::For(_, _, _, _) => todo!(),
        }
    }

    fn eval_fn_call(&self, callee: TypeId, args: &[ExprId]) -> Value {
        todo!()
    }

    fn infer(&self, id: ExprId, annot_ty: &'b Type<'b>) -> &'b Type<'b> {
        let metaty = self.sema.get_intern_type(TypeKind::Metatype);
        let infer_ty = self.sema.get_intern_type(TypeKind::Infer);
        let err_ty = self.sema.get_intern_type(TypeKind::Err);

        let metaty = &self.sema.types[infer_ty];
        let infer_ty = &self.sema.types[infer_ty];
        let err_ty = &self.sema.types[err_ty];
        let err_sym = err_ty.decl;

        let Expression { kind, ty, span: _ } = self.sema.ast.exprs[id];
        // assert! or return, ty == unresolved_ty
        let resolved_ty = match kind {
            ExpressionKind::Use(path, current_scope) => {
                let (first_id, path) = &mut self.sema.ast.ids[path].split_first_mut().unwrap();

                #[derive(Copy, Clone)]
                struct ScopePredIter<'a> {
                    scopes: &'a IndexVec<Scope>,
                    current_scope: Option<ScopeId>,
                }
                impl Iterator for ScopePredIter<'_> {
                    type Item = ScopeId;

                    fn next(&mut self) -> Option<Self::Item> {
                        let curr = self.current_scope?;
                        let pred = self.scopes[curr].pred;
                        self.current_scope = pred;
                        pred
                    }
                }

                first_id.id.symbol = self
                    .sema
                    .symbol_table
                    .lookup(
                        first_id.id.t,
                        ScopePredIter {
                            scopes: &self.sema.scopes,
                            current_scope: Some(current_scope),
                        },
                    )
                    .unwrap_or(err_sym);

                let mut sym = self.sema.symbol_table.symbol(first_id.id.symbol);
                if let Some(item) = sym.as_item()
                    && let item @ Item { generics: Some(generics), .. } = &self.sema.ast.items[item] {
                    if let Some(valty) = item.as_const() {

                    } else {
                        if let Some(apply) = first_id.apply {
                            match apply {
                                Apply::Juxtaposition(arg) => {
                                    let (val, _) = self.infer_eval(arg, infer_ty);
                                    //self.inst_type(first_id.id.symbol, &[val])
                                    todo!()
                                }
                                Apply::AngleBrackets(args) => {
                                    let vals = Vec::new();
                                    for arg in args.iter() {
                                        let (val, _) = self.infer_eval(arg, infer_ty);
                                        vals.push(val);
                                    }
                                    //self.inst_type(first_id.id.symbol, &vals)
                                    todo!()
                                }
                            }
                        } else {

                            //vec.extend(std::iter::repeat(val).take(generics.len()));
                            //self.inst_type(sym, args)
                            todo!()
                        }
                    }
                }
                // id.apply ...

                for id in path.iter() {
                    let lookup_scope = match sym.backref {
                        AstNodeId::Null
                        | AstNodeId::Param(_)
                        | AstNodeId::Enumerator(_)
                        | AstNodeId::Variant(_)
                        | AstNodeId::Field(_)
                        | AstNodeId::Local(_) => self.sema.dummy_scope,
                        AstNodeId::GenericParam(_) => unimplemented!(),
                        AstNodeId::File(file) => self.sema.ast.files[file].sub_scope,
                        AstNodeId::Item(item) => match self.sema.ast.items[item].kind {
                            ItemKind::Const(_) => self.sema.dummy_scope,
                            ItemKind::Module(ModuleItem(_, sub_scope)) => sub_scope,
                            ItemKind::Struct(_) => self.sema.dummy_scope,
                            ItemKind::Union(_) => self.sema.dummy_scope,
                            ItemKind::Enum(EnumItem(_, sub_scope)) => sub_scope,
                            ItemKind::Func(_) => self.sema.dummy_scope,
                        },
                    };

                    id.id.symbol = self
                        .sema
                        .symbol_table
                        .lookup_exact(id.id.t, lookup_scope)
                        .unwrap_or(err_sym);
                    sym = self.sema.symbol_table.symbol(id.id.symbol);
                    // id.apply ...
                }
                match sym.kind {
                    SymbolKind::Unresolved => err_ty.clone(),
                    SymbolKind::Builtin(kind) => {
                        //
                        match kind {
                            BuiltinSymbolKind::BInt => todo!(),
                            BuiltinSymbolKind::Int => todo!(),
                            BuiltinSymbolKind::UInt => todo!(),
                            BuiltinSymbolKind::Array => todo!(),
                        }
                    }
                    SymbolKind::Generic(_) => todo!(),
                    SymbolKind::Namespace => err_ty.clone(),
                    SymbolKind::Type(_) => metaty.clone(),
                    SymbolKind::Const(_, ty) => self.sema.types[ty].clone(),
                }
            }
            ExpressionKind::Literal(lit) => match lit {
                Literal::Number(_) => match annot_ty.kind {
                    // those aren't directly representable in source code but used by compiler internals
                    // i.e. while/if <cond_expr> ... infer(cond_expr, bint(1)))
                    TypeKind::BInt(width) => {
                        // retrieve appropriate decl for this
                        Type { decl: annot_ty.decl, kind: TypeKind::BInt(width) }
                    }
                    TypeKind::Int(width) => {
                        Type { decl: annot_ty.decl, kind: TypeKind::Int(width) }
                    }
                    TypeKind::UInt(width) => {
                        Type { decl: annot_ty.decl, kind: TypeKind::UInt(width) }
                    }
                    TypeKind::Inst(ty, ref args) => {
                        //self.inst_type(ty, &args.clone()),
                        todo!()
                    }
                    _ => {
                        // try i32, u32, i64, u64
                        err_ty.clone()
                    }
                },
                Literal::Char(_) => todo!(),
                Literal::Quote(_) => todo!(),
                Literal::DotId(id_id, args) => {
                    let id = self.sema.ast.ids[id_id];
                    let (assoc_sym, ty) = match annot_ty.kind {
                        TypeKind::Inst(ty, ref args) => {
                            //let ty = self.inst_type(ty, &args.clone());
                            //let ty_decl = self.sema.types[ty].decl;
                            //(ty_decl, ty)
                            todo!()
                        }
                        TypeKind::Func(_, _, _) => {
                            //
                            (annot_ty.decl, annot_ty)
                        }
                        TypeKind::Enum(_, _) => {
                            let decl = self.sema.symbol_table.symbol(annot_ty.decl);
                            let item = decl.as_item().unwrap();
                            //let item = self.sema.ast.items[item].unwrap_enum();
                            let scope = todo!();
                            let enumerator = self.sema.symbol_table.lookup_exact(id.id.t, scope);
                            (enumerator.unwrap(), annot_ty)
                        }
                        TypeKind::Union(_) => {
                            let sym = self
                                .sema
                                .symbol_table
                                .symbol(annot_ty.decl)
                                ;
                            //let item = self.sema.ast.items[sym.as_item().unwrap()].as_union().unwrap().1;
                            let scope = todo!();
                            let variant = self.sema.symbol_table.lookup_exact(id.id.t, scope);
                            match variant {
                                Ok(a) => {
                                    let ty = self.sema.symbol_table.symbol(a).as_type().unwrap();
                                    assert!(args.unwrap().len() == 1);
                                    let arg = args.unwrap().nth(0);
                                    self.infer(arg, &self.sema.types[ty]);
                                    (variant.unwrap(), annot_ty)
                                }
                                Err(_) => (err_sym, annot_ty),
                            }
                        }
                        _ => {
                            let sym = self
                                .sema
                                .symbol_table
                                .lookup_adjacent(id.id.t, annot_ty.decl)
                                .unwrap_or(err_sym);
                            let sym_data = self.sema.symbol_table.symbol(sym);
                            //(sym, sym_data.ty)
                            todo!()
                        }
                    };
                    self.sema.ast.ids[id_id].id.symbol = assoc_sym;
                    ty.clone()
                }
                Literal::Undef => annot_ty.clone(),
            },
            ExpressionKind::Group(sub) => self.infer(sub, annot_ty),
            ExpressionKind::Compound(elts) => {
                //
                match annot_ty.kind {
                    TypeKind::Inst(sym, ref args) => {
                        //self.inst_type(sym, &args.clone())
                        todo!()
                    }
                    TypeKind::Array(_, _) => {
                        //
                        annot_ty.clone()
                    }
                    TypeKind::Struct(ref fields) => {
                        for (init_expr, field_ty) in elts.iter().zip(fields.clone()) {
                            self.infer(init_expr, &self.sema.types[field_ty]);
                        }
                        annot_ty.clone()
                    }
                    _ => {
                        //
                        err_ty.clone()
                    }
                }
            }
            ExpressionKind::Unary(mut kind, op) => {
                let op_name = match kind.t {
                    UnaryOpKind::BrNot => "brnot",
                    UnaryOpKind::Not => "not",
                    UnaryOpKind::Neg => "neg",
                    UnaryOpKind::RefTo => unimplemented!(),
                };
                let op_id = mk_ident(&mut self.sema.interner, op_name);
                // also lookup for x.__not(), check for ambiguity and select one
                // same goes for binaryop.
                let sym = self.sema.symbol_table.lookup_adjacent(op_id, annot_ty.decl);
                kind.symbol = sym.unwrap_or(err_sym);
                self.sema.ast.exprs[id].kind = ExpressionKind::Unary(kind, op);

                let callee = self.sema.symbol_table.symbol(kind.symbol);
                self.infer_call(callee.as_type().unwrap(), &[op])
            }
            ExpressionKind::Binary(mut kind, left, right) => {
                let op_name = match kind.t {
                    BinaryOpKind::Add => "add",
                    BinaryOpKind::Sub => "sub",
                    BinaryOpKind::Mul => "mul",
                    BinaryOpKind::Div => "div",
                    BinaryOpKind::Mod => "mod",
                    BinaryOpKind::BitAnd => "bitand",
                    BinaryOpKind::BitOr => "bitor",
                    BinaryOpKind::BitXor => "bitxor",
                    BinaryOpKind::Shl => "shl",
                    BinaryOpKind::Shr => "shr",
                    BinaryOpKind::Lt => "lt",
                    BinaryOpKind::Le => "le",
                    BinaryOpKind::Gt => "gt",
                    BinaryOpKind::Ge => "ge",
                    BinaryOpKind::Eq => "eq",
                    BinaryOpKind::Ne => "ne",
                    BinaryOpKind::BrAnd => todo!(),
                    BinaryOpKind::BrOr => todo!(),
                    BinaryOpKind::Index => "subscript",
                };
                let op_id = mk_ident(&mut self.sema.interner, op_name);
                let sym = self.sema.symbol_table.lookup_adjacent(op_id, annot_ty.decl);
                kind.symbol = sym.unwrap_or(err_sym);
                self.sema.ast.exprs[id].kind = ExpressionKind::Binary(kind, left, right);

                let callee = self.sema.symbol_table.symbol(kind.symbol);
                self.infer_call(callee.as_type().unwrap(), &[left, right])
            }
            ExpressionKind::Try(op) => {
                let op_ty = self.infer(op, infer_ty);
                let ty_decl = self.sema.symbol_table.symbol(op_ty.decl);
                //let sub_scope = ty_decl.sub_scope.unwrap(); // del unwrap

                // .ok(e) vs e.ok, .err(e) vs e.err; .res(e).ok, .res(e).err
                // check return type
                todo!()
            }
            ExpressionKind::Yield(op) => {
                // use enclosing function's args&ret type for inference
                let op_ty = self.infer(op, infer_ty);
                annot_ty.clone()
            }
            ExpressionKind::Await(op, for_) => {
                let op_ty = self.infer(op, infer_ty);
                if let Some(e) = for_ {
                    self.infer(e, infer_ty);
                }
                todo!()
            }
            ExpressionKind::Call(callee, args) => {
                let callee_ty = if let ExpressionKind::Literal(Literal::DotId(id, _)) =
                    self.sema.ast.exprs[callee].kind
                {
                    let id = self.sema.ast.ids[id].id.t;
                    let sym = self.sema.symbol_table.lookup_adjacent(id, annot_ty.decl);
                    let sym = sym.unwrap_or(err_sym);
                    let sym = self.sema.symbol_table.symbol(sym);
                    let ty = sym.as_type().unwrap();
                    self.sema.types[ty].clone()
                } else {
                    infer_ty.clone()
                };
                let callee_ty = self.infer(callee, &callee_ty);
                let args: Vec<_> = args.iter().collect();
                todo!()
                //self.infer_call(callee_ty, &args)
            }
            ExpressionKind::Field(expr, mut field) => {
                let left = self.infer(expr, infer_ty);
                let left_decl = self.sema.symbol_table.symbol(left.decl);
                let sub_scope =  todo!(); // del unwrap
                let sym = self.sema.symbol_table.lookup_exact(field.t, sub_scope);

                field.symbol = sym.unwrap_or(err_sym);
                self.sema.ast.exprs[id].kind = ExpressionKind::Field(expr, field);

                let sym_data = self.sema.symbol_table.symbol(field.symbol);
                let ty = sym_data.as_type().unwrap();
                self.sema.types[ty].clone()
            }
            ExpressionKind::MethodCall(_, _, _) => todo!(),
            ExpressionKind::Case(expr, pat) => {
                let ty = self.infer(expr, infer_ty);
                //self.infer_pat(pat, ty);
                self.sema.types[self.sema.builtin_bint(1)].clone()
            }
            ExpressionKind::Block(blk) => match blk.kind {
                BlockKind::Void => {
                    self.traverse()
                        .helper_visit_list(blk.stmts, Self::visit_stmt);
                    // just return uninterned value
                    self.sema.get_intern_type(TypeKind::Void);
                    todo!()
                }
                BlockKind::Trailing(expr) => {
                    for i in 0..blk.stmts.len().checked_sub(1).unwrap() {
                        self.visit_stmt(blk.stmts.nth(i));
                    }
                    self.infer(expr, annot_ty)
                }
            },
            ExpressionKind::If(cond, then, els) => {
                self.infer(cond, &self.sema.types[self.sema.builtin_bint(1)].clone());
                let then_ty = self.infer(then, annot_ty);
                let else_ty = if let Some(els) = els {
                    let ty = self.infer(els, annot_ty);
                    Some(ty)
                } else {
                    None
                };
                // else_ty ..?
                then_ty
            }
            ExpressionKind::Match(op, cases, bodies) => {
                let is_infer = annot_ty.is_infer();
                let op_ty = self.infer(op, infer_ty);
                for case in cases.iter() {
                    //self.infer_pat(case, op_ty)
                }
                let mut tyy = if is_infer {
                    self.sema.types[self.sema.get_intern_type(TypeKind::Unit)]
                } else {
                    annot_ty.clone()
                };
                for body in bodies.iter() {
                    tyy =
                    self.infer(body, &tyy);
                }
                tyy
            }
            ExpressionKind::Do(body) => self.infer(body, annot_ty),
            ExpressionKind::While(cond, body) => {
                self.infer(cond, &self.sema.types[self.sema.builtin_bint(1)]);
                self.infer(body, &self.sema.types[self.sema.get_intern_type(TypeKind::Void)]);
                self.sema.types[self.sema.get_intern_type(TypeKind::Void)].clone()
            }
            ExpressionKind::For(_, _, _, _) => todo!(),
        };
        resolved_ty
    }
}

impl AstVisitor<()> for EvaluatorRef<'_> {
    fn ast(&self) -> &Ast {
        &self.sema.ast
    }

    fn int(&self) -> &Interner {
        &self.sema.interner
    }

    fn visit_root(&mut self) {
        self.traverse().visit_root()
    }

    fn visit_ident(&mut self, _id: Ident) {}

    fn visit_path(&mut self, id: IdListId) {
        /*
            let mut lookup_scope = LookupScope::Current;
            for id in id.iter() {
                let ident = self.sema.ast.ids[id].id.id;
                let symbol = self.lookup_id(ident, lookup_scope);
                let id = &mut self.sema.ast.ids[id];
                let symbol = match symbol {
                    Some(sym) => {
                        id.id.kind = NameKind::Resolved(sym);
                        sym
                    }
                    None => {
                        id.id.kind = NameKind::Unresolved;
                        break;
                    }
                };
                if let Some(sub_scope) = self.symbol_table.symbol(symbol).sub_scope {
                    lookup_scope = LookupScope::Exact(sub_scope);
                } else {
                    id.id.kind = NameKind::Err;
                    break;
                }
            }
        */
        //self.traverse().visit_path(id)
    }

    fn visit_id(&mut self, id: IdId) {
        //let ident = self.sema.ast.ids[id].id.t;
        //self.lookup_id(ident, LookupScope::Current);
        self.traverse().visit_id(id)
    }

    fn visit_source_file(&mut self, id: SourceFileId) {
        self.traverse().visit_source_file(id); // where to place traversals?
        let meta_ty = self.sema.get_intern_type(TypeKind::Metatype);
        let elem = &mut self.sema.ast.files[id];
        let sym = self.sema.symbol_table.symbol_mut(elem.name.symbol);
        assert!(matches!(sym.kind, SymbolKind::Unresolved));
        sym.kind = SymbolKind::Namespace;
    }

    fn visit_item(&mut self, id: ItemId) {
        self.traverse().visit_item(id);
    }

    fn visit_stmt(&mut self, id: StmtId) {
        self.traverse().visit_stmt(id)
    }

    fn visit_expr(&mut self, id: ExprId) {
        //let Expression { kind, ty, span: _ } = &self.sema.ast.exprs[id];
        let infer_ty = self.sema.get_intern_type(TypeKind::Infer);
        self.infer(id, &self.sema.types[infer_ty]);
    }

    fn visit_pat(&mut self, id: PatId) {
        self.traverse().visit_pat(id)
    }

    fn visit_local(&mut self, id: PatDeclId) {
        self.traverse().visit_local(id)
    }

    fn visit_param(&mut self, id: PatDeclId) {
        self.traverse().visit_param(id)
    }

    fn visit_generic_param(&mut self, id: PatDeclId) {
        self.traverse().visit_generic_param(id)
    }

    fn visit_enumerator(&mut self, id: PatDeclId) {
        self.traverse().visit_enumerator(id)
    }

    fn visit_variant(&mut self, id: FieldId) {
        self.traverse().visit_variant(id)
    }

    fn visit_field(&mut self, id: FieldId) {
        self.traverse().visit_field(id)
    }
}
*/
