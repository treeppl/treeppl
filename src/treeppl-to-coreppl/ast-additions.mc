include "mexpr/ast.mc"
include "mexpr/json-debug.mc"
include "mexpr/type-check.mc"
include "mexpr/op-overload.mc"

-- Pattern match over an algebraic type where each constructor carries
-- a record, pulling out the field with the given name.
lang ProjMatchAst = Ast
  syn Expr =
  | TmProjMatch {info : Info, target : Expr, field : SID, ty : Type}

  sem infoTm =
  | TmProjMatch x -> x.info

  sem tyTm =
  | TmProjMatch x -> x.ty

  sem withInfo info =
  | TmProjMatch x -> TmProjMatch {x with info = info}

  sem withType ty =
  | TmProjMatch x -> TmProjMatch {x with ty = ty}

  sem smapAccumL_Expr_Expr f acc =
  | TmProjMatch x ->
    match f acc x.target with (acc, target) in
    (acc, TmProjMatch {x with target = target})
end

lang ProjMatchPprint = PrettyPrint + ProjMatchAst
  sem isAtomic =
  | TmProjMatch _ -> false

  sem pprintCode indent env =
  | TmProjMatch x ->
    match printParen indent env x.target with (env, target) in
    (env, join [target, ".", pprintLabelString x.field])
end

lang PullName = AppTypeAst + ConTypeAst + MetaVarTypeAst + AliasTypeAst
  sem pullName =
  | TyApp x -> pullName x.lhs
  | TyCon x -> Some x.ident
  | ty -> (rappAccumL_Type_Type (lam. lam ty. (pullName ty, ty)) (None ()) ty).0
end

lang PullNameFromConstructor = PullName + FunTypeAst + AllTypeAst
  sem pullName =
  | TyArrow x -> pullName x.to
  | TyAll x -> pullName x.ty
end

lang ProjMatchTypeCheck = TypeCheck + ProjMatchAst + FunTypeAst + RecordTypeAst + RecordPat
  sem typeCheckExpr env =
  | TmProjMatch x ->
    let target = typeCheckExpr env x.target in
    match (use PullName in pullName) (tyTm target) with Some tyName then
      let constructorIsRelevant = lam pair.
        match pair with (conName, (conLvl, conTy)) in
        match use PullNameFromConstructor in pullName conTy with Some conTyName then
          if nameEq conTyName tyName then
            match inst x.info env.currentLvl conTy with TyArrow arr in
            unify env [infoTm target] arr.to (tyTm target);
            match unwrapType arr.from with recTy & TyRecord rec then
              optionMap (lam x. {conName = conName, recTy = recTy, fieldTy = x}) (mapLookup x.field rec.fields)
            else None ()
          else None ()
        else None () in
      -- OPT(vipa, 2022-12-20): This potentially repeats a lot of work
      let relevantConstructors = mapOption constructorIsRelevant (mapBindings env.conEnv) in
      match relevantConstructors with [c] ++ cs then
        for_ cs (lam c2. unify env [infoTy c.fieldTy, infoTy c2.fieldTy] c.fieldTy c2.fieldTy);
        desugarTmProjMatch x.info target x.field c.fieldTy relevantConstructors
      else errorSingle [infoTm target] (join ["This value doesn't appear to have a '", sidToString x.field, "' field."])
    else errorSingle [infoTm target] "This type isn't known to be a custom type, maybe you need to add a type annotation?"

  sem desugarTmProjMatch info target field fieldTy = | relevantConstructors  ->
    let errorMsg =
      let msg = join ["Field '", sidToString field, "' not found"] in
      match errorMsg [{errorDefault with info = info, msg = msg}] {single = "", multi = ""}
      with (info, msg) in
      let msg = infoErrorString info msg in
      let print = print_ (str_ msg) in
      let exit = exit_ (int_ 1) in
      semi_ print exit
    in

    -- TODO(vipa, 2022-12-20): Move these to ast-builder
    let inpcon_ = lam i. lam n. lam p. withInfoPat i (npcon_ n p) in
    let invar_ = lam i. lam n. withInfo i (nvar_ n) in
    let imatch_ = lam i. lam s. lam p. lam t. lam e. withInfo i (match_ s p t e) in
    let inpvar_ = lam i. lam n. withInfoPat i (npvar_ n) in

    let iSidRecordProj_ = lam i. lam target. lam sid. lam recordTy.
      let varName = nameSym "x" in
      let pat = PatRecord
        { bindings = mapInsert sid (withTypePat fieldTy (inpvar_ i varName)) (mapEmpty cmpSID)
        , info = i
        , ty = recordTy
        } in
      withType fieldTy (imatch_ i target pat (withType fieldTy (invar_ i varName)) (withType fieldTy never_)) in
    let match_ = imatch_ info in
    let npcon_ = inpcon_ info in
    let nvar_ = invar_ info in
    let npvar_ = inpvar_ info in
    let targetTy = tyTm target in
    let varName = nameSym "target" in
    let var = withType targetTy (invar_ info varName) in
    let wrap : Expr -> {conName : Name, recTy : Type, fieldTy : Type} -> Expr = lam next. lam alt.
      let xName = nameSym "x" in
      withType alt.fieldTy
        (match_ var (withTypePat targetTy (npcon_ alt.conName (withTypePat alt.recTy (npvar_ xName))))
          (iSidRecordProj_ info (withType alt.recTy (nvar_ xName)) field alt.recTy)
          next) in
    withType fieldTy
      (bind_
        (nulet_ varName target)
        (foldl wrap errorMsg relevantConstructors))
end

lang ProjMatchToJson = AstToJson + ProjMatchAst
  sem exprToJson =
  | TmProjMatch x -> JsonObject (mapFromSeq cmpString
    [ ("con", JsonString "TmProjMatch")
    , ("target", exprToJson x.target)
    , ("field", JsonString (sidToString x.field))
    , ("ty", typeToJson x.ty)
    , ("info", infoToJson x.info)
    ] )
end

lang TreePPLOperators
  = OverloadedOpDesugarLoader
  + IntTypeAst + CmpIntTypeAst
  + FloatTypeAst + CmpFloatAst
  + CmpCharAst
  + GenerateEqLoader

  syn Op =
  | OpEq
  | OpNeq

  | OpLt
  | OpGt
  | OpLeq
  | OpGeq

  | OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpPow

  | OpNeg

  sem opMkTypes info env =
  | OpLt _ | OpGt _ | OpLeq _ | OpGeq _ ->
    let l = newmonovar env.currentLvl info in
    let r = newmonovar env.currentLvl info in
    {params = [l, r], return = tyWithInfo info tybool_}
  | OpEq _ | OpNeq _ ->
    let ty = newmonovar env.currentLvl info in
    {params = [ty, ty], return = tyWithInfo info tybool_}
  | OpAdd _ | OpSub _ | OpMul _ | OpDiv _ | OpPow _ ->
    let ty = newmonovar env.currentLvl info in
    {params = [ty, ty], return = ty}
  | OpNeg _ ->
    let ty = newmonovar env.currentLvl info in
    {params = [ty], return = ty}

  sem _liftIntToFloat : Expr -> (Bool, Bool) -> Expr
  sem _liftIntToFloat f =
  | (false, false) -> f
  | (true, false) ->
    let l = nameSym "l" in
    (nlam_ l tyint_ (app_ f (int2float_ (nvar_ l))))
  | (false, true) ->
    let l = nameSym "l" in
    let r = nameSym "r" in
    (nlam_ l tyfloat_ (nlam_ r tyint_ (appf2_ f (nvar_ l) (int2float_ (nvar_ r)))))
  | (true, true) ->
    let l = nameSym "l" in
    let r = nameSym "r" in
    (nlam_ l tyint_ (nlam_ r tyint_ (appf2_ f (int2float_ (nvar_ l)) (int2float_ (nvar_ r)))))

  sem resolveOp info =
  | {op = OpLt _, params = [TyInt _, TyInt _]} -> mkConst info (CLti ())
  | {op = OpLt _, params = [TyInt _, TyFloat _]} -> _liftIntToFloat (mkConst info (CLtf ())) (true, false)
  | {op = OpLt _, params = [TyFloat _, TyInt _]} -> _liftIntToFloat (mkConst info (CLtf ())) (false, true)
  | {op = OpLt _, params = [TyFloat _, TyFloat _]} -> mkConst info (CLtf ())

  | {op = OpGt _, params = [TyInt _, TyInt _]} -> mkConst info (CGti ())
  | {op = OpGt _, params = [TyInt _, TyFloat _]} -> _liftIntToFloat (mkConst info (CGtf ())) (true, false)
  | {op = OpGt _, params = [TyFloat _, TyInt _]} -> _liftIntToFloat (mkConst info (CGtf ())) (false, true)
  | {op = OpGt _, params = [TyFloat _, TyFloat _]} -> mkConst info (CGtf ())

  | {op = OpLeq _, params = [TyInt _, TyInt _]} -> mkConst info (CLeqi ())
  | {op = OpLeq _, params = [TyInt _, TyFloat _]} -> _liftIntToFloat (mkConst info (CLeqf ())) (true, false)
  | {op = OpLeq _, params = [TyFloat _, TyInt _]} -> _liftIntToFloat (mkConst info (CLeqf ())) (false, true)
  | {op = OpLeq _, params = [TyFloat _, TyFloat _]} -> mkConst info (CLeqf ())

  | {op = OpGeq _, params = [TyInt _, TyInt _]} -> mkConst info (CGeqi ())
  | {op = OpGeq _, params = [TyInt _, TyFloat _]} -> _liftIntToFloat (mkConst info (CGeqf ())) (true, false)
  | {op = OpGeq _, params = [TyFloat _, TyInt _]} -> _liftIntToFloat (mkConst info (CGeqf ())) (false, true)
  | {op = OpGeq _, params = [TyFloat _, TyFloat _]} -> mkConst info (CGeqf ())

  | {op = OpAdd _, params = [TyInt _] ++ _} -> mkConst info (CAddi ())
  | {op = OpAdd _, params = [TyFloat _] ++ _} -> mkConst info (CAddf ())

  | {op = OpSub _, params = [TyInt _] ++ _} -> mkConst info (CSubi ())
  | {op = OpSub _, params = [TyFloat _] ++ _} -> mkConst info (CSubf ())

  | {op = OpMul _, params = [TyInt _] ++ _} -> mkConst info (CMuli ())
  | {op = OpMul _, params = [TyFloat _] ++ _} -> mkConst info (CMulf ())

  | {op = OpDiv _, params = [TyInt _] ++ _} -> mkConst info (CDivi ())
  | {op = OpDiv _, params = [TyFloat _] ++ _} -> mkConst info (CDivf ())

  | {op = OpNeg _, params = [TyInt _] ++ _} -> mkConst info (CNegi ())
  | {op = OpNeg _, params = [TyFloat _] ++ _} -> mkConst info (CNegf ())

  sem resolveOpLoader loader info =
  | {op = OpEq _, params = [ty] ++ _} ->
    match eqFunctionsFor [ty] loader with (loader, [eqf]) in
    (loader, eqf)
  | {op = OpNeq _, params = [ty] ++ _} ->
    match eqFunctionsFor [ty] loader with (loader, [eqf]) in
    let l = nameSym "l" in
    let r = nameSym "r" in
    (loader, nlam_ l ty (nlam_ r ty (not_ (appf2_ eqf (nvar_ l) (nvar_ r)))))
  | {op = OpPow _, params = [TyFloat _] ++ _} ->
    match includeFileExn "." "stdlib::ext/math-ext.mc" loader with (mathEnv, loader) in
    (loader, nvar_ (_getVarExn "pow" mathEnv))
end
