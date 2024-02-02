/-
TreePPL Compiler and Unit Tests

Unit tests available via:

  mi compile models/treeppl-to-coreppl/compile.mc --test

-/

include "../treeppl-ast.mc"
include "../src-location.mc"
include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"
include "mexpr/boot-parser.mc"
include "mexpr/type-check.mc"
include "mexpr/generate-json-serializers.mc"
include "mexpr/utils.mc"
include "mexpr/duplicate-code-elimination.mc"

include "sys.mc"

include "coreppl::coreppl-to-mexpr/compile.mc"
include "coreppl::coreppl-to-mexpr/runtimes.mc"
include "coreppl::coreppl-to-rootppl/compile.mc"
include "coreppl::coreppl.mc"
include "coreppl::parser.mc"
include "coreppl::build.mc"

include "matrix.mc"

-- Version of parseMCoreFile needed to get input data into the program
let parseMCoreFileNoDeadCodeElim = lam filename.
  use BootParser in
    parseMCoreFile
      {defaultBootParserParseMCoreFileArg with eliminateDeadCode = false}
        filename

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
        match pair with (conName, conTy) in
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

lang TreePPLCompile = TreePPLAst + MExprPPL + MExprFindSym + RecLetsAst + Externals + MExprSym + FloatAst + ProjMatchAst + Resample + GenerateJsonSerializers + MExprEliminateDuplicateCode

  -- a type with useful information passed down from compile
  type TpplCompileContext = {
    serializeResult: Name,
    logName: Name, expName: Name,
    json2string: Name,
    particles: Name, sweeps: Name, input: Name, some: Name,
    matrixMul: Name,
    matrixPow: Name,
    matrixAdd: Name,
    matrixMulFloat: Name,
    pow: Name
  }

  sem isemi_: Expr -> Expr -> Expr
  sem isemi_ l =
  | r ->
    let infoL = infoTm l in
    let infoR = infoTm r in
    let mergedInfo = mergeInfo infoL infoR in
    withInfo mergedInfo (semi_ l r)


  sem compileFunctionsOnly: TpplCompileContext -> FileTppl -> [Expr]
  sem compileFunctionsOnly (cc: TpplCompileContext) =
  | DeclSequenceFileTppl x ->

      -- Compile type definitions
      let types = map (compileTpplTypeDecl cc) x.decl in

      let typeNames = mapOption (lam x. x.0) types in
      let constructors = join (map (lam x. x.1) types) in
      let bindType = lam inexpr. lam name.
        TmType {
          ident = name.v,
          params = [],
          tyIdent = tyWithInfo name.i (tyvariant_ []),
          inexpr = inexpr,
          ty = tyunknown_,
          info = name.i
        }
      in
      let bindCon = lam inexpr. lam pair.
        TmConDef {
          ident = pair.0 .v,
          tyIdent = pair.1,
          inexpr = inexpr,
          ty = tyunknown_,
          info = pair.0 .i
        }
      in
      let types = foldl bindCon unit_ constructors in
      let types = foldl bindType types typeNames in

      -- Compile TreePPL functions
      let functions = TmRecLets {
        bindings = mapOption (compileTpplFunction cc) x.decl,
        inexpr = unit_,  -- No specific logic to execute, just a placeholder
        ty = tyunknown_,
        info = x.info
      } in

      -- Combine the compiled types and functions
      --let complete = bindall_ [ types, functions ] in

      -- Remove duplicate definitions
      --let complete = eliminateDuplicateCode complete in

      --complete
      [types, functions]




  sem compile: FileTppl -> Expr
  sem compile =
  | DeclSequenceFileTppl x ->

    -- Retrieve standard library AST
    let stdlib = parseMCorePPLFileLib false (concat tpplSrcLoc "/lib/standard.mc") in

    -- Retrieve compiler library AST
    let libCompile =
      parseMCorePPLFileLib false
        (concat tpplSrcLoc "/treeppl-to-coreppl/lib-compile.mc") in

    -- Symbolize the compiler library AST: it is not visible in the compiled TreePPL program.
    let libCompile = symbolize libCompile in

    -- Extract names and use them to build the compile context
    match findNamesOfStringsExn [
      "serializeResult", "externalLog", "externalExp", "json2string",
      "particles", "sweeps", "input", "Some", "matrixMul", "mtxPow", "matrixElemAdd",
      "matrixMulFloat", "pow"
    ] libCompile with [sr, el, ee, j2s, p, sw, i, s, mm, mp, ma, mmf, po] in
    let cc: TpplCompileContext = {
      serializeResult = sr,
      logName = el, expName = ee,
      json2string = j2s,
      particles = p,
      sweeps = sw,
      input = i,
      some = s,
      matrixMul = mm,
      matrixPow = mp,
      matrixAdd = ma,
      matrixMulFloat = mmf,
      pow = po
    } in

    let tpplLibLoc = (concat tpplSrcLoc "/lib/standard.tppl") in
    let tpplLibContent = readFile tpplLibLoc in
    match parseTreePPLExn tpplLibLoc tpplLibContent with tpplLibFile in
    match (compileFunctionsOnly cc tpplLibFile) with [libTypes, libFunctions] in

    -- Compile the model function
    let invocation = match findMap mInvocation x.decl with Some x
      then x
      else printLn "You need a model function!"; exit 1
    in
    match invocation with (invocation, argNameTypes, returnType) in

    -- Compile type definitions and generate required JSON
    -- serializers/deserializers
    let types = map (compileTpplTypeDecl cc) x.decl in
    let typeNames = mapOption (lam x. x.0) types in
    let constructors = join (map (lam x. x.1) types) in
    let bindType = lam inexpr. lam name.
      TmType {
        ident = name.v,
        params = [],
        tyIdent = tyWithInfo name.i (tyvariant_ []),
        inexpr = inexpr,
        ty = tyunknown_,
        info = name.i  -- NOTE(vipa, 2022-12-22): This makes `type T in e` just point to `T`, which might not be desirable
      }
    in
    let bindCon = lam inexpr. lam pair.
      TmConDef {
        ident = pair.0 .v,
        tyIdent = pair.1,
        inexpr = inexpr,
        ty = tyunknown_,
        info = pair.0 .i
      }
    in
    let types = foldl bindCon unit_ constructors in
    let types = foldl bindType types typeNames in
    let types = bindall_ [libTypes, types] in
    let inputType: Type = tyrecord_ (map (lam t. (nameGetStr t.0, t.1)) argNameTypes) in

    let modelTypes: [Type] = [inputType, returnType] in
    match addJsonSerializers modelTypes types with (result, types, _) in
    match mapLookup returnType result with Some outputHandler in

    -- Compile TreePPL functions
    let functions = TmRecLets {
      bindings = mapOption (compileTpplFunction cc) x.decl, --functionBindings,
      --inexpr = infer_ (Default {}) (ulam_ "" invocation), -- need to serialize on top, otherwise the Dist excapes
      inexpr = invocation, -- will be thrown away due to subsequent bind_
      ty = tyunknown_,
      info = x.info
    } in

    -- Generate code that deserializes input
    match mapLookup inputType result with Some inputHandler in
    let error = error_ (str_ "Could not deserialize data input!") in
    let m = nameSym "m" in
    let inputR = ulet_ "r" (
      match_
        (app_ inputHandler.deserializer (nvar_ cc.input))
        (npcon_ cc.some (npvar_ m))
        (nvar_ m)
        error) in
    let inputArgs = map
      (lam t. nulet_ t.0 (recordproj_ (nameGetStr t.0) (var_ "r")))
      argNameTypes in
    -- (map (lam x. x.0) argNameTypes)
    --  mapLookup argType result -- gives us the deserializer
    -- 1. Define a function that deserializes
    -- A sequence of let bindings, one for every model parameter

    let inferCode = bindall_
      [ ulet_ "res" (infer_ (Default { runs = (nvar_ cc.particles) }) (ulam_ "" invocation))
      , ulet_ "resJson"
        (appf2_ (nvar_ cc.serializeResult) outputHandler.serializer (var_ "res"))
      , bindSemi_
        (print_ (app_ (nvar_ cc.json2string) (var_ "resJson")))
        (print_ (str_ "\n"))
      ] in

    -- Put everything together ...
    let complete = bindall_ (join
      [ [ stdlib
        , libCompile
        , types
        , libFunctions
        , functions
        , inputR
        ]
      , inputArgs
      , [ appf2_ (var_ "repeat") (ulam_ "" inferCode) (nvar_ cc.sweeps) ]
      ]) in

    -- ... and also make sure to remove duplicate definitions
    let complete = eliminateDuplicateCode complete in

    complete

    -- NOTE(2023-08-11,dlunde): No need to symbolize here actually, the CorePPL compiler will symbolize everything anyway.
    -- let env = symEnvEmpty in
    -- symbolizeExpr ({env with varEnv = mapInsert "log" cc.logName (mapInsert "exp" cc.expName env.varEnv)}) complete

 sem mInvocation: DeclTppl -> Option (Expr, [(Name, Type)], Type)
 sem mInvocation =

  | _ -> None ()

  | FunDeclTppl (x & {model = Some _}) ->
    let getNameType = lam arg.
      let aName: Name = arg.name.v in
      (aName, compileTypeTppl arg.ty)
    in
    let argNameTypes: [(Name, Type)] = map getNameType x.args in
    let returnType =
      match x.returnTy with Some ty
        then compileTypeTppl ty
        else
          printError "Error: The model function must have a return type, even if it is void, i.e. `()`!\n";
          flushStderr ();
          exit 1
    in
    let invar = TmVar {
        ident = x.name.v,
        info = x.name.i,
        ty = tyunknown_,
        frozen = false
      } in

    let f = lam f. lam arg.
      TmApp {
        lhs = f,
        rhs = parseArgument arg,
        ty = tyunknown_,
        info = x.info
      }
    in
    -- (vsenderov, 2023-07-20) Check if the arguments list is empty.
    -- If it is, don't wrap the invar in a lambda!
    -- Only ensure that the function application happens
    if null x.args then
      Some ((app_ invar (int_ 0)), [], returnType)
    else
      Some ((foldl f invar x.args), argNameTypes, returnType)

  sem parseArgument: {name:{v:Name, i:Info}, ty:TypeTppl} -> Expr
  sem parseArgument =

  | x -> TmVar {
      ident = x.name.v,
      ty = tyunknown_,
      info = x.name.i,
      frozen = false
    }

  sem compileTpplTypeDecl: TpplCompileContext -> DeclTppl -> (Option {v:Name, i:Info}, [({v:Name, i:Info}, Type)])
  sem compileTpplTypeDecl context =
  | TypeDeclTppl x ->
    let f = lam c. match c with TypeCon c in
      let mkField = lam x. (x.name.v, compileTypeTppl x.ty) in
      let tycon = tyWithInfo x.name.i (ntycon_ x.name.v) in
      let record = tyWithInfo x.info (tyrecord_ (map mkField c.fields)) in
      (c.name, tyWithInfo x.info (tyarrow_ record tycon))
    in
    (Some x.name, map f x.cons)
  | _ -> (None (), [])

  sem compileTpplFunction: TpplCompileContext -> DeclTppl -> Option RecLetBinding
  sem compileTpplFunction (context: TpplCompileContext) =

  | FunDeclTppl f ->
    let body = foldr (lam f. lam e. f e)
      (withInfo f.info unit_)
      (concat (map compileFunArg f.args) (map (compileStmtTppl context) f.body))
    in
    let argTypes = if null f.args
      then [tyint_] -- nullary functions are ints
      else map (lam a. compileTypeTppl a.ty) f.args in
    let returnType = match f.returnTy with Some ty
      then compileTypeTppl ty
      else tyunknown_
    in
    let mType = foldr tyarrow_ returnType argTypes in
    Some {
      ident = f.name.v,
      tyBody = tyunknown_,
      tyAnnot = tyWithInfo f.name.i mType,
      body = if null f.args then
        -- vsenderov 2023-08-04 Taking care of nullary functions by wrapping them in a lambda
        printError (join [ "NOTE: Zero-argument function, `"
                         , f.name.v.0
                         , "`, converted to Int. "
                         , "Potential type errors might refer to Int type."
                         , "\n"
                         ]);
        flushStderr () ;
        TmLam {
          ident =  nameNoSym "_",
          tyAnnot = TyInt { info = f.info },
          tyParam = tyunknown_,
          body = body,
          ty = tyunknown_,
          info = f.info
        } else
          body,
      info = f.info
    }
  | TypeDeclTppl _ -> None ()

  sem compileFunArg: {name:{v:Name, i:Info}, ty:TypeTppl} -> (Expr -> Expr)

  sem compileFunArg =
  | arg ->
    lam cont.
    TmLam {
      ident = arg.name.v,
      tyAnnot = compileTypeTppl arg.ty,
      tyParam = tyunknown_,
      body = cont,
      ty = tyunknown_,
      info = arg.name.i
    }

  sem compileTypeTppl: TypeTppl -> Type

  sem compileTypeTppl =
  | TypeUsageTypeTppl x -> TyCon {
      ident = x.name.v,
      info = x.name.i
    }

  | AtomicRealTypeTppl x -> TyFloat {
      info = x.info
    }

  | AtomicBoolTypeTppl x -> TyBool {
      info = x.info
    }

  | AtomicIntTypeTppl x -> TyInt {
      info = x.info
    }

  | TypeSequenceTypeTppl x -> TySeq {
    info = x.info,
    ty = compileTypeTppl x.ty
  }
-- TODO type matrix
  | TpplStrTypeTppl x -> TySeq {
    info = x.info,
    ty = TyChar {
      info = NoInfo () -- I put the info up
    }
  }

  | NothingTypeTppl x -> tyunit_

  | TensorTypeTppl x -> TyTensor {
    info = x.info,
    ty = compileTypeTppl x.ty
  }

  sem compileStmtTppl: TpplCompileContext -> StmtTppl -> (Expr -> Expr)

  sem compileStmtTppl (context: TpplCompileContext) =

  | ExprStmtTppl x ->
    -- TODO(vipa, 2022-12-22): Info field for the entire semi?
    lam cont. isemi_ (compileExprTppl context x.e) cont

  | AssumeStmtTppl a ->
    lam cont. TmLet {
      ident = a.randomVar.v,
      tyBody = tyunknown_,
      tyAnnot = optionMapOr tyunknown_ compileTypeTppl a.ty,
      body = TmAssume {
        dist = compileExprTppl context a.dist,
        ty = tyunknown_,
        info = a.info
      },
      inexpr = cont,
      ty = tyunknown_,
      info = a.info
    }

  | ObserveStmtTppl x ->
    lam cont.
      let obs = TmObserve {
        info = x.info,
        value = compileExprTppl context x.value,
        dist = compileExprTppl context x.dist,
        ty = tyunknown_
      } in
      -- TODO(vipa, 2022-12-22): Info for semi?
      (isemi_ obs cont)

  | ResampleStmtTppl x ->
    lam cont.
      let res = TmResample { info = x.info, ty = tyunknown_ } in
      isemi_ res cont

  | AssignStmtTppl a ->
    lam cont. TmLet {
      ident = a.var.v,
      tyBody = tyunknown_,
      tyAnnot = optionMapOr tyunknown_ compileTypeTppl a.ty,
      body =  compileExprTppl context a.val,
      inexpr = cont,
      ty = tyunknown_,
      info = a.info
    }

  | WeightStmtTppl a ->
    lam cont.

    let cExpr: Expr = (compileExprTppl context a.value) in
    let logExpr: Expr = withInfo a.info (app_ (withInfo a.info (nvar_ context.logName)) cExpr) in
    let tmp = TmLet {
      ident = nameNoSym "foo",
      tyBody = tyunknown_,
      tyAnnot = tyunknown_,
      body =  TmWeight {
        weight = logExpr,
        --weight = cExpr,
        ty = tyunknown_,
        info = a.info
      },
      inexpr = cont,
      ty = tyunknown_,
      info = a.info
    } in
    --printLn (mexprPPLToString tmp);
    tmp

  | LogWeightStmtTppl a ->
    lam cont.

    let tmp = TmLet {
      ident = nameNoSym "foo",
      tyBody = tyunknown_,
      tyAnnot = tyunknown_,
      body =  TmWeight {
        weight = compileExprTppl context a.value,
        ty = tyunknown_,
        info = a.info
      },
      inexpr = cont,
      ty = tyunknown_,
      info = a.info
    } in
    tmp

  /--
  To avoid code duplication.
  Intuition: compiling with continuations
  Instead of
    a; cont
  we do
    let c = lam x:(). cont in
    a; c()

  Here c corresponds to contF.
  --/
  -- TODO for Daniel: have C compiler handle f()
  | IfStmtTppl a ->
    lam cont.
    let contName = nameSym "ifCont" in
    let contF = lam_ "" tyint_ cont in -- continuation function
    let cont: Expr = withInfo a.info (app_ (nvar_ contName) (int_ 0)) in
    TmLet {
      ident = contName,
      body = contF,
      tyBody = tyunknown_,
      tyAnnot = tyunknown_,
      ty = tyunknown_,
      info = a.info,
      inexpr = TmMatch {
        target = compileExprTppl context a.condition,
        pat    = withInfoPat (get_ExprTppl_info a.condition) ptrue_,
        thn    = foldr (lam f. lam e. f e) cont (map (compileStmtTppl context) a.ifTrueStmts),
        els    = foldr (lam f. lam e. f e) cont (map (compileStmtTppl context) a.ifFalseStmts),
        ty     = tyunknown_,
        info   = a.info
      }
    }

  | ForLoopStmtTppl x -> lam cont.
    let var_ = lam n. TmVar {ident = n, ty = tyunknown_, info = x.info, frozen = false} in
    let lam_ = lam n. lam body. TmLam {ident = n, ty = tyunknown_, info = x.info, body = body, tyAnnot = tyunknown_, tyParam = tyunknown_} in
    let match_ = lam target. lam pat. lam thn. lam els. TmMatch { target = target, pat = pat, thn = thn, els = els, info = x.info, ty = tyunknown_ } in
    let app_ = lam l. lam r. TmApp { lhs = l, rhs = r, info = x.info, ty = tyunknown_ } in
    let consPat_ = lam head. lam rest. PatSeqEdge
      { prefix = [PatNamed {ident = PName head.v, info = head.i, ty = tyunknown_}]
      , middle = PName rest
      , postfix = []
      , info = x.info
      , ty = tyunknown_
      } in
    let loop_ : Expr -> ((Expr -> Expr) -> Expr) -> Expr = lam arg. lam mkBody.
      let fName = nameSym "for" in
      TmRecLets
      { bindings =
        [ { ident = fName
          , tyAnnot = tyunknown_
          , tyBody = tyunknown_
          , body = mkBody (app_ (var_ fName))
          , info = x.info
          }
        ]
      , inexpr = app_ (var_ fName) arg
      , ty = tyunknown_
      , info = x.info
      } in
    let param = nameSym "l" in
    let rest = nameSym "l" in
    loop_ (compileExprTppl context x.range)
      (lam recur.
        lam_ param
          (match_ (var_ param) (consPat_ x.iterator rest)
            (foldr (lam f. lam e. f e) (recur (var_ rest)) (map (compileStmtTppl context) x.forStmts))
            cont))

  | ReturnStmtTppl r ->
    lam cont. match r.return with Some x then compileExprTppl context x else withInfo r.info unit_

  | PrintStmtTppl x ->
    lam cont.
      foldr isemi_ cont
        [ dprint_ (compileExprTppl context x.printable)
        -- , print_ (str_ "\n") in
        , flushStdout_ unit_
        ]

  sem compileExprTppl: TpplCompileContext -> ExprTppl -> Expr

  sem compileExprTppl (context: TpplCompileContext) =

  | ProjectionExprTppl x ->
    TmProjMatch {
      info = x.info,
      target = compileExprTppl context x.target,
      field = stringToSid x.field.v,
      ty = tyunknown_
    }

  | FunCallExprTppl x ->
    let f = compileExprTppl context x.f in
    let app = lam f. lam arg.
      TmApp {
        info = x.info,
        lhs = f,
        rhs = compileExprTppl context arg,
        ty = tyunknown_
      } in
    -- (vsenderov, 2023-08-04): If we are calling a nullary function,
    -- in reailty the function is a function of int
    if null x.args then
      TmApp {
        info = x.info,
        lhs = f,
        rhs = int_ 0,
        ty = tyunknown_
      }
    else
      foldl app f x.args

  | BernoulliExprTppl d ->
    TmDist {
      dist = DBernoulli {
        p = compileExprTppl context d.prob
      },
      ty = tyunknown_,
      info = d.info
    }

  | GaussianExprTppl d ->
    TmDist {
      dist = DGaussian {
        mu = compileExprTppl context d.mean,
        sigma = compileExprTppl context d.dev
      },
      ty = tyunknown_,
      info = d.info
    }

  | PoissonExprTppl d ->
    TmDist {
      dist = DPoisson {
        lambda = compileExprTppl context d.rate
      },
      ty = tyunknown_,
      info = d.info
    }

  | ExponentialExprTppl d ->
    TmDist {
      dist = DExponential {
        rate = compileExprTppl context d.rate
      },
      ty = tyunknown_,
      info = d.info
    }

  | GammaExprTppl d ->
    TmDist {
      dist = DGamma {
        k = compileExprTppl context d.shape,
        theta = compileExprTppl context d.scale
      },
      ty = tyunknown_,
      info = d.info
    }

  | CategoricalExprTppl d ->
    TmDist {
      dist = DCategorical {
        p = compileExprTppl context d.probs
      },
      ty = tyunknown_,
      info = d.info
  }

  | BetaExprTppl d ->
    TmDist {
      dist = DBeta {
        a = compileExprTppl context d.a,
        b = compileExprTppl context d.b
      },
      ty = tyunknown_,
      info = d.info
    }

  | UniformExprTppl d ->
    TmDist {
      dist = DUniform {
        a = compileExprTppl context d.a,
        b = compileExprTppl context d.b
      },
      ty = tyunknown_,
      info = d.info
  }

  | MultinomialExprTppl d ->
    TmDist {
      dist = DMultinomial {
        n = compileExprTppl context d.n,
        p = compileExprTppl context d.probs
      },
      ty = tyunknown_,
      info = d.info
  }

  | EmpiricalExprTppl d ->
    TmDist {
      dist = DEmpirical {
        samples = compileExprTppl context d.samples
      },
      ty = tyunknown_,
      info = d.info
  }

  | DirichletExprTppl d ->
    TmDist {
      dist = DDirichlet {
        a = compileExprTppl context d.alphas
      },
      ty = tyunknown_,
      info = d.info
  }

  | BinomialExprTppl d ->
    TmDist {
      dist = DBinomial {
        n = compileExprTppl context d.n,
        p = compileExprTppl context d.prob
      },
      ty = tyunknown_,
      info = d.info
  }

  | VariableExprTppl v ->
    TmVar {
      ident = v.ident.v,
      ty = tyunknown_,
      info = v.info,
      frozen = false
    }

  | IsExprTppl x ->
    TmMatch {
      info = x.info,
      target = compileExprTppl context x.thing, -- and constructor
      pat = PatCon {
        info = x.constructor.i,
        ty = tyunknown_,
        ident = x.constructor.v,
        subpat = withInfoPat x.info pvarw_
      },
      thn = withInfo x.info true_,
      els = withInfo x.info false_,
      ty = tyunknown_
    }

  | ToExprTppl x ->
    let let_ = lam n. lam body. lam inexpr. TmLet
      { ident = n
      , tyAnnot = tyunknown_
      , tyBody = tyunknown_
      , body = body
      , inexpr = inexpr
      , info = x.info
      , ty = tyunknown_
      } in
    let var_ = lam n. TmVar {ident = n, ty = tyunknown_, info = x.info, frozen = false} in
    let app_ = lam l. lam r. TmApp { lhs = l, rhs = r, info = x.info, ty = tyunknown_ } in
    let const_ = lam c. TmConst { info = x.info, val = c, ty = tyunknown_ } in
    let int_ = lam i. const_ (CInt { val = i }) in
    let lam_ = lam n. lam body. TmLam {ident = n, tyAnnot = tyunknown_, tyParam = tyunknown_, body = body, ty = tyunknown_, info = x.info} in
    let addi_ = lam l. lam r. app_ (app_ (const_ (CAddi ())) l) r in
    let subi_ = lam l. lam r. app_ (app_ (const_ (CSubi ())) l) r in
    let s = nameSym "start" in
    let e = nameSym "end" in
    let idx = nameSym "idx" in
    let_ s (compileExprTppl context x.beginVal)
      (let_ e (compileExprTppl context x.endVal)
        (app_ (app_ (const_ (CCreate {})) (addi_ (subi_ (var_ e) (var_ s)) (int_ 1)))
          (lam_ idx (addi_ (var_ idx) (var_ s)))))

  | ConstructorExprTppl x ->
    let mkField : {key : {v:String, i:Info}, value : Option ExprTppl} -> (SID, Expr) = lam field.
      let sid = stringToSid field.key.v in
      let ce = lam x.
        compileExprTppl context x
      in
      let val = optionMapOr (withInfo field.key.i (var_ field.key.v)) ce field.value in
      (sid, val) in
    let fields = mapFromSeq cmpSID (map mkField x.fields) in
    let record = TmRecord { bindings = fields, ty = tyunknown_, info = x.info } in
    TmConApp { ident = x.name.v, body = record, ty = tyunknown_, info = x.info }

  -- TODO(vipa, 2022-12-22): We want to pick the right one depending
  -- on the type of the expressions, but that requires
  -- inference/type-checking. This seems like a problem that should
  -- appear in many languages, i.e., we want a good way of supporting
  -- it in MExpr. I guess a superset that includes some form of ad-hoc
  -- overloading?
  | AddExprTppl x ->
    TmApp {
      info = x.info,
      lhs = TmApp {
        info = x.info,
        lhs = TmConst {
          ty = tyunknown_,
          info = x.info,
          val = CAddf ()
        },
        rhs = compileExprTppl context x.left,
        ty = tyunknown_
      },
      rhs = compileExprTppl context x.right,
      ty = tyunknown_
    }

  | SubExprTppl x ->
    TmApp {
      info = x.info,
      lhs = TmApp {
        info = x.info,
        lhs = TmConst {
          ty = tyunknown_,
          info = x.info,
          val = CSubf ()
        },
        rhs = compileExprTppl context x.left,
        ty = tyunknown_
      },
      rhs = compileExprTppl context x.right,
      ty = tyunknown_
    }

  | NegExprTppl x ->
    TmApp {
      info = x.info,
      lhs = TmConst {
        ty = tyunknown_,
        info = x.info,
        val = CNegf ()  -- assuming CNegf constant for negation
      },
      rhs = compileExprTppl context x.right,
      ty = tyunknown_
    }

  | MulExprTppl x ->
    TmApp {
      info = x.info,
      lhs = TmApp {
        info = x.info,
        lhs = TmConst {
          ty = tyunknown_,
          info = x.info,
          val = CMulf ()
        },
        rhs = compileExprTppl context x.left,
        ty = tyunknown_
      },
      rhs = compileExprTppl context x.right,
      ty = tyunknown_
    }

  | MatrixMulExprTppl x ->
    TmApp {
      info = x.info,
      lhs = TmApp {
        info = x.info,
        lhs = nvar_ context.matrixMul,
        rhs = compileExprTppl context x.left,
        ty = tyunknown_
      },
      rhs = compileExprTppl context x.right,
      ty = tyunknown_
    }

  | MatrixAddExprTppl x ->
    TmApp {
      info = x.info,
      lhs = TmApp {
        info = x.info,
        lhs = nvar_ context.matrixAdd,
        rhs = compileExprTppl context x.left,
        ty = tyunknown_
      },
      rhs = compileExprTppl context x.right,
      ty = tyunknown_
    }

  | MatrixPowerExprTppl x ->
    TmApp {
        info = x.info,
        lhs = TmApp {
          info = x.info,
          lhs = nvar_ context.matrixPow,
          rhs = compileExprTppl context x.left,
          ty = tyunknown_
        },
        rhs = compileExprTppl context x.right,
        ty = tyunknown_
      }

  | PowerExprTppl x ->
   TmApp {
        info = x.info,
        lhs = TmApp {
          info = x.info,
          lhs = nvar_ context.pow,
          rhs = compileExprTppl context x.left,
          ty = tyunknown_
        },
        rhs = compileExprTppl context x.right,
        ty = tyunknown_
      }

  | MatrixLeftScalarMulExprTppl x ->
    TmApp {
        info = x.info,
        lhs = TmApp {
          info = x.info,
          lhs = nvar_ context.matrixMulFloat,
          rhs = compileExprTppl context x.left,
          ty = tyunknown_
        },
        rhs = compileExprTppl context x.right,
        ty = tyunknown_
      }

  | MatrixRightScalarMulExprTppl x ->
    TmApp {
        info = x.info,
        lhs = TmApp {
          info = x.info,
          lhs = nvar_ context.matrixMulFloat,
          rhs = compileExprTppl context x.right, -- the scalar is on the right
          ty = tyunknown_
        },
        rhs = compileExprTppl context x.left,
        ty = tyunknown_
      }

  | DivExprTppl x ->
    TmApp {
      info = x.info,
      lhs = TmApp {
        info = x.info,
        lhs = TmConst {
          ty = tyunknown_,
          info = x.info,
          val = CDivf ()
        },
        rhs = compileExprTppl context x.left,
        ty = tyunknown_
      },
      rhs = compileExprTppl context x.right,
      ty = tyunknown_
    }

  | ConvIntToRealExprTppl x ->
    withInfo x.info (int2float_ (compileExprTppl context x.val))

  | LessEqExprTppl x ->
    TmApp {
      info = x.info,
      lhs = TmApp {
        info = x.info,
        lhs = TmConst {
          ty = tyunknown_,
          info = x.info,
          val = CLeqf ()
        },
        rhs = compileExprTppl context x.left,
        ty = tyunknown_
      },
      rhs = compileExprTppl context x.right,
      ty = tyunknown_
    }

  | LessExprTppl x ->
    TmApp {
      info = x.info,
      lhs = TmApp {
        info = x.info,
        lhs = TmConst {
          ty = tyunknown_,
          info = x.info,
          val = CLtf ()
        },
        rhs = compileExprTppl context x.left,
        ty = tyunknown_
      },
      rhs = compileExprTppl context x.right,
      ty = tyunknown_
    }

  | GreaterExprTppl x ->
    TmApp {
      info = x.info,
      lhs = TmApp {
        info = x.info,
        lhs = TmConst {
          ty = tyunknown_,
          info = x.info,
          val = CGtf ()
        },
        rhs = compileExprTppl context x.left,
        ty = tyunknown_
      },
      rhs = compileExprTppl context x.right,
      ty = tyunknown_
    }


  | GreaterEqExprTppl x ->
    TmApp {
      info = x.info,
      lhs = TmApp {
        info = x.info,
        lhs = TmConst {
          ty = tyunknown_,
          info = x.info,
          val = CGeqf ()
        },
        rhs = compileExprTppl context x.left,
        ty = tyunknown_
      },
      rhs = compileExprTppl context x.right,
      ty = tyunknown_
    }

  | EqualExprTppl x ->
    TmApp {
      info = x.info,
      lhs = TmApp {
        info = x.info,
        lhs = TmConst {
          ty = tyunknown_,
          info = x.info,
          val = CEqf ()
        },
        rhs = compileExprTppl context x.left,
        ty = tyunknown_
      },
      rhs = compileExprTppl context x.right,
      ty = tyunknown_
    }

  | UnequalExprTppl x ->
    TmApp {
      info = x.info,
      lhs = TmApp {
        info = x.info,
        lhs = TmConst {
          ty = tyunknown_,
          info = x.info,
          val = CNeqf ()
        },
        rhs = compileExprTppl context x.left,
        ty = tyunknown_
      },
      rhs = compileExprTppl context x.right,
      ty = tyunknown_
    }

  | AndExprTppl x ->
    TmMatch {
      info = x.info,
      target = compileExprTppl context x.left,
      pat = PatBool { val = true, info = get_ExprTppl_info x.left, ty = tyunknown_ },
      thn = compileExprTppl context x.right,
      els = TmConst { info = x.info, ty = tyunknown_, val = CBool { val = false } },
      ty = tyunknown_
    }

  | OrExprTppl x ->
    TmMatch {
      info = x.info,
      target = compileExprTppl context x.left,
      pat = PatBool { val = true, info = get_ExprTppl_info x.left, ty = tyunknown_ },
      thn = TmConst { info = x.info, ty = tyunknown_, val = CBool { val = true } },
      els = compileExprTppl context x.right,
      ty = tyunknown_
    }

  | NotExprTppl x ->
    TmMatch {
      info = x.info,
      target = compileExprTppl context x.right,
      pat = PatBool { val = true, info = get_ExprTppl_info x.right, ty = tyunknown_ },
      thn = TmConst { info = x.info, ty = tyunknown_, val = CBool { val = false } },
      els = TmConst { info = x.info, ty = tyunknown_, val = CBool { val = true } },
      ty = tyunknown_
    }

  -- NOTE(vipa, 2023-05-05): Currently using 1-based indexing
  | SubscriptExprTppl x ->
    let app_ = lam l. lam r. TmApp { lhs = l, rhs = r, info = x.info, ty = tyunknown_ } in
    let const_ = lam c. TmConst { info = x.info, val = c, ty = tyunknown_ } in
    let int_ = lam i. const_ (CInt { val = i }) in
    let addi_ = lam l. lam r. app_ (app_ (const_ (CAddi ())) l) r in
    let var_ = lam n. TmVar {ident = n, ty = tyunknown_, info = x.info, frozen = false} in
    let subseq_ = lam seq. lam idx. lam len. app_ (app_ (app_ (const_ (CSubsequence ())) seq) idx) len in
    let let_ = lam n. lam body. lam inexpr. TmLet
      { ident = n
      , tyAnnot = tyunknown_
      , tyBody = tyunknown_
      , body = body
      , inexpr = inexpr
      , info = x.info
      , ty = tyunknown_
      } in
    let idx = compileExprTppl context x.idx in
    match x.lastIdx with Some lastIdx then
      let n = nameSym "start" in
      let start = subi_ (var_ n) (int_ 1) in
      let len = addi_ (subi_ (compileExprTppl context lastIdx) (var_ n)) (int_ 1) in
      let_ n idx (subseq_ (compileExprTppl context x.left) start len)
    else
      app_ (app_ (const_ (CGet ())) (compileExprTppl context x.left)) (subi_ idx (int_ 1))

  | SequenceExprTppl x ->
    let ce = lam x.
      compileExprTppl context x
    in
    TmSeq {
      tms = map ce x.values,
      ty = tyunknown_,
      info = x.info
    }

  | RealExprTppl r ->
    TmConst {
      val = CFloat { val = r.val.v },
      ty = tyunknown_,
      info = r.info
    }

  | IntegerExprTppl r ->
    TmConst {
      val = CInt { val = r.val.v },
      ty = tyunknown_,
      info = r.info
    }

  | TrueExprTppl r ->
    TmConst {
      val = CBool { val = true },
      ty = tyunknown_,
      info = r.info
    }

  | FalseExprTppl r ->
    TmConst {
      val = CBool { val = false },
      ty = tyunknown_,
      info = r.info
    }

  | TpplStringExprTppl r ->
    TmSeq {
      tms = map char_ r.val.v,
      ty = tyunknown_,
      info = r.info
    }
end


lang CPPLLang =
  MExprAst + MExprCompile + TransformDist + MExprEliminateDuplicateCode +
  MExprSubstitute + MExprPPL + GenerateJsonSerializers

  -- Check if a CorePPL program uses infer
  sem hasInfer =
  | expr -> hasInferH false expr

  sem hasInferH acc =
  | TmInfer _ -> true
  | expr -> sfold_Expr_Expr hasInferH acc expr

end


lang TreePPLThings = TreePPLAst + TreePPLCompile
  + ProjMatchTypeCheck + ProjMatchPprint + MExprAst
end


let compileTpplToExecutable = lam filename: String. lam options: Options.
  use TreePPLThings in
  let content = readFile filename in
  match parseTreePPLExn filename content with file in
  let corePplAst: Expr = compile file in
  --printLn (mexprPPLToString corePplAst);
  let prog: Expr = typeCheckExpr _tcEnvEmpty corePplAst in
  use CPPLLang in
  let prog =  mexprCpplCompile options false prog in
  buildMExpr options prog

-- output needs to be an absolute path
let runCompiledTpplProgram: Options -> String -> Int -> ExecResult =
  lam options: Options. lam jsonData: String. lam sweeps: Int.
  let cmd = [options.output, jsonData, int2string options.particles, int2string sweeps] in
  let res: ExecResult = sysRunCommand cmd "" "." in -- one sweep
  res

mexpr

let testOptions =  {
    method = "is-lw",
    target = "mexpr",
    test = false,
    particles = 1,
    resample = "manual",
    align = false,
    printModel = false,
    printMCore = false,
    exitBefore = false,
    skipFinal = false,
    outputMc = false,
    output = sysTempFileMake (), -- absolute path
    transform = false,
    printSamples = true,
    stackSize = 10000,
    cps = "partial",
    earlyStop = true,
    mcmcLightweightGlobalProb = 0.1,
    mcmcLightweightReuseLocal = true,
    printAcceptanceRate = false,
    pmcmcParticles = 2,
    seed = None (),
    extractSimplification = "none"
  } in

-- test hello world
let testTpplProgram = "models/lang/hello.tppl" in
let testJsonInput = "models/data/coin.json" in
compileTpplToExecutable testTpplProgram testOptions;
let testProgramExecResult = runCompiledTpplProgram testOptions testJsonInput 1 in

utest testProgramExecResult.returncode with 0 in
utest testProgramExecResult.stderr with "Hello, world!\n" in
sysDeleteFile testOptions.output;

-- test externals
let testTpplProgram = "models/lang/externals.tppl" in
let testJsonInput = "models/data/coin.json" in
compileTpplToExecutable testTpplProgram testOptions;
let testProgramExecResult = runCompiledTpplProgram testOptions testJsonInput 1 in

utest testProgramExecResult.returncode with 0 in
utest testProgramExecResult.stdout with "{\"samples\":[0.69314718056],\"weights\":[0.0],\"normConst\":0.0}\n" in
sysDeleteFile testOptions.output;

-- test multiple print statements
let testTpplProgram = "models/lang/blub.tppl" in
let testJsonInput = "models/data/blub.json" in
compileTpplToExecutable testTpplProgram testOptions;
let testProgramExecResult = runCompiledTpplProgram testOptions testJsonInput 1 in

utest testProgramExecResult.returncode with 0 in
utest testProgramExecResult.stderr with
"blab

blab
7
3.14159265359
True
Trump was elected: False
1
2
The length of the vector is 20
TrueTrueTrueFalseTrueFalseFalseTrueTrueFalseFalseFalseTrueFalseTrueFalseFalseTrueFalseFalse
5.
6.
0.666666666667
-1.
True
True
False
False
False
True
False
False
False
True
True
True
False
True
False
.....
6 is more than 5.
" in
sysDeleteFile testOptions.output;

-- test sweep number

let testTpplProgram = "models/lang/coin.tppl" in
let testJsonInput = "models/data/coin.json" in
compileTpplToExecutable testTpplProgram testOptions;
let testProgramExecResult = runCompiledTpplProgram testOptions testJsonInput 3 in

utest testProgramExecResult.returncode with 0 in
let numberLines = length (strSplit "\n" testProgramExecResult.stdout) in
utest numberLines with 4 in -- NOTE(vsenderov, 2023-09-11): for some reason it needs to be one more
sysDeleteFile testOptions.output;

-- Test syntax for matrix multiplication
let testTpplProgram = "models/lang/matrix-tests.tppl" in
let testJsonInput = "models/data/empty.json" in
compileTpplToExecutable testTpplProgram testOptions;
let testProgramExecResult = runCompiledTpplProgram testOptions testJsonInput 1 in

utest testProgramExecResult.returncode with 0 in
utest testProgramExecResult.stderr with
"[[ 1 x 5 matrix ]]:
1.\t2.\t3.\t4.\t5.\t

[[ 2 x 2 matrix ]]:
81.\t0.\t
-580.\t2401.\t

" in


-- TODO(2023-09-08, vsenderov): need to test probailistic stuff as well such as coin.tppl

()
