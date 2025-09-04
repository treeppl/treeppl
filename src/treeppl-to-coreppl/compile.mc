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
include "mexpr/generate-utest.mc"
include "mexpr/uncurried.mc"
include "ocaml/mcore.mc"
include "ocaml/compile.mc"

include "sys.mc"

include "coreppl::coreppl-to-mexpr/compile.mc"
include "coreppl::coreppl.mc"
include "coreppl::inference/mcmc.mc"
include "coreppl::parser.mc"

include "coreppl::coreppl-to-mexpr/is-lw/compile.mc"
include "coreppl::coreppl-to-mexpr/smc-bpf/compile.mc"
include "coreppl::coreppl-to-mexpr/smc-apf/compile.mc"
include "coreppl::coreppl-to-mexpr/mcmc-naive/compile.mc"
include "coreppl::coreppl-to-mexpr/mcmc-trace/compile.mc"
include "coreppl::coreppl-to-mexpr/mcmc-lightweight/compile.mc"
include "coreppl::coreppl-to-mexpr/pmcmc-pimh/compile.mc"

include "matrix.mc"

include "ast-additions.mc"

lang TreePPLCompile
  = TreePPLAst + MExprPPL + MExprFindSym + RecLetsDeclAst + Externals + MExprSym
  + FloatAst + Resample + GenerateJsonSerializers + MExprEliminateDuplicateCode
  + MCoreLoader + JsonSerializationLoader
  + ProjMatchAst + TreePPLOperators
  + TyVarOrConAst
  + UncurriedAst

  -- a type with useful information passed down from compile
  type TpplCompileContext = {
    serializeResult: Name,
    logName: Name, expName: Name,
    printJsonLn: Name,
    particles: Name, sweeps: Name, input: Name, some: Name,
    matrixMul: Name,
    matrixAdd: Name,
    matrixMulFloat: Name,
    pow: Name,
    repeat: Name
  }

  sem isemi_: Expr -> Expr -> Expr
  sem isemi_ l =
  | r ->
    let infoL = infoTm l in
    let infoR = infoTm r in
    let mergedInfo = mergeInfo infoL infoR in
    withInfo mergedInfo (semi_ l r)

  sem compileTpplTypeDecl : DeclTppl -> [Decl]
  sem compileTpplTypeDecl =
  | TypeDeclTppl (x & {constructor = Some c, alias = None _}) ->
    [ DeclType
      { ident = c.name.v
      , params = map (lam x. x.v) c.params
      , tyIdent = tyvariant_ []
      , info = x.info
      }
    ]
  | TypeDeclTppl (x & {alias = Some a, constructor = None _}) ->
    [ DeclType
      { ident = a.name.v
      , params = map (lam x. x.v) a.params
      , tyIdent = compileTypeTppl a.ty
      , info = x.info
      }
    ]
  | _ -> []

  sem compileTpplConDecl : DeclTppl -> [Decl]
  sem compileTpplConDecl =
  | TypeDeclTppl (x & {constructor = Some c}) ->
    let f = lam constr.
      match constr with TypeCon constr in DeclConDef
      { ident = constr.name.v
      , tyIdent =
        let f = lam field. (field.name.v, compileTypeTppl field.ty) in
        let lhs = tyrecord_ (map f constr.fields) in
        let rhs = ntycon_ c.name.v in
        tyarrow_ lhs rhs
      , info = constr.name.i
      } in
    map f c.cons
  | _ -> []

  sem compileTpplFunction: TpplCompileContext -> DeclTppl -> [DeclLetRecord]
  sem compileTpplFunction (context: TpplCompileContext) =
  | FunDeclTppl f ->
    let positional =
      let g = lam x.
        { ident = x.name.v
        , tyAnnot = compileTypeTppl x.ty
        , tyParam = tyunknown_
        , info = x.name.i
        } in
      map g f.args in
    let ret = optionMapOr tyunit_ compileTypeTppl f.returnTy in
    let body = foldr (lam f. lam e. f e)
      (withInfo f.info unit_)
      (map (compileStmtTppl context) f.body) in
    let function = TmUncurriedLam
      { positional = positional
      , body = body
      , info = f.info
      , ty = tyunknown_
      } in
    let fullTy = TyUncurriedArrow
      { positional = map (lam x. x.tyAnnot) positional
      , ret = ret
      , info = f.info
      } in
    let fullTy = foldr (lam p. lam ty. ntyall_ p.v ty) fullTy f.tyParams in
    [ { ident = f.name.v
      , tyBody = tyunknown_
      , tyAnnot = fullTy
      , body = function
      , info = f.info
      }
    ]
  | TypeDeclTppl _ -> []

  sem compileTypeTppl: TypeTppl -> Type

  sem compileTypeTppl =
  | NamedTypeTppl x -> TyVarOrCon {
      ident = x.name.v,
      info = x.name.i
    }

  | FunTypeTppl x -> TyUncurriedArrow
    { positional = map compileTypeTppl x.params
    , ret = compileTypeTppl x.ret
    , info = x.info
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

  | TypeAppOrSequenceTypeTppl (x & {args = []}) -> TySeq {
    info = x.info,
    ty = compileTypeTppl x.ty
  }
  | TypeAppOrSequenceTypeTppl x ->
    let tyapp_ = lam lhs. lam rhs.
      tyWithInfo x.info (tyapp_ lhs (compileTypeTppl rhs)) in
    foldl tyapp_ (compileTypeTppl x.ty) x.args

  | TpplStrTypeTppl x -> TySeq {
    info = x.info,
    ty = TyChar {
      info = x.info
    }
  }

  | NothingTypeTppl x -> tyunit_

  sem compileModelInvocation : TpplCompileContext -> (Type -> Loader -> (Loader, InferMethod)) -> Loader -> SymEnv -> DeclTppl -> Option (Loader, SymEnv, Expr)
  sem compileModelInvocation context mkInferenceMethod loader fileEnv =
  | FunDeclTppl (x & {model = Some modelInfo}) ->
    -- TODO(vipa, 2024-12-13): We could technically shadow a model
    -- function, in which case the generated code will refer to the
    -- wrong function, which is bad.
    let params : [(Name, Type)] =
      map (lam param. (param.name.v, compileTypeTppl param.ty)) x.args in

    let tcEnv = _getTCEnv loader in
    let inputType = tyrecord_ (map (lam x. (nameGetStr x.0, x.1)) params) in
    let inputType = resolveType modelInfo tcEnv false (symbolizeType fileEnv inputType) in
    let outputType = match x. returnTy with Some ty
      then compileTypeTppl ty
      else errorSingle [modelInfo] "A model function must have an explicit return type, even if it returns nothing, i.e., ()" in
    let outputType = resolveType modelInfo tcEnv false (symbolizeType fileEnv outputType) in
    match serializationPairsFor [inputType, outputType] loader with (loader, [inputSer, outputSer]) in

    let app_ = lam f. lam a. withInfo modelInfo (app_ f a) in

    let parsedName = nameSym "input" in
    let parsedDecl = DeclLet
      { ident = parsedName
      , body =
        let m = nameSym "m" in
        match_
          (app_ inputSer.deserializer (nvar_ context.input))
          (npcon_ context.some (npvar_ m))
          (nvar_ m)
          (error_ (str_ "Could not deserialize data input, malformed json"))
      , tyAnnot = tyunknown_
      , tyBody = tyunknown_
      , info = modelInfo
      } in
    match _addDeclWithEnvExn fileEnv loader parsedDecl with (fileEnv, loader) in

    match mkInferenceMethod outputType loader with (loader, inferenceMethod) in

    let invocation =
      -- Either apply to 0 (if nullary model function) or each
      -- parameter in sequence
      let f = withInfo modelInfo (nvar_ x.name.v) in
      let positional = map (lam p. recordproj_ (nameGetStr p.0) (nvar_ parsedName)) params in
      TmUncurriedApp
        { f = f
        , positional = positional
        , info = NoInfo ()
        , ty = tyunknown_
        } in

    let inferCode = appf2_ (nvar_ context.repeat)
      (ulam_ ""
        (app_ (nvar_ context.printJsonLn)
          (appf2_ (nvar_ context.serializeResult) outputSer.serializer
            (infer_ inferenceMethod
              (ulam_ "" invocation)))))
      (nvar_ context.sweeps) in
    Some (loader, fileEnv, inferCode)
  | _ -> None ()

  sem compileStmtTppl: TpplCompileContext -> StmtTppl -> (Expr -> Expr)

  sem compileStmtTppl (context: TpplCompileContext) =

  | ExprStmtTppl x ->
    lam cont. isemi_ (compileExprTppl context x.e) cont

  | AssumeStmtTppl a ->
    lam cont.
      let driftKernel =
        match a.driftKernel with Some dk then
          let dk = compileExprTppl context dk in
          Some (withInfo (infoTm dk) (nulam_ a.randomVar.v dk))
        else None () in
      let body = TmAssume
        { dist = compileExprTppl context a.dist
        , ty = tyunknown_
        , info = a.info
        , driftKernel = driftKernel
        } in
      let tyAnnot = optionMapOr tyunknown_ compileTypeTppl a.ty in
      bind_ (declWithInfo a.info (nlet_ a.randomVar.v (tyWithInfo a.info tyAnnot) (withInfo a.info body))) cont

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
    lam cont. TmDecl
    { decl = DeclLet
      { ident = a.var.v
      , tyBody = tyunknown_
      , tyAnnot = optionMapOr tyunknown_ compileTypeTppl a.ty
      , body =  compileExprTppl context a.val
      , info = a.info
      }
    , info = a.info
    , inexpr = cont
    , ty = tyunknown_
    }

  | WeightStmtTppl a ->
    lam cont.

    let cExpr: Expr = (compileExprTppl context a.value) in
    let logExpr: Expr = withInfo a.info (app_ (withInfo a.info (nvar_ context.logName)) cExpr) in
    TmDecl
    { decl = DeclLet
      { ident = nameNoSym "foo"
      , tyBody = tyunknown_
      , tyAnnot = tyunknown_
      , body = TmWeight
        { weight = logExpr
        , ty = tyunknown_
        , info = a.info
        }
      , info = a.info
      }
    , info = a.info
    , inexpr = cont
    , ty = tyunknown_
    }

  | LogWeightStmtTppl a ->
    lam cont. TmDecl
    { decl = DeclLet
      { ident = nameNoSym "foo"
      , tyBody = tyunknown_
      , tyAnnot = tyunknown_
      , body =  TmWeight
        { weight = compileExprTppl context a.value
        , ty = tyunknown_
        , info = a.info
        }
      , info = a.info
      }
    , info = a.info
    , inexpr = cont
    , ty = tyunknown_
    }

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
    TmDecl
    { decl = DeclLet
      { ident = contName
      , body = contF
      , tyBody = tyunknown_
      , tyAnnot = tyunknown_
      , info = a.info
      }
    , info = a.info
    , ty = tyunknown_
    , inexpr = TmMatch
      { target = compileExprTppl context a.condition
      , pat = withInfoPat (get_ExprTppl_info a.condition) ptrue_
      , thn = foldr (lam f. lam e. f e) cont (map (compileStmtTppl context) a.ifTrueStmts)
      , els = foldr (lam f. lam e. f e) cont (map (compileStmtTppl context) a.ifFalseStmts)
      , ty = tyunknown_
      , info = a.info
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
      TmDecl
      { decl = DeclRecLets
        { bindings =
          [ { ident = fName
            , tyAnnot = tyunknown_
            , tyBody = tyunknown_
            , body = mkBody (app_ (var_ fName))
            , info = x.info
            }
          ]
        , info = x.info
        }
      , info = x.info
      , inexpr = app_ (var_ fName) arg
      , ty = tyunknown_
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

  | AnonFunExprTppl x ->
    -- TODO(vipa, 2025-06-05): Ascribe a return type as well
    let mkArg = lam a.
      { ident = a.name.v
      , tyAnnot = compileTypeTppl a.ty
      , tyParam = tyunknown_
      , info = a.name.i
      } in
    let positional = map mkArg x.args in
    let body = foldr (lam f. lam e. f e)
      (withInfo x.info unit_)
      (map (compileStmtTppl context) x.stmts) in
    let retTy = optionMapOr tyunit_ compileTypeTppl x.retTy in
    let fun = TmUncurriedLam
      { positional = positional
      , body =
        let x = nameSym "x" in
        bind_ (nlet_ x retTy body) (nvar_ x)
      , info = x.info
      , ty = tyunknown_
      } in
    fun

  | ProjectionExprTppl x ->
    TmProjMatch {
      info = x.info,
      target = compileExprTppl context x.target,
      field = stringToSid x.field.v,
      ty = tyunknown_
    }

  | PartialApplicationExprTppl x ->
    errorSingle [x.info] "'_' is only allowed in a (partial) function application."
  | FunCallExprTppl x ->
    let compileArg = lam e.
      match e with PartialApplicationExprTppl a
      then Left (nameSym "x", a.info)
      else Right (compileExprTppl context e) in
    let args = map compileArg x.args in
    match eitherLefts args with params & ![]
    then TmUncurriedLam
      { positional = map
        (lam p. {ident = p.0, tyAnnot = tyunknown_, tyParam = tyunknown_, info = p.1})
        params
      , body = TmUncurriedApp
        { f = compileExprTppl context x.f
        , positional = map (eitherEither (lam p. withInfo p.1 (nvar_ p.0)) (lam x. x)) args
        , info = x.info
        , ty = tyunknown_
        }
      , info = x.info
      , ty = tyunknown_
      }
    else TmUncurriedApp
      { f = compileExprTppl context x.f
      , positional = eitherRights args
      , info = x.info
      , ty = tyunknown_
      }

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
    let let_ = lam n. lam body. lam inexpr. TmDecl
      { decl = DeclLet
        { ident = n
        , tyAnnot = tyunknown_
        , tyBody = tyunknown_
        , body = body
        , info = x.info
        }
      , info = x.info
      , inexpr = inexpr
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

  | AddExprTppl x ->
    TmApp {
      info = x.info,
      lhs = TmApp {
        info = x.info,
        lhs = mkOp x.info (OpAdd ()),
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
        lhs = mkOp x.info (OpSub ()),
        rhs = compileExprTppl context x.left,
        ty = tyunknown_
      },
      rhs = compileExprTppl context x.right,
      ty = tyunknown_
    }

  | NegExprTppl x ->
    TmApp {
      info = x.info,
      lhs = mkOp x.info (OpNeg ()),
      rhs = compileExprTppl context x.right,
      ty = tyunknown_
    }

  | MulExprTppl x ->
    TmApp {
      info = x.info,
      lhs = TmApp {
        info = x.info,
        lhs = mkOp x.info (OpMul ()),
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

  | PowerExprTppl x ->
   TmApp {
        info = x.info,
        lhs = TmApp {
          info = x.info,
          lhs = mkOp x.info (OpPow ()),
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
        lhs = mkOp x.info (OpDiv ()),
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
        lhs = mkOp x.info (OpLeq ()),
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
        lhs = mkOp x.info (OpLt ()),
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
        lhs = mkOp x.info (OpGt ()),
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
        lhs = mkOp x.info (OpGeq ()),
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
        lhs = mkOp x.info (OpEq ()),
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
        lhs = mkOp x.info (OpNeq ()),
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
    let let_ = lam n. lam body. lam inexpr. TmDecl
      { decl = DeclLet
        { ident = n
        , tyAnnot = tyunknown_
        , tyBody = tyunknown_
        , body = body
        , info = x.info
        }
      , inexpr = inexpr
      , ty = tyunknown_
      , info = x.info
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


lang TreePPLThings = TreePPLAst + TreePPLCompile
  + ProjMatchTypeCheck + ProjMatchPprint + MExprAst
  + TyVarOrConSym
  + MExprGenerateEq
  + OverloadedOpTypeCheck
  + OverloadedOpPrettyPrint
  + TransformDist + CPPLLoader
  + ProjMatchToJson
  + JsonSerializationLoader
  + DPrintViaPprintLoader + MExprGeneratePprint + GeneratePprintMissingCase
  + TreePPLOperators
  + StripUtestLoader + PprintUnifyErrorNumArguments + UncurriedTypeCheck + SymUncurried + UncurriedPrettyPrint + LowerUncurryLoader
  + UnifyUncurriedMixed
  + MExprLowerNestedPatterns + MCoreCompileLang
  + PhaseStats
  + BPFCompilerPicker + APFCompilerPicker + ImportanceCompilerPicker
  + NaiveMCMCCompilerPicker + TraceMCMCCompilerPicker + PIMHCompilerPicker
  + LightweightMCMCCompilerPicker

  syn FileType =
  | FTreePPL ()

  syn Hook =
  | TreePPLHook { mkInferenceMethod : Type -> Loader -> (Loader, InferMethod) }

  sem _fileType = | _ ++ ".tppl" -> FTreePPL ()
  sem _loadFile path = | (FTreePPL _, loader & Loader x) ->
    -- NOTE(vipa, 2024-12-12): Return if we've already included this
    -- file
    match mapLookup path x.includedFiles with Some symEnv then (symEnv, loader) else
    let loader = Loader {x with includedFiles = mapInsert path _symEnvEmpty x.includedFiles} in
    match getHookOpt (lam x. match x with TreePPLHook x then Some x else None ()) loader with Some hook in
    -- For things referencing the entirety of the file, and no
    -- particular part of it
    let fileInfo = infoVal path 0 0 0 0 in

    let content = readFile path in
    match parseTreePPLExn path content with DeclSequenceFileTppl top in

    -- Standard library (these should be in scope in the program)
    match includeFileExn "." "treeppl::lib/standard.mc" loader with (stdlibMCEnv, loader) in
    match includeFileExn "." "treeppl::lib/standard.tppl" loader with (stdlibTPPLEnv, loader) in
    let fileEnv = stdlibTPPLEnv.env in
    -- Compiler libraries (these should *not* be in scope in the program)
    match includeFileExn "." "stdlib::ext/dist-ext.mc" loader with (distEnv, loader) in
    match includeFileExn "." "stdlib::ext/math-ext.mc" loader with (mathEnv, loader) in
    match includeFileExn "." "stdlib::ext/mat-ext.mc" loader with (matrixEnv, loader) in
    match includeFileExn "." "stdlib::json.mc" loader with (jsonEnv, loader) in
    match includeFileExn "." "stdlib::basic-types.mc" loader with (optionEnv, loader) in
    match includeFileExn "." "stdlib::common.mc" loader with (commonEnv, loader) in
    match includeFileExn "." "treeppl::treeppl-to-coreppl/lib-compile.mc" loader with (compileLibEnv, loader) in
    -- Explicit imports (these should be in scope in the program)
    let import = lam acc. lam imp.
      match includeFileExn (dirname path) imp.v acc.1 with (newEnv, loader) in
      (mergeSymEnv acc.0 newEnv.env, loader) in
    match foldl import (fileEnv, loader) top.imports with (fileEnv, loader) in

    let context =
      { serializeResult = _getVarExn "serializeResult" compileLibEnv
      , particles = _getVarExn "particles" compileLibEnv
      , sweeps = _getVarExn "sweeps" compileLibEnv
      , input = _getVarExn "input" compileLibEnv
      , logName = _getVarExn "externalLog" mathEnv
      , expName = _getVarExn "externalExp" mathEnv
      , pow = _getVarExn "pow" mathEnv
      , printJsonLn = _getVarExn "printJsonLn" jsonEnv
      , some = _getConExn "Some" optionEnv
      , matrixMul = _getVarExn "matMulExn" matrixEnv
      , matrixAdd = _getVarExn "matAddExn" matrixEnv
      , matrixMulFloat = _getVarExn "matScale" matrixEnv
      , repeat = _getVarExn "repeat" commonEnv
      } in

    -- 1. Type and constructor definitions. *All* types are inserted
    -- before *all* constructors, which lets constructors refer to
    -- other types in mutually recursive ways.
    let work = lam f. lam acc. lam decl.
      foldl (lam acc. _addDeclWithEnvExn acc.0 acc.1) acc (f decl) in
    match foldl (work compileTpplTypeDecl) (fileEnv, loader) top.decls with (fileEnv, loader) in
    match foldl (work compileTpplConDecl) (fileEnv, loader) top.decls with (fileEnv, loader) in

    -- 2. Functions. These are inserted into a single recursive let,
    -- which enables mutual recursion.
    let functions = DeclRecLets {bindings = joinMap (compileTpplFunction context) top.decls, info = fileInfo} in
    match _addDeclWithEnvExn fileEnv loader functions with (fileEnv, loader) in

    -- 3. Model invocations.
    let work = lam loader. lam decl.
      match compileModelInvocation context hook.mkInferenceMethod loader fileEnv decl with Some (loader, fileEnv, invocation) then
        let decl = DeclLet
          { body = invocation
          , ident = nameSym ""
          , tyAnnot = tyunknown_
          , tyBody = tyunknown_
          , info = infoTm invocation
          } in
        (_addDeclWithEnvExn fileEnv loader decl).1
      else loader in
    let loader = foldl work loader top.decls in

    match loader with Loader x in
    (mapFindExn path x.includedFiles, loader)

  sem registerMatrixFunctions : Loader -> Loader
  sem registerMatrixFunctions = | loader ->
    match includeFileExn "." "stdlib::ext/arr-ext.mc" loader with (arrEnv, loader) in
    match includeFileExn "." "stdlib::json.mc" loader with (jsonEnv, loader) in
    match includeFileExn "." "stdlib::basic-types.mc" loader with (basicEnv, loader) in
    match includeFileExn "." "stdlib::option.mc" loader with (optionEnv, loader) in
    let extArrName = _getTyConExn "ExtArr" arrEnv in
    let jsonArrName = _getConExn "JsonArray" jsonEnv in
    let optionMapM_ = appf2_ (nvar_ (_getVarExn "optionMapM" optionEnv)) in
    let optionMap_ = appf2_ (nvar_ (_getVarExn "optionMap" optionEnv)) in
    let extArrToSeq_ = app_ (nvar_ (_getVarExn "extArrToSeq" arrEnv)) in
    let extArrOfSeq = app_
      (nvar_ (_getVarExn "extArrOfSeq" arrEnv))
      (nvar_ (_getVarExn "extArrKindFloat64" arrEnv)) in

    -- NOTE(vipa, 2025-04-30): Json serialization
    let ser = ulam_ "serElem" (ulam_ "seq"
      (nconapp_ jsonArrName
        (map_ (var_ "serElem") (extArrToSeq_ (var_ "seq"))))) in
    let deser = ulam_ "deserElem" (ulam_ "json"
      (match_ (var_ "json") (npcon_ jsonArrName (pvar_ "arr"))
        (optionMap_ extArrOfSeq (optionMapM_ (var_ "deserElem") (var_ "arr")))
        (nconapp_ (_getConExn "None" basicEnv) unit_))) in
    let loader = registerCustomJsonSerializer extArrName {serializer = ser, deserializer = deser} loader in

    loader
end

type TpplFrontendOptions =
  { input : String
  , output : String
  , outputMl : Option String
  }

let tpplFrontendOptions : OptParser TpplFrontendOptions =
  let mk = lam input. lam output. lam outputMl.
    { input = input
    , output = output
    , outputMl = outputMl
    } in
  let input = optPos
    { optPosDefString with arg = "<program>"
    , description = "The TreePPL program to compile."
    } in
  let output =
    let default = "out" in
    let opt = optArg
      { optArgDefString with long = "output"
      , description = concat "The name of the final compiled executable. Default: " default
      , arg = "<file>"
      } in
    optOr opt (optPure default) in
  let outputMl = optOptional (optArg
    { optArgDefString with long = "output-ml"
    , description = "Output the intermediate .ml file to this path."
    }) in
  optMap3 mk input output outputMl

let compileTpplToExecutable = lam frontend. lam transformations. lam mkInferenceMethod.
  use TreePPLThings in
  let log = mkPhaseLogState transformations.debugDumpPhases transformations.debugPhases in
  let loader = mkLoader symEnvDefault typcheckEnvDefault [StripUtestHook ()] in
  let loader = enableCPPLCompilation transformations loader in
  let loader = enableEqGeneration loader in
  let loader = enableDesugar loader in
  let loader = enableJsonSerialization loader in
  let loader = enableDPrintViaPprint loader in
  let loader = registerMatrixFunctions loader in
  let loader = addHook loader (TreePPLHook {mkInferenceMethod = mkInferenceMethod}) in
  let loader = addHook loader (LowerUncurryHook ()) in
  endPhaseStatsExpr log "mk-cppl-loader" unit_;
  let loader = (includeFileExn "." frontend.input loader).1 in
  endPhaseStatsExpr log "include-file" unit_;
  let ast = buildFullAst loader in
  endPhaseStatsExpr log "build-full-ast" ast;

  let ocamlCompile : [String] -> [String] -> String -> String = lam libs. lam clibs. lam prog.
    let opts =
      { defaultCompileOptions
      with libraries = libs
      , cLibraries = clibs
      } in
    (match frontend.outputMl with Some path
     then writeFile path prog
     else ());
    let res = ocamlCompileWithConfig opts prog in
    sysMoveFile res.binaryPath frontend.output;
    sysChmodWriteAccessFile frontend.output;
    res.cleanup ();
    frontend.output in
  let hooks = mkEmptyHooks ocamlCompile in
  let ast = lowerAll ast in
  endPhaseStatsExpr log "lower-all" ast;
  let res = compileMCore ast hooks in
  endPhaseStatsExpr log "compile-mcore" ast;
  res
