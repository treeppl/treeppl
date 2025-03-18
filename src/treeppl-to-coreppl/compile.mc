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
include "ocaml/mcore.mc"
include "ocaml/compile.mc"

include "sys.mc"

include "coreppl::coreppl-to-mexpr/compile.mc"
include "coreppl::coreppl-to-mexpr/runtimes.mc"
include "coreppl::coreppl.mc"
include "coreppl::parser.mc"

include "matrix.mc"

include "ast-additions.mc"

lang TreePPLCompile
  = TreePPLAst + MExprPPL + MExprFindSym + RecLetsAst + Externals + MExprSym
  + FloatAst + Resample + GenerateJsonSerializers + MExprEliminateDuplicateCode
  + MCoreLoader + JsonSerializationLoader
  + ProjMatchAst + TreePPLOperators

  -- a type with useful information passed down from compile
  type TpplCompileContext = {
    serializeResult: Name,
    logName: Name, expName: Name,
    printJsonLn: Name,
    particles: Name, sweeps: Name, input: Name, some: Name,
    matrixMul: Name,
    matrixPow: Name,
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
  | TypeDeclTppl x ->
    [ DeclType
      { ident = x.name.v
      , params = []
      , tyIdent = tyvariant_ []
      , info = x.info
      }
    ]
  | _ -> []

  sem compileTpplConDecl : DeclTppl -> [Decl]
  sem compileTpplConDecl =
  | TypeDeclTppl x ->
    let f = lam constr.
      match constr with TypeCon constr in DeclConDef
      { ident = constr.name.v
      , tyIdent =
        let f = lam field. (field.name.v, compileTypeTppl field.ty) in
        let lhs = tyrecord_ (map f constr.fields) in
        let rhs = ntycon_ x.name.v in
        tyarrow_ lhs rhs
      , info = constr.name.i
      } in
    map f x.cons
  | _ -> []

  sem compileTpplFunction: TpplCompileContext -> DeclTppl -> [RecLetBinding]
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
    [{
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
    }]
  | TypeDeclTppl _ -> []

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
      data = tyunknown_,
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

  sem compileModelInvocation : TpplCompileContext -> Loader -> DeclTppl -> Option (Loader, Expr)
  sem compileModelInvocation context loader =
  | FunDeclTppl (x & {model = Some modelInfo}) ->
    -- TODO(vipa, 2024-12-13): We could technically shadow a model
    -- function, in which case the generated code will refer to the
    -- wrong function, which is bad.
    let params : [(Name, Type)] =
      map (lam param. (param.name.v, compileTypeTppl param.ty)) x.args in

    let symEnv = _getSymEnv loader in
    let inputType = tyrecord_ (map (lam x. (nameGetStr x.0, x.1)) params) in
    let inputType = symbolizeType symEnv inputType in
    let outputType = match x. returnTy with Some ty
      then compileTypeTppl ty
      else errorSingle [modelInfo] "A model function must have an explicit return type, even if it returns nothing, i.e., ()" in
    let outputType = symbolizeType symEnv outputType in
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
    let loader = _addDeclExn loader parsedDecl in

    let invocation =
      -- Either apply to 0 (if nullary model function) or each
      -- parameter in sequence
      let f = withInfo modelInfo (nvar_ x.name.v) in
      if null x.args
      then app_ f (int_ 0)
      else foldl (lam l. lam p. app_ l (recordproj_ (nameGetStr p.0) (nvar_ parsedName))) f params in

      let inferCode = appf2_ (nvar_ context.repeat)
        (ulam_ ""
          (app_ (nvar_ context.printJsonLn)
            (appf2_ (nvar_ context.serializeResult) outputSer.serializer
              (infer_ (Default {runs = nvar_ context.particles})
                (ulam_ "" invocation)))))
        (nvar_ context.sweeps) in
    Some (loader, inferCode)
  | _ -> None ()

  sem compileStmtTppl: TpplCompileContext -> StmtTppl -> (Expr -> Expr)

  sem compileStmtTppl (context: TpplCompileContext) =

  | ExprStmtTppl x ->
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

  | AnonFunExprTppl x ->
    let args = if null x.args
      then [(nameNoSym "", tyint_)]
      else map (lam a. (a.name.v, compileTypeTppl a.ty)) x.args in
    let body = foldr (lam f. lam e. f e)
      (withInfo x.info unit_)
      (map (compileStmtTppl context) x.stmts) in
    let wrap = lam pair. lam body.
      withInfo x.info (nlam_ pair.0 pair.1 body) in
    foldr wrap body args

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


lang TreePPLThings = TreePPLAst + TreePPLCompile
  + ProjMatchTypeCheck + ProjMatchPprint + MExprAst
  + MExprGenerateEq
  + OverloadedOpTypeCheck
  + OverloadedOpPrettyPrint
  + TransformDist + CPPLLoader
  + ProjMatchToJson
  + JsonSerializationLoader
  + TreePPLOperators
  + StripUtestLoader
  + MExprLowerNestedPatterns + MCoreCompileLang
  + PhaseStats

  syn FileType =
  | FTreePPL ()

  sem _fileType = | _ ++ ".tppl" -> FTreePPL ()
  sem _loadFile path = | (FTreePPL _, loader & Loader x) ->
    -- NOTE(vipa, 2024-12-12): Return if we've already included this
    -- file
    match mapLookup path x.includedFiles with Some symEnv then (symEnv, loader) else
    let loader = Loader {x with includedFiles = mapInsert path _symEnvEmpty x.includedFiles} in
    -- For things referencing the entirety of the file, and no
    -- particular part of it
    let fileInfo = infoVal path 0 0 0 0 in

    let content = readFile path in
    match parseTreePPLExn path content with DeclSequenceFileTppl top in

    -- Standard library (these should be in scope in the program)
    match includeFileExn "." "treeppl::lib/standard.mc" loader with (stdlibMCEnv, loader) in
    match includeFileExn "." "treeppl::lib/standard.tppl" loader with (stdlibTPPLEnv, loader) in
    let fileEnv = mergeSymEnv stdlibMCEnv.env stdlibTPPLEnv.env in
    -- Compiler libraries (these should *not* be in scope in the program)
    match includeFileExn "." "stdlib::ext/dist-ext.mc" loader with (distEnv, loader) in
    match includeFileExn "." "stdlib::ext/math-ext.mc" loader with (mathEnv, loader) in
    match includeFileExn "." "stdlib::ext/matrix-ext.mc" loader with (matrixEnv, loader) in
    match includeFileExn "." "stdlib::json.mc" loader with (jsonEnv, loader) in
    match includeFileExn "." "stdlib::basic-types.mc" loader with (optionEnv, loader) in
    match includeFileExn "." "stdlib::common.mc" loader with (commonEnv, loader) in
    match includeFileExn "." "treeppl::treeppl-to-coreppl/lib-compile.mc" loader with (compileLibEnv, loader) in
    -- Explicit imports (these should be in scope in the program)
    let import = lam acc. lam imp.
      match includeFileExn "." imp.v acc.1 with (newEnv, loader) in
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
      , matrixMul = _getVarExn "matrixMul" matrixEnv
      , matrixPow = _getVarExn "mtxPow" stdlibMCEnv
      , matrixAdd = _getVarExn "matrixElemAdd" matrixEnv
      , matrixMulFloat = _getVarExn "matrixMulFloat" matrixEnv
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
      match compileModelInvocation context loader decl with Some (loader, invocation) then
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
end


let compileTpplToExecutable = lam filename: String. lam options: Options.
  use TreePPLThings in
  let log = mkPhaseLogState options.debugDumpPhases options.debugPhases in
  let loader = mkLoader symEnvDefault typcheckEnvDefault [StripUtestHook ()] in
  let loader = enableCPPLCompilation options loader in
  let loader = enableEqGeneration loader in
  let loader = enableDesugar loader in
  let loader = enableJsonSerialization loader in
  endPhaseStatsExpr log "mk-cppl-loader" unit_;
  let loader = (includeFileExn "." filename loader).1 in
  endPhaseStatsExpr log "include-file" unit_;
  let ast = buildFullAst loader in
  endPhaseStatsExpr log "build-full-ast" ast;

  let ocamlCompile : [String] -> [String] -> String -> String = lam libs. lam clibs. lam prog.
    let opts =
      { defaultCompileOptions
      with libraries = libs
      , cLibraries = clibs
      } in
    (if options.outputMl then
      writeFile "program.ml" prog
     else ());
    let res = ocamlCompileWithConfig opts prog in
    sysMoveFile res.binaryPath options.output;
    sysChmodWriteAccessFile options.output;
    res.cleanup ();
    options.output in
  let hooks = mkEmptyHooks ocamlCompile in
  let ast = lowerAll ast in
  endPhaseStatsExpr log "lower-all" ast;
  let res = compileMCore ast hooks in
  endPhaseStatsExpr log "compile-mcore" ast;
  res
