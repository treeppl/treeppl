/-
TreePPL Compiler command line
-/

-- (vsenderov, 2923-06-16 I don't remeber what are all the includes for any more;
-- perhaps we should test and see if we need all of them)
include "sys.mc"

include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"
include "mexpr/boot-parser.mc"
include "mexpr/type-check.mc"
include "mexpr/generate-json-serializers.mc"

include "treeppl-to-coreppl/compile.mc"
include "treeppl-to-coreppl/runtime-flags.mc"

include "coreppl::parser.mc"


mexpr

use TreePPLThings in

let _cpsDefault : String = "full" in
let _cps : OptParser String =
  let opt = optArg
    { optArgDefString with long = "cps"
    , description = concat "Configuration of CPS transformation. The supported options are: none, partial, and full. Default: " _cpsDefault
    , category = catCompile
    } in
  optOr opt (optPure _cpsDefault) in

let _dynamicDelayDefault : Bool = false in
let _dynamicDelay : OptParser Bool = optMap (xor _dynamicDelayDefault) (optFlag
  { optFlagDef with long = "dynamic-delay"
  , description = "Runs dynamic delayed sampling on the model."
  , category = catCompile
  }) in

let _pruneDefault : Bool = false in
let _prune : OptParser Bool = optMap (xor _pruneDefault) (optFlag
  { optFlagDef with long = "prune"
  , description = "The model is pruned if possible."
  , category = catCompile
  }) in

let _resampleDefault : String = "manual" in
let _resample : OptParser String =
  let opt = optArg
    { optArgDefString with long = "resample"
    , description = concat "The selected resample placement method, for inference algorithms where applicable. The supported methods are: likelihood (resample immediately after all likelihood updates), align (resample after aligned likelihood updates, forces --align), and manual (sample only at manually defined resampling locations). Default: " _resampleDefault
    , category = catCompile
    } in
  optOr opt (optPure _resampleDefault) in

let _driftKernelDefault : Bool = false in
let _driftKernel : OptParser Bool = optMap (xor _driftKernelDefault) (optFlag
  { optFlagDef with long = "kernel"
  , description = "Use drift kernels in MCMC."
  , category = catCompile
  }) in

let _driftScaleDefault : Float = 0.1 in
let _driftScale : OptParser Float =
  let opt = optArg
    { optArgDefFloat with long = "drift"
    , description = concat "Standard deviation of the automatic drift kernels. Default: " (float2string _driftScaleDefault)
    , category = catCompile
    } in
  optOr opt (optPure _driftScaleDefault) in

let _incrementalPrinting : Bool -> OptParser Bool = lam default.
  optMap (xor default) (optFlag
    { optFlagDef with long = if default then "no-incremental-printing" else "incremental-printing"
    , description = concat (if default then "Disable" else "Enable") " printing each sample as it is produced instead of at the end."
    , category = catCompile
    }) in
let incrementalPrinting = _incrementalPrinting false in

let _alignDefault : Bool = false in
let _align : OptParser Bool = optMap (xor _alignDefault) (optFlag
  { optFlagDef with long = "align"
  , description = "Whether or not to align the model for certain inference algorithms."
  , category = catCompile
  }) in

let _debugAlignment : OptParser (Option String) = optOptional (optArg
  { optArgDefString with long = "debug-alignment-html"
  , description = "Output an interactive .html file showing alignment results to the given file."
  , arg = "FILE"
  , category = catDebug
  }) in

let _pigeons : OptParser Bool = optNoArg
  { optNoArgDef true with long = "pigeons"
  , description = "Let Pigeons.jl control inference via stdio."
  , category = catCompile
  } in
let _pigeonsGlobalDefault : Bool = true in
let _pigeonsGlobal : OptParser Bool = optMap (xor _pigeonsGlobalDefault) (optFlag
  { optFlagDef with long = "pigeons-no-global"
  , description = "Do not use global moves when sampling at temperature 0.0"
  , category = catCompile
  }) in
let _pigeonsExploreStepsDefault : Int = 1 in
let _pigeonsExploreSteps : OptParser Int =
  let opt = optArg
    { optArgDefInt with long = "pigeons-explore-steps"
    , description = concat "The number of local MCMC steps to take before communicating with Pigeons.jl. Default: " (int2string _pigeonsExploreStepsDefault)
    , category = catCompile
    } in
  optOr opt (optPure _pigeonsExploreStepsDefault) in
let pigeonsOptions = optOptional (optMap3 (lam. lam pg. lam pes. (pg, pes)) _pigeons _pigeonsGlobal _pigeonsExploreSteps) in

let makeOptParser : Loader -> [(String, Expr)] -> (Loader, Expr) = lam loader. lam fieldParsers.
  match includeFileExn "." "stdlib::optparse-applicative.mc" loader with (optParseEnv, loader) in
  let optMap = appf2_ (nvar_ (_getVarExn "optMap" optParseEnv)) in
  let optApply = appf2_ (nvar_ (_getVarExn "optApply" optParseEnv)) in
  let mkFunc =
    let names = map (lam x. nameSym x.0) fieldParsers in
    let record = urecord_ (map (lam x. (nameGetStr x, nvar_ x)) names) in
    foldr (lam pair. lam tm. nulam_ pair tm) record names in
  ( loader
  , match fieldParsers with [f] ++ fs
    then foldl (lam tm. lam f. optApply tm f.1) (optMap mkFunc f.1) fs
    else app_ (nvar_ (_getVarExn "optPure" optParseEnv)) unit_
  ) in

let mcmcLightweightOptions : OptParser MkInferMethod =
  let mk =
    lam pigeonsInfo.
    lam debugIterations. lam samplingPeriod. lam incrementalPrinting. lam iterations. lam globalProb : Float.
    lam driftKernel. lam driftScale. lam cps.
    lam align. lam debugAlignment. lam resampleBehaviorFlag.
    lam outputType. lam loader.

    match includeFileExn "." "stdlib::json.mc" loader with (jsonEnv, loader) in
    match includeFileExn "." "coreppl::coreppl-to-mexpr/mcmc-lightweight/config.mc" loader with (configEnv, loader) in
    match includeFileExn "." "treeppl::treeppl-to-coreppl/mcmc-lightweight.mc" loader with (lwEnv, loader) in
    match includeFileExn "." "treeppl::treeppl-to-coreppl/runtime-flags.mc" loader with (flagEnv, loader) in
    let debugTypeFields = switch mapLookup (_getTyConExn "DebugInfo" configEnv) (_getTCEnv loader).tyConEnv
      case Some (_, [], TyRecord x) then x
      case Some (_, _, _) then error "Compiler error: unexpected shape of DebugInfo"
      case None _ then error "Compiler error: no info about DebugInfo in TCEnv"
      end in
    let fullDebugType = TyRecord
      { debugTypeFields with fields = mapInsert (stringToSid "durationMs") tyfloat_ debugTypeFields.fields } in
    match serializationPairsFor [outputType, fullDebugType] loader with (loader, [outputSer, debugSer]) in

    let getOptParser : String -> Expr -> Expr = lam ident. lam default.
      app_ (nvar_ (_getVarExn ident flagEnv)) default in
    let fields = mapEmpty cmpString in

    match
      if or incrementalPrinting (optionIsSome pigeonsInfo) then
        ( fields
        , lam n. ulam_ "" false_
        )
      else
        ( mapUnion fields (mapFromSeq cmpString
          [ ("samplingPeriod", getOptParser "_samplingPeriod" (int_ samplingPeriod))
          ])
        , lam n. if_ (eqi_ (int_ 1) (recordproj_ "samplingPeriod" (nvar_ n)))
          (ulam_ "" true_)
          (ulam_ "idx" (eqi_ (int_ 0) (modi_ (var_ "idx") (recordproj_ "samplingPeriod" (nvar_ n)))))
        )
    with (fields, mkKeepSample) in

    match
      match pigeonsInfo with Some (pigeonsGlobal, pigeonsExploreSteps) then
        ( mapUnion fields (mapFromSeq cmpString
          [ ("pigeonsGlobal", getOptParser "_pigeonsGlobal" (bool_ pigeonsGlobal))
          , ("pigeonsExploreSteps", getOptParser "_pigeonsExploreSteps" (int_ pigeonsExploreSteps))
          , ("samplingPeriod", getOptParser "_samplingPeriod" (int_ samplingPeriod))
          , ("globalProb", getOptParser "_mcmcLightweightGlobalProb" (float_ globalProb))
          ])
        , lam n. appf3_ (nvar_ (_getVarExn "continuePigeons" lwEnv))
          (recordproj_ "pigeonsExploreSteps" (nvar_ n))
          (recordproj_ "samplingPeriod" (nvar_ n))
          outputSer.serializer
        , lam n. appf1_ (nvar_ (_getVarExn "accInitPigeons" lwEnv))
          (bool_ incrementalPrinting)
        , lam n. nvar_ (_getVarExn "temperaturePigeons" lwEnv)
        , lam n. appf2_ (nvar_ (_getVarExn "globalProbPigeons" lwEnv))
          (recordproj_ "pigeonsGlobal" (nvar_ n))
          (recordproj_ "globalProb" (nvar_ n))
        )
      else if incrementalPrinting then
        ( mapUnion fields (mapFromSeq cmpString
          [ ("iterations", getOptParser "_iterations" (int_ iterations))
          , ("samplingPeriod", getOptParser "_samplingPeriod" (int_ samplingPeriod))
          , ("globalProb", getOptParser "_mcmcLightweightGlobalProb" (float_ globalProb))
          ])
        , lam n. appf3_ (nvar_ (_getVarExn "continueIncremental" lwEnv))
          (recordproj_ "iterations" (nvar_ n))
          (recordproj_ "samplingPeriod" (nvar_ n))
          outputSer.serializer
        , lam n. nvar_ (_getVarExn "accInitIncremental" lwEnv)
        , lam n. ulam_ "" (float_ 1.0)
        , lam n. ulam_ "" (recordproj_ "globalProb" (nvar_ n))
        )
      else
        ( mapUnion fields (mapFromSeq cmpString
          [ ("iterations", getOptParser "_iterations" (int_ iterations))
          , ("globalProb", getOptParser "_mcmcLightweightGlobalProb" (float_ globalProb))
          ])
        , lam n. appf1_ (nvar_ (_getVarExn "continueBase" lwEnv))
          (recordproj_ "iterations" (nvar_ n))
        , lam n. nvar_ (_getVarExn "accInitBase" lwEnv)
        , lam n. ulam_ "" (float_ 1.0)
        , lam n. ulam_ "" (recordproj_ "globalProb" (nvar_ n))
        )
    with (fields, mkContinue, mkInit, mkTemp, mkGlobalProb) in

    match
      if neqi 0 resampleBehaviorFlag then
        ( mapUnion (mapRemove "globalProb" fields) (mapFromSeq cmpString
          [ ("resampleBehavior", getOptParser "_resampleBehavior" (int_ resampleBehaviorFlag))
          ])
        , lam n. ulam_ "acc" (ulam_ "length"
          (bind_
            (ulet_ "iter"
              (if optionIsSome pigeonsInfo
                then tupleproj_ 0 (var_ "acc")
                else var_ "acc"))
            (utuple_
              [ var_ "acc"
              , if_
                (eqi_ (int_ 0)
                  (modi_ (var_ "iter") (muli_ (var_ "length") (recordproj_ "resampleBehavior" (nvar_ n)))))
                (utuple_
                  [ create_ (var_ "length") (ulam_ "" false_)
                  , int_ (negi 1)
                  ])
                (utuple_
                  [ create_ (var_ "length") (ulam_ "" true_)
                  , modi_ (subi_ (var_ "iter") (int_ 1)) (var_ "length")
                  ])
              ])))
        )
      else
        ( fields
        , lam n. ulam_ "acc" (ulam_ "length"
          (utuple_
            [ var_ "acc"
            , if_ (assume_ (bern_ (app_ (mkGlobalProb n) (var_ "acc"))))
              (utuple_
                [ create_ (var_ "length") (ulam_ "" false_)
                , int_ (negi 2)
                ])
              (utuple_
                [ create_ (var_ "length") (ulam_ "" true_)
                , assume_ (uniformDiscrete_ (int_ 0) (subi_ (var_ "length") (int_ 1)))
                ])
            ]))
        )
    with (fields, mkResampleBehavior) in

    match
      ( mapUnion fields (mapFromSeq cmpString
        [ ("debugIterations", getOptParser "_debugIterations" (bool_ debugIterations))
        ])
      , lam n. if_ (recordproj_ "debugIterations" (nvar_ n))
        (utuple_
          [ wallTimeMs_ unit_
          , ulam_ "start" (ulam_ "debug"
            (bindall_
              [ ulet_ "end" (wallTimeMs_ unit_)
              , ulet_ "output" (TmRecord
                { bindings = mapInsert (stringToSid "durationMs") (subf_ (var_ "end") (var_ "start"))
                  (mapMapWithKey (lam k. lam. recordproj_ (sidToString k) (var_ "debug")) debugTypeFields.fields)
                , ty = tyunknown_
                , info = NoInfo ()
                })
              , ulet_ "output" (app_ debugSer.serializer (var_ "output"))
              , ulet_ "" (printError_ (app_ (nvar_ (_getVarExn "json2string" jsonEnv)) (var_ "output")))
              , ulet_ "" (printError_ (str_ "\n"))
              , ulet_ "" (flushStderr_ unit_)
              ]
              (wallTimeMs_ unit_)))
          ])
        (utuple_
          [ float_ 0.0
          , ulam_ "acc" (ulam_ "" (var_ "acc"))
          ])
      )
    with (fields, mkDebug) in

    let mkMethod = lam n. LightweightMCMC
      { keepSample = mkKeepSample n
      , continue = utuple_ [mkInit n, mkContinue n]
      , temperature = mkTemp n
      , resampleBehavior = mkResampleBehavior n
      , debug = mkDebug n
      , driftKernel = driftKernel
      , driftScale = driftScale
      , cps = cps
      , align = align
      , debugAlignment = debugAlignment
      } in
    match makeOptParser loader (mapBindings fields) with (loader, optParser) in
    (loader, {mkMethod = mkMethod, optParser = optParser}) in
  let res = optApply2
    (optApply5
      (optMap5 mk pigeonsOptions debugIterations samplingPeriod incrementalPrinting iterations)
      mcmcLightweightGlobalProb _driftKernel _driftScale _cps _align)
    _debugAlignment resampleBehavior in
  let method = optSpecificArg
    { optExactArg "mcmc" with short = "m", long = "method"
    , description = "MCMC using the lightweight approach"
    , category = catMethod
    } in
  optMap2 (lam. lam x. x) method res in

let mcmcNaiveOptions : OptParser MkInferMethod =
  let mk = lam iterations. lam. lam loader.
    match includeFileExn "." "treeppl::treeppl-to-coreppl/runtime-flags.mc" loader with (flagEnv, loader) in
    let getOptParser : String -> Expr -> Expr = lam ident. lam default.
      app_ (nvar_ (_getVarExn ident flagEnv)) default in
    let fields =
      [ ("iterations", getOptParser "_iterations" (int_ iterations))
      ] in
    match makeOptParser loader fields with (loader, optParser) in
    ( loader
    , { mkMethod = lam n. NaiveMCMC
        { iterations = recordproj_ "iterations" (nvar_ n)
        }
      , optParser = optParser
      }
    ) in
  let res = optMap mk iterations in
  let method = optSpecificArg
    { optExactArg "mcmc-naive" with short = "m", long = "method"
    , description = "MCMC exploring completely independent runs of the program (mainly for experiments)"
    , category = catMethod
    } in
  optMap2 (lam. lam x. x) method res in

let mcmcTraceOptions : OptParser MkInferMethod =
  let mk = lam iterations. lam. lam loader.
    match includeFileExn "." "treeppl::treeppl-to-coreppl/runtime-flags.mc" loader with (flagEnv, loader) in
    let getOptParser : String -> Expr -> Expr = lam ident. lam default.
      app_ (nvar_ (_getVarExn ident flagEnv)) default in
    let fields =
      [ ("iterations", getOptParser "_iterations" (int_ iterations))
      ] in
    match makeOptParser loader fields with (loader, optParser) in
    ( loader
    , { mkMethod = lam n. NaiveMCMC
        { iterations = recordproj_ "iterations" (nvar_ n)
        }
      , optParser = optParser
      }
    ) in
  let res = optMap mk iterations in
  let method = optSpecificArg
    { optExactArg "mcmc-trace" with short = "m", long = "method"
    , description = "MCMC running complete programs with stored traces, changing one assume at a time (mainly for experiments)"
    , category = catMethod
    } in
  optMap2 (lam. lam x. x) method res in

let isLwOptions : OptParser MkInferMethod =
  let mk = lam particles. lam earlyStop. lam cps. lam dynamicDelay. lam prune. lam. lam loader.
    match includeFileExn "." "treeppl::treeppl-to-coreppl/runtime-flags.mc" loader with (flagEnv, loader) in
    let getOptParser : String -> Expr -> Expr = lam ident. lam default.
      app_ (nvar_ (_getVarExn ident flagEnv)) default in
    let fields =
      [ ("particles", getOptParser "_particles" (int_ particles))
      , ("earlyStop", getOptParser "_earlyStop" (bool_ earlyStop))
      ] in
    match makeOptParser loader fields with (loader, optParser) in
    ( loader
    , { mkMethod = lam n. Importance
        { particles = recordproj_ "particles" (nvar_ n)
        , earlyStop = recordproj_ "earlyStop" (nvar_ n)
        , cps = cps
        , dynamicDelay = dynamicDelay
        , prune = prune
        }
      , optParser = optParser
      }
    ) in
  let res = optMap5 mk particles earlyStop _cps _dynamicDelay _prune in
  let method = optSpecificArg
    { optExactArg "is" with short = "m", long = "method"
    , description = "Importance sampling using likelihood weights"
    , category = catMethod
    } in
  optMap2 (lam. lam x. x) method res in

let smcApfOptions : OptParser MkInferMethod =
  let mk = lam particles. lam subsample. lam subsampleSize. lam resample. lam prune. lam cps. lam. lam loader.
    match includeFileExn "." "treeppl::treeppl-to-coreppl/runtime-flags.mc" loader with (flagEnv, loader) in
    let getOptParser : String -> Expr -> Expr = lam ident. lam default.
      app_ (nvar_ (_getVarExn ident flagEnv)) default in
    let fields =
      [ ("particles", getOptParser "_particles" (int_ particles))
      , ("subsample", getOptParser "_subsample" (bool_ subsample))
      , ("subsampleSize", getOptParser "_subsampleSize" (int_ subsampleSize))
      ] in
    match makeOptParser loader fields with (loader, optParser) in
    ( loader
    , { mkMethod = lam n. APF
        { particles = recordproj_ "particles" (nvar_ n)
        , subsample = recordproj_ "subsample" (nvar_ n)
        , subsampleSize = recordproj_ "subsampleSize" (nvar_ n)
        , resample = resample
        , prune = prune
        , cps = cps
        }
      , optParser = optParser
      }
    ) in
  let res = optApply (optMap5 mk particles subsample subsampleSize _resample _prune) _cps in
  let method = optSpecificArg
    { optExactArg "smc-apf" with short = "m", long = "method"
    , description = "SMC using the alive particles filter"
    , category = catMethod
    } in
  optMap2 (lam. lam x. x) method res in

let smcBpfOptions : OptParser MkInferMethod =
  let mk = lam particles. lam subsample. lam subsampleSize. lam resample. lam resampleFrac. lam cps. lam prune. lam dynamicDelay. lam. lam loader.
    match includeFileExn "." "treeppl::treeppl-to-coreppl/runtime-flags.mc" loader with (flagEnv, loader) in
    let getOptParser : String -> Expr -> Expr = lam ident. lam default.
      app_ (nvar_ (_getVarExn ident flagEnv)) default in
    let fields =
      [ ("particles", getOptParser "_particles" (int_ particles))
      , ("subsample", getOptParser "_subsample" (bool_ subsample))
      , ("subsampleSize", getOptParser "_subsampleSize" (int_ subsampleSize))
      , ("resampleFrac", getOptParser "_resampleFrac" (float_ resampleFrac))
      ] in
    match makeOptParser loader fields with (loader, optParser) in
    ( loader
    , { mkMethod = lam n. BPF
        { particles = recordproj_ "particles" (nvar_ n)
        , subsample = recordproj_ "subsample" (nvar_ n)
        , subsampleSize = recordproj_ "subsampleSize" (nvar_ n)
        , resample = resample
        , resampleFrac = recordproj_ "resampleFrac" (nvar_ n)
        , cps = cps
        , prune = prune
        , dynamicDelay = dynamicDelay
        }
      , optParser = optParser
      }
    ) in
  let res = optApply3
    (optMap5 mk particles subsample subsampleSize _resample resampleFrac)
    _cps _prune _dynamicDelay in
  let method = optSpecificArg
    { optExactArg "smc-bpf" with short = "m", long = "method"
    , description = "SMC using the bootstrap particle filter"
    , category = catMethod
    } in
  optMap2 (lam. lam x. x) method res in

let pmcmcPimhOptions : OptParser MkInferMethod =
  let mk = lam particles. lam iterations. lam cps. lam. lam loader.
    match includeFileExn "." "treeppl::treeppl-to-coreppl/runtime-flags.mc" loader with (flagEnv, loader) in
    let getOptParser : String -> Expr -> Expr = lam ident. lam default.
      app_ (nvar_ (_getVarExn ident flagEnv)) default in
    let fields =
      [ ("particles", getOptParser "_particles" (int_ particles))
      , ("iterations", getOptParser "_iterations" (int_ iterations))
      ] in
    match makeOptParser loader fields with (loader, optParser) in
    ( loader
    , { mkMethod = lam n. PIMH
        { particles = recordproj_ "particles" (nvar_ n)
        , iterations = recordproj_ "iterations" (nvar_ n)
        , cps = cps
        }
      , optParser = optParser
      }
    ) in
  let res = optMap3 mk particles iterations _cps in
  let method = optSpecificArg
    { optExactArg "pmcmc-pimh" with short = "m", long = "method"
    , description = "Particle MCMC applying MH moves to SMC samples of the full model"
    , category = catMethod
    } in
  optMap2 (lam. lam x. x) method res in

let tpplFrontendOptions : OptParser TpplFrontendOptions =
  let mk = lam input. lam output. lam outputMl. lam inferTimeMs. lam seed. lam sweeps.
    { input = input
    , output = output
    , outputMl = outputMl
    , inferTimeMs = inferTimeMs
    , seed = seed
    , sweeps = sweeps
    } in
  let input = optPos
    { optPosDefString with arg = "<program>"
    , description = "The TreePPL program to compile."
    , category = catCompile
    } in
  let output =
    let default = "out" in
    let opt = optArg
      { optArgDefString with long = "output"
      , description = concat "The name of the final compiled executable. Default: " default
      , arg = "<file>"
      , category = catCompile
      } in
    optOr opt (optPure default) in
  let outputMl = optOptional (optArg
    { optArgDefString with long = "output-ml"
    , description = "Output the intermediate .ml file to this path."
    , category = catDebug
    }) in
  optApply (optMap5 mk input output outputMl inferTimeMs seed) sweeps in

let transformationOptions : OptParser TransformationOptions =
  let mk = lam printModel. lam extractSimplification. lam staticDelay. lam debugPhases. lam debugDumpPhases.
    { printModel = printModel
    , extractSimplification = extractSimplification
    , staticDelay = staticDelay
    , debugPhases = debugPhases
    , debugDumpPhases = debugDumpPhases
    , seed = None ()  -- NOTE(vipa, 2026-03-11): We hande seed differently from coreppl, so we don't set it here
    , invariantsToCheck = lam.
      use UnboundErrorAttr in
      use WithoutInfoAttr in
      use DefinedAttr in
      let scope =
        {_scopeEmpty () with tyConstructors = setOfSeq nameCmp (mapValues builtinTypeNames)} in
      [ InScopeAttr (filledThunk scope)
      , UnboundErrorAttr (mkThunk (lazyPure "UnboundErrorAttr#root"))
      , WithoutInfoAttr (mkThunk (lazyPure "WithoutInfoAttr#root"))
      , DefinedAttr (mkThunk (lazyPure "DefinedAttr#root"))
      ]
    } in
  let printModel = optFlag
    { optFlagDef with long = "print-model"
    , description = "The parsed model is pretty printed before inference method transformations."
    , category = catDebug
    } in
  let extractSimplification =
    let default = "none" in
    let opt = optArg
      { optArgDefString with long = "extract-simplification"
      , arg = "<option>"
      , description = join
        [ "Temporary flag that decides the simplification approach after extraction"
        , " in the MExpr compiler backend. The supported options are: none, inline,"
        , " and peval. Default: ", default, ". Eventually, we will remove this option"
        , " and only use peval."
        ]
      , category = catCompile
      } in
    optOr opt (optPure default) in
  let staticDelay = optFlag
    { optFlagDef with long = "static-delay"
    , description = "The model is transformed to an efficient representation if possible."
    , category = catCompile
    } in
  let debugPhases = optFlag
    { optFlagDef with long = "debug-phases"
    , description = "Show debug and profiling information about each pass"
    , category = catDebug
    } in
  let debugDumpPhases =
    let opt = optArg
      { optArgDefString with long = "debug-log-phases"
      , arg = "<phases>"
      , description = "Print a json representation of the AST after the given (comma-separated) passes."
      , category = catDebug
      } in
    optOr (optMap (lam str. setOfSeq cmpString (strSplit "," str)) opt) (optPure (setEmpty cmpString)) in
  optMap5 mk printModel extractSimplification staticDelay debugPhases debugDumpPhases in

let options : OptParser (TpplFrontendOptions, TransformationOptions, MkInferMethod) =
  let inference = foldl1 optOr
    [ isLwOptions
    , smcApfOptions
    , smcBpfOptions
    , mcmcLightweightOptions
    , mcmcNaiveOptions
    , mcmcTraceOptions
    , pmcmcPimhOptions
    ] in
  optMap3 (lam a. lam b. lam c. (a, b, c)) tpplFrontendOptions transformationOptions inference in

match optParseWithHelp "tpplc" "" options (tail argv)
  with (frontend, transformations, mkInferenceMethod) in
compileTpplToExecutable frontend transformations mkInferenceMethod
