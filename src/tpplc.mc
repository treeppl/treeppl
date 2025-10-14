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

include "coreppl::dppl-arg.mc" -- inherit cmd-line opts from cppl
include "coreppl::parser.mc"




mexpr

use TreePPLThings in

let mcmcLightweightOptions : OptParser (Type -> Loader -> (Loader, InferMethod)) =
  let mk = lam debugIterations. lam samplingPeriod. lam incrementalPrinting. lam iterations. lam globalProb.
    lam driftKernel. lam driftScale. lam cps.
    lam align. lam debugAlignment.
    lam pigeons. lam pigeonsGlobal. lam pigeonsExploreSteps.
    lam outputType. lam loader.

    match includeFileExn "." "stdlib::json.mc" loader with (jsonEnv, loader) in
    match includeFileExn "." "coreppl::coreppl-to-mexpr/mcmc-lightweight/config.mc" loader with (configEnv, loader) in
    match includeFileExn "." "treeppl::treeppl-to-coreppl/mcmc-lightweight.mc" loader with (lwEnv, loader) in
    let debugTypeFields = switch mapLookup (_getTyConExn "DebugInfo" configEnv) (_getTCEnv loader).tyConEnv
      case Some (_, [], TyRecord x) then x
      case Some (_, _, _) then error "Compiler error: unexpected shape of DebugInfo"
      case None _ then error "Compiler error: no info about DebugInfo in TCEnv"
      end in
    let fullDebugType = TyRecord
      { debugTypeFields with fields = mapInsert (stringToSid "durationMs") tyfloat_ debugTypeFields.fields } in
    match serializationPairsFor [outputType, fullDebugType] loader with (loader, [outputSer, debugSer]) in
    let keepSample = if incrementalPrinting
      then ulam_ "" false_
      else if eqi 1 samplingPeriod
        then ulam_ "" true_
        else ulam_ "idx" (eqi_ (int_ 0) (modi_ (var_ "idx") (int_ samplingPeriod))) in
    match (if pigeons then 
      (appf3_ (nvar_ (_getVarExn "continuePigeons" lwEnv)) (int_ pigeonsExploreSteps)
      , nvar_ (_getVarExn "accInitPigeons" lwEnv)
      , nvar_ (_getVarExn "temperaturePigeons" lwEnv)
      )
    else if incrementalPrinting then
      (appf3_ (nvar_ (_getVarExn "continueIncremental" lwEnv)) (int_ iterations)
      , nvar_ (_getVarExn "accInitIncremental" lwEnv)
      , nvar_ (_getVarExn "temperatureIncremental" lwEnv)
      )
    else 
      (lam. lam. appf1_ (nvar_ (_getVarExn "continueBase" lwEnv)) (int_ iterations)
      , nvar_ (_getVarExn "accInitBase" lwEnv)
      , nvar_ (_getVarExn "temperatureBase" lwEnv)
      )
    ) with (unappContinue, accInit, temperature) in
    let continue =
      let appFunc = unappContinue (int_ samplingPeriod) outputSer.serializer in
      utuple_ [accInit, appFunc] in
    let debug =
      if debugIterations then
        utuple_
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
          ]
      else utuple_ [unit_, ulam_ "" (ulam_ "" unit_)] in
    let method = LightweightMCMC
      { keepSample = keepSample
      , continue = continue
      , temperature = temperature
      , globalProb = float_ globalProb
      , debug = debug
      , forceGlobal = pigeonsGlobal
      , driftKernel = driftKernel
      , driftScale = driftScale
      , cps = cps
      , align = align
      , debugAlignment = debugAlignment
      } in
    (loader, method) in
  let debugIterations = optFlag
    { optFlagDef with long = "debug-iterations"
    , description = "Output various debug information each iteration."
    } in
  let samplingPeriod =
    let default = 1 in
    let opt = optArg
      { optArgDefInt with long = "sampling-period"
      , description = concat "Sample the mcmc-chain every Nth iteration. Default: " (int2string default)
      , arg = "N"
      } in
    optOr opt (optPure default) in
  let incrementalPrinting = optFlag
    { optFlagDef with long = "incremental-printing"
    , description = "Print each sample as it is produced instead of at the end."
    } in
  let _pigeonsDefault : Bool = false in
  let _pigeons : OptParser Bool = optMap (xor _pigeonsDefault) (optFlag
    { optFlagDef with long = "pigeons"
    , description = "Let Pigeons.jl control inference via stdio."
    }) in
  let _pigeonsGlobalDefault : Bool = true in
  let _pigeonsGlobal : OptParser Bool = optMap (xor _pigeonsGlobalDefault) (optFlag
    { optFlagDef with long = "pigeons-no-global"
    , description = "Requires --pigeons. Do not use global moves when sampling at temperature 0.0"
    }) in
  let _pigeonsExploreStepsDefault : Int = 1 in
  let _pigeonsExploreSteps : OptParser Int =
    let opt = optArg
      { optArgDefInt with long = "pigeons-explore-steps"
      , description = concat "Requires --pigeons. The number of local MCMC steps to take before communicating with Pigeons.jl. Default: " (int2string _pigeonsExploreStepsDefault)
      } in
    optOr opt (optPure _pigeonsExploreStepsDefault) in
  let res = optApply (
    optApply (
      optApply (
        optApply (
          optApply (
            optApply (
              optApply (
                optApply (
                  optMap5 mk debugIterations samplingPeriod incrementalPrinting _particles _mcmcLightweightGlobalProb
                ) _driftKernel
              ) _driftScale
            ) _cps
          )_align
        ) _debugAlignment
      ) _pigeons
    ) _pigeonsGlobal
  ) _pigeonsExploreSteps in
  optMap2 (lam. lam x. x) (_methodFlag false "mcmc-lightweight") res in

let wrapSimpleInferenceMethod
  : OptParser InferMethod -> OptParser (Type -> Loader -> (Loader, InferMethod))
  = optMap (lam m. lam. lam loader. (loader, m)) in

let options : OptParser (TpplFrontendOptions, TransformationOptions, Type -> Loader -> (Loader, InferMethod)) =
  let inference = foldl1 optOr
    [ mcmcLightweightOptions
    , wrapSimpleInferenceMethod isLwOptions
    , wrapSimpleInferenceMethod smcApfOptions
    , wrapSimpleInferenceMethod smcBpfOptions
    , wrapSimpleInferenceMethod mcmcNaiveOptions
    , wrapSimpleInferenceMethod mcmcTraceOptions
    , wrapSimpleInferenceMethod pmcmcPimhOptions
    ] in
  optMap3 (lam a. lam b. lam c. (a, b, c)) tpplFrontendOptions transformationOptions inference in

match optParseWithHelp "tpplc" "" options (tail argv)
  with (frontend, transformations, mkInferenceMethod) in
compileTpplToExecutable frontend transformations mkInferenceMethod
