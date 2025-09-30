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
  let mk = lam debugIterations. lam samplingPeriod. lam incrementalPrinting. lam iterations. lam globalProb. lam driftKernel. lam driftScale. lam cps. lam align. lam debugAlignment. lam outputType. lam loader.
    match includeFileExn "." "stdlib::json.mc" loader with (jsonEnv, loader) in
    match includeFileExn "." "stdlib::string.mc" loader with (stringEnv, loader) in
    match includeFileExn "." "stdlib::basic-types.mc" loader with (basicEnv, loader) in
    match includeFileExn "." "stdlib::ext/file-ext.mc" loader with (fileEnv, loader) in
    match includeFileExn "." "coreppl::coreppl-to-mexpr/mcmc-lightweight/config.mc" loader with (configEnv, loader) in
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
    let continue =
      let idx = nameSym "idx" in
      let beta = nameSym "beta" in
      let optWc = nameSym "optWc" in
      let accLets = [  
        nulet_ idx (tupleproj_ 0 (var_ "acc")),
        nulet_ beta (tupleproj_ 1 (var_ "acc")),
        nulet_ optWc (tupleproj_ 2 (var_ "acc"))
      ] in
      let ret = utuple_ [utuple_ [addi_ (nvar_ idx) (int_ 1), nvar_ beta, nvar_ optWc], if pigeons then true_ else neqi_ (nvar_ idx) (int_ iterations)] in
      let ret =
        if incrementalPrinting then
          let ch = nameSym "ch" in
          let jsonLn = nvar_ (_getVarExn "getJsonLn" jsonEnv) in
          let writeFile_ = appf2_ (nvar_ (_getVarExn "fileWriteString" fileEnv)) in
          let flushFile_ = app_ (nvar_ (_getVarExn "fileWriteFlush" fileEnv)) in
          let printer = (match_
            (nvar_ optWc)
            (npcon_ (_getConExn "Some" basicEnv) (npvar_ ch))
            (ulam_ "s" (semi_ (writeFile_ (nvar_ ch) (var_ "s")) (flushFile_ (nvar_ ch))))
            (ulam_ "s" (print_ (var_ "s")))
          ) in
          let serSample = app_ jsonLn (app_ outputSer.serializer (var_ "sample")) in
          let strJoin_ = appf2_ (nvar_ (_getVarExn "strJoin" stringEnv)) in
          let int2string_ = app_ (nvar_ (_getVarExn "int2string" stringEnv)) in
          let serSample = if pigeons then
            (strJoin_ (str_ "\t") (seq_ [int2string_ (nvar_ idx), serSample]))
            else serSample in 
          let print =  app_ printer serSample in
          let print = if pigeons
            then if_ (eqf_ (float_ 1.0) (nvar_ beta)) print unit_ 
            else if eqi samplingPeriod 1
              then print
              else if_ (eqi_ (int_ 0) (modi_ (nvar_ idx) (int_ samplingPeriod))) print unit_ in
          semi_ print ret
        else ret in
      let bret = bindall_ accLets ret in
      utuple_ [utuple_ [int_ 0, float_ 1.0, nconapp_ (_getConExn "None" basicEnv) unit_], ulam_ "acc" (ulam_ "sample" bret)] in
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
      , globalProb = float_ globalProb
      , debug = debug
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
  let res = optApply (optApply (optApply (optApply (optApply (optMap5 mk debugIterations samplingPeriod incrementalPrinting _particles _mcmcLightweightGlobalProb) _driftKernel) _driftScale) _cps) _align) _debugAlignment in
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
