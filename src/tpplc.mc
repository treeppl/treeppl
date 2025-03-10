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
  let mk = lam samplingPeriod. lam incrementalPrinting. lam iterations. lam globalProb. lam driftKernel. lam driftScale. lam cps. lam align. lam outputType. lam loader.
    match includeFileExn "." "stdlib::json.mc" loader with (jsonEnv, loader) in
    match serializationPairsFor [outputType] loader with (loader, [outputSer]) in
    let keepSample = if incrementalPrinting
      then ulam_ "" false_
      else if eqi 1 samplingPeriod
        then ulam_ "" true_
        else ulam_ "idx" (eqi_ (int_ 0) (modi_ (var_ "idx") (int_ samplingPeriod))) in
    let continue =
      let ret = utuple_ [addi_ (var_ "idx") (int_ 1), neqi_ (var_ "idx") (int_ iterations)] in
      let ret =
        if incrementalPrinting then
          let print = app_ (nvar_ (_getVarExn "printJsonLn" jsonEnv)) (app_ outputSer.serializer (var_ "sample")) in
          let print = if eqi samplingPeriod 1
            then print
            else if_ (eqi_ (int_ 0) (modi_ (var_ "idx") (int_ samplingPeriod))) print unit_ in
          semi_ print ret
        else ret in
      utuple_ [int_ 0, ulam_ "idx" (ulam_ "sample" ret)] in
    let method = LightweightMCMC
      { keepSample = keepSample
      , continue = continue
      , globalProb = float_ globalProb
      , driftKernel = driftKernel
      , driftScale = driftScale
      , cps = cps
      , align = align
      } in
    (loader, method) in
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
  let res = optApply (optApply (optApply (optMap5 mk samplingPeriod incrementalPrinting _particles _mcmcLightweightGlobalProb _driftKernel) _driftScale) _cps) _align in
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
