include "stdlib::test-spec.mc"

mexpr

use TestSpec in

let substituter : Substituter =
  { noSubstituter with substitutions = mapFromSeq cmpChar
    [ ( 't'
      , { tup =
          { actual = "MCORE_LIBS=treeppl=$(ROOT)/lib:$MCORE_LIBS %<tpplc>"
          , deps = ["$(ROOT)/src/<tpplc>"]
          }
        , make =
          { actual = "MCORE_LIBS=treeppl=$(ROOT)/lib:$$MCORE_LIBS $(ROOT)/build/tpplc"
          , deps = ["build/tpplc"]
          }
        , friendly = "TPPLC"
        }
      )
    ]
  } in
let directories = ["src", "lib"] in
let location = Some
  { src = "misc/test-spec.mc"
  , exe = "misc/test"
  } in
testMain [substituter] directories location (lam api.
  let and = lam l. lam r. lam x. if l x then r x else false in
  let or = lam l. lam r. lam x. if l x then true else r x in
  let elem = lam elems.
    let set = setOfSeq cmpString elems in
    lam x. setMem x set in
  let dirIs = lam dir. lam path. eqString dir (dirname path) in
  let strContains = lam needle. lam str. optionIsSome (subseqFindIdx eqc needle str) in

  -- === Test .mc files ===

  let miBuild = api.midStep
    { tag = "mi-build"
    , uses = []
    , cmd = "mi compile --test %f --output %o"
    } in
  let miRun = api.endStep
    { tag = "mi-run"
    , uses = [miBuild]
    , cmd = "command %i"
    } in

  api.tests []
    (and (strStartsWith "src/") (strEndsWith ".mc"))
    [(miBuild, Succ ()), (miRun, Succ ())];

  -- NOTE(vipa, 2026-04-13): Skip tpplc itself, it's tested indirectly
  -- through compilation
  api.tests []
    (eqString "src/tpplc.mc")
    [(miBuild, Dont ()), (miRun, Dont ())];
  -- NOTE(vipa, 2026-04-13): These files depend on the generated AST,
  -- which isn't inserted in the source tree directly, so they'll fail
  -- without special care.
  api.tests []
    (elem
      [ "src/treeppl-to-coreppl/compile.mc"
      , "src/treeppl-ast-print.mc"
      ])
    [(miBuild, Dont ()), (miRun, Dont ())];
  -- NOTE(vipa, 2026-04-13): This is actually a coreppl file, which
  -- we're not testing explicitly for the moment
  api.tests []
    (eqString "src/treeppl-to-coreppl/lib-compile.mc")
    [(miBuild, Fail ())];

  -- === Test models ===

  let mkTestModel = lam flags.
    let compile = api.midStep
      { tag = strReplace " " "_" (concat "compile " flags)
      , uses = []
      , cmd = concat "%t %f --output %o " flags
      } in
    let run = api.endStep
      { tag = strReplace " " "_" (concat "run " flags)
      , uses = [compile, api.file (lam f. concat "data/testdata_" (withExtension ".json" f))]
      , cmd = "%i"
      } in
    let flags = strSplit " " flags in
    [(cons "compile" flags, compile), (cons "run" flags, run)] in

  let modes = join (map mkTestModel
    [ "--particles 2 -m is --cps none"
    , "--particles 2 -m is --cps partial"
    , "--particles 2 -m is --cps full"
    , "--particles 2 -m is --cps full --debug-phases"
    , "--particles 2 -m smc-bpf --cps full --resample manual"
    , "--particles 2 -m smc-bpf --cps full --resample likelihood"
    , "--particles 2 -m smc-bpf --cps full --resample align"
    , "--particles 2 -m smc-bpf --cps partial  --resample manual"
    , "--particles 2 -m smc-bpf --cps partial --resample likelihood"
    , "--particles 2 -m smc-bpf --cps partial --resample align"
    , "--particles 2 -m smc-bpf --cps partial --resample align --debug-phases"
    , "--particles 2 -m smc-apf --cps full --resample manual"
    , "--particles 2 -m smc-apf --cps full --resample likelihood"
    , "--particles 2 -m smc-apf --cps full --resample align"
    , "--particles 2 -m smc-apf --cps partial --resample manual"
    , "--particles 2 -m smc-apf --cps partial --resample likelihood"
    , "--particles 2 -m smc-apf --cps partial --resample align"
    , "--particles 2 -m smc-apf --cps partial --resample align --debug-phases"
    , "--iterations 2 -m mcmc-trace"
    , "--iterations 2 -m mcmc-trace --debug-phases"
    , "--iterations 2 -m mcmc-naive"
    , "--iterations 2 -m mcmc-naive --debug-phases"
    , "--iterations 2 -m mcmc --cps none"
    , "--iterations 2 -m mcmc --debug-mcmc --cps none"
    , "--iterations 2 -m mcmc --debug-mcmc --align --cps none"
    , "--iterations 2 -m mcmc --debug-mcmc --align --cps full"
    , "--iterations 2 -m mcmc --debug-mcmc --align --cps partial"
    , "--iterations 2 -m mcmc --debug-mcmc --align --cps full --incremental-printing"
    , "--iterations 2 -m mcmc --debug-mcmc --align --cps none --data-augmentation 1"
    , "--iterations 2 -m mcmc --debug-mcmc --align --cps full --data-augmentation 1"
    , "--iterations 2 -m mcmc --debug-mcmc --align --cps full --data-augmentation 1 --incremental-printing"
    , "--particles 2 --iterations 2 -m pmcmc-pimh --cps full --debug-phases"
    , "--particles 2 --iterations 2 -m pmcmc-pimh --cps partial"
    , "--iterations 2 -m mcmc-graph"
    , "--iterations 2 -m mcmc-graph --debug-phases"
    ]) in
  let allModesWith = lam run. map (lam x. (x.1, run)) modes in
  let someModesWith = lam required. lam run.
    let checkPass = lam pair. forAll (lam req. seqMem eqString pair.0 req) required in
    map (lam x. (x.1, run)) (filter checkPass modes) in

  api.tests []
    (and (strStartsWith "lib/models/") (strEndsWith ".tppl"))
    (allModesWith (Succ ()));

  -- NOTE(vipa, 2026-05-06): Some models have additional library files
  -- in a `-lib` folder, do not test them as though they were models.
  api.tests []
    (strContains "-lib/")
    (allModesWith (Dont ()));

  -- NOTE(vipa, 2026-05-06): These tests either have no `assume`s (and
  -- thus no aligned `assume`s) or are written in such a way that our
  -- current alignment analysis marks all `assume`s as unaligned
  api.tests []
    (elem
      [ "lib/models/lang/anonymous.tppl"
      , "lib/models/lang/externals.tppl"
      , "lib/models/lang/hello.tppl"
      , "lib/models/lang/matrix-tests.tppl"
      , "lib/models/lang/mini-mat-test.tppl"
      , "lib/models/lang/tensors.tppl"
      , "lib/models/tree-inference/tree_inference_pruning_scaled.tppl"
      , "lib/models/tree-inference/tree_inference_pruning.tppl"
      , "lib/models/tree-inference/tree_inference.tppl"
      ])
    (someModesWith ["run", "mcmc", "--align"] (Fail ()));

  -- NOTE(vipa, 2026-04-13): Some models have a decent likelihood of
  -- getting stuck when running with APF and very few particles, so we
  -- turn off those tests there
  api.tests []
    (strStartsWith "lib/models/diversification/")
    (someModesWith ["smc-apf"] (Dont ()));

  -- NOTE(vipa, 2026-06-15): These models have no manual resamples or
  -- `observe`/`weight` statements, thus smc will never work on them.
  api.tests []
    (elem
     [ "lib/models/lang/anonymous.tppl"
     , "lib/models/lang/blub.tppl"
     , "lib/models/lang/distributions.tppl"
     , "lib/models/lang/externals.tppl"
     , "lib/models/lang/hello.tppl"
     , "lib/models/lang/matrix-tests.tppl"
     , "lib/models/lang/mini-mat-test.tppl"
     , "lib/models/lang/ser-list.tppl"
     , "lib/models/lang/tensors.tppl"
     ])
    (concat
      (someModesWith ["compile", "smc-apf"] (Fail ()))
      (someModesWith ["compile", "smc-bpf"] (Fail ())));

  -- NOTE(vipa, 2026-06-15): Many models have no manual resample
  -- points and will thus fail to compile with --resample manual
  api.tests []
    (elem
      [ "lib/models/diversification/crbd_analytical.tppl"
      , "lib/models/host-repertoire-evolution/flat-root-prior-HRM.tppl"
      , "lib/models/host-repertoire-evolution/subroot-HRM.tppl"
      , "lib/models/lang/coin.tppl"
      , "lib/models/lang/poly.tppl"
      , "lib/models/metabarcoding/optmodel.tppl"
      , "lib/models/tree-inference/substmodel_belief_propagation.tppl"
      , "lib/models/tree-inference/tree_inference_pruning_scaled.tppl"
      , "lib/models/tree-inference/tree_inference.tppl"
      ])
    (concat
      (someModesWith ["compile", "smc-apf", "manual"] (Fail ()))
      (someModesWith ["compile", "smc-bpf", "manual"] (Fail ())));

  -- NOTE(vipa, 2026-06-15): Our alignment analysis fails to detect
  -- some `observe`/`weight` statements as aligned, thus smc with
  -- --resample align sees no resamples, i.e., it fails to compile
  api.tests []
    (elem
      [ "lib/models/tree-inference/tree_inference_pruning_scaled.tppl"
      , "lib/models/tree-inference/tree_inference.tppl"
      ])
    (concat
      (someModesWith ["compile", "smc-apf", "align"] (Fail ()))
      (someModesWith ["compile", "smc-bpf", "align"] (Fail ())));

  let pigeonsCompile = api.midStep
    { tag = "pigeons-compile"
    , uses = []
    , cmd = "%t %f --output %o --method mcmc --pigeons --pigeons-explore-steps 2 --incremental-printing"
    } in
  let pigeonsRun = api.midStep
    { tag = "pigeons-run"
    , uses = [pigeonsCompile, api.file (lam f. concat "data/testdata_" (withExtension ".json" f))]
    , cmd = "PPL_OUTPUT=%o $(ROOT)/misc/pigeons/test_responses.sh %i < $(ROOT)/misc/pigeons/commands.txt"
    } in
  let pigeonsCheckSamples = api.endStep
    { tag = "pigeons-samples"
    , uses = [pigeonsRun]
    , cmd = "$(ROOT)/misc/pigeons/test_samples.sh $(ROOT)/misc/pigeons/commands.txt %i"
    } in
  let pigeonsTests = [pigeonsCompile, pigeonsRun, pigeonsCheckSamples] in


  api.tests []
    (and (strStartsWith "lib/models/") (strEndsWith ".tppl"))
    (map (lam x. (x, Succ ())) pigeonsTests);

  -- NOTE(vipa, 2026-05-06): Some models have additional library files
  -- in a `-lib` folder, do not test them as though they were models.
  api.tests []
    (strContains "-lib/")
    (map (lam x. (x, Dont ())) pigeonsTests);

  ()
);

()
