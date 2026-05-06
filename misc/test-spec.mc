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

  let mkTestModelPair = lam flags.
    let compile = api.midStep
      { tag = strReplace " " "_" (concat "compile " flags)
      , uses = []
      , cmd = concat "%t %f --output %o --debug-phases " flags
      } in
    let run = api.endStep
      { tag = strReplace " " "_" (concat "run " flags)
      , uses = [compile, api.file (lam f. concat "data/testdata_" (withExtension ".json" f))]
      , cmd = "%i"
      } in
    (compile, run) in
  let mkTestModel = lam flags.
    match mkTestModelPair flags with (l, r) in [l, r] in
  let mostModes =
    [ "--particles 2 -m is --cps none"
    , "--particles 2 -m is --cps partial"
    , "--particles 2 -m is --cps full"
    , "--particles 2 -m smc-bpf --cps full" --resample
    , "--particles 2 -m smc-bpf --cps partial " --resample
    , "--iterations 2 -m mcmc --cps none" --resample
    , "--iterations 2 -m mcmc-trace"
    , "--iterations 2 -m mcmc-naive"
    , "--particles 2 --iterations 2 -m pmcmc-pimh --cps full"
    , "--particles 2 --iterations 2 -m pmcmc-pimh --cps partial"
    ] in
  let alignedModes =
    [ "--iterations 2 -m mcmc --align --cps none" --resample
    , "--iterations 2 -m mcmc --align --cps full" --resample
    , "--iterations 2 -m mcmc --align --cps partial" --resample
    , "--iterations 2 -m mcmc --align --cps full --incremental-printing" --resample
    , "--iterations 2 -m mcmc --align --cps none --data-augmentation 1" --resample
    , "--iterations 2 -m mcmc --align --cps full --data-augmentation 1" --resample
    , "--iterations 2 -m mcmc --align --cps full --data-augmentation 1 --incremental-printing" --resample
    ] in
  let apfModes =
    [ "--particles 2 -m smc-apf --cps full" --resample
    , "--particles 2 -m smc-apf --cps partial" --resample
    ] in
  let mostTests = join (map mkTestModel mostModes) in
  let alignedTests = unzip (map mkTestModelPair alignedModes) in
  let apfTests = join (map mkTestModel apfModes) in
  let allModelTests = join [mostTests, apfTests, alignedTests.0, alignedTests.1] in

  api.tests []
    (and (strStartsWith "lib/models/") (strEndsWith ".tppl"))
    (map (lam x. (x, Succ ())) allModelTests);

  -- NOTE(vipa, 2026-05-06): Some models have additional library files
  -- in a `-lib` folder, do not test them as though they were models.
  api.tests []
    (strContains "-lib/")
    (map (lam x. (x, Dont ())) allModelTests);

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
    (map (lam x. (x, Fail ())) alignedTests.1);

  -- NOTE(vipa, 2026-04-13): Some models have a decent likelihood of
  -- getting stuck when running with APF and very few particles, so we
  -- turn off those tests there
  api.tests []
    (strStartsWith "lib/models/diversification/")
    (map (lam x. (x, Dont ())) apfTests);

  ()
);

()
