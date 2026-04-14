include "stdlib::test-spec.mc"

mexpr

use TestSpec in

let substituter : Substituter =
  { noSubstituter with substitutions = mapFromSeq cmpChar
    [ ( 't'
      , { tup =
          { actual = "MCORE_LIBS=$MCORE_LIBS:treeppl=$(ROOT)/src %<tpplc>"
          , deps = ["$(ROOT)/src/<tpplc>"]
          }
        , make =
          { actual = "MCORE_LIBS=$$MCORE_LIBS:treeppl=$(ROOT)/src $(ROOT)/build/tpplc"
          , deps = ["build/tpplc"]
          }
        , friendly = "TPPLC"
        }
      )
    ]
  } in
let directories = ["src", "models"] in
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
      , cmd = concat "%t %f --output %o --debug-phases " flags
      } in
    let run = api.endStep
      { tag = strReplace " " "_" (concat "run " flags)
      , uses = [compile, api.file (lam f. concat "data/testdata_" (withExtension ".json" f))]
      , cmd = "%i"
      } in
    [compile, run] in
  let modes =
    [ "--particles 2 -m is --cps none"
    , "--particles 2 -m is --cps partial"
    , "--particles 2 -m is --cps full"
    , "--particles 2 -m smc-bpf --cps full" --resample
    , "--particles 2 -m smc-bpf --cps partial " --resample
    , "--particles 2 -m smc-apf --cps full " --resample
    , "--particles 2 -m smc-apf --cps partial " --resample
    , "--iterations 2 -m mcmc --cps none" --resample
    , "--iterations 2 -m mcmc --align --cps none" --resample
    , "--iterations 2 -m mcmc --align --cps full" --resample
    , "--iterations 2 -m mcmc --align --cps partial" --resample
    , "--iterations 2 -m mcmc-trace"
    , "--iterations 2 -m mcmc-naive"
    , "--particles 2 --iterations 2 -m pmcmc-pimh --cps full"
    , "--particles 2 --iterations 2 -m pmcmc-pimh --cps partial"
    ] in
  let allModelTests = join (map mkTestModel modes) in

  api.tests []
    (and (strStartsWith "models/") (strEndsWith ".tppl"))
    (map (lam x. (x, Succ ())) allModelTests);

  -- NOTE(vipa, 2026-04-13): We disable the clads tests for the
  -- moment, since it's likely to loop forever when running with
  -- APF. There are some model changes that can be made to fix this,
  -- but in the meanwhile we just skip those tests
  api.tests []
    (eqString "models/diversification/clads.tppl")
    (map (lam x. (x, Dont ())) allModelTests);

  ()
);

()
