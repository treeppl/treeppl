-- NOTE(2023-08-11,dlunde): This CorePPL library file contains functions
-- referred to by the TreePPL compiler.

-- 1. We define this `ext.mc` which contains the includes needed for log, etc.
-- external functions
include "ext/dist-ext.mc"
include "ext/math-ext.mc"
include "matrix.mc"
include "treeppl::lib/standard.mc"
include "coreppl::coreppl-to-mexpr/runtime-dists.mc"

-- 2. We parse this file to Expr
-- let externals = parseMCoreFile "src/externals/ext.mc" in

-- 3. We bind this, as well as our input, to the AST we're generating
--   let input = bind_ externals input in
-- and then in TmRecLets
--   inexpr = bind_ input invocation

-- 4. We need to get  the name for symbol that has the string "log"
-- this is where I am stuck
--   getExternalIds externals
-- will give us a Set String
-- but I am looking for a function like
--   getNameFor "log" externals
-- But I can't find that
-- There is nameGetStr, but this is from Name -> Str
--

include "json.mc"

let serializeResult: all a. (a -> JsonValue) -> Dist(a) -> JsonValue =
    lam sampleSerializer. lam resultDist.
      match distEmpiricalSamples resultDist with (samples, weights) in
      match distEmpiricalNormConst resultDist with nc in -- nc is NaN when inference method does not produce it
      let samples: [a] = samples in
      let weights: [Float] = weights in
      let nc: Float = nc in
      JsonObject
        (mapInsert "normConst" (jsonSerializeFloat nc)
        (mapInsert "samples" (JsonArray (map sampleSerializer samples))
        (mapInsert "weights" (JsonArray (map jsonSerializeFloat weights))
        (mapEmpty cmpString))))

let outputinferTimeMs: all a. (() -> a) -> a = lam f.
  let beginT = wallTimeMs () in
  let inf = f () in
  let endT = wallTimeMs () in
  printError "{\"Inference time (ms) \":";  printError (float2string (subf endT beginT));  printErrorLn "}";
  inf

let particles = if leqi (length argv) 2 then 10 else string2int (get argv 2)

let sweeps    = if leqi (length argv) 3 then 1 else string2int (get argv 3)


let input: JsonValue =
  if leqi (length argv) 1 then error "You must provide a data file!"
  else jsonParseExn (readFile (get argv 1))
