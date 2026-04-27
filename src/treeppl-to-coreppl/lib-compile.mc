-- NOTE(2023-08-11,dlunde): This CorePPL library file contains functions
-- referred to by the TreePPL compiler.

include "json.mc"
include "optparse-applicative.mc"
include "ext/dist-ext.mc"

include "runtime-flags.mc"

let serializeResult: all a. (a -> JsonValue) -> Dist(a) -> JsonValue
  = lam sampleSerializer. lam resultDist.
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

let outputInferTimeMs: all a. (() -> a) -> a = lam f.
  let beginT = wallTimeMs () in
  let inf = f () in
  let endT = wallTimeMs () in
  printError "{\"Inference time (ms)\":";  printError (float2string (subf endT beginT));  printErrorLn "}";
  inf

type RunInferenceConfig res inferOpts =
  { sweeps : Int
  , seed : Option Int
  , inferTimeMs : Bool
  , inferOpts : OptParser inferOpts
  , sampleSerializer : res -> JsonValue
  , runOnce : JsonValue -> inferOpts -> Dist res
  }
let runInference : all res. all inferOpts. RunInferenceConfig res inferOpts -> ()
  = lam config.
    let dataFile = optPos
      { optPosDefString with arg = "<data.json>"
      , description = "The data to pass to the main model function, in a json file"
      } in
    let run = lam dataFile. lam inferTimeMs. lam sweeps. lam seed. lam inferOpts. lam.
      (match seed with Some seed then setSeed seed else ());
      let runOnce = config.runOnce (jsonParseExn (readFile dataFile)) in
      let runOnce = if inferTimeMs
        then lam. printJsonLn (serializeResult config.sampleSerializer (outputInferTimeMs (lam. runOnce inferOpts)))
        else lam. printJsonLn (serializeResult config.sampleSerializer (runOnce inferOpts)) in
      repeat runOnce sweeps in
    let options = optMap5 run
      dataFile (_inferTimeMs config.inferTimeMs) (_sweeps config.sweeps) (_seed config.seed) config.inferOpts in
    optParseWithHelp (head argv) "" options (tail argv) ()
