include "optparse-applicative.mc"


-- NOTE(vipa, 2026-03-11): Only `catRun` and `catDebug` is used in
-- this file. The rest are defined here to be close to each other.
let catCompile = "Compile options:"
let catRun = "Runtime options:"
let catDebug = "Debug options:"
let catMethod = "Inference methods:"

let _debugIterations : Bool -> OptParser Bool = lam default.
  optMap (xor default) (optFlag
    { optFlagDef with long = if default then "no-debug-mcmc" else "debug-mcmc"
    , description = concat (if default then "Disable" else "Enable") " the output of various debug information each iteration."
    , category = catDebug
    })
let debugIterations = _debugIterations false

let _particles : Int -> OptParser Int = lam default.
  let opt = optArg
    { optArgDefInt with long = "particles", short = "p"
    , description = concat "The number of particles in smc. Default: " (int2string default)
    , category = catRun
    } in
  optOr opt (optPure default)
let particles = _particles 1000

let _iterations : Int -> OptParser Int = lam default.
  let opt = optArg
    { optArgDefInt with long = "iterations", short = "i"
    , description = concat "The number of iterations in mcmc. Default: " (int2string default)
    , category = catRun
    } in
  optOr opt (optPure default)
let iterations = _iterations 10000

let _earlyStop : Bool -> OptParser Bool = lam default.
  optMap (xor default) (optFlag
    { optFlagDef with long = if default then "no-early-stop" else "early-stop"
    , description = concat (if default then "Disable" else "Enable") " early stopping in certain inference algorithms."
    , category = catRun
    })
let earlyStop = _earlyStop true

let _resampleFrac : Float -> OptParser Float = lam default.
  let opt = optArg
    { optArgDefFloat with long = "resample-frac"
    , description = concat "Floating point number to trigger resampling for SMC-BPF when ESS is less than resampleFrac × particleCount. Default: " (float2string default)
    , category = catRun
    } in
  optOr opt (optPure default)
let resampleFrac = _resampleFrac 0.7

let _subsample : Bool -> OptParser Bool = lam default.
  optMap (xor default) (optFlag
    { optFlagDef with long = if default then "no-subsample" else "subsample"
    , description = concat (if default then "Disable" else "Enable") " subsampling of the posterior distribution."
    , category = catRun
    })
let subsample = _subsample false

let _subsampleSize : Int -> OptParser Int = lam default.
  let opt = optArg
    { optArgDefInt with long = "subsample-size", short = "n"
    , description = concat "The number of subsamples to draw if --subsample is selected. Default: " (int2string default)
    , category = catRun
    } in
  optOr opt (optPure default)
let subsampleSize = _subsampleSize 1

let _samplingPeriod : Int -> OptParser Int = lam default.
  let opt = optArg
    { optArgDefInt with long = "sampling-period"
    , description = concat "Sample the mcmc-chain every Nth iteration. Default: " (int2string default)
    , arg = "N"
    , category = catRun
    } in
  optOr opt (optPure default)
let samplingPeriod = _samplingPeriod 1

let _mcmcLightweightGlobalProb : Float -> OptParser Float = lam default.
  let opt = optArg
    { optArgDefFloat with long = "mcmc-global-prob"
    , description = concat "The probability of performing a global MH step (non-global means only modify a single sample in the previous trace). Default: " (float2string default)
    , category = catRun
    } in
  optOr opt (optPure default)
let mcmcLightweightGlobalProb = _mcmcLightweightGlobalProb 0.1

let _resampleBehavior : Int -> OptParser Int = lam default.
  let opt = optArg
    { optArgDefInt with long = "data-augmentation"
    , description = concat "Use the data augmentation algorithm, cycling N times before a full redraw. Default: " (int2string default)
    , arg = "N"
    , category = catRun
    } in
  optOr opt (optPure default)
let resampleBehavior = _resampleBehavior 0

let _sweeps : Int -> OptParser Int = lam default.
  let opt = optArg
    { optArgDefInt with long = "sweeps"
    , description = concat "The number of independent runs of the base algorithm. Default: " (int2string default)
    , category = catRun
    } in
  optOr opt (optPure default)
let sweeps = _sweeps 1

let _seed : Option Int -> OptParser (Option Int) = lam default.
  let opt = optArg
    { optArgDefInt with long = "seed"
    , description = concat "The starting seed to use. Default: " (optionMapOr "random" int2string default)
    , category = catRun
    } in
  optOr (optMap (lam x. Some x) opt) (optPure default)
let seed = _seed (None ())

let _inferTimeMs : Bool -> OptParser Bool = lam default.
  optMap (xor default) (optFlag
  { optFlagDef with long = if default then "no-infer-time" else "infer-time"
  , description = concat (if default then "Disable" else "Enable") " printing inference time to standard error (ms)."
  , category = catDebug
  })
let inferTimeMs = _inferTimeMs false
