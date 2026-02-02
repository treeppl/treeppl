include "ext/file-ext.mc"
include "sys.mc"
include "coreppl::coreppl-to-mexpr/mcmc-lightweight/config.mc"

-- 
-- Base -- only record the iteration index
--
let accInitBase : () -> Int = lam. 0

let continueBase : all a. all b. Int -> Int -> b -> a -> (Int, Bool) =
  lam iterations. lam acc. lam sampleInfo. lam sample.
    -- printLn (strJoin " : " ["Base continue", int2string idx, int2string iterations]);
    (addi acc 1, neqi acc iterations)

-- 
-- Incremental printing
--
let getJsonLn: JsonValue -> String = lam value.
  let jStr = json2string value in
  concat jStr "\n"

let sample2string : all a. (a -> JsonValue) -> a -> [String] -> String = 
  lam serializer. lam sample. lam sampleInfo.
    let serSample = getJsonLn (serializer sample) in
    let serSample = match sampleInfo with []
    then serSample
    else strJoin "\t" (snoc sampleInfo serSample) in
    serSample

let printFile : all a. String -> WriteChannel -> () = 
  lam sampleStr. lam wc. 
    fileWriteString wc sampleStr;
    fileWriteFlush wc 

let tryOpenSampleFile : () -> Option WriteChannel = lam.
  match sysGetEnv "PPL_OUTPUT" with Some fn then
    match fileWriteOpen fn with Some wc then Some wc
    else error (join ["Failed to open file ", fn])
  else None ()

let accInitIncremental : () -> (Int, Option WriteChannel) = lam.
  -- Check for a file set by PPL_OUTPUT. If none is given then default to printing to stdout
  let optWc = tryOpenSampleFile () in
  (0, optWc)

let continueIncremental : all a. all b. Int -> Int -> (a -> JsonValue) -> (Int, Option WriteChannel) -> b -> a -> ((Int, Option WriteChannel), Bool) =
  lam iterations. lam samplingPeriod. lam serializer. lam acc. lam sampleInfo. lam sample.
    match acc with (idx, optWc) in
    -- printLn (strJoin " : " ["Incremental continue", int2string idx, int2string iterations]);
    (if eqi 0 (modi idx samplingPeriod) then
      let sampleStr = sample2string serializer sample [] in
      let printer = match optWc with Some wc
        then lam s. printFile s wc
        else lam s. print s; flushStdout () in
      printer sampleStr
    else ()); 
    if eqi idx iterations then 
      (match optWc with Some wc then fileWriteClose wc else ());
      ((addi idx 1, optWc), false)
    else
      ((addi idx 1, optWc), true)
  
-- 
-- Pigeons
--
recursive let listenPigeons : Float -> Float -> Option Float = lam weight. lam priorWeight.
  switch fileReadLine fileStdin
  case Some ("log_potential(" ++ beta ++ ")") then
    let beta = string2float beta in
    -- printErrorLn (join ["weight: ", float2string weight, " priorWeight: ", float2string priorWeight, " beta: ", float2string beta]);
    let result = addf priorWeight (mulf weight beta) in
    printLn (join ["response(", float2string result, ")"]);
    listenPigeons weight priorWeight
  case Some ("call_sampler!(" ++ beta ++ ")") then
    printLn "response()"; 
    Some (string2float beta)
  case Some cmd then
    printErrorLn (concat "Unrecognized command, ignoring: " cmd);
    listenPigeons weight priorWeight
  case None () then
    None ()
  end
end

let accInitPigeons : Bool -> () -> (Int, Float, Option WriteChannel) = lam incrementalPrinting. lam.
  -- Initialize Pigeons continue state. We assume that Pigeons first issues a `call_sampler!(beta)`
  fileReadLine fileStdin;
  printLn "response()";
  let optWc = tryOpenSampleFile () in
  -- If we are printing samples from Pigeons then we must ensure that a filename is set
  (if incrementalPrinting then 
    match optWc with None () then error "You must set `PPL_OUTPUT` with a filename to print samples with Pigeons"
    else ()
  else ());
  (0, 1.0, optWc)

let continuePigeons : all a. Int -> Int -> (a -> JsonValue) -> (Int, Float, Option WriteChannel) -> SampleInfo -> a -> ((Int, Float, Option WriteChannel), Bool) =
  lam exploreSteps. lam samplingPeriod. lam serializer. lam acc. lam sampleInfo. lam sample.
    match acc with (idx, beta, optWc) in
    let weight = sampleInfo.weight in
    let priorWeight = sampleInfo.priorWeight in
    -- printLn (strJoin " : " ["Pigeons continue", int2string idx, float2string beta, float2string weight, float2string priorWeight]);
    (if and (eqi 0 (modi idx samplingPeriod)) (eqf beta 1.0) then 
      match optWc with Some wc then
        let sampleStr = sample2string serializer sample [int2string idx] in
        printFile sampleStr wc
      else () 
    else ()); 
    let optBeta = if (eqi 0 (modi idx exploreSteps)) then 
      listenPigeons weight priorWeight
    else Some beta in
    match optBeta with Some beta then 
        ((addi idx 1, beta, optWc), true) 
    else 
        (match optWc with Some wc then fileWriteClose wc else ());
        ((addi idx 1, beta, optWc), false)

let temperaturePigeons : (Int, Float, Option WriteChannel) -> Float =
  lam acc. match acc with (_, beta, _) in beta

let globalProbPigeons : Bool -> Float -> (Int, Float, Option WriteChannel) -> Float =
  lam forceGlobal. lam globalProb. lam acc.
    match acc with (_, beta, _) in
    if and forceGlobal (eqf beta 0.0)
    then 1.0 else globalProb