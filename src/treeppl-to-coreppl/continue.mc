include "ext/file-ext.mc"

let printSample : all a. (a -> JsonValue) -> Option WriteChannel -> a -> [String] ->  () = 
  lam serializer. lam optWc. lam sample. lam sampleInfo.
    let printer = match optWc with Some ch then
      lam s. (fileWriteString ch s); (fileWriteFlush ch)
    else print in
    let serSample = getJsonLn (serializer sample) in
    let serSample = match sampleInfo with [] then serSample else strJoin "\t" (snoc sampleInfo serSample) in
    printer serSample

-- Do nothing when initalizing 
let baseInit : () -> () = lam. ()

let mkContinueStateBase : Option WriteChannel -> (Int, Option WriteChannel) =
  lam optWc. (0, optWc)

let continueBase : all a. all b. Int -> Int -> (a -> JsonValue) -> (Int, Option WriteChannel) -> b -> a -> ((Int, Option WriteChannel), Bool) =
  lam iterations. lam samplingPeriod. lam serializer. lam acc. lam sampleInfo. lam sample.
    match acc with (idx, optWc) in
    -- printLn (strJoin " : " ["Base continue", int2string idx, int2string iterations]);
    (if eqi 0 (modi idx samplingPeriod) then printSample serializer optWc sample [] else ()); 
    ((addi idx 1, optWc), neqi idx iterations)
  
let temperatureBase : (Int, Option WriteChannel) -> Float = lam acc. 1.0

-- We assume that Pigeons first issues a `call_sampler!(beta)`
let pigeonsInit : () -> () = lam.
  fileReadLine fileStdin;
  printLn "response()"

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

let mkContinueStatePigeons : Option WriteChannel -> (Int, Float, Option WriteChannel) =
  lam optWc. (0, 1.0, optWc)

let continuePigeons : all a. Int -> Int -> (a -> JsonValue) -> (Int, Float, Option WriteChannel) -> (Float, Float) -> a -> ((Int, Float, Option WriteChannel), Bool) =
  lam exploreSteps. lam samplingPeriod. lam serializer. lam acc. lam sampleInfo. lam sample.
    match acc with (idx, beta, optWc) in
    match sampleInfo with (weight, priorWeight) in
    -- printLn (strJoin " : " ["Pigeons continue", int2string idx, float2string beta, float2string weight, float2string priorWeight]);
    (if and (eqi 0 (modi idx samplingPeriod)) (eqf beta 1.0) then 
      printSample serializer optWc sample [int2string idx, float2string beta]
    else ()); 
    let optBeta = if (eqi 0 (modi idx exploreSteps)) then 
      listenPigeons weight priorWeight
    else Some beta in
    match optBeta with Some beta then 
        ((addi idx 1, beta, optWc), true) 
    else 
        ((addi idx 1, beta, optWc), false)

let temperaturePigeons : (Int, Float, Option WriteChannel) -> Float =
  lam acc. match acc with (_, beta, _) in beta

