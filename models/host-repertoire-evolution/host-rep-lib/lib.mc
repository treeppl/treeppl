include "seq.mc"
include "common.mc"
include "float.mc"
include "math.mc"

let cons = cons 
let absf = absf
let mulf = mulf
let isNaN = isNaN
let maxf = maxf

let nestList : all a. [Int] -> Int -> Int -> [[Int]] = lam l. lam r. lam c.
  let row = lam i.
    let rs = addi (muli i c) 1 in
    let re = addi (muli (addi i 1) c) 1 in
    slice l rs re in
  map row (range 0 r 1)

let seqKroneckerDelta : Int -> Int -> [Float] = lam i. lam n.
  let k = subi i 1 in
  let delta = compose bool2real (eqi k) in
  map delta (range 0 n 1)

-- This creates a row vector with a single one
let mtxRowKroneckerDelta : Int -> Int -> Mat Float = lam i. lam n. 
  mtxCreate 1 n (seqKroneckerDelta i n)

-- let mtxColKroneckerDelta : Int -> Int -> Mat Float = lam i. lam n. 
--   mtxCreate n 1 (seqKroneckerDelta i n)
