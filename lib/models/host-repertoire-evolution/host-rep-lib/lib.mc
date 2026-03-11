include "seq.mc"
include "common.mc"
include "float.mc"
include "math.mc"
include "ext/arr-ext.mc"
include "ext/mat-ext.mc"

let mathIsNaN = isNaN

let seqNest : all a. [a] -> Int -> Int -> [[a]] = lam l. lam r. lam c.
  let row = lam i.
    let rs = muli i c in
    let re = addi rs c in
    subsequence l rs c in
  map row (range 0 r 1)

let seqKroneckerDelta : Int -> Int -> [Float] = lam i. lam n.
  let k = subi i 1 in
  let delta = lam x. if eqi k x then 1.0 else 0.0 in
  map delta (range 0 n 1)

mexpr
let _range = lam n. range 1 (addi n 1) 1 in

-- Test the seqNest function
utest seqNest (_range 6) 2 3 with [[1, 2, 3], [4, 5, 6]] in
utest seqNest (_range 8) 2 4 with [[1, 2, 3, 4], [5, 6, 7, 8]] in
utest seqNest (_range 6) 3 2 with [[1, 2], [3, 4], [5, 6]] in
utest seqNest (_range 12) 4 3 with [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12]] in

-- Test the seqKroneckerDelta function
utest seqKroneckerDelta 1 1 with [1.] in 
utest seqKroneckerDelta 1 3 with [1., 0., 0.] in 
utest seqKroneckerDelta 2 3 with [0., 1., 0.] in 
utest seqKroneckerDelta 3 5 with [0., 0., 1., 0., 0.] in 
()
