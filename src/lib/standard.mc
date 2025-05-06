-- Supporting MCore code for the TreePPL standard library (which can
-- be found in `standard.tppl`)

include "common.mc"
include "string.mc"
include "ext/mat-ext.mc"
include "seq.mc"

let _iterateni = lam bound. lam f.
  recursive let work = lam i. lam acc.
    if lti i bound
    then work (addi i 1) (f i acc)
    else acc
  in work 0

let printStd = print
let printErr = printError
let crash = error

let seqSnoc = snoc
let seqCons = cons
let seqCreate = create
let seqConcat = concat
let seqLength = length
let seqMap = map
let seqMapi = mapi
let seqZipWith = zipWith
let seqSubsequence = subsequence
let seqFoldl = foldl
let seqFoldli = foldli
let seqAny = any

let float2string = float2string
let const_int2string = int2string

let mathExp = exp
let mathLog = log
let mathSqrt = sqrt
let mathModi = modi

-- NOTE(vipa, 2025-05-05): Some of the functions below use the
-- internal mutability of matrices for their workings. This ok,
-- because these mutations are never observable; we mutate matrices
-- that are created here, and then never mutate them after returning.
let matNumRows : all x. Mat x -> Int = lam mtx. mtx.m
let matNumCols : all x. Mat x -> Int = lam mtx. mtx.n
let matNormalize = lam mtx.
  let sum = _iterateni (muli mtx.m mtx.n) (lam i. lam acc. addf acc (extArrGetExn mtx.arr i)) 0.0 in
  let mtx = matCopy mtx in
  repeati (lam i. extArrSetExn mtx.arr i (divf (extArrGetExn mtx.arr i) sum)) (muli mtx.m mtx.n);
  mtx
let matGetRow : all a. Int -> Mat a -> Mat a = lam row. lam mtx.
  let new = matMakeUninit (externalExtArrKind mtx.arr) 1 mtx.n in
  let r = subi row 1 in
  -- OPT(vipa, 2025-03-07): Working with individual cells is likely
  -- inefficient
  repeati (lam i. matSetExn new 0 i (matGetExn mtx r i)) mtx.n;
  new
let matElemPow = lam mtx. lam f.
  let mtx = matCopy mtx in
  -- OPT(vipa, 2025-03-07): Working with individual cells is likely
  -- inefficient
  repeati (lam i. extArrSetExn mtx.arr i (pow (extArrGetExn mtx.arr i) f)) (muli mtx.m mtx.n);
  mtx
let matMean = lam t.
  -- OPT(vipa, 2025-03-07): Working with individual cells is likely
  -- inefficient
  let sum = _iterateni (muli t.m t.n) (lam i. lam acc. addf acc (extArrGetExn t.arr i)) 0.0 in
  divf sum (int2float (muli t.m t.n))
let matApplyToSeq : all a. all b. Mat a -> (a -> b) -> [b] = lam x. lam f.
  create (muli x.m x.n) (lam i. f (extArrGetExn x.arr i))
-- TODO(mariana/vipa, 2023-10-09): the idea is to have mtxRowCols, mtxRowsCol, and mtxRowsCols
-- if we get the appropriate form of overloading we could make indexing (a[idxs])
-- call the correct one of those later on
let matRowCols = lam matrix. lam row. lam cols.
  let r = subi row 1 in
  let new = matMakeUninit (externalExtArrKind matrix.arr) 1 (length cols) in
  -- OPT(vipa, 2025-03-07): Working with individual cells is likely
  -- inefficient
  iteri (lam i. lam c. matSetExn new 0 i (matGetExn matrix r (subi c 1))) cols;
  new
