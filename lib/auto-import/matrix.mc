include "ext/mat-ext.mc"

let _iterateni : all a. Int -> (Int -> a -> a) -> a -> a = lam bound. lam f.
  recursive let work = lam i. lam acc.
    if lti i bound
    then work (addi i 1) (f i acc)
    else acc
  in work 0

-- NOTE(vipa, 2025-05-05): Some of the functions below use the
-- internal mutability of matrices for their workings. This ok,
-- because these mutations are never observable; we mutate matrices
-- that are created here, and then never mutate them after returning.
let matNumRows : all x. Mat x -> Int = lam mtx. mtx.m
let matNumCols : all x. Mat x -> Int = lam mtx. mtx.n
let matNormalize = lam mtx.
  let sum = _iterateni (muli mtx.m mtx.n) (lam i. lam acc. addf acc (extArrGetExn mtx.arr i)) 0.0 in
  tmOpaque (
    let mtx = matCopy mtx in
    let f = lam i. extArrSetExn mtx.arr i (divf (extArrGetExn mtx.arr i) sum) in
    recursive let repeati : Int -> () = lam i.
      if geqi i 0
      then f i; repeati (subi i 1)
      else () in
    repeati (subi (muli mtx.m mtx.n) 1);
    mtx
  )
let matGetRow : all a. Int -> Mat a -> Mat a = lam row. lam mtx.
  tmOpaque (
    let new = matMakeUninit (externalExtArrKind mtx.arr) 1 mtx.n in
    let r = subi row 1 in
    -- OPT(vipa, 2025-03-07): Working with individual cells is likely
    -- inefficient
    let f = lam i. matSetExn new 0 i (matGetExn mtx r i) in
    recursive let repeati : Int -> () = lam i.
      if geqi i 0
      then f i; repeati (subi i 1)
      else () in
    repeati (subi mtx.n 1);
    new
  )
let matElemPow = lam mtx. lam f.
  tmOpaque (
    let mtx = matCopy mtx in
    -- OPT(vipa, 2025-03-07): Working with individual cells is likely
    -- inefficient
    let f = lam i. extArrSetExn mtx.arr i (pow (extArrGetExn mtx.arr i) f) in
    recursive let repeati : Int -> () = lam i.
      if geqi i 0
      then f i; repeati (subi i 1)
      else () in
    repeati (subi (muli mtx.m mtx.n) 1);
    mtx
  )
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
  tmOpaque (
    let r = subi row 1 in
    let new = matMakeUninit (externalExtArrKind matrix.arr) 1 (length cols) in
    -- OPT(vipa, 2025-03-07): Working with individual cells is likely
    -- inefficient
    let f = lam i. lam c. matSetExn new 0 i (matGetExn matrix r (subi c 1)) in
    let iteri = lam seq.
      recursive let work = lam i. lam seq.
        match seq with [s] ++ seq
        then f i s; work (addi i 1) seq
        else () in
      work 0 seq in
    iteri cols;
    new
  )

let _mtxCreate = lam rows. lam cols. lam data.
  matFromArrExn rows cols (tmOpaque (extArrOfSeq extArrKindFloat64 data))

let _mtxCreateId = lam sideLength.
  tmOpaque (
    let mtx = matMake extArrKindFloat64 sideLength sideLength 0.0 in
    let f = lam i. matSetExn mtx i i 1.0 in
    recursive let repeati : Int -> () = lam i.
      if geqi i 0
      then f i; repeati (subi i 1)
      else () in
    repeati (subi sideLength 1);
    mtx
  )

let _rvecCreate = lam cols. lam seq.
  matFromArrExn 1 cols (tmOpaque (extArrOfSeq extArrKindFloat64 seq))

let _cvecCreate = lam rows. lam seq.
  matFromArrExn rows 1 (tmOpaque (extArrOfSeq extArrKindFloat64 seq))
