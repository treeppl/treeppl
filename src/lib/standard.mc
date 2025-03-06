-- Exposes CorePPL features to TreePPL as part of a standard library
-- Intrinsics have to be exposed as top level let bindings

include "common.mc"
include "string.mc"
include "mexpr/ast.mc"
include "ext/mat-ext.mc"
include "iterator.mc"
include "ext/dist-ext.mc"
include "seq.mc"

let error = lam x. error x

let subf = lam x. subf x
let muli = lam x. muli x
let addi = lam x. addi x
let subi = lam x. subi x
let eqi = lam x. eqi x
let neqi = lam x. neqi x
let geqi = lam x. geqi x
let gti = lam x. gti x

let log = lam x. log x
let exp = lam x. exp x
let sqrt = lam x. sqrt x

let slice = lam seq. lam beg. lam mend.
    subsequence seq (subi beg 1) (subi mend beg)


----------------------------
--- Printing and strings ---
----------------------------

let concat = lam x. concat x

let paste0 = lam x. join x

let paste = lam seq. lam sep.
  strJoin sep seq

utest paste0 ["a", "b", "c"] with "abc" using eqString
utest paste ["a", "b", "c"] " " with "a b c" using eqString
utest paste ["a", "b", "c"] ", " with "a, b, c" using eqString

-- In TreePPL "printing" is only for debugging purposes
let print = lam s.
  printError s;
  flushStderr ()

let printLn = lam s.
  printError (join [s, "\n"]);
  flushStderr ()

let int2string = lam x. int2string x

let real2string = lam x. float2string x

let bool2string: Bool -> String  = lam b.
  if b then
    "True"
  else
    "False"

-----------------
--- Sequences ---
-----------------

let length = lam x.
  length x

let zipWith = lam x. zipWith x

let fold = lam x.
  foldl x

let qSort = lam f. lam seq.
  quickSort f seq

let any = any

let zipWith = zipWith

-- sapply1 for passing 1 argument (a) to function f
let sapply1 = lam x. lam f. lam a.
  map (lam e. f e a) x

-- switching the order of map to make it more R-like
-- the "etymology" should be understood as
-- "sequence" apply, even though in R it is something slightly different
-- sapply == for sequences, tapply == for tensors
let sapply = lam x. lam f.
  map f x

let tapply = lam x. lam f.
  create (muli x.m x.n) (lam i. f (extArrGetExn x.arr i))

-- sapply1 for passing 1 argument (a) to function f
let sapply1 = lam x. lam f. lam a.
  map (lam e. f e a) x

-- sapplyi1 is a mapping that additionally passes the current index and one argument a
let sapplyi1 = lam x. lam f. lam a.
  mapi (lam i. lam e. f (addi i 1) e a) x

-- sapplyi2 is a mapping that additionally passes the current index and two arguments (a and b)
let sapplyi2 = lam x. lam f. lam a. lam b.
  mapi (lam i. lam e. f (addi i 1) e a b) x

-- convert an integer sequences to a real sequence
let sint2real = lam seq.
  sapply seq int2float -- using the Miking function as tppl equiv only below

-- convert a real sequence to a string (useful for printing)
let sreal2string = lam seq.
  sapply seq real2string

-- convert a int sequence to a string (useful for printing)
let sint2string = lam seq.
  sapply seq int2string

-- convert a bool sequence to a string (useful for printing)
let sbool2string = lam seq.
  sapply seq bool2string

-- remap make to rep to make it more R-like
let rep = lam x. make x

-- WebPPL inspired
let repApply = lam x. create x

-- Sequence normalization
let seqNormalize = lam seq.
  let sum = foldl addf 0. seq in
  map (lam f. divf f sum) seq

utest seqNormalize [1.0, 1.0] with [0.5, 0.5] using (eqSeq eqf)

-- Find elements of a sequence that are true
let whichTrue = lam elems.
  foldli (lam acc. lam i. lam x. if x then snoc acc (addi i 1) else acc) [] elems

-- Test cases
utest whichTrue [true, false, true, true, false] with [1, 3, 4]
utest whichTrue [false, false, false] with []
utest whichTrue [] with []

-- Sum all elements of a sequence
let seqSumReal = lam seq.
  foldl (lam acc. lam x. addf acc x) 0.0 seq

utest seqSumReal [1., 2., 3., 4., 5.] with divf (mulf 5. 6.) 2. using eqf

-- Sum all elements of a sequence (int)
let seqSumInt = lam seq.
  foldl (lam acc. lam x. addi acc x) 0 seq

utest seqSumInt [1, 2, 3, 4, 5] with 15 using eqi


----------------
--- Matrices ---
----------------

type Matrix a = Mat a

let int2real = lam x. int2float x -- we can also use the compiler built-in Real(x)

let dim = lam mtx. [mtx.m, mtx.n]

let mtxMul = matMulExn

let mtxSclrMul = matScale

let mtxAdd = matAddExn

let mtxElemMul = matElemMulExn

let mtxTrans = matTranspose

let mtxExp = matExpExn

let mtxGet = lam row. lam col. lam mtx.
  matGetExn mtx (subi row 1) (subi col 1)

let mtxGetRow : all a. Int -> Matrix a -> Matrix a = lam row. lam mtx.
  let new = matMakeUninit (externalExtArrKind mtx.arr) 1 mtx.n in
  let r = subi row 1 in
  -- OPT(vipa, 2025-03-07): Working with individual cells is likely
  -- inefficient
  repeati (lam i. matSetExn new 0 i (matGetExn mtx r i)) mtx.n;
  new

-- we cannot change the function parametrization and keep the same name
-- will bring confusion
let mtxCreate = lam row. lam col. lam seq.
  matFromArrExn row col (extArrOfSeq extArrKindFloat64 seq)

let mtxCreateId = lam dim.
  let mat = matMake extArrKindFloat64 dim dim 0.0 in
  repeati (lam i. matSetExn mat i i 1.0) dim;
  mat

utest extArrToSeq (mtxCreateId 2).arr with [1., 0., 0., 1.] using (eqSeq eqf)
utest extArrToSeq (mtxCreateId 3).arr with [1., 0., 0., 0., 1., 0., 0., 0., 1.] using (eqSeq eqf)

let rvecCreate = lam cols. lam seq.
  matFromArrExn 1 cols (extArrOfSeq extArrKindFloat64 seq)
let cvecCreate = lam rows. lam seq.
  matFromArrExn rows 1 (extArrOfSeq extArrKindFloat64 seq)


-- matrix exponentiation
recursive let mtxPow = lam mtx: Matrix Float. lam pow: Int.
  if not (matIsSquare mtx) then
    error "Matrix must be square"
  else if eqi pow 0 then
    mtxCreateId mtx.m -- Assuming a squareMatrix
  else if eqi pow 1 then
    mtx
  else if eqi (modi pow 2) 0 then
    let halfPow = mtxPow mtx (divi pow 2) in
    mtxMul halfPow halfPow
  else
    mtxMul mtx (mtxPow mtx (subi pow 1))
end

utest extArrToSeq (mtxPow (mtxCreateId 3) 3).arr with [1., 0., 0., 0., 1., 0., 0., 0., 1.] using (eqSeq eqf)

-- Define the matrix
let __test_43FS35GF: Matrix Float = mtxCreate 3 3 [
  1., 2., 3.,
  4., 5., 6.,
  7., 8., 9.
]


-- Test for exponent 0
utest extArrToSeq (mtxPow __test_43FS35GF 0).arr with [1., 0., 0., 0., 1., 0., 0., 0., 1.] using (eqSeq eqf)

-- Test for exponent 1
utest extArrToSeq (mtxPow __test_43FS35GF 1).arr with [1., 2., 3., 4., 5., 6., 7., 8., 9.] using (eqSeq eqf)

-- Test for exponent 2
utest extArrToSeq (mtxPow __test_43FS35GF 2).arr with [30., 36., 42., 66., 81., 96., 102., 126., 150.] using (eqSeq eqf)

-- Test for exponent 3
utest extArrToSeq (mtxPow __test_43FS35GF 3).arr with [468., 576., 684., 1062., 1305., 1548., 1656., 2034., 2412.] using (eqSeq eqf)

-- Test for exponent 4
utest extArrToSeq (mtxPow __test_43FS35GF 4).arr with [7560., 9288., 11016., 17118., 21033., 24948., 26676., 32778., 38880.] using (eqSeq eqf)

-- indexing from 1, not from 0!

let mtxSclrMul = lam scalar. lam tensor.
  matScale scalar tensor

let iid = lam f. lam p. lam n.
  let params = make n p in
  map f params

-- Retrieve a row vector with the columns in cols
-- TODO(mariana/vipa, 2023-10-09): the idea is to have mtxRowCols, mtxRowsCol, and mtxRowsCols
-- if we get the appropriate form of overloading we could make indexing (a[idxs])
-- call the correct one of those later on
let mtxRowCols = lam matrix. lam row. lam cols.
  let r = subi row 1 in
  let new = matMakeUninit (externalExtArrKind matrix.arr) 1 (length cols) in
  -- OPT(vipa, 2025-03-07): Working with individual cells is likely
  -- inefficient
  iteri (lam i. lam c. matSetExn new 0 i (matGetExn matrix r (subi c 1))) cols;
  new

let _iterateni = lam bound. lam f.
  recursive let work = lam i. lam acc.
    if lti i bound
    then work (addi i 1) (f i acc)
    else acc
  in work 0

-- Mean of a tensor (e.g., vectors and matrices)
-- Was commented out by Viktor, used now by Mariana
let mtxMean = lam t.
  -- OPT(vipa, 2025-03-07): Working with individual cells is likely
  -- inefficient
  let sum = _iterateni (muli t.m t.n) (lam i. lam acc. addf acc (extArrGetExn t.arr i)) 0.0 in
  divf sum (int2float (muli t.m t.n))

let __test_tesnor1: Matrix Float = mtxCreate 3 3 [
    1., 1., 1.,
    2., 2., 2.,
    3., 3., 3.
  ]

utest mtxMean __test_tesnor1 with 2. using eqf

let mtxNormalize = lam v.
  let sum = _iterateni (muli v.m v.n) (lam i. lam acc. addf acc (extArrGetExn v.arr i)) 0.0 in
  let v = matCopy v in
  repeati (lam i. extArrSetExn v.arr i (divf (extArrGetExn v.arr i) sum)) (muli v.m v.n);
  v

let mtxElemPow = lam mtx. lam f.
  let mtx = matCopy mtx in
  repeati (lam i. extArrSetExn mtx.arr i (pow (extArrGetExn mtx.arr i) f)) (muli mtx.m mtx.n);
  mtx

----------------
--- Messages ---
----------------

-- NOTE(mariana, 2023-10-05): attempt to use functions Daniel wrote
-- to handle Messages, which are Tensor[Real][]

-- Message normalization
let messageNormalize = lam m.
  map mtxNormalize m

-- Elementwise multiplication of state likelihoods/probabilities
let messageElemMul = zipWith mtxElemMul

-- Raises each element to the power of the float argument
let messageElemPow = lam m. lam f.
    map (lam v. mtxElemPow v f) m
