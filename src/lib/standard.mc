-- Exposes CorePPL features to TreePPL as part of a standard library
-- Intrinsics have to be exposed as top level let bindings

include "common.mc"
include "string.mc"
include "mexpr/ast.mc"
include "matrix.mc"
include "ext/matrix-ext.mc"
include "iterator.mc"
include "ext/dist-ext.mc"
include "seq.mc"

let error = error

let muli = muli
let eqi = eqi
let neqi = neqi
let addi = addi
let subi = subi

let slice = lam seq. lam beg. lam mend.
    subsequence seq (subi beg 1) (subi mend beg)
    
let geqi = geqi
let gti = gti
let addi = addi
let subi = subi
let subf = subf

----------------------------
--- Printing and strings ---
----------------------------

let concat = concat

let paste0 = join

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

let real2string = float2string

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

let zipWith = zipWith

let fold = lam x.
  foldl x

let qSort = lam f. lam seq.
  quickSort f seq

let any = any

-- switching the order of map to make it more R-like
-- the "etymology" should be understood as 
-- "sequence" apply, even though in R it is something slightly different
-- sapply == for sequences, tapply == for tensors
let sapply = lam x. lam f.
  map f x

let tapply = lam x. lam f.
  reverse (tensorFold (lam acc. lam c. cons (f c) acc) [] x)

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
let rep = make

-- Sequence normalization
let normalizeRealSeq = lam seq.
  let sum = foldl addf 0. seq in
  map (lam f. divf f sum) seq

-- Find elements of a sequence that are true
let whichTrue = lam elems. 
  foldli (lam acc. lam i. lam x. if x then snoc acc (addi i 1) else acc) [] elems

-- Test cases
utest whichTrue [true, false, true, true, false] with [1, 3, 4]
utest whichTrue [false, false, false] with []
utest whichTrue [] with []

-- Sum all elements of a sequence
let sumReal = lam seq.
  foldl (lam acc. lam x. addf acc x) 0.0 seq


---------------
--- Tensors ---
---------------

let int2real = int2float -- we can also use the compiler built-in Real(x)

let dim = tensorShape

let mtxMul = matrixMul

let mtxSclrMul = matrixMulFloat

let mtxAdd = matrixElemAdd

let mtxElemMul = matrixElemMul

let mtxTrans = matrixTranspose

let mtxExp = matrixExponential

-- we cannot change the function parametrization and keep the same name
-- will bring confusion
let mtxCreate = lam row. lam col. lam seq.
  matrixCreate [row, col] seq

let mtxCreateId = lam dim.
  let isDiagonal = lam idx. eqi (divi idx dim) (modi idx dim) in
  let seq = map (lam idx. if isDiagonal idx then 1.0 else 0.0) (iteratorToSeq (iteratorRange 0 (subi (muli dim dim) 1))) in
  mtxCreate dim dim seq

utest tensorToSeqExn (tensorSliceExn (mtxCreateId 2) [0]) with [1., 0.] using (eqSeq eqf)
utest tensorToSeqExn (tensorSliceExn (mtxCreateId 2) [1]) with [0., 1.] using (eqSeq eqf)

-- matrix exponentiation
recursive
  let mtxPow = lam mtx: Tensor[Float]. lam pow: Int.
    if eqi pow 0 then
      mtxCreateId (tensorRank mtx) -- Assuming tensorRank gives the dimension of the matrix
    else if eqi pow 1 then
      mtx
    else if eqi (modi pow 2) 0 then
      let halfPow = mtxPow mtx (divi pow 2) in
      matrixMul halfPow halfPow
    else
      matrixMul mtx (mtxPow mtx (subi pow 1))
end

utest tensorToSeqExn (tensorSliceExn (mtxPow (mtxCreateId 3) 3 ) [0]) with [1., 0., 0.] using (eqSeq eqf)

-- Define the matrix
let __test_43FS35GF: Tensor[Float] = mtxCreate 3 3 [
  1., 2., 3.,
  4., 5., 6.,
  7., 8., 9.
]

-- Test for exponent 2
utest tensorToSeqExn (tensorSliceExn (mtxPow __test_43FS35GF 2) [0]) with [30., 36., 42.] using (eqSeq eqf)
utest tensorToSeqExn (tensorSliceExn (mtxPow __test_43FS35GF 2) [1]) with [66., 81., 96.] using (eqSeq eqf)
utest tensorToSeqExn (tensorSliceExn (mtxPow __test_43FS35GF 2) [2]) with [102., 126., 150.] using (eqSeq eqf)

-- Test for exponent 3
utest tensorToSeqExn (tensorSliceExn (mtxPow __test_43FS35GF 3) [0]) with [468., 576., 684.] using (eqSeq eqf)
utest tensorToSeqExn (tensorSliceExn (mtxPow __test_43FS35GF 3) [1]) with [1062., 1305., 1548.] using (eqSeq eqf)
utest tensorToSeqExn (tensorSliceExn (mtxPow __test_43FS35GF 3) [2]) with [1656., 2034., 2412.] using (eqSeq eqf)

-- Test for exponent 4
utest tensorToSeqExn (tensorSliceExn (mtxPow __test_43FS35GF 4) [0]) with [7560., 9288., 11016.] using (eqSeq eqf)
utest tensorToSeqExn (tensorSliceExn (mtxPow __test_43FS35GF 4) [1]) with [17118., 21033., 24948.] using (eqSeq eqf)
utest tensorToSeqExn (tensorSliceExn (mtxPow __test_43FS35GF 4) [2]) with [26676., 32778., 38880.] using (eqSeq eqf)

-- indexing from 1, not from 0!
let mtxGet = lam row. lam col. lam tensor.
  tensorGetExn tensor [subi row 1, subi col 1]

let mtxGetRow = lam row. lam tensor.
  let r = subi row 1 in
  tensorSubExn tensor r 1

let mtxSclrMul = lam scalar. lam tensor.
  externalMatrixMulFloat scalar tensor

let iid = lam f. lam p. lam n.
  let params = make n p in
  map f params

-- Retrieve a row vector with the columns in cols
-- OPT(mariana/vipa, 2023-10-09): the idea is to have mtxRowCols, mtxRowsCol, and mtxRowsCols
-- if we get the appropriate form of overloading we could make indexing (a[idxs]) 
-- call the correct one of those later on
let mtxRowCols = lam matrix. lam row. lam cols.
  tensorSubSeqExn tensorCreateCArrayFloat matrix [[subi row 1], map (lam v. subi v 1) cols]

-- Mean of a tensor (e.g., vectors and matrices)
-- Was commented out by Viktor, used now by Mariana
let tensorMean = lam t.
  divf (tensorFold addf 0. t) (int2float (tensorSize t))

-- NOTE(vsenderov, 23-10-01): Commenting two functions as they should not be
-- used under 0-CFA
-- NOTE(vsenderov, 2023-09-15): Without setting tensorSetExn to a symbol in here,
-- -- the CFA is not going to work for matrixSet.
-- let ts = tensorSetExn

-- -- NOTE(vsenderov, 2023-09-15): for some reason the types need to be declared,
-- -- otherwise type error.
-- let mtxSet = lam row:Int. lam col:Int. lam tensor:Tensor[Float]. lam val:Float.
--   ts tensor [subi row 1, subi col 1] val


----------------
--- Messages ---
----------------

-- NOTE(mariana, 2023-10-05): attempt to use functions Daniel wrote 
-- to handle Messages, which are Tensor[Real][]

-- TODO(vsenderov, 2023-11-02): Unit tests have to be written, but beware of floating-point comparisons!
-- also this needs to be moved to the matrix stuff above
-- Vector normalization
let normalizeVec = lam v.
  let sum = tensorFold addf 0. v in
  tensorCreateCArrayFloat (tensorShape v) (lam i. divf (tensorGetExn v i) sum)

-- Elementwise multiplication of state likelihoods/probabilities
-- TODO(vsenderov, 2023-11-02): This should not be in the Miking file, but rather in TPPL file
let mulMessage = zipWith matrixElemMul

-- Raises each element of a tensor to the power of the float argument
let mtxElemPow = lam tensor. lam f.
  tensorCreateCArrayFloat (tensorShape tensor) (lam i. pow (tensorGetExn tensor i) f)

-- The original function, refactored to use the new mtxElemPow function
-- TODO(vsenderov, 2023-11-02): This should not be in the Miking file, but rather in TPPL file
let messageElementPower = lam m. lam f. 
    map (lam v. mtxElemPow v f) m

