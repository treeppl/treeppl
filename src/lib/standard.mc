-- Exposes CorePPL features to TreePPL as part of a standard library
-- Intrinsics have to be exposed as top level let bindings

include "common.mc"
include "string.mc"
include "mexpr/ast.mc"
include "matrix.mc"
include "ext/matrix-ext.mc"
include "iterator.mc"

let muli = muli
let eqi = eqi

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

let length = length

-- switching the order of map to make it more R-like
-- the "etymology" should be understood as 
-- "sequence" apply, even though in R it is something slightly different
let sapply = lam x. lam f.
  map f x

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

---------------
--- Tensors ---
---------------

let int2real = int2float -- we can also use the compiler built-in Real(x)

let dim = tensorShape

let mtxMul = matrixMul

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

-- NOTE(vsenderov, 2023-09-15): Without setting tensorSetExn to a symbol in here,
-- the CFA is not going to work for matrixSet.
let ts = tensorSetExn

-- NOTE(vsenderov, 2023-09-15): for some reason the types need to be declared,
-- otherwise type error.
let mtxSet = lam row:Int. lam col:Int. lam tensor:Tensor[Float]. lam val:Float.
  ts tensor [subi row 1, subi col 1] val


