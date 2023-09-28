-- Exposes CorePPL features to TreePPL as part of a standard library
-- Intrinsics have to be exposed as top level let bindings

include "common.mc"
include "string.mc"
include "mexpr/ast.mc"
include "matrix.mc"
include "ext/matrix-ext.mc"
include "iterator.mc"

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

let matrixCreate = lam row. lam col. lam seq.
  matrixCreate [row, col] seq

let matrixCreateId = lam dim.
  let isDiagonal = lam idx. eqi (divi idx dim) (modi idx dim) in
  let seq = map (lam idx. if isDiagonal idx then 1.0 else 0.0) (iteratorToSeq (iteratorRange 0 (subi (muli dim dim) 1))) in
  printLn (join (map float2string seq));
  matrixCreate dim dim seq

utest tensorToSeqExn (tensorSliceExn (matrixCreateId 2) [0]) with [1., 0.] using (eqSeq eqf)
utest tensorToSeqExn (tensorSliceExn (matrixCreateId 2) [1]) with [0., 1.] using (eqSeq eqf)

-- matrix exponentiation
-- recursive
--   let matrixPow = lam mtx: Tensor[Float]. lam pow: Int.
--     if eqi pow 0 then
--       matrixCreateId (tensorRank mtx) -- Assuming tensorRank gives the dimension of the matrix
--     else if eqi pow 1 then
--       mtx
--     else if eqi (modi pow 2) 0 then
--       let halfPow = matrixPow mtx (divi pow 2) in
--       matrixMul halfPow halfPow
--     else
--       matrixMul mtx (matrixPow mtx (subi pow 1))
-- end

-- utest tensorToSeqExn (tensorSliceExn (matrixPow (matrixCreateId 3) 3 ) [0]) with [1., 0.4343242, 0.] using (eqSeq eqf)

-- indexing from 1, not from 0!
let matrixGet = lam row. lam col. lam tensor.
  tensorGetExn tensor [subi row 1, subi col 1]

-- NOTE(vsenderov, 2023-09-15): Without setting tensorSetExn to a symbol in here,
-- the CFA is not going to work for matrixSet.
let ts = tensorSetExn

-- NOTE(vsenderov, 2023-09-15): for some reason the types need to be declared,
-- otherwise type error.
let matrixSet = lam row:Int. lam col:Int. lam tensor:Tensor[Float]. lam val:Float.
  ts tensor [subi row 1, subi col 1] val


