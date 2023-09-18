-- Exposes CorePPL features to TreePPL as part of a standard library
-- Provides some example inputs

include "common.mc"
include "string.mc"
include "mexpr/ast.mc"
include "matrix.mc"
include "iterator.mc"

-- Intrinsics have to be exposed as top level let bindings

let concat = concat

-- In TreePPL "printing" is only for debugging purposes 
let print = lam s.
  printError s;
  flushStderr ()

let printLn = lam s.
  printError (join [s, "\n"]);
  flushStderr ()

let length = length
let real2string = float2string

let bool2string: Bool -> String  = lam b.
  if b then
    "True"
  else
    "False"

let apply = map

-- Some example constants
-- a boolean
let trump = false

-- just flipping some coins
let coinflips = [true, true, true, false, true, false, false, true, true, false, false, false, true, false, true, false, false, true, false, false]

-- a given integer
let n = 7

-- a negative integer
let m = (negi 1)

-- zero
let z = 0

-- inverse golden ratio
let p = 0.618

-- hundred digits of pi
let pi = 3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117068

let x = 1.0

let text = "blab\n"

---------------
--- Tensors ---
---------------

let matrixCreate = lam row. lam col. lam seq.
  matrixCreate [row, col] seq

let matrixCreateId = lam dim.
  let isDiagonal = lam idx. eqi (modi idx dim) 0 in
  let seq = map (lam idx. if isDiagonal idx then 1.0 else 0.0) (iteratorToSeq (iteratorRange 0 (subi (muli dim dim) 1))) in
  printLn (join (map float2string seq));
  matrixCreate dim dim seq

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


