-- Exposes CorePPL features to TreePPL as part of a standard library
-- Provides some example inputs

include "common.mc"
include "string.mc"
include "mexpr/ast.mc"

-- Intrinsics have to be exposed as top level let bindings

let concat = concat
let print = print
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

