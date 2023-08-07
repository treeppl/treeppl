-- Exposes MCore features to TreePPL as part of a standard library
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
