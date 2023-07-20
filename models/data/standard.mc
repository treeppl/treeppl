-- Exposes MCore features to TreePPL as part of a standard library
-- Provides some example inputs

include "common.mc"

-- Intrinsics have to be exposed as top level let bindings

let concat = concat
let print = print

-- Example inputs ("global constants")

let p = 0.61804697157 -- inverse golden ratio