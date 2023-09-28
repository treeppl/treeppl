include "map.mc"
include "math.mc"
include "seq.mc"
include "matrix.mc"
include "tensor.mc"

mexpr

let x = [1.0, 2.0, 3.0, 4.0, 5.0] in
let y = rvecCreate 5  x in
let z =  rvecToSeqExn y in

()