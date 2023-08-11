-- Serialization/ deserialization

let serializeResult: all a. (a -> JsonValue) -> Dist(a) -> JsonValue =
    lam sampleSerializer. lam resultDist.
      match distEmpiricalSamples resultDist with (samples, weights) in
      match distEmpiricalNormConst resultDist with nc in -- nc is NaN when inference method does not produce it
      let samples: [a] = samples in
      let weights: [Float] = weights in
      let nc: Float = nc in
      JsonObject
        (mapInsert "normConst" (jsonSerializeFloat nc)
        (mapInsert "samples" (JsonArray (map sampleSerializer samples))
        (mapInsert "weights" (JsonArray (map jsonSerializeFloat weights))
        (mapEmpty cmpString))))