module Sieve

let primes limit =
    [ 2 .. limit |> float |> sqrt |> int ]
    |> Seq.fold (fun primes i -> primes - (set [ i * i .. i .. limit ])) (set [ 2 .. limit ])
    |> List.ofSeq
