module Grains

let private squares = 64
let private pow2n n = pown 2UL (n-1)

let square (n: int): Result<uint64,string> = 
    match n > 0 && n <= squares with
    | false -> Error "square must be between 1 and 64"
    | true -> Ok (pow2n n)

let total: Result<uint64,string> = 
    [1 .. squares]
    |> List.map pow2n
    |> List.sum
    |> (fun x -> Ok (uint64 x))