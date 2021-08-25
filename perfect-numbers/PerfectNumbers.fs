module PerfectNumbers

type Classification =
    | Perfect
    | Abundant
    | Deficient

let validate n =
    if n > 0 then Some n else None

let getAliquotSum n =
    [ 1 .. n / 2 ]
    |> List.filter (fun x -> n % x = 0)
    |> List.sum

let classify n: Classification option =
    n
    |> validate
    |> Option.map getAliquotSum
    |> Option.map (function
        | s when s = n -> Perfect
        | s when s > n -> Abundant
        | _ -> Deficient)
