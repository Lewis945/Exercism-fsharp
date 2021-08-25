module PerfectNumbers

type Classification = Perfect | Abundant | Deficient 

let getFactors n =
    [1..n-1]
    |> List.filter (fun x -> n % x = 0)

let validate n =
    if n <= 0 then None else Some n

let classify n : Classification option = 
    n
    |> validate
    |> Option.map getFactors
    |> Option.map List.sum
    |> Option.map (function
                    | s when s = n -> Perfect
                    | s when s > n -> Abundant
                    | _ -> Deficient)
    
