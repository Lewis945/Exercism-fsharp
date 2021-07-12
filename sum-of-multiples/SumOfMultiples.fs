module SumOfMultiples

let sum (numbers: int list) (upperBound: int): int = 
    let numbers' = seq { 1 .. upperBound - 1 }
    let divisiors = numbers |> Seq.filter (fun d -> d > 0)
    Seq.allPairs numbers' divisiors
    |> Seq.filter (fun (n, d) -> n % d = 0)
    |> Seq.map (fun (n, _) -> n)
    |> Seq.distinct
    |> Seq.sum