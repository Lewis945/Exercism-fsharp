module Etl

let transform (scoresWithLetters: Map<int, char list>): Map<char, int> = 
    scoresWithLetters
    |> Map.toSeq
    |> Seq.collect (fun (k,v) -> v |> List.map (fun c -> (c |> System.Char.ToLowerInvariant, k)))
    |> Seq.distinctBy fst
    |> Map.ofSeq
