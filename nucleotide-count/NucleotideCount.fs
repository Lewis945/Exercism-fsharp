module NucleotideCount
open System.Text.RegularExpressions

let validate strand = 
    match (Regex("^[ACGT]*$").IsMatch strand) with
    | false -> None
    | true -> Some strand

let count (strand: string option) =
    let count' =
        (fun v -> "ACTG" + v)
        >> Seq.countBy id
        >> Seq.map (fun (k, v) -> (k, v - 1))
        >> Map.ofSeq
    Option.map count' strand

let nucleotideCounts (strand: string): Option<Map<char, int>> = 
    strand
    |> validate
    |> count
