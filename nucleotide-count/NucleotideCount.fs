module NucleotideCount

let validate strand = 
    let nucleotides = System.Text.RegularExpressions.Regex("^[ACGT]*$")    
    match (nucleotides.IsMatch strand) with
    | false -> None
    | true -> Some strand

let fillGapsWithZeros strand =
    let defaultMap = [ 'A', 0; 'C', 0; 'G', 0; 'T', 0 ] |> Map.ofSeq
    Map.fold (fun acc key value -> Map.add key value acc) defaultMap strand

let count =
    Seq.countBy id 
    >> Map.ofSeq
    >> fillGapsWithZeros
    |> Option.map

let nucleotideCounts (strand: string): Option<Map<char, int>> = 
    strand
    |> validate
    |> count
