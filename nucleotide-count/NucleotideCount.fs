module NucleotideCount

let validate strand = 
    let nucleotides = System.Text.RegularExpressions.Regex("^[ACGT]*$")    
    match (nucleotides.IsMatch strand) with
    | false -> None
    | true -> Some strand

let fillGapsWithZeros strand =
    let defaultMap = [ 'A', 0; 'C', 0; 'G', 0; 'T', 0 ] |> Map.ofSeq
    Map.fold (fun acc key value -> Map.add key value acc) defaultMap strand

let (|>>) v f = Option.map f v

let homeRolledBind fn result =
    match result with
    | Some s -> fn s
    | None -> None

let nucleotideCounts (strand: string): Option<Map<char, int>> = 
    strand
    |> validate
    |>> (Seq.countBy id >> Map.ofSeq)
    |>> fillGapsWithZeros
