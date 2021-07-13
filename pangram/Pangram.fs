module Pangram

let isPangram (input: string): bool = 
    input.ToLowerInvariant()
    |> Seq.filter System.Char.IsLetter
    |> Seq.distinct
    |> Seq.length = 26