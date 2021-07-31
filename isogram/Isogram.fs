module Isogram

let isIsogram str = 
    str
    |> Seq.map System.Char.ToLower
    |> Seq.filter System.Char.IsLetter
    |> Seq.countBy id
    |> Seq.forall (fun (_, c) -> c = 1)