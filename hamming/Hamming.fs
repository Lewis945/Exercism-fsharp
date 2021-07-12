module Hamming

let distance (strand1: string) (strand2: string): int option = 
    match strand1.Length = strand2.Length with
    | false -> None
    | true -> Seq.zip strand1 strand2
                |> Seq.filter (fun (s1, s2) -> s1 <> s2)
                |> Seq.length
                |> Some