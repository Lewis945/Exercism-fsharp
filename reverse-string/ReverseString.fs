module ReverseString

let reverse (input: string): string = 
    input
    |> Seq.rev
    |> Seq.map string
    |> Seq.fold (+) ""