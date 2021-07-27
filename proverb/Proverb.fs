module Proverb

let recite1 (input: string list): string list = 
    let rec recite' acc items =
        match items with
        | [] -> acc []
        | _::[] -> recite' (acc >> (fun a -> $"And all for the want of a {input.Head}." :: a)) []
        | x::y::ws -> recite' (acc >> (fun a -> $"For want of a {x} the {y} was lost." :: a)) (y::ws)

    input
    |> recite' id
    |> List.rev
