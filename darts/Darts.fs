module Darts

let score (x: double) (y: double): int = 
    pown x 2 + pown y 2
    |> sqrt
    |> function
       | d when d > 10. -> 0
       | d when d > 5. -> 1
       | d when d > 1. -> 5
       | _ -> 10