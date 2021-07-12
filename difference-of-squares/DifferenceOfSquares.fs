module DifferenceOfSquares

let pow2 x = pown x 2  

let squareOfSum (number: int): int = 
    [1..number]
    |> List.sum
    |> pow2

let sumOfSquares (number: int): int = 
    [1..number]
    |> List.sumBy pow2

let differenceOfSquares (number: int): int = 
    squareOfSum number - sumOfSquares number