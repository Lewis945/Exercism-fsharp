module CollatzConjecture

let isEven n = (n % 2) = 0
let next = function
    | n when isEven n -> n / 2
    | n -> 3 * n + 1

let steps (number: int): int option = 
    let rec evaluate acc = 
        function
        | 1 -> Some acc
        | n when n < 1 -> None
        | n -> evaluate (acc + 1) (next n)

    evaluate 0 number