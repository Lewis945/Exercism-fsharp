module ArmstrongNumbers

let toDigits number = 
    let cons x xs = x :: xs
    let rec split number divisor acc = 
        let digit = (number / divisor) % 10
        match digit with
        | 0 -> acc []
        | d -> split number (divisor * 10) (acc >> cons d)
    split number 1 id

let isArmstrongNumber (number: int): bool = 
    let digits = number |> toDigits
    let power = digits |> List.length
    
    digits
    |> List.sumBy (fun d -> pown d power)
    |> (=) number
