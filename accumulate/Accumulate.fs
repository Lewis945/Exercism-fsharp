module Accumulate

let mapWithRev transform list =
    let rec mapInternal transform acc list =
        match list with
        | [] -> acc |> List.rev
        | head::tail -> mapInternal transform ((transform head) :: acc) tail
    mapInternal transform [] list

let accumulate (func: 'a -> 'b) (input: 'a list): 'b list =
    mapWithRev func input
