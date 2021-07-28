module Seq

let private filter pred xs =
    let rec filter' acc =
        function
        | [] -> acc []
        | h :: t ->
            filter'
                (acc
                 >> (fun a -> if pred h then h :: a else a)) t
    xs 
    |> List.ofSeq
    |> filter' id
    |> List.rev

let keep pred xs = filter pred xs
let discard pred xs = filter (pred >> not) xs
