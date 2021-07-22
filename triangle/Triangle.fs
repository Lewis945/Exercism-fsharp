module Triangle

let isTriangle sides =
    sides |> List.length |> (=) 3
    && sides |> List.forall (fun x -> x <> 0.)
    && List.sum sides >= (List.max sides) * 2.

let equilateral triangle =
    let equilateral' =
        let h = triangle |> List.head
        triangle |> Seq.forall (fun x -> x = h)
    isTriangle triangle && equilateral'

let isosceles triangle =
    let isosceles' =
        triangle
        |> List.countBy id
        |> List.length
        |> (fun l -> l > 0 && l < 3)
    isTriangle triangle && isosceles'

let scalene triangle =
    let scalene' =
        triangle
        |> List.countBy id
        |> List.length
        |> (=) 3
    isTriangle triangle && scalene'
