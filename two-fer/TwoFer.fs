module TwoFer

let filterOutEmptyStrings =
    function
    | Some null -> None
    | Some "" -> None
    | Some " " -> None
    | Some value -> Some value
    | _ -> None

let twoFer (input: string option): string =
    input
    |> filterOutEmptyStrings
    |> Option.defaultValue "you"
    |> sprintf "One for %s, one for me."
