module PigLatin

let vowelSounds = ["a";"i";"e";"o";"u";"xr";"yt"]
let vowels = ["a";"i";"e";"o";"u";"y"]

let startsWith arr (s:string) =
    arr |> List.exists (fun (v:string) -> s.StartsWith(v)) 

let isConsonant (s:string) =
    not <| startsWith vowelSounds s

let (|Vowel|_|) (w:string) =
    if startsWith vowelSounds w.[0..1] then
        Some(w)
    else
        None

let (|ConsonantWithY|_|) (w:string) =
    if isConsonant w.[0..1] && w.[2..3].Contains("y") then
        let i = w.IndexOf("y")
        Some(w,i)
    else
        None

let (|ConsonantWithQu|_|) (w:string) =
    let b = w.[0..3]
    if isConsonant b && b.Contains("qu") then
        let i = w.IndexOf("qu") + 2
        Some(w,i)
    else
        None

let (|Consonant|_|) (w:string) =
    let b = w.[0..3]
    if isConsonant b then
        let i = b.ToCharArray() |> Array.mapi (fun i c -> if c.ToString()|>(startsWith vowels) then i else 0)|> Array.filter ((<>) 0) |> Array.head
        Some(w,i)
    else
        None

let mapWord (word: string) = 
    match word with
    | Vowel w -> w
    | ConsonantWithY (w,i) | ConsonantWithQu (w,i) | Consonant (w,i) -> w.Substring(i) + w.Substring(0, i)
    | _ -> failwith "Unexpected word."

let addPostfix s = s + "ay"

let translate (input: string) = 
    input.Split(" ")
    |> Array.map (mapWord >> addPostfix)
    |> String.concat " "
