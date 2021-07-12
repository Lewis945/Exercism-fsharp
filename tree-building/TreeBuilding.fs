module TreeBuilding

open TreeBuildingTypes

type Tree =
    | Branch of int * Tree list
    | Leaf of int

let recordId t =
    match t with
    | Branch (id, _) -> id
    | Leaf id -> id

let isBranch t =
    match t with
    | Branch _ -> true
    | Leaf _ -> false

let children t =
    match t with
    | Branch (_, c) -> c
    | Leaf _ -> []

let checkRecords records =
    let rec check records current =
        match records with
        | [] -> true
        | { ParentId = x; RecordId = y }::tail when (x < y && y = current) || (y = 0 && x = 0) -> check tail (y + 1) 
        | _ -> false
    check records 0

let rec createTree recordId (records: Record list) =
    match List.filter (fun record -> record.ParentId = recordId) records.Tail with
    | []      -> Leaf recordId
    | list    -> Branch (recordId, (list |> List.map (fun record -> createTree record.RecordId records)))

let buildTree records =
    records
    |> List.sortBy (fun x -> x.RecordId)
    |> (fun rs -> if (List.isEmpty rs) || (checkRecords rs |> not) then failwith "Records are invalid" else rs)
    |> createTree 0