// This is the file you need to modify for your own solution.
// The unit tests will use this code, and it will be used by the benchmark tests
// for the "Mine" row of the summary table.

// Remember to not only run the unit tests for this exercise, but also the
// benchmark tests using `dotnet run -c Release`.
// Please refer to the instructions for more information about the benchmark tests.

module mysolution

open TreeBuildingTypes
open System

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

let compareDepth tree records =
    let rec getTreeNodesCount acc tree =
        match tree with
        | Leaf _ -> acc + 1
        | Branch (_, c) -> c |> List.map (getTreeNodesCount acc) |> List.fold (+) 1

    let nodes = getTreeNodesCount 0 tree

    let error = sprintf "Nodes %d. Records %d" nodes (records |> List.length)
    if(nodes <> (records |> List.length)) then failwith error

let filterRecords id =
    List.filter (fun x -> x.RecordId <> id)

let buildNode (records: Record list) (id:int) =
    let rec buildNode' (records: Record list) (id:int) =
        records
        |> List.filter (fun x -> x.ParentId = id) 
        |> function
            | x when x.Length = 0 -> Leaf (id)
            | x -> Branch (id, x
                |> List.map (fun r -> if (r.ParentId > r.RecordId || r.ParentId = r.RecordId) then  raise (new Exception("Nodes with invalid parents")) else r)
                |> List.map (fun r -> buildNode' (filterRecords r.RecordId records) r.RecordId)
                |> List.sortBy recordId
        )

    let tree = buildNode' (filterRecords id records) id
    compareDepth tree records
    tree

let buildTree records =
    match records with
    | rs when rs |> List.length = 0 -> failwith "Empty records list"
    | rs when (rs |> List.length) - 1 <> (rs |> List.map (fun r -> r.RecordId) |> List.max) -> failwith "Non-continious records list"
    | rs -> rs 
            |> List.tryFind (fun r -> r.RecordId = 0 && r.ParentId = 0)
            |> function
                | None -> failwith "No root found"
                | Some r -> buildNode records r.RecordId