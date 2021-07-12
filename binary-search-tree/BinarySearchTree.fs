module BinarySearchTree

type BinarySearchTree =
  | Empty
  | Tree of BinarySearchTree * int * BinarySearchTree

let treeToOption = function
    | Empty -> None
    | t -> Some t

let left node = 
    match node with
    | Empty -> None
    | Tree(l, _, _) -> treeToOption l

let right node = 
    match node with
    | Empty -> None
    | Tree(_, _, r) -> treeToOption r

let data node = 
    match node with
    | Empty -> failwith "Get data on empty tree."
    | Tree(_, v, _) -> v

let create items = 
    let rec insert v t =
        match t with
        | Empty -> Tree (Empty, v, Empty)
        | Tree(l, v', r) ->
            match v <= v' with
            | true -> Tree((insert v l), v', r)
            | false -> Tree(l, v', (insert v r))

    let rec build items insert' =
        match items with
        | [] -> insert' Empty
        | h::t -> build t (insert' >> (insert h))

    build items id

let sortedData node = 
    let rec dfs node =
        match node with
        | Empty -> Seq.empty
        | Tree (l, v, r) -> seq {
                yield! dfs l
                yield v
                yield! dfs r
            }

    dfs node |> Seq.toList