module BinarySearchTree

type Tree = Node of int * option<Tree> * option<Tree>


let left = function
    | Node(_, left, _) -> left

let right = function
    | Node(_, _, right) -> right

let data = function
    | Node(value, _, _) -> value

let rec addNode tree newValue =
    match tree with
    | Some(node) ->
        match node with
        | Node(value, left, right) ->
            if newValue <= value then Some(Node(value, (addNode left newValue), right))
            else Some(Node(value, left, (addNode right newValue)))
    | None -> Some(Node(newValue, None, None))

let create =
    List.fold addNode None
    >> function
    | Some tree -> tree
    | None -> failwith ("No value found")


let rec sortedData node =
    match node with
    | Node(value, left, right) ->
        match (left, right) with
        | (None, None) -> [ value ]
        | (None, Some r) -> [ value ] @ sortedData r
        | (Some l, None) -> sortedData l @ [ value ]
        | (Some l, Some r) -> sortedData l @ [ value ] @ sortedData r
