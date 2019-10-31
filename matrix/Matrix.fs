module Matrix

let toArray (str: string) = str.Split '\n' |> Array.map (fun s -> s.Split ' ' |> Array.map int)

let row index matrix =
    matrix
    |> toArray
    |> (fun a -> a.[index - 1])
    |> List.ofArray

let column index matrix =
    matrix
    |> toArray
    |> Array.map (fun a -> a.[index - 1])
    |> List.ofArray
