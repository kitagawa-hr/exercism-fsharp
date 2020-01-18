module CollatzConjecture

let rec steps (x: int): int option =
    if x <= 0 then None
    elif x =1 then Some 0
    elif x % 2 = 0 then steps (x / 2) |> Option.map (fun y -> y + 1)
    else steps (3 * x + 1) |> Option.map (fun y -> y + 1)
