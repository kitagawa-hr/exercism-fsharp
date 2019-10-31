module Raindrops

let convert (number: int): string =
    let pling x = if x % 3 = 0 then "Pling" else sprintf ""
    let plang x = if x % 5 = 0 then "Plang" else sprintf ""
    let plong x = if x % 7 = 0 then "Plong" else sprintf ""
    let s = pling number + plang number + plong number
    if (s = "") then sprintf "%d" number else s

