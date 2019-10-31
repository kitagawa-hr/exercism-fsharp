module QueenAttack

let create (position: int * int) =
    let (x, y) = position
    let validPos pos = (0 <= pos) && (pos < 8)
    (validPos x) && (validPos y)

let canAttack (queen1: int * int) (queen2: int * int) =
    let (x1, y1) = queen1
    let (x2, y2) = queen2
    (x1 = x2) || (y1 = y2) || (abs (x1 - x2) = abs(y1 - y2))
