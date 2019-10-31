module BeerSong

let bottleOrBottles bottle =
    if bottle > 1 then "bottles"
    else "bottle"

let currentState bottle =
    let s = bottleOrBottles bottle
    if bottle = 0 then "No more bottles of beer on the wall, no more bottles of beer."
    else sprintf "%i %s of beer on the wall, %i %s of beer." bottle s bottle s

let afterState bottle =
    let s = bottleOrBottles bottle
    if bottle = -1 then "Go to the store and buy some more, 99 bottles of beer on the wall."
    elif bottle = 0 then "Take it down and pass it around, no more bottles of beer on the wall."
    else sprintf "Take one down and pass it around, %i %s of beer on the wall." bottle s

let rec recite (startBottles: int) (takeDown: int) =
    let afterBottles = startBottles - 1

    let song =
        [ currentState startBottles
          afterState afterBottles ]
    if takeDown = 0 then []
    elif takeDown = 1 then song
    else song @ [ "" ] @ recite afterBottles (takeDown - 1)
