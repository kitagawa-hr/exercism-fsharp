module Clock

type Clock =
    { Hour: int
      Minute: int }

let umod p q =
    let m = p % q
    if m >= 0 then m
    else m + q

let create hours minutes =
    let seconds = umod (hours * 3600 + minutes * 60) (24 * 3600)
    let h = seconds / 3600
    let m = (seconds - h * 3600) / 60
    { Hour = umod h 24
      Minute = umod m 60 }

let add minutes clock = create clock.Hour (clock.Minute + minutes)

let subtract minutes clock = create clock.Hour (clock.Minute - minutes)

let display clock = sprintf "%02i:%02i" clock.Hour clock.Minute
