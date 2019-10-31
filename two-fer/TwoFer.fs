module TwoFer

let twoFer (input: string option): string =
    let name =
        match input with
        | Some(x) -> x
        | None -> "you"
    sprintf "One for %s, one for me." name
