module ReverseString

let rec reverse (input: string): string =
    match input with
    | "" -> ""
    | _ -> (reverse input.[1..]) + input.[0..0]
