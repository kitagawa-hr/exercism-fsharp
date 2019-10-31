module RunLengthEncoding

open System.Text.RegularExpressions

let (|RegexMatches|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some m.Groups.[0].Value
    else None

type Code =
    { Length: int
      Char: char }

let rec encStrToCodes str =
    match str with
    | "" -> []
    | RegexMatches @"^\d+" number ->
        [ { Length = int number
            Char = str.[number.Length] } ]
        @ encStrToCodes str.[number.Length + 1..]
    | _ ->
        [ { Length = 1
            Char = str.[0] } ]
        @ encStrToCodes str.[1..]

let rec decStrToCodes str =
    match str with
    | "" -> []
    | RegexMatches @"(.)\1*" repeatedStr ->
        [ { Length = repeatedStr.Length
            Char = repeatedStr.[0] } ]
        @ decStrToCodes str.[repeatedStr.Length..]
    | _ -> []

let decodeCode code =
    seq {
        for _ in 1 .. code.Length do
            yield code.Char
    }
    |> System.String.Concat

let encodeCode code =
    if code.Length > 1 then sprintf "%i%c" code.Length code.Char
    else string code.Char

let encode =
    decStrToCodes
    >> List.map encodeCode
    >> Seq.ofList
    >> String.concat ""

let decode =
    encStrToCodes
    >> List.map decodeCode
    >> Seq.ofList
    >> String.concat ""
