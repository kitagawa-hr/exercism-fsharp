module Bob

open System.Text.RegularExpressions

let (|RegexMatches|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some m.Groups.[0].Value
    else None

let response (input: string): string =
    match input with
    | RegexMatches @"^[\s\t]*$" _ -> "Fine. Be that way!"
    | RegexMatches @"^[A-Z][^a-z]+\?[\s\t]*$" _ -> "Calm down, I know what I'm doing!"
    | RegexMatches @"^[^a-z]*[A-Z]+[^a-z]*[^?]$" _ -> "Whoa, chill out!"
    | RegexMatches @"\?[\s\t]*$" _ -> "Sure."
    | _ -> "Whatever."
