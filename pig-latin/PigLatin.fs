module PigLatin

open System.Text.RegularExpressions

let (|RegexMatches|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then
        Some
            [ for g in m.Groups do
                if g.Success then yield g.Value ]
    else
        None

type Word =
    | Translated of string
    | NotTranslated of string

let applyRule rule word =
    match word with
    | Translated _ -> word
    | NotTranslated str -> rule str

let ruleOne str =
    match str with
    | RegexMatches @"^(a|i|u|e|o|xr|yt)" [ _; _ ] -> Translated(str + "ay")
    | _ -> NotTranslated str

let ruleTwo str =
    match str with
    | RegexMatches @"^[^aiueo]+" [ all ] -> Translated(str.[all.Length..] + all + "ay")
    | _ -> NotTranslated str

let ruleThree str =
    match str with
    | RegexMatches @"^[^aiueo]*qu" [ all ] -> Translated(str.[all.Length..] + all + "ay")
    | _ -> NotTranslated str

let ruleFour str =
    match str with
    | RegexMatches @"^.y$" [ _ ] -> Translated("y" + str.[0..0] + "ay")
    | RegexMatches @"^[^aiueo]+y" [ all ] when all.Length >= 2 ->
        Translated(str.[all.Length - 1..] + str.[..all.Length - 2] + "ay")
    | _ -> NotTranslated str

let convert =
    NotTranslated
    >> applyRule ruleFour
    >> applyRule ruleOne
    >> applyRule ruleThree
    >> applyRule ruleTwo
    >> function
    | Translated s -> s
    | NotTranslated s -> s

let translate (input: string) =
    input.Split ' '
    |> Array.map convert
    |> String.concat " "
