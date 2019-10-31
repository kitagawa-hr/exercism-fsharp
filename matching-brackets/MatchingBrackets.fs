module MatchingBrackets

open System.Collections.Generic

let (|LeftBracket|RightBracket|Other|) input =
    match input with
    | '{'
    | '['
    | '(' -> LeftBracket
    | '}'
    | ']'
    | ')' -> RightBracket
    | _ -> Other

let isPair a b =
    match (a, b) with
    | ('{', '}') -> true
    | ('[', ']') -> true
    | ('(', ')') -> true
    | _ -> false

let rec isPairedCheck input (stack: char list) =
    match input with
    | "" -> stack.IsEmpty
    | _ ->
        let headChar, tailChars = input.[0], input.[1..]
        match headChar with
        | LeftBracket -> isPairedCheck tailChars (headChar :: stack)
        | RightBracket ->
            match stack with
            | head :: tail when isPair head headChar -> isPairedCheck tailChars tail
            | _ -> false
        | Other -> isPairedCheck tailChars stack

let isPaired input = isPairedCheck input []
