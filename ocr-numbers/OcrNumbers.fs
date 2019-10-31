module OcrNumbers

let rec splitToLines (input: string list): string list list =
    match input with
    | [] -> []
    | _ ->
        let (first, second) = List.splitAt 4 input
        [ first ] @ splitToLines second

let rec lineToSymbol (line: string list) =
    match line with
    | [] -> []
    | [ ""; ""; ""; "" ] -> []
    | _ ->
        let tmp = line |> List.map (fun str -> (str.[0..2], str.[3..]))
        [ List.map fst tmp ] @ (List.map snd tmp |> lineToSymbol)

let symbolToNumber (symbol: string list) =
    let one = [ "   "; "  |"; "  |"; "   " ]
    let two = [ " _ "; " _|"; "|_ "; "   " ]
    let three = [ " _ "; " _|"; " _|"; "   " ]
    let four = [ "   "; "|_|"; "  |"; "   " ]
    let five = [ " _ "; "|_ "; " _|"; "   " ]
    let six = [ " _ "; "|_ "; "|_|"; "   " ]
    let seven = [ " _ "; "  |"; "  |"; "   " ]
    let eight = [ " _ "; "|_|"; "|_|"; "   " ]
    let nine = [ " _ "; "|_|"; " _|"; "   " ]
    let zero = [ " _ "; "| |"; "|_|"; "   " ]

    let symbolMap =
        [ one, "1"
          two, "2"
          three, "3"
          four, "4"
          five, "5"
          six, "6"
          seven, "7"
          eight, "8"
          nine, "9"
          zero, "0" ]
        |> Map.ofList

    let lengths = symbol |> List.map (fun str -> str.Length)
    match lengths with
    | [ 3; 3; 3; 3 ] ->
        symbolMap
        |> Map.tryFind symbol
        |> function
        | Some x -> Some x
        | None -> Some "?"
    | _ -> None

let symbolsToNumber =
    (List.map symbolToNumber)
    >> List.choose id
    >> String.concat ""

let (|ValidInput|InvalidInput|) (input: string list) =
    let row = input.Length
    if row % 4 <> 0 then
        InvalidInput
    else
        let invalidColumn = input |> List.filter (fun str -> str.Length % 3 <> 0)
        if invalidColumn.IsEmpty then ValidInput
        else InvalidInput

let convert input =
    match input with
    | InvalidInput -> None
    | _ ->
        input
        |> splitToLines
        |> List.map (lineToSymbol >> symbolsToNumber)
        |> List.toSeq
        |> String.concat ","
        |> Some
