module Diamond

open System

let alphabets = [ 'A' .. 'Z' ] |> List.map string
let charToNum letter = List.tryFindIndex (fun x -> x = letter) alphabets

let spaces size =
    [ for i in 1 .. size do
        yield " " ]
    |> Seq.ofList
    |> String.concat ""

// makePiramid 0 = ["A"]
// makePiramid 1 = [" A "; "B B"]
// makePiramid 2 = ["  A  "; " B B "; "C   C"]
let makePiramid rank =
    seq {
        yield [ spaces rank
                "A"
                spaces rank ]
              |> String.concat ""
        for i in 1 .. rank do
            yield seq {
                      spaces (rank - i)
                      alphabets.[i]
                      spaces (2 * i - 1)
                      alphabets.[i]
                      spaces (rank - i)
                  }
                  |> String.concat ""
    }

let make (letter: char) =
    let num = charToNum (string letter)
    match num with
    | None -> failwith ("invalid input. alphabet expected")
    | Some(num) ->
        let piramid = makePiramid num
        let diamond = seq{
            yield! piramid
            yield! piramid |> Seq.rev |> Seq.tail
        }
        diamond |> String.concat "\n"
