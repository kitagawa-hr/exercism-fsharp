module Poker

open System

type Suit =
    | Diamond
    | Spade
    | Crub
    | Heart

    member this.ToString =
        match this with
        | Diamond -> "D"
        | Spade -> "S"
        | Crub -> "C"
        | Heart -> "H"

    static member FromChar(c: char) =
        match c with
        | 'D' -> Ok Diamond
        | 'S' -> Ok Spade
        | 'C' -> Ok Crub
        | 'H' -> Ok Heart
        | _ -> Error <| sprintf "invalid suit %c" c

type Number = int

type Card =
    { Number: Number
      Suit: Suit }

    member this.ToString = string this.Number + string this.Suit

    static member FromString(s: string) =
        let string2Number (s: string) =
            match s with
            | "J" -> 11
            | "Q" -> 12
            | "K" -> 13
            | "A" -> 14
            | _ -> int s
        let number =
            match s.Length with
            | 2 -> Ok <| string2Number s.[0..0]
            | 3 -> Ok <| string2Number s.[0..1]
            | _ -> Error "invalid length"

        let suit = Suit.FromChar s.[s.Length - 1]
        match (number, suit) with
        | (Ok n, Ok s) ->
            Ok
                { Number = n
                  Suit = s }
        | (Error msg, _) -> Error msg
        | (_, Error msg) -> Error msg

type Hand =
    | StraightFlush = 8
    | FourCard = 7
    | FullHouse = 6
    | Flush = 5
    | Straight = 4
    | ThreeCard = 3
    | TwoPair = 2
    | OnePair = 1
    | HighCard = 0

type Score = Number list
let cardsToScore (c1, c2, c3, c4, c5) = [ c5; c4; c3; c2; c1 ] |> List.map (fun c -> c.Number)

type Strength =
    { Hand: Hand
      Score: Score }
    static member Stronger (s1: Strength) (s2: Strength) =
        if s1.Hand < s2.Hand || (s1.Hand = s2.Hand && s1.Score < s2.Score) then s2
        else s1

let checkFourCard (c1, c2, c3, c4, c5) =
    let cs = [ c1; c2; c3; c4; c5 ]
    let isFourCardPart c = (cs |> List.filter (fun c_ -> c_.Number = c.Number)).Length = 4
    match cs |> List.filter isFourCardPart with
    | (head :: _) ->
        let rest =
            cs
            |> List.filter (isFourCardPart >> not)
            |> List.map (fun c -> c.Number)
        Error
            { Hand = Hand.FourCard
              Score = head.Number :: rest }
    | _ -> Ok(c1, c2, c3, c4, c5)

let checkThreeCard (c1, c2, c3, c4, c5) =
    let cs = [ c1; c2; c3; c4; c5 ]
    let isThreeCardPart c = (cs |> List.filter (fun c_ -> c_.Number = c.Number)).Length = 3

    let rest =
        cs
        |> List.filter (isThreeCardPart >> not)
        |> List.map (fun c -> c.Number)

    match cs |> List.filter isThreeCardPart with
    | (head :: _) ->

        Error
            { Hand = Hand.ThreeCard
              Score = head.Number :: rest }
    | _ -> Ok(c1, c2, c3, c4, c5)

let isFlush (c1, c2, c3, c4, c5) = [ c2; c3; c4; c5 ] |> List.forall (fun c -> c.Suit = c1.Suit)
let isStraight (c1, c2, c3, c4, c5) =
    (c2.Number = c1.Number + 1) && (c3.Number = c2.Number + 1) && (c4.Number = c3.Number + 1)
    && ((c5.Number = c4.Number + 1) || (c5.Number = 14) && (c1.Number = 2))

let checkStraightFlush (c1, c2, c3, c4, c5) =
    let cards = (c1, c2, c3, c4, c5)
    if isFlush cards && isStraight cards then
        Error
            { Hand = Hand.StraightFlush
              Score = [ c1; c2; c3; c4; c5 ] |> List.map (fun c -> c.Number) }
    else
        Ok cards

let checkFullHouse (c1, c2, c3, c4, c5) =
    let pairAndTriplet =
        if c1.Number = c2.Number && c4.Number = c5.Number then
            if c3.Number = c2.Number then Some(c5.Number, c1.Number)
            elif c3.Number = c4.Number then Some(c1.Number, c5.Number)
            else None
        else
            None
    match pairAndTriplet with
    | Some(a, b) ->
        Error
            { Hand = Hand.FullHouse
              Score = [ b; a ] }
    | _ -> Ok(c1, c2, c3, c4, c5)

let checkFlush (c1, c2, c3, c4, c5) =
    let cards = c1, c2, c3, c4, c5
    if isFlush cards then
        Error
            { Hand = Hand.Flush
              Score = cardsToScore cards }
    else
        Ok cards

let checkStraight (c1, c2, c3, c4, c5) =
    let cards = c1, c2, c3, c4, c5
    if isStraight cards then
        // 2 3 4 5 A pattern
        if c4.Number = 5 && c5.Number = 14 then
            Error
                { Hand = Hand.Straight
                  Score = 1 :: ([ c1; c2; c3; c4 ] |> List.map (fun c -> c.Number)) }
        else
            Error
                { Hand = Hand.Straight
                  Score = [ c1 ] |> List.map (fun c -> c.Number) }
    else
        Ok cards

let checkPair (c1, c2, c3, c4, c5) =
    let cs = [ c1; c2; c3; c4; c5 ]
    let isPairPart c = (cs |> List.filter (fun c_ -> c_.Number = c.Number)).Length = 2

    let pairCards =
        cs
        |> List.filter isPairPart
        |> List.map (fun c -> c.Number)
        |> List.distinct

    let rest =
        cs
        |> List.filter (isPairPart >> not)
        |> List.map (fun x -> x.Number)

    match pairCards with
    | [ a; b ] ->
        let (lower, higher) =
            if (a < b) then (a, b)
            else (b, a)
        Error
            { Hand = Hand.TwoPair
              Score = higher :: lower :: rest }
    | [ a ] ->
        Error
            { Hand = Hand.OnePair
              Score = a :: rest }
    | _ -> Ok(c1, c2, c3, c4, c5)

let getStrength (cs: Card list) =
    let scs = List.sortBy (fun c -> c.Number) cs
    match scs with
    | [ c1; c2; c3; c4; c5 ] ->
        Ok(c1, c2, c3, c4, c5)
        |> Result.bind checkStraightFlush
        |> Result.bind checkFourCard
        |> Result.bind checkFullHouse
        |> Result.bind checkFlush
        |> Result.bind checkStraight
        |> Result.bind checkThreeCard
        |> Result.bind checkPair
        |> function
        | Ok s ->
            { Hand = Hand.HighCard
              Score = cardsToScore s }
        | Error s -> s
    | _ -> failwith "invalid card length"

let isOk =
    function
    | Ok _ -> true
    | _ -> false

let stringToHand (s: string) =
    s.Split " "
    |> Seq.map
        (Card.FromString
         >> function
         | Error msg -> failwith msg
         | Ok c -> c)
    |> List.ofSeq

let bestHands (hands: string list) =
    let strengths = hands |> List.map (stringToHand >> getStrength)
    let strongest = strengths |> List.reduce Strength.Stronger

    let strongestIndices =
        strengths
        |> List.mapi (fun i v -> i, v)
        |> List.filter (fun s -> (snd s) = strongest)
        |> List.map fst
    [ for i in strongestIndices do
        yield hands.[i] ]
