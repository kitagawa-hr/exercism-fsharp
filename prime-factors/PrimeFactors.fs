module PrimeFactors

let minFactor (number: int64) =
    let s = int (sqrt (double number))
    seq { 2 .. s }
    |> Seq.filter (fun i -> number % int64 (i) = 0L)
    |> Seq.tryHead

let rec factors number =
    let m = minFactor number
    match m with
    | Some x -> [ x ] @ factors (number / int64 (x))
    | None when number > 1L -> [ int number ]
    | _ -> []
