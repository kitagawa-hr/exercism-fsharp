module RationalNumbers

open System

type RationalNumber =
    { Numer: int
      Denom: int }

let rec gcd x y =
    if y = 0 then x
    else gcd y (x % y)

let create numerator denominator =
    { Numer = numerator
      Denom = denominator }


let reduce r =
    let (numerator, denominator) = (r.Numer, r.Denom)
    if denominator = 0 then
        failwith ("ZeroDivisionError")
    elif numerator = 0 then
        create 0 1
    else
        let g = Math.Abs (gcd numerator denominator)
        let (a, b) = ((Math.Abs numerator) / g, (Math.Abs denominator) / g)
        if (numerator * denominator) < 0 then create -a b
        else create a b

let add r1 r2 =
    let (a, b) = r1.Numer, r1.Denom
    let (c, d) = r2.Numer, r2.Denom
    create (a * d + b * c) (b * d) |> reduce

let minus r = create -r.Numer r.Denom

let sub r1 r2 = add r1 (minus r2)

let mul r1 r2 =
    let (a, b) = r1.Numer, r1.Denom
    let (c, d) = r2.Numer, r2.Denom
    create (a * c) (b * d) |> reduce

let rev r = create r.Denom r.Numer

let div r1 r2 = mul r1 (rev r2)

let abs r = create (abs r.Numer) (abs r.Denom)

// r ^ n
let exprational n r =
    let (a, b) = r.Numer, r.Denom
    create (pown a n) (pown b n)

// n ^ r
let expreal r n =
    let (a, b) = r.Numer, r.Denom
    (double n) ** ((double a) / (double b))