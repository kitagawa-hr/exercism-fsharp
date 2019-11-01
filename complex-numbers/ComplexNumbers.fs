module ComplexNumbers

open System

type Complex =
    { Real: float
      Imaginary: float }

let create real imaginary =
    { Real = real
      Imaginary = imaginary }

let mul z1 z2 =
    let { Real = a; Imaginary = b } = z1
    let { Real = c; Imaginary = d } = z2
    create (a * c - b * d) (a * d + b * c)


let add z1 z2 =
    let { Real = a; Imaginary = b } = z1
    let { Real = c; Imaginary = d } = z2
    create (a + c) (b + d)

let sub z1 z2 =
    let { Real = a; Imaginary = b } = z1
    let { Real = c; Imaginary = d } = z2
    create (a - c) (b - d)


let abs z =
    let { Real = a; Imaginary = b } = z
    sqrt (a ** 2.0 + b ** 2.0)

let conjugate z =
    let { Real = a; Imaginary = b } = z
    create a -b

let div z1 z2 =
    let z = mul z1 (conjugate z2)
    let { Real = a; Imaginary = b } = z
    let r = (abs z2) ** 2.0
    create (a / r) (b / r)

let real z = z.Real

let imaginary z = z.Imaginary

// exp(a+ib) = exp(a)*(cosb + isinb)
let exp z =
    let { Real = a; Imaginary = b } = z
    let r = Math.Exp a
    create (r * Math.Cos b) (r * Math.Sin b)
