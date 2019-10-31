module Allergies

open System

// TODO: define the Allergen type

type Allergen =
    | Eggs
    | Peanuts
    | Shellfish
    | Strawberries
    | Tomatoes
    | Chocolate
    | Pollen
    | Cats

let allergenToCode allergen =
    match allergen with
    | Eggs -> 0
    | Peanuts -> 1
    | Shellfish -> 2
    | Strawberries -> 3
    | Tomatoes -> 4
    | Chocolate -> 5
    | Pollen -> 6
    | Cats -> 7

let allergicTo codedAllergies allergen =
    let allergenCode = allergenToCode allergen
    ((codedAllergies >>> allergenCode) &&& 1) = 1


let list codedAllergies =
    [ Eggs; Peanuts; Shellfish; Strawberries; Tomatoes; Chocolate; Pollen; Cats ]
    |> List.filter (fun allergen -> allergicTo codedAllergies allergen)
