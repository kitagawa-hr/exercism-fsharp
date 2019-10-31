module GradeSchool

type Grade = int

type Name = string

type School = Map<Grade, Name list>

let empty: School = Map.empty

let add (student: Name) (grade: Grade) (school: School): School =
    let oldNames =
        match Map.tryFind grade school with
        | Some names -> names
        | None -> []

    let updatedNames = (student :: oldNames) |> List.sort
    school
    |> Map.remove grade
    |> Map.add grade updatedNames

let roster (school: School): string list =
    school
    |> Map.toSeq
    |> Seq.map snd
    |> List.concat

let grade (number: Grade) (school: School): string list =
    school
    |> Map.toSeq
    |> Seq.filter (fun (k, v) -> k = number)
    |> Seq.map snd
    |> List.concat
