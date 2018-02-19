module Parser

open System.IO

let parse (tokens: char []) =
    let parseField = function
        | 'G' -> Grass
        | 'M' -> Mountain
        | 'S' | 'W' -> Water
        | 'T' -> Tree
        | 'V' -> RoadV
        | 'H' -> RoadH
        | token -> sprintf "Invalid field token %c" token |> failwith
    
    let parseEntity = function
        | 'T' | 't' -> Some Tank
        | 'I' | 'i' -> Some Infantry
        | 'J' | 'j' -> Some Jeep
        | 'B' | 'b' -> Some Base
        | '0' -> None
        | token -> sprintf "Invalid entity token %c" token |> failwith 

    let team =
        if System.Char.IsUpper tokens.[1] then
            Friendly
        else
            Enemy

    match (parseField tokens.[0], parseEntity tokens.[1]) with
    | field, Some entity -> OccupiedKind (field, entity, team)
    | field, None -> EmptyKind field

let parseFile fileName : SquareKind [,] =
    let contents = File.ReadAllLines fileName

    contents
    |> Seq.map (Seq.chunkBySize 2)
    |> array2D
    |> Array2D.map parse