[<AutoOpen>]
module Extensions
open Microsoft.FSharp.Reflection

// https://stackoverflow.com/a/1259500/3641365
///Returns the case name of the object with union type 'ty.
let getUnionCaseName (x:'a) = 
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name  

///Returns the case names of union type 'ty.
let getUnionCaseNames<'ty> () = 
    FSharpType.GetUnionCases(typeof<'ty>) |> Array.map (fun info -> info.Name)

let coerce value = (box >> unbox) value

let duplet a b = (a, b)

module Graph =
    type Vertex = int
    type Cost = int
    type Egde = Vertex * Vertex * Cost

module Array2D =
    // Credit: https://stackoverflow.com/a/12871091/3641365
    let toArray (arr: 'T [,]) = arr |> Seq.cast<'T> |> Seq.toArray
    let toList (arr: 'T [,]) = arr |> Seq.cast<'T> |> Seq.toList
    let indexed (arr: 'T [,]) = arr |> Array2D.mapi (fun i j x -> ((i, j), x))

    let tryGet (arr: 'T [,]) (i: int) (j: int) =
        if i < Array2D.length1 arr && i >= 0 && j < Array2D.length2 arr && j >= 0 then
            Some (Array2D.get arr i j)
        else
            None

    let zeroLike (arr: 'T [,]) : 'T [,] =
        Array2D.zeroCreate (arr |> Array2D.length1) (arr |> Array2D.length2)

    let createLike (arr: 'T [,]) (value: 'T) : 'T [,] =
        Array2D.create (arr |> Array2D.length1) (arr |> Array2D.length2) value
