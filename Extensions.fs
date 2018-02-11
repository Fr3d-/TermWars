[<AutoOpen>]
module Extensions

let coerce value = (box >> unbox) value

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
