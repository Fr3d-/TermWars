[<AutoOpen>]
module Extensions

module Array2D =
    // Credit: https://stackoverflow.com/a/12871091/3641365
    let toArray (arr: 'T [,]) = arr |> Seq.cast<'T> |> Seq.toArray
    let toList (arr: 'T [,]) = arr |> Seq.cast<'T> |> Seq.toList