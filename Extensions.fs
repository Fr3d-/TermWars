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

let coerce<'b> value : 'b = (box >> unbox) value

let tryCoerce<'b> o : 'b option =
    match box o with
    | :? 'b as b -> Some b
    | _ -> None 

let duplet a b = (a, b)

let debugPrint s =
    System.Diagnostics.Debug.Write <| sprintf "%A" s

let toList x = [x]

let trd (_, _, x) = x

module ComputationalExpressions =
    type MaybeBuilder() =
        member __.Bind(x, f) = Option.bind f x

        member __.Return(x) = 
            Some x

        member __.ReturnFrom x = x

        member __.Zero () = None

        // From F# for fun and profit
        member this.Combine (a, b) = 
            match a with
            | Some _ -> a  // a succeeds -- use it
            | None -> b    // a fails -- use b instead

    type ReturnFirstBuilder () =
        member __.ReturnFrom(x) = 
            x

        member __.Zero() = 
            None

        member __.Combine (m,f) = 
            match m with
            | Some _ -> m
            | None -> f ()


        member __.Run(f) = 
            f ()

        member __.Delay(f) = 
            f
        

let maybe = ComputationalExpressions.MaybeBuilder ()

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
