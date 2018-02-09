module World

open Fields
open Entities
open System

type SquareState =
    | MarkedSquare
    | AttackableSquare
    | MoveableSquare
    | NoneSquare

type Square =
    | Occupied of Field * Entity * SquareState
    | Empty of Field * SquareState

let setSquareState squareState = function
    | Occupied (f, e, _) -> Occupied (f, e, squareState)
    | Empty (e, _) -> Empty (e, squareState)

let setMarked = setSquareState MarkedSquare
let setAttackable = setSquareState AttackableSquare
let setMoveable = setSquareState MoveableSquare
let setNone = setSquareState NoneSquare

type ShowState =
    | ShowEntities
    | ShowBackground
    | ShowHealth

let nextState = function
    | ShowEntities -> ShowBackground
    | ShowBackground -> ShowHealth
    | ShowHealth -> ShowEntities

type World () as this =
    let create = function
        | OccupiedKind (fk, tk, team) ->
            Occupied (Fields.getFieldFromKind fk, Entities.createEntityFromKind team tk, NoneSquare)
        | EmptyKind fk -> Empty (Fields.getFieldFromKind fk, NoneSquare)


    let _map =
        Parser.parseFile "map.txt"
        |> Array2D.map create

    let getAllSquresWithEntities (m: Square [,]) =
        // Array2D coordinates is in (y, x)
        let containsEntity y x = function
            | Occupied _ -> Some (x, y)
            | _ -> None

        m
        |> Array2D.mapi containsEntity
        |> Array2D.toList
        |> List.choose id

    let mutable updatedSquares : (int * int) list = []

    do
        this.UpdateAll ()

    member __.GetMap = updatedSquares

    member __.UpdateAllEntities () =
        updatedSquares <- (getAllSquresWithEntities _map) @ updatedSquares
    member __.UpdateAll () =
        _map
        |> Array2D.iteri (fun y x _ -> updatedSquares <- (x, y) :: updatedSquares)

    member __.IsWithinBounds ((x, y) : Position) =
        let maxY = _map |> Array2D.length1
        let maxX = _map |> Array2D.length2

        x >= 0 && x < maxX &&
        y >= 0 && y < maxY

    member __.Item
        with get(x, y) = _map.[y, x]
        and set(x, y) (s : Square) = 
          _map.[y, x] <- s
          updatedSquares <- (x, y) :: updatedSquares

    member __.Item
        with get ((x, y) : Position)  = _map.[y, x]
        and set ((x, y) : Position) (s : Square) = 
          _map.[y, x] <- s

          // Add changed square to the updatedSquare list
          updatedSquares <- (x, y) :: updatedSquares

    member __.Draw showState =
        let getSymbol = function
            | Occupied (f, e, s) ->
                match s with
                | MarkedSquare -> Some "X"
                | _ ->
                    match showState with
                    | ShowEntities -> Some e.Symbol
                    | ShowHealth ->
                        match box e with
                        | :? IAttackable as e -> Some (e.Health.ToString ())
                        | _ -> Some e.Symbol
                    | ShowBackground ->
                        match f.symbol with
                        | Some symbol -> Some symbol
                        | None -> Some e.Symbol

            | Empty (f, s) ->
                match s with
                | MarkedSquare -> Some "X"
                | _ -> f.symbol

        let getFrontColor = function
            | Occupied (f, e, s) ->
                match s with
                | MarkedSquare -> Some ConsoleColor.White
                | _ ->
                    match showState with
                    | ShowEntities 
                    | ShowHealth -> Some (e.Team |> Entities.getTeamColor)
                    | ShowBackground ->
                        match f.symbol with
                        | Some _ -> f.fgcol
                        | None -> Some (e.Team |> Entities.getTeamColor)

            | Empty (f, s) ->
                match s with
                | MarkedSquare -> Some ConsoleColor.White
                | _ -> f.fgcol

        let getBackgroundColor = function
            | Occupied (f, _, _) -> f.bgcol
            | Empty (f, _) -> f.bgcol

        let draw x y s =
            let backgroundColor = getBackgroundColor s

            let frontColor =
                match getFrontColor s with
                | Some c -> c
                | None -> backgroundColor

            let symbol = getSymbol s
                
            Terminal.draw x y frontColor backgroundColor symbol

        updatedSquares
        |> List.iter (fun (x, y) -> _map.[y, x] |> draw x y)

        updatedSquares <- List.Empty