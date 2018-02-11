module World

open Fields
open Entities
open System

type SquareState =
    // Square where X (cursor) is
    | MarkedSquare
    | AttackableSquare
    | MoveableSquare
    | NoneSquare

(*
type Square =
    | Occupied of Field * Entity * SquareState * bool
    | Empty of Field * SquareState * bool
*)

type Square = {
    field: Field
    entity: Entity option
    state: SquareState
    cursor: bool
}

let setSquareState squareState (s: Square) =
    {s with state = squareState}

let setMarked = setSquareState MarkedSquare
let setAttackable = setSquareState AttackableSquare
let setMoveable = setSquareState MoveableSquare
let setNone = setSquareState NoneSquare
let setSelected = setSquareState NoneSquare

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
            {
                field = Fields.getFieldFromKind fk
                entity = Some (Entities.createEntityFromKind team tk)
                state = NoneSquare
                cursor = false
            }
        | EmptyKind fk ->
            {
                field = Fields.getFieldFromKind fk
                entity = None
                state = NoneSquare
                cursor = false
            }

    let _map =
        Parser.parseFile "map.txt"
        |> Array2D.map create

    let getAllSquresWithEntities (m: Square [,]) =
        // Array2D coordinates is in (y, x)
        let containsEntity y x (s : Square) =
            match s.entity with
            | Some _ -> Some (x, y)
            | None -> None

        m
        |> Array2D.mapi containsEntity
        |> Array2D.toList
        |> List.choose id

    let mutable updatedSquares : (int * int) list = []

    do
        this.UpdateAll ()



    member this.Select (pos : Position) =
        let unsetAllSquares () =
            _map
            |> Array2D.iteri (fun y x square ->
                                match square with
                                | {state = MoveableSquare}
                                | {state = AttackableSquare}
                                | {state = MarkedSquare} ->
                                    this.[x, y] <- square |> setNone
                                | _ -> ())

        let unsetAttMove () =
            _map
            |> Array2D.iteri (fun y x square ->
                                match square with
                                | {state = MoveableSquare}
                                | {state = AttackableSquare} ->
                                    this.[x, y] <- square |> setNone
                                | _ -> ())


        let getMarkedSquare () =
            let ((y, x), square) =
                _map
                |> Array2D.indexed
                |> Array2D.toList
                |> List.find (fun (_, s) -> s.state = MarkedSquare)

            ((x, y), square)

        let s = this.[pos]

        match s.entity with
        | Some e ->
            match s.state with
            // Unselect if marked
            | MarkedSquare ->
                unsetAllSquares ()

            // Attack here and set states back
            | AttackableSquare ->
                let (_, attackerSquare) =
                    getMarkedSquare ()

                let attacker =
                    attackerSquare.entity
                    |> Option.get
                    |> coerce
                
                attacker
                |> Entities.attack (s.entity |> Option.get |> coerce)

                unsetAllSquares ()
            
            // Check entity, if moveable set marked.
            | NoneSquare when e.Team = Friendly ->
                unsetAllSquares ()

                match box e with
                | :? IMoveable as e' ->
                    this.[pos] <- s |> setMarked
                    
                    pos
                    // Get all moveable positions according to pattern
                    |> Movement.patternPositions e'.MovePattern
                    // Get squares for all positions and removes out of bounds entries
                    |> List.choose (fun ((x, y) as p) ->
                        match Array2D.tryGet _map y x with
                        | Some square -> Some (p, square)
                        | None -> None)
                    // Filter all squares with entities away
                    |> List.filter (fun (_, s) -> s.entity |> Option.isNone)
                    // Take positions left
                    |> List.map fst
                    // Mark them
                    |> List.iter (fun p -> this.[p] <- this.[p] |> setMoveable)

                | _ -> ()
                // We gotta mark all squares that can be moved to

            | _ -> ()

        | None ->
            match s.state with
            // Move to there
            | MoveableSquare ->
                let (fromPos, fromSquare) =
                    getMarkedSquare ()

                this.[fromPos] <- {fromSquare with entity = None; state = NoneSquare}
                this.[pos] <- {s with entity = fromSquare.entity} 

                unsetAttMove ()

                // The entity must exist, as we moved it
                let e = fromSquare.entity |> Option.get

                match box e with 
                | :? IAttacker as e' ->
                    let attackablePositions =
                        pos
                        // Get all moveable positions according to pattern
                        |> Movement.patternPositions e'.AttackPattern
                        // Get squares for all positions and removes out of bounds entries
                        |> List.choose (fun ((x, y) as p) ->
                            match Array2D.tryGet _map y x with
                            | Some square -> Some (p, square)
                            | None -> None)
                        // Filter all squares without entites away and unpack entity option
                        |> List.choose (fun (a, b) -> match b.entity with | Some e -> Some (a, e) | None -> None)
                        // Only select enemies
                        |> List.filter (fun (_, e') -> e.Team <> e'.Team)
                        // Take positions left
                        |> List.map fst

                    if attackablePositions |> (List.isEmpty >> not)  then
                        attackablePositions
                        |> List.iter (fun p -> this.[p] <- this.[p] |> setAttackable)

                        // Set the new positions as marked, as we can attack.
                        this.[pos] <- {this.[pos] with state = MarkedSquare} 
                    else
                        ()

                | _ ->
                    let a =  5
                    ()

            | _ -> ()

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
        let getSymbol (s : Square) =
            match s.entity with
            | Some e ->
                match s.state with
                | MarkedSquare -> Some "M"
                | _ ->
                    match s.cursor with
                    | true -> Some "X"
                    | false ->
                        match showState with
                        | ShowEntities -> Some e.Symbol
                        | ShowHealth ->
                            match box e with
                            | :? IAttackable as e -> Some (e.Health.ToString ())
                            | _ -> Some e.Symbol
                        | ShowBackground ->
                            match s.field.symbol with
                            | Some symbol -> Some symbol
                            | None -> Some e.Symbol

            | None ->
                match s.cursor with
                | true -> Some "X"
                | false -> s.field.symbol

        let getFrontColor (s: Square) =
            match s.entity with
            | Some e ->
                match s.state with
                | MarkedSquare -> Some ConsoleColor.White
                | _ when s.cursor -> Some ConsoleColor.White
                | _ ->
                    match showState with
                    | ShowEntities | ShowHealth -> Some (e.Team |> Entities.getTeamColor)
                    | ShowBackground ->
                        match s.field.symbol with
                        | Some _ -> s.field.fgcol
                        | None -> Some (e.Team |> Entities.getTeamColor)

            | None ->
                match s.state with
                | MarkedSquare -> Some ConsoleColor.White
                | _ when s.cursor -> Some ConsoleColor.White
                | _ -> s.field.fgcol

        let getBackgroundColor (s: Square) =
            match s.state with
            | MoveableSquare -> ConsoleColor.Magenta
            | AttackableSquare -> ConsoleColor.DarkMagenta
            | _ -> s.field.bgcol

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