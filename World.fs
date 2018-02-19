module World

open Fields
open Entities
open System
open Movement

type SquareState =
    // Square where X (cursor) is
    | MarkedSquare of unit
    | AttackableSquare of int
    | MoveableSquare of int
    | NoneSquare of unit

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

let setMarked = setSquareState (MarkedSquare ())
let setAttackable damage = setSquareState (AttackableSquare damage)
let setMoveable movementCost = setSquareState (MoveableSquare movementCost)
let setNone = setSquareState (NoneSquare ())

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
                state = NoneSquare ()
                cursor = false
            }
        | EmptyKind fk ->
            {
                field = Fields.getFieldFromKind fk
                entity = None
                state = NoneSquare ()
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
                                | {state = MoveableSquare _}
                                | {state = AttackableSquare _}
                                | {state = MarkedSquare _} ->
                                    this.[x, y] <- square |> setNone
                                | _ -> ())

        let unsetAttMove () =
            _map
            |> Array2D.iteri (fun y x square ->
                                match square with
                                | {state = MoveableSquare _}
                                | {state = AttackableSquare _} ->
                                    this.[x, y] <- square |> setNone
                                | _ -> ())


        let getMarkedSquare () =
            let ((y, x), square) =
                _map
                |> Array2D.indexed
                |> Array2D.toList
                |> List.find (fun (_, s) -> s.state = MarkedSquare ())

            ((x, y), square)

        let s = this.[pos]

        match s.entity with
        | Some e ->
            match s.state with
            // Unselect if marked
            | MarkedSquare _ ->
                unsetAllSquares ()

            // Attack here and set states back
            | AttackableSquare _ ->
                let (attackerPos, attackerSquare) =
                    getMarkedSquare ()

                let attacker =
                    attackerSquare.entity
                    |> Option.get
                    |> coerce
                
                // We attack
                let victimDied = 
                    attacker
                    |> Entities.attack (s.entity |> Option.get |> coerce) s.field

                if not victimDied then
                    // Let the enemy counter-attack
                    let attackerDied =
                        s.entity
                        |> Option.get
                        |> coerce
                        |> Entities.attack attacker s.field
                    
                    if attackerDied then
                        this.[attackerPos] <- { this.[attackerPos] with entity = None}

                else
                    this.[pos] <- { this.[pos] with entity = None}

                unsetAllSquares ()
            
            // Check entity, if moveable set marked.
            | NoneSquare _ when e.Team = Friendly ->
                unsetAllSquares ()

                match box e with
                | :? IMoveable as e' when e'.CanMove ->
                    
                    let costMap =
                        Array2D.copy _map
                        |> Array2D.map (fun s' ->
                            match s'.entity with
                            // Squares with entities are not moveable
                            | Some _ -> Movement.NotMoveable
                            | None ->
                                // Get movement cost for terrain
                                Movement.MovementCostForTerrain e'.MovementType s'.field.kind)

                    Movement.possibleMoves costMap pos e'.MovementPoints
                    |> List.iter (fun (squarePos, _) -> this.[squarePos] <- this.[squarePos] |> setMoveable 0)

                    this.[pos] <- s |> setMarked

                | _ -> ()

            | _ -> ()

        | None ->
            match s.state with
            // Move to there
            | MoveableSquare _ ->
                let (fromPos, fromSquare) =
                    getMarkedSquare ()

                this.[fromPos] <- {fromSquare with entity = None; state = NoneSquare ()}
                this.[pos] <- {s with entity = fromSquare.entity} 

                unsetAttMove ()

                // The entity must exist, as we moved it
                let e = fromSquare.entity |> Option.get

                (coerce e : IMoveable).CanMove <- false

                match box e with 
                | :? ICombat as e' ->
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
                        |> List.iter (fun p -> this.[p] <- this.[p] |> setAttackable 0)

                        // Set the new positions as marked, as we can attack.
                        this.[pos] <- {this.[pos] with state = MarkedSquare ()} 
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
                | MarkedSquare _ -> Some "M"
                | _ ->
                    match s.cursor with
                    | true -> Some "X"
                    | false ->
                        match showState with
                        | ShowEntities -> Some e.Symbol
                        | ShowHealth ->
                            match box e with
                            | :? ICombat as e -> Some (e.Health.ToString ())
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
                | MarkedSquare _ -> Some ConsoleColor.White
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
                | MarkedSquare _ -> Some ConsoleColor.White
                | _ when s.cursor -> Some ConsoleColor.White
                | _ -> s.field.fgcol

        let getBackgroundColor (s: Square) =
            match s.state with
            | MoveableSquare _ -> ConsoleColor.Magenta
            | AttackableSquare _ -> ConsoleColor.DarkMagenta
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