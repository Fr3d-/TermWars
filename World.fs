module World

open Fields
open Entities
open System

type SquareState =
    // Square where M (mark) is
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

    let mutable updatedSquares : (int * int) list = []

    do
        this.UpdateAll ()
    
    member __.GetAllPositionsWithEntities () =
        // Array2D coordinates is in (y, x)
        let containsEntity y x (s : Square) =
            match s.entity with
            | Some _ -> Some (x, y)
            | None -> None

        _map
        |> Array2D.mapi containsEntity
        |> Array2D.toList
        |> List.choose id

    member this.GetAllPositionsWithTeam team =
        this.GetAllPositionsWithEntities ()
        |> List.map (fun pos -> (pos, this.[pos].entity |> Option.get))
        |> List.filter (fun (_, e) -> e.Team = team)

    member __.GetAllEntities () =
        _map
        |> Array2D.toList
        |> List.choose (fun x -> x.entity)

    member __.TryGetMarkedSquare () =
        let markedSquare =
            _map
            |> Array2D.indexed
            |> Array2D.toList
            |> List.tryFind (fun (_, s) -> s.state = MarkedSquare ())
        
        // Flip coordinates
        markedSquare
        |> Option.bind (fun ((y, x), square) -> Some ((x, y), square))


    member this.MoveEntity (fromPos: Position) (toPos: Position) =
        if fromPos = toPos then
            (this.[toPos].entity |> Option.get |> coerce<IMoveable>).CanMove <- false
        else
            match this.[fromPos].entity with
            | Some e ->
                match box e with
                | :? Base as bas when bas.Inhabitant.IsSome ->
                    this.[toPos] <- {this.[toPos] with entity = bas.Inhabitant} 
                    this.[fromPos] <- {this.[fromPos] with state = NoneSquare ()}

                    // Assuming only moveable entites can be in a base
                    (bas.Inhabitant |> Option.get |> coerce<IMoveable>).CanMove <- false
                    bas.Inhabitant <- None
                    
                | :? IMoveable as e' when e'.CanMove ->
                    match this.[toPos].entity with
                    | Some toEntity ->
                        match box toEntity with
                        | :? Base as bas ->
                            bas.Inhabitant <- this.[fromPos].entity
                            this.[fromPos] <- {this.[fromPos] with entity = None; state = NoneSquare ()}
                            e'.CanMove <- false

                        | _ -> failwith "Attempting to move to square with invalid entity"
                    | None ->
                        this.[toPos] <- {this.[toPos] with entity = this.[fromPos].entity} 
                        this.[fromPos] <- {this.[fromPos] with entity = None; state = NoneSquare ()}
                        e'.CanMove <- false

                | _ -> failwith "Entity is not a moveable type or is unable to move"
            | _ -> failwith "Can't move non existing entity"

    member this.AttackEntity attackerPos victimPos =
        let attackerSquare = this.[attackerPos]
        let victimSquare = this.[victimPos]

        let attacker =
            attackerSquare.entity
            |> Option.get
            |> coerce<ICombat>
        
        // We attack
        let victimDied = 
            attacker
            |> Entities.attack (victimSquare.entity |> Option.get |> coerce) victimSquare.field

        if not victimDied then
            // Let the enemy counter-attack
            let attackerDied =
                victimSquare.entity
                |> Option.get
                |> coerce
                |> Entities.attack attacker victimSquare.field
            
            if attackerDied then
                this.[attackerPos] <- { this.[attackerPos] with entity = None}
        else
            this.[victimPos] <- { this.[victimPos] with entity = None}
    
    member __.GenerateCostMap (moveableEntity: IMoveable) =
        Array2D.copy _map
        |> Array2D.map (fun s' ->
            match s'.entity with
            // Squares with entities are not moveable
            | Some entity ->
                match box entity with
                | :? Base as bas when bas.Inhabitant.IsNone -> Movement.MovementCostForTerrain moveableEntity.MovementType s'.field.kind
                | _ -> Movement.NotMoveable
            | None ->
                // Get movement cost for terrain
                Movement.MovementCostForTerrain moveableEntity.MovementType s'.field.kind)

    member __.GetAttackablePositions (attackerPos: Position) (entity: Entity) = 
        attackerPos
        // Get all moveable positions according to pattern
        |> Movement.patternPositions (coerce<ICombat> entity).AttackPattern
        // Get squares for all positions and removes out of bounds entries
        |> List.choose (fun ((x, y) as p) ->
            match Array2D.tryGet _map y x with
            | Some square -> Some (p, square)
            | None -> None)
        // Filter all squares without entites away and unpack entity option
        |> List.choose (fun (a, b) -> match b.entity with | Some e -> Some (a, e) | None -> None)
        // Only select enemies
        |> List.filter (fun (_, e') -> entity.Team <> e'.Team)
        // Take positions left
        |> List.map fst

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
            this.TryGetMarkedSquare ()
            |> Option.get

        let s = this.[pos]

        match s.entity with
        | Some e ->
            match s.state with
            // Unselect if marked
            | MarkedSquare _ ->
                unsetAllSquares ()

            // Attack here and set states back
            | AttackableSquare _ ->
                this.AttackEntity ((getMarkedSquare >> fst) ()) pos

                unsetAllSquares ()
            
            // Check entity, if moveable set marked.
            | NoneSquare _ when e.Team = Friendly ->
                unsetAllSquares ()

                match box e with
                | :? IMoveable as e' when e'.CanMove ->
                    
                    let costMap = this.GenerateCostMap e'

                    Movement.possibleMoves costMap pos e'.MovementPoints
                    |> List.iter (fun (squarePos, cost) -> this.[squarePos] <- this.[squarePos] |> setMoveable cost)

                    this.[pos] <- s |> setMarked

                | _ -> ()
            
            | NoneSquare _ -> ()

            // Since there's an entity on this square, we're probably moving into a base.
            | MoveableSquare _ ->
                let (fromPos, fromSquare) =
                    getMarkedSquare ()

                let movingEntity = fromSquare.entity                 

                this.MoveEntity fromPos pos

                unsetAttMove ()

        | None ->
            match s.state with
            // Move to there
            | MoveableSquare _ ->
                let (fromPos, fromSquare) =
                    getMarkedSquare ()

                this.MoveEntity fromPos pos

                unsetAttMove ()

                // The entity must exist, as we moved it
                let e = fromSquare.entity |> Option.get

                match box e with 
                | :? ICombat ->
                    let attackablePositions = this.GetAttackablePositions pos e

                    if attackablePositions |> (List.isEmpty >> not)  then
                        attackablePositions
                        |> List.iter (fun p -> this.[p] <- this.[p] |> setAttackable 0)

                        // Set the new positions as marked, as we can attack.
                        this.[pos] <- {this.[pos] with state = MarkedSquare ()} 
                
                | _ ->
                    ()

            | _ -> ()

    member __.GetMap = updatedSquares

    member this.UpdateAllEntities () =
        updatedSquares <- (this.GetAllPositionsWithEntities ()) @ updatedSquares
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