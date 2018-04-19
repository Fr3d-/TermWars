module AI

open World
open Entities
open Movement

type Action =
    | Move of from:Position * dest:Position
    | MoveAttack of from:Position * dest:Position * victim:Position * netDamage:int

let getMoveableSquares (world: World) (moveablePos, moveable: IMoveable) : (Position * Position) list =
    let costMap = world.GenerateCostMap moveable
    Movement.possibleMoves costMap moveablePos moveable.MovementPoints
    |> List.map (fun (pos, _) -> (moveablePos, pos))

let getAttackableSquares (world: World) combatable possibleMoves =
    possibleMoves
    |> List.map (fun (from, dest) -> (from, dest, (world.GetAttackablePositions dest combatable)))

let getActions (world: World) pos entity =
    let toActions (from, dest, targets) =
        if List.isEmpty targets then
            [Move (from, dest)]
        else
            let combatAttacker =
                world.[from].entity |> Option.get |> coerce

            targets
            |> List.map (fun targetPos ->
                let netDamage = Entities.nettoDamage (world.[targetPos].entity |> Option.get |> coerce) world.[targetPos].field combatAttacker world.[dest].field
                MoveAttack (from, dest, targetPos, netDamage))

    match box entity with
    | :? IMoveable as moveable -> 
        let moveableSquares = getMoveableSquares world (pos, moveable)
        let attackableSquares = getAttackableSquares world (coerce<ICombat> entity) moveableSquares

        attackableSquares
        |> List.collect toActions
    | _ -> []

let CalculateBestMove (world: World) pos entity =
    // Priorities, kill enemy?
    // Make sure that several units don't go to the same spot
    ()

let DoBestMove (word: World) =
    // Find the best move
    // Actually perform it on the world
    // repeat while moves are available
    ()
