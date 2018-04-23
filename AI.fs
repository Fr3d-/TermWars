module AI

open World
open Entities

// damageGiven * damageTaken
type DamageInfo = int * int

// myHealth * enemyHealth
type EntityInfo = int

type Action =
    | Move of from:Position * dest:Position * EntityInfo
    | MoveAttack of from:Position * dest:Position * victim:Position * EntityInfo * DamageInfo

let getEntityInfo = function
    | Move (_, _, entityInfo)
    | MoveAttack (_, _, _, entityInfo, _) -> entityInfo

let getDest = function
    | Move (_, dest, _)
    | MoveAttack (_, dest, _, _, _) -> dest

let getDamageInfo = function
    | Move _ -> None
    | MoveAttack (_, _, _, _, dmgInfo) -> Some dmgInfo

let getEntity (world: World) (action: Action) =
    match action with
    | Move (from, _, _)
    | MoveAttack (from, _, _, _, _) ->
        world.[from].entity |> Option.get

let getMoveableSquares (world: World) (moveablePos, moveable: IMoveable) : (Position * Position) list =
    let costMap = world.GenerateCostMap moveable
    Movement.possibleMoves costMap moveablePos moveable.MovementPoints
    |> List.map (fun (pos, _) -> (moveablePos, pos))

let getAttackableSquares (world: World) combatable possibleMoves =
    possibleMoves
    |> List.map (fun (from, dest) -> (from, dest, (world.GetAttackablePositions dest combatable)))

let getActions (world: World) (pos: Position, entity: Entity) =
    let toActions (from, dest, targets) =
        let combatAttacker: ICombat =
            world.[from].entity |> Option.get |> coerce

        if List.isEmpty targets then
            [Move (from, dest, combatAttacker.Health)]
        else
            targets
            |> List.map (fun targetPos ->
                let netDamage = Entities.damageInfo (world.[targetPos].entity |> Option.get |> coerce) world.[targetPos].field combatAttacker world.[dest].field
                MoveAttack (from, dest, targetPos, combatAttacker.Health, netDamage))

    match box entity with
    | :? IMoveable as moveable -> 
        let moveableSquares = getMoveableSquares world (pos, moveable)
        let attackableSquares = getAttackableSquares world (coerce entity) moveableSquares

        attackableSquares
        |> List.collect toActions
    | _ -> []

let CalculateBestMove (world: World) =
    let isAttackMove = function
    | MoveAttack _ -> true
    | _ -> false

    let averageEnemyPosition () =
        let enemies = world.GetAllPositionsWithTeam Team.Friendly
        
        enemies
        |> List.map (fst >> (fun (a, b) -> (float a, float b)))
        |> List.unzip
        ||> (fun a b -> (List.average a, List.average b))
        ||> (fun a b -> (int a, int b))

    let positionsAndEntities = world.GetAllPositionsWithTeam Team.Enemy

    let allActions =
        positionsAndEntities
        |> List.choose (fun (pos, e) ->
            match box e with
            | :? IMoveable as e' when e'.CanMove -> Some (pos, e)
            | _ -> None)
        |> List.collect (getActions world)

    let moveTowardsBase () =
        ()

    let escape () =
        let avgEnemyPosition = averageEnemyPosition ()

        let possibleActions =
            allActions
            |> List.map (fun action ->
                match action with
                | Move (_, dest, _)
                | MoveAttack (_, dest, _, _, _) -> (Movement.distance dest avgEnemyPosition, action))

        match possibleActions with
        | [] -> None
        | _ ->
            possibleActions
            |> List.maxBy fst
            |> snd
            |> Some

    let moveCloserIfHealthy () =
        let avgEnemyPosition = averageEnemyPosition ()
        let allEnemyPositions = world.GetAllPositionsWithTeam Team.Friendly        


        let possibleActions =
            allActions
            |> List.filter (fun a -> (getEntityInfo a) >= 5)


        match possibleActions with
        | [] -> None
        | _ ->
            possibleActions
            |> List.groupBy (fun action ->
                match action with
                | Move (_, dest, _)
                | MoveAttack (_, dest, _, _, _) -> dest)
            |> List.map (fun (dest, actions) ->
                let closestEnemy = allEnemyPositions |> List.minBy (fst >> (Movement.distance dest))
                (dest, Movement.distance dest (fst closestEnemy), actions))
            |> List.minBy (fun (_, b, _) -> b)
            |> trd
            |> List.head
            |> Some

    let multipleAttack () = ()
        (*
        allActions
        |> List.filter isAttackMove
        // Group by victims
        |> List.groupBy (function | MoveAttack (_, _, victim, _, _) -> victim)
        // Only have distinctive destinations for each target
        |> List.map (fun (_, actions) -> actions |> List.distinctBy getDest)
        // 
        |> List.map (fun actions -> 1)
        *)

    let attackInstantKill () =
        allActions
        |> List.tryFind (fun action ->
            match action |> getDamageInfo with
            | Some (_, 0) -> true
            | _ -> false) 

    let attackWithHighestNettoDamage () = 
        let subtractTuple (a, b) = a - b

        match allActions |> List.filter isAttackMove with
        | [] -> None
        | attackMoves ->
            match attackMoves |> List.maxBy (getDamageInfo >> Option.get >> subtractTuple) with
            | MoveAttack (_, _, _, _, (givenDamage, takenDamage)) as action when givenDamage - takenDamage > -3 -> Some action
            | _ -> None

    let random () =
        let rnd = System.Random()  

        if (rnd.Next 3 = 0) then
            match allActions with
            | [] -> None
            | _ ->
                allActions
                |> List.item (rnd.Next allActions.Length)
                |> Some
        else
            None

    let returnFirst = ComputationalExpressions.ReturnFirstBuilder ()

    returnFirst {
        return! attackInstantKill ()
        return! attackWithHighestNettoDamage ()
        return! random ()
        return! moveCloserIfHealthy ()
        return! escape ()        
    }
    (*   
    if List.exists (function | MoveAttack _ -> true | _ -> false) allActions then
        allActions
        |> List.groupBy 
        // First check if we can kill 1 guy with multiple people
        // Then check if 
    else
        let avgEnemyPosition = averageEnemyPosition ()

        let (weakActions, strongActions) =
            allActions
            |> List.map (fun action ->
                match action with
                | Move (_, dest, _) -> (Movement.distance dest avgEnemyPosition, action)
                | _ -> failwith "MoveAttack in branch")
            |> List.partition (fun (_, a) -> (getEntityInfo a) < 5)

        if strongActions |> List.isEmpty then
            weakActions
            |> List.maxBy fst
            |> snd
        else
            strongActions
            |> List.minBy fst
            |> snd
    *)
    // Priorities, kill enemy?
    // Make sure that several units don't go to the same spot

let DoBestMoves (world: World) =
    let performAction = function
    | Move (from, dest, _) -> world.MoveEntity from dest
    | MoveAttack (from, dest, victim, _, _) ->
        world.MoveEntity from dest
        world.AttackEntity dest victim

    let rec iter = function
    | Some action ->
        performAction action
        iter (CalculateBestMove world)
    | None -> ()

    iter (CalculateBestMove world)

    // Actually perform it on the world
    // repeat while moves are available
    ()