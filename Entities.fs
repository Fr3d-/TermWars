module Entities
open System

[<AbstractClass>]
type Entity (team : Team) =
    member val Team = team
    abstract member Name : string
    abstract member Symbol : string

type IMoveable =
    abstract member CanMove: bool with get, set
    abstract member MovementPoints: int
    abstract member MovementType: Movement.MovementType

type ICaptureable =
    abstract member Capture: unit -> unit

type ICombat =
    abstract member AttackPattern: Movement.Pattern
    abstract member AttackPowerOn: ICombat -> int
    abstract Health : int with get, set

// Returns a bool indicating if the victim died
let attack (victim: ICombat) (victimTerrain: Fields.Field) (attacker: ICombat) =
    let attackPower = attacker.AttackPowerOn victim |> float
    let finalAttackPower = ((attackPower - (attackPower * victimTerrain.defensiveBonus)) * (float attacker.Health / 9.0)) |> int
    victim.Health <- victim.Health - finalAttackPower

    // Don't set health below zero
    victim.Health <- max victim.Health 0

    victim.Health = 0

type Infantry (team) =
    inherit Entity (team)
    
    let mutable health = 9
    let mutable canMove = true

    interface ICombat with
        member __.Health with get() = health and set(v) = health <- v
        member __.AttackPowerOn (enemy: ICombat) =
            match enemy with
            | :? Infantry -> 6
            | :? Tank -> 1
            | _ -> raise (NotImplementedException ())

        member __.AttackPattern =
            array2D (
                [|
                    [|true; true; true|];
                    [|true; false; true|];
                    [|true; true; true|]
                |]
            )

    interface IMoveable with
        member __.CanMove with get () = canMove and set(v) = canMove <- v
        member __.MovementType = Movement.Walking
        member __.MovementPoints = 3

    override __.Name = "Infantry"
    override __.Symbol = "I"

and Tank (team) =
    inherit Entity (team)

    override __.Name = "Tank"
    override __.Symbol = "T"

let isDead (entity: Entity) =
    match box entity with
    | :? ICombat as e -> e.Health = 0
    | _ -> failwith "isDead only works on entities that implement ICombat" 

let createEntityFromKind team = function
    | Infantry -> (Infantry team :> Entity)
    | Tank -> (Tank team :> Entity)
    | _ -> raise (NotImplementedException ())

let getTeamColor = function
    | Friendly -> ConsoleColor.Yellow
    | Enemy -> ConsoleColor.DarkRed
