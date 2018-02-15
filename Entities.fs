module Entities
open System

[<AbstractClass>]
type Entity (team : Team) =
    member val Team = team
    abstract member Name : string
    abstract member Symbol : string

type IMoveable =
    abstract member MovePattern: Movement.Pattern
    abstract member CanMove: bool
    abstract member MovementPoints: int
    abstract member MovementType: Movement.MovementType

type ICaptureable =
    abstract member Capture: unit -> unit

type IAttacker =
    abstract member AttackPattern: Movement.Pattern
    abstract member AttackPower: int

type IAttackable =
    abstract Health : int with get, set

let attack (victim: IAttackable) (attacker: IAttacker) =
    victim.Health <- victim.Health - attacker.AttackPower

    // Don't set health below zero
    victim.Health <- max victim.Health 0

type Infantry (team) =
    inherit Entity (team)
    
    let mutable health = 9
    interface IAttackable with
        member __.Health with get() = health and set(v) = health <- v

    interface IAttacker with
        member __.AttackPower = 5
        member __.AttackPattern =
            array2D (
                [|
                    [|true; true; true|];
                    [|true; false; true|];
                    [|true; true; true|]
                |]
            )

    interface IMoveable with
        member __.CanMove = true
        member __.MovePattern =
            array2D (
                [|
                    [|true; true; true;  true; true|];
                    [|true; true; true;  true; true|];
                    [|true; true; false; true; true|];
                    [|true; true; true;  true; true|];
                    [|true; true; true;  true; true|]
                |]
            )

        member __.MovementType = Movement.Walking
        member __.MovementPoints = 5

    override __.Name = "Infantry"
    override __.Symbol = "I"

type Tank (team) =
    inherit Entity (team)

    override __.Name = "Tank"
    override __.Symbol = "T"

let createEntityFromKind team = function
    | Infantry -> (Infantry team :> Entity)
    | Tank -> (Tank team :> Entity)
    | _ -> raise (NotImplementedException ())

let getTeamColor = function
    | Friendly -> ConsoleColor.Yellow
    | Enemy -> ConsoleColor.DarkRed
