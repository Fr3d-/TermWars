module Entities
open System

type IMoveable =
    abstract member MovePattern: Movement.MovementPattern

type ICaptureable =
    abstract member Capture: unit -> unit

type IAttackable =
    abstract member Health : int with get

[<AbstractClass>]
type Entity (team : Team) =
    member val Team = team
    abstract member Name : string
    abstract member Symbol : string

type Infantry (team) =
    inherit Entity (team)
    
    interface IAttackable with
        member __.Health = 9

    interface IMoveable with
        member __.MovePattern =
            array2D (
                [|
                    [|false; true; false|];
                    [|true; false; true|];
                    [|false; true; false|]
                |]
            )


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
