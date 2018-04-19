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

type ICombat =
    abstract member AttackPattern: Movement.Pattern
    abstract member Armor: Combat.ArmorType
    abstract member Weapon: Combat.WeaponType
    abstract Health : int with get, set

type IThink =
    abstract member Think: unit -> unit

let damageAgainst (victim: ICombat) (victimTerrain: Fields.Field) (attacker: ICombat) =
    let attackPower =
        attacker.Weapon
        |> Combat.damageAgainstArmor victim.Armor
        |> (*) 9.0

    ((attackPower - (attackPower * victimTerrain.defensiveBonus)) * (float attacker.Health / 9.0))
    |> int

let nettoDamage (victim: ICombat) (victimTerrain: Fields.Field) (attacker: ICombat) (attackerTerrain: Fields.Field) =
    let attackDamage = damageAgainst victim victimTerrain attacker

    if attackDamage < victim.Health then
        let responseDamage = damageAgainst attacker attackerTerrain victim

        attackDamage - responseDamage
    else
        attackDamage

// Returns a bool indicating if the victim died
let attack (victim: ICombat) (victimTerrain: Fields.Field) (attacker: ICombat) =
    let damage = 
        attacker
        |> damageAgainst victim victimTerrain

    victim.Health <- victim.Health - damage

    // Don't set health below zero
    victim.Health <- max victim.Health 0

    victim.Health = 0

type Base (team) =
    inherit Entity (team)

    let mutable inhabitant : Entity option = None

    override __.Name = "Base"
    override __.Symbol = "⌂"

    member  __.Inhabitant
        with get () = inhabitant
        and set (x) =
            match inhabitant with
            | Some _ -> failwith "Trying to set inhabitant while inhabitant already exists"
            | None -> inhabitant <- x

    member val CapturePoints = 0 with get


type Infantry (team) =
    inherit Entity (team)
    

    let mutable health = 9
    let mutable canMove = true

    interface ICombat with
        member __.Health with get() = health and set(v) = health <- v
        member __.Armor = Combat.Minimal
        member __.Weapon = Combat.Machinegun

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
    
    let mutable health = 9
    let mutable canMove = true

    interface ICombat with
        member __.Health with get() = health and set(v) = health <- v
        member __.Armor = Combat.Medium
        member __.Weapon = Combat.Rockets

        member __.AttackPattern =
            array2D (
                [|
                    [|false; true; false|];
                    [|true; false; true|];
                    [|false; true; false|]
                |]
            )

    interface IMoveable with
        member __.CanMove with get () = canMove and set(v) = canMove <- v
        member __.MovementType = Movement.Threads
        member __.MovementPoints = 5

    override __.Name = "Tank"
    override __.Symbol = "T"
and Jeep (team) =
    inherit Entity (team)
    
    let mutable health = 9
    let mutable canMove = true

    interface ICombat with
        member __.Health with get() = health and set(v) = health <- v
        member __.Armor = Combat.Light
        member __.Weapon = Combat.Machinegun

        member __.AttackPattern =
            array2D (
                [|
                    [|false; true; false|];
                    [|true; false; true|];
                    [|false; true; false|]
                |]
            )

    interface IMoveable with
        member __.CanMove with get () = canMove and set(v) = canMove <- v
        member __.MovementType = Movement.Wheels
        member __.MovementPoints = 6

    override __.Name = "Jeep"
    override __.Symbol = "J"

let isDead (entity: Entity) =
    match box entity with
    | :? ICombat as e -> e.Health = 0
    | _ -> failwith "isDead only works on entities that implement ICombat" 

let createEntityFromKind team = function
    | Infantry -> (Infantry team :> Entity)
    | Jeep -> (Jeep team :> Entity)
    | Tank -> (Tank team :> Entity)
    | Base -> (Base team :> Entity)

let getTeamColor = function
    | Friendly -> ConsoleColor.Yellow
    | Enemy -> ConsoleColor.DarkRed
