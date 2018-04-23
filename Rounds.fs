module Rounds

open World
open Entities

let mutable currentRound = 0

let Next (world: World) =
    let entities = world.GetAllEntities ()
    let moveables =
        entities
        |> List.choose (fun e ->
            match box e with
            | :? Entities.Base as bas ->
                match bas.Inhabitant with
                | Some inhabitant -> coerce inhabitant
                | None -> None
            | :? IMoveable as e' -> Some e'
            | _ -> None)

    moveables
    |> List.iter (fun e -> e.CanMove <- true)

    currentRound <- currentRound + 1

let NextTurn () =
    ()