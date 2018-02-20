module InfoPanel

open World
open Controls
open Entities
open System

type Row = World -> (string * System.ConsoleColor option)
type Column = Row list
type Panel = Column * Column * Column


let addRow r (column: Column) =
    r :: column

let fieldColumn : Column =
    []
    // Field name
    |> addRow (fun w ->
        let f = (w.Item cursor).field

        f
        |> Fields.getName
        |> duplet <| (Fields.getColor f |> Some) )

    // Defensive bonus
    |> addRow (fun w ->
        let f = (w.Item cursor).field

        sprintf "Defensive Bonus: %i%%" (f.defensiveBonus * 100.0 |> int)
        |> duplet <| None)

    // Movement cost to square
    |> addRow (fun w ->
        match (w.Item cursor).state with
        | MoveableSquare cost ->        
            sprintf "Movement Cost: %i" cost
            |> duplet <| None
        | _ -> ("", None))

    |> addRow (fun x -> ("", None))
    |> List.rev

let entityColumn : Column =
    []
    // Entity name
    |> addRow (fun w ->
        match (w.Item cursor).entity with
        | Some entity ->
            let infoString =
                match box entity with 
                | :? ICombat as e ->
                    sprintf "%s (HP: %i)" entity.Name e.Health
                | :? Base as e ->
                    sprintf "%s (CP: %i/%i)" entity.Name e.CapturePoints Constants.maxCapturePoints
                | _ ->
                    sprintf "%s" entity.Name
            
            (infoString, Entities.getTeamColor entity.Team |> Some)
        | None -> ("", None))
    |> addRow (fun w ->
        match (w.Item cursor).entity with
        | Some entity ->
            let infoString =
                match box entity with 
                | :? ICombat as e ->
                    sprintf "W: %s A: %s" (e.Weapon |> getUnionCaseName) (e.Armor |> getUnionCaseName)
                | :? Base as e ->
                    match e.Inhabitant with
                    | Some inhabitant ->
                        sprintf "Inhabitant: %s" inhabitant.Name
                    | None -> ""
                | _ ->
                    sprintf "%s" entity.Name
            
            infoString
            |> duplet <| (Entities.getTeamColor entity.Team |> Some)
        | None -> ("", None))

    |> addRow (fun w ->
        let victimSquare = (w.Item cursor)

        let damage =
            maybe {
                let! victimEntity = victimSquare.entity
                let! victim = tryCoerce<ICombat> victimEntity
                let! attackerSquare = w.TryGetMarkedSquare ()
                let! attackerEntity = (snd attackerSquare).entity
                let! attacker = tryCoerce<ICombat> attackerEntity

                if attackerEntity.Team <> victimEntity.Team then
                    return
                        attacker
                        |> Entities.damageAgainst victim victimSquare.field
                        |> sprintf "Damage against unit %i"

            }

        match damage with
        | Some s -> (s, None)
        | None -> ("", None)
        )


    |> addRow (fun x -> ("", None))
    |> List.rev

let lastColumn : Column =
    []
    |> addRow (fun x -> ("", None))
    |> addRow (fun x -> ("", None))
    |> addRow (fun x -> ("", None))
    |> addRow (fun x -> ("", None))
    |> List.rev

let panel = (fieldColumn, entityColumn, lastColumn)

let columnSize = 26

let printPanelRow (s: string) (c: System.ConsoleColor option list) =
    let print c s =
        match c with
        | Some c -> System.Console.ForegroundColor <- c
        | None -> System.Console.ResetColor ()

        printf "%s" s

    Terminal.print None c.[0] s.[0..25]
    Terminal.printWithBackground System.ConsoleColor.White (s.[26] |> string)
    Terminal.print None c.[1] s.[27..51]
    Terminal.printWithBackground System.ConsoleColor.White (s.[52] |> string)
    Terminal.print None c.[2] s.[53..] 

let rec drawPanel (world: World) (panel: Panel) =
    match panel with
    | (x::xs, y::ys, z::zs) ->
        let rows = 
            [x; y; z]
            |> List.map (fun f -> f world)
            |> List.map (fun (s, c) -> 
                let leftPadded = s.PadLeft (s.Length + 1)
                let allPadded = leftPadded.PadRight columnSize
                (allPadded, c))

        let panelString =
            rows
            |> List.map fst
            |> List.toArray
            |> duplet " "
            |> System.String.Join

        let colors =
            rows
            |> List.map snd
        
        printPanelRow panelString colors

        drawPanel world (xs, ys, zs)
    | _ -> ()

type InfoPanel (world : World) =
    let splitter =
        String.init 80 (fun _ -> "*")
        |> String.mapi (fun i c ->
            if i % 26 = 0 && i < 78 || i = 79 then '|' else c)

    let mutable counter = 0
    let mutable lastKey = ' '
    (*
    let drawField (square : Square) =
        let col =
            match square.field.fgcol with
            | Some c -> c
            | None -> square.field.bgcol

        square.field
        |> Fields.getName
        |> Terminal.printLine (Some col)

        match square.entity with
        | Some e ->
            match box e with
            | :? ICombat as e' ->
                Terminal.printLine (Some (e.Team |> getTeamColor))
                <| sprintf "%s (HP: %i)" e.Name  e'.Health
        | None -> Terminal.printLine None ""
    *)
    member __.Draw key s =
        match key with
        | Some k -> lastKey <- k
        | None -> ()

        System.Console.SetCursorPosition (0, 18)
        System.Console.ResetColor ()

        let currentSquare = world.Item cursor

        Terminal.printWithForeground System.ConsoleColor.White splitter

        panel
        |> drawPanel world
        (*
        currentSquare |> drawField 

        counter.ToString ()
        |> Terminal.printLine None
        //printfn "Last keypress: %c" lastKey
        printfn "%s" s

        *)

        Terminal.printWithForeground System.ConsoleColor.White splitter

        counter <- counter + 1