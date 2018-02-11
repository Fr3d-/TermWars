module InfoPanel

open World
open Controls

type InfoPanel (world : World) =
    let splitter = String.init 80 (fun _ -> "-")
    let mutable counter = 0
    let mutable lastKey = ' '

    let drawField (square : Square) =
        let col =
            match square.field.fgcol with
            | Some c -> c
            | None -> square.field.bgcol

        square.field
        |> Fields.getName
        |> Terminal.printLine (Some col)

    member __.Draw key s =
        match key with
        | Some k -> lastKey <- k
        | None -> ()

        System.Console.SetCursorPosition (0, 18)
        System.Console.ResetColor ()

        let currentSquare = world.Item cursor

        printfn "%s" splitter
        currentSquare |> drawField 

        counter.ToString ()
        |> Terminal.printLine None 

        counter.ToString ()
        |> Terminal.printLine None
        //printfn "Last keypress: %c" lastKey
        printfn "%s" s
        printf "%s" splitter
        counter <- counter + 1