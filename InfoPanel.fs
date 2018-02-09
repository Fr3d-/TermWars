module InfoPanel

type InfoPanel () =
    let splitter = String.init 80 (fun _ -> "=")
    let mutable counter = 0
    let mutable lastKey = ' '

    member __.Draw key s =
        match key with
        | Some k -> lastKey <- k
        | None -> ()

        System.Console.SetCursorPosition (0, 18)
        System.Console.ResetColor ()

        printfn "%s" splitter
        for _ in [0..1] do
            printfn "COUNTER: %i" counter
        
        //printfn "Last keypress: %c" lastKey
        printfn "%s" s
        printfn "%s" splitter
        counter <- counter + 1