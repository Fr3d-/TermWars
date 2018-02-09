[<EntryPoint>]
let main _ =
    System.Console.CursorVisible <- false
    Terminal.disableTerminalEcho ()
    // New    
    Game.run World.ShowEntities 0
    0 // return an integer exit code
