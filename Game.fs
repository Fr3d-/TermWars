module Game

open World
open InfoPanel
open Controls

/// Return all values for an enumeration type
let EnumValues (enumType : System.Type) : 'a list =
    let values = System.Enum.GetValues enumType
    let lb = values.GetLowerBound 0
    let ub = values.GetUpperBound 0
    [lb .. ub] |> List.map (fun i -> values.GetValue i :?> 'a) 

let world = World ()
let infopanel = InfoPanel ()

let cols : System.ConsoleColor seq = unbox (System.Enum.GetValues(typeof<System.ConsoleColor>))

let rec run (state: ShowState)  counter : unit =
    let key =
        if (System.Console.KeyAvailable) then
            Some (System.Console.ReadKey (true)).KeyChar
        else
            None

    let nextState = 
        if counter % 200 = 0 then
            world.UpdateAllEntities ()
            World.nextState state
        else
            state

    world.Draw nextState
    infopanel.Draw key (sprintf "%A\t\t%A\t\t" nextState (world.GetMap |> List.length))
    Controls.handleControl world key
    System.Threading.Thread.Sleep (10)
    run nextState (counter + 1)