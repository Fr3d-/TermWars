module Game

open World
open InfoPanel

let world = World ()
let infopanel = InfoPanel (world)

let rec run (state: ShowState)  counter : unit =
    let key =
        if (System.Console.KeyAvailable) then
            Some (System.Console.ReadKey (true)).KeyChar
        else
            None

    let nextState = 
        if counter % 100 = 0 then
            world.UpdateAllEntities ()
            World.nextState state
        else
            state

    Controls.handleControl world key
    world.Draw nextState
    infopanel.Draw key ("Hey")

    System.Threading.Thread.Sleep (10)
    run nextState (counter + 1)