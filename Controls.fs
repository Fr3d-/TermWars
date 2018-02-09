module Controls

let mutable cursor : Position = (0, 0)

let getDirectionVector key : Position =
    match key with
    | 'h' -> (-1, 0)
    | 'j' -> (0, 1)
    | 'k' -> (0, -1)
    | 'l' -> (1, 0)
    | _ -> failwith "unsupported key for direction"

let updatePosition (world : World.World) key =
    let dirVec = getDirectionVector key
    let newPos = Vec2.add dirVec cursor

    if newPos |> world.IsWithinBounds then
        let unsetSquare =
            world.[cursor]
            |> World.setNone

        world.[cursor] <- unsetSquare

        let setSquare =
            world.[newPos]
            |> World.setMarked
        
        world.[newPos] <- setSquare

        cursor <- newPos

let handleControl world keyOpt =
    match keyOpt with
        | Some key ->
            match key with
            | 'h' | 'j' | 'k' | 'l' -> updatePosition world key
            | _ -> ()
        | None -> ()
        