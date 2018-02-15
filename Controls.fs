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
        world.[cursor] <- {world.[cursor] with cursor = false}
        world.[newPos] <- {world.[newPos] with cursor = true}
        cursor <- newPos

let handleControl (world : World.World) keyOpt =
    match keyOpt with
        | Some key ->
            match key with
            | 'h' | 'j' | 'k' | 'l' -> updatePosition world key
            | 'x' -> world.Select cursor
            | _ -> ()
        | None -> ()
        