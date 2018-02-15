module Movement
open System

type Pattern = bool [,]

type MovementType =
    | Walking
    | Wheels
    | Threads
    | Naval
    | Flying

[<Literal>]
let NotMoveable = 999

let MovementCostForTerrain (movementType: MovementType) (terrain: FieldKind) =
    match movementType with
    | Walking ->
        match terrain with
        | Grass -> 1
        | Water -> NotMoveable
        | Mountain -> 2
        | Tree -> 1
        | RoadV | RoadH -> 1
    | _ -> raise (NotImplementedException ())

let possibleMoves (costMap: int [,]) (srcPosition: Position) (movementPoints: int) =
    // I'm pretty sure width has to be higher than length
    let width = Array2D.length2 costMap
    // V is width squared
    let V = pown width 2

    let toVertex ((x, y) : Position) : Graph.Vertex = x + width * y
    let toPosition (i : Graph.Vertex) : Position = (i % width, i / width)

    let createGraph (map : int [,]) : Graph.Egde list =
        let g ((x, y) : Position) =
            Array2D.tryGet map y x

        let getNeighbours (x, y) =
            // All neighbours
            [(-1, 0); (0, -1); (1, 0); (0, 1)]
            // From vertex, to vertex, and weight.
            |> List.map (fun (x', y') -> ((x, y) |> toVertex, (x + x', y + y') |> toVertex, g (x + x', y + y')))
            |> List.choose (fun (a, b, elem) -> match elem with | Some e -> Some (a, b, e) | None -> None)

        map
        |> Array2D.mapi (fun i j _ -> getNeighbours (j, i))
        |> Array2D.toList
        |> List.concat


    let adjacencyMatrix (graph: (Graph.Vertex * Graph.Vertex * int) list) =
        let matrix = Array2D.zeroCreate V V

        graph
        |> List.iter (fun (x, y, w) -> matrix.[x, y] <- w)

        matrix

    let minDistance (dist: int []) (sptSet: bool []) =
        dist
        |> Array.indexed
        |> Array.filter (fun (i, _) -> not sptSet.[i])
        |> Array.minBy (fun (_, v) -> v)
        |> fst

    // Returns shortest path weight to all positions
    let djikstra (src: Graph.Vertex) (matrix: int [,]) =
        let dist = Array.create V 999
        dist.[src] <- 0
        
        let sptSet = Array.create V false

        for i = 0 to V - 2 do
            let u = minDistance dist sptSet

            sptSet.[u] <- true

            for v = 0 to V - 1 do
                if (not (sptSet.[v]) && matrix.[u, v] > 0 && dist.[u] <> 999 && dist.[u] + matrix.[u, v] < dist.[v]) then
                    dist.[v] <- dist.[u] + matrix.[u, v]

        dist
        |> Array.mapi (fun i w ->
            let dst = i |> toPosition
            (dst, w))
        |> Array.toList
    
    costMap
    // Create graph from 2d grid
    |> createGraph
    // Create adjencency matrix from graph
    |> adjacencyMatrix
    // Get costs to all other vertices from our chosen vertex.
    |> djikstra (srcPosition |> toVertex)
    // Filter out too expensive moves
    |> List.filter (fun (pos, cost) -> cost <= movementPoints)

let patternPositions (pattern: Pattern) (currentPosition: Position) =
    let helper i j canMove =
        if canMove then
            Some (j - (Array2D.length2 pattern / 2), i - (Array2D.length1 pattern / 2))
        else
            None
    pattern
    |> Array2D.mapi helper
    |> Array2D.toList
    |> List.choose id
    |> List.map (fun pos -> Vec2.add pos currentPosition)