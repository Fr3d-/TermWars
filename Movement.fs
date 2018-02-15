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
    | Wheels ->
        match terrain with
        | Grass -> 2
        | Water -> NotMoveable
        | Mountain -> NotMoveable
        | Tree -> NotMoveable
        | RoadV | RoadH -> 1
    | Threads ->
        match terrain with
        | Grass -> 1
        | Water -> NotMoveable
        | Mountain -> NotMoveable
        | Tree -> 1
        | RoadV | RoadH -> 1
    | Naval ->
        match terrain with
        | Grass -> NotMoveable
        | Water -> 1
        | Mountain -> NotMoveable
        | Tree -> NotMoveable
        | RoadV | RoadH -> NotMoveable
    | Flying ->
        match terrain with
        | Grass -> 1
        | Water -> 1
        | Mountain -> 1
        | Tree -> 1
        | RoadV | RoadH -> 1

// Get all the possible moves from a costMap
let possibleMoves (costMap: int [,]) (originalPosition: Position) (movementPoints: int) =
    let maxCost = movementPoints
    // Dimension for our movement matrix, from which we'll calculate paths.
    let dim = maxCost * 2 + 1

    // Translation so that our entity is in the middle of our movement matrix
    let translatedX = fst originalPosition - (dim / 2)
    let translatedY = snd originalPosition - (dim / 2)

    // Size of our adjencacy matrix
    let V = pown dim 2

    let toVertex ((x, y) : Position) : Graph.Vertex = x + dim * y
    let toPosition (i : Graph.Vertex) : Position = (i % dim, i / dim)

    // Create a graph from a matrix
    let createGraph (map : int [,]) : Graph.Egde list =
        let g ((x, y) : Position) =
            Array2D.tryGet map y x

        let getNeighbours (x, y) =
            // All neighbours
            [(-1, 0); (0, -1); (1, 0); (0, 1)]
            |> List.map (fun (x', y') -> ((x, y) |> toVertex, (x + x', y + y') |> toVertex, g (x + x', y + y')))
            |> List.choose (fun (a, b, elem) -> match elem with | Some e -> Some (a, b, e) | None -> None)

        map
        |> Array2D.mapi (fun i j _ -> getNeighbours (j, i))
        |> Array2D.toList
        |> List.concat

    // Create adjacency matrix from graph
    let adjacencyMatrix V (graph: (Graph.Vertex * Graph.Vertex * int) list) =
        let matrix = Array2D.zeroCreate V V

        graph
        |> List.iter (fun (x, y, w) -> matrix.[x, y] <- w)

        matrix

    let djikstra V (graph: int [,]) (src: int) =
        let minDistance (dist : int []) (sptSet : bool []) =
            dist
            |> Array.indexed
            |> Array.filter (fun (i, _) -> not sptSet.[i])
            |> Array.minBy (fun (_, v) -> v)
            |> fst

        let dist = Array.create V 999
        dist.[src] <- 0
        
        let sptSet = Array.create V false

        for i = 0 to V - 2 do
            let u = minDistance dist sptSet

            sptSet.[u] <- true

            for v = 0 to V - 1 do
                if (not (sptSet.[v]) && graph.[u, v] > 0 && dist.[u] <> 999 && dist.[u] + graph.[u, v] < dist.[v]) then
                    dist.[v] <- dist.[u] + graph.[u, v]

        dist
            |> Array.mapi (fun i w ->
                let dst = i |> toPosition
                (dst, w))
            |> Array.toList

    let getLocalCostMap map =
        Array2D.init dim dim (fun y x ->
            match Array2D.tryGet map (translatedY + y) (translatedX + x) with
            | Some v -> v
            // Out of bounds is not moveable
            | None -> NotMoveable)

    costMap
    // Create local cost map from our position and global cost map
    |> getLocalCostMap
    // Create graph from local costmap
    |> createGraph
    |> adjacencyMatrix V
    // Run djikstras algorithm on our local costmap. Inside this map our position is in the middle (dim / 2, dim / 2)
    |> djikstra V <| ((dim / 2, dim / 2) |> toVertex)
    // Filter out positions that are unreachable
    |> List.filter (fun (dst, w) -> w <= maxCost)
    // Translate back to global coordinates
    |> List.map (fun ((x, y), w) -> ((x - dim / 2 + (fst originalPosition), y - dim / 2 + (snd originalPosition)), w))

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