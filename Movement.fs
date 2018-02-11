module Movement

type MovementPattern = bool [,]

let moveablePositions (pattern: MovementPattern) (currentPosition: Position) =
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