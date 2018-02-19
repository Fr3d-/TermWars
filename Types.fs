[<AutoOpen>]
module Types

type Team =
    | Friendly
    | Enemy

type FieldKind =
    | Grass
    | Water
    | Mountain
    | Tree
    | RoadV
    | RoadH

type EntityKind =
    | Infantry
    | Jeep
    | Tank
    | Base

type SquareKind =
    | EmptyKind of FieldKind
    | OccupiedKind of FieldKind * EntityKind * Team

type Position = int * int

module Vec2 =
    let add (v1 : Position) (v2 : Position) =
        let (x1, y1) = v1
        let (x2, y2) = v2

        ((x1 + x2, y1 + y2) : Position)