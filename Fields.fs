module Fields

open System

type Field = {
    kind: FieldKind
    defensiveBonus: float
    fgcol: System.ConsoleColor option
    bgcol: System.ConsoleColor
    symbol: string option
}

let getFieldFromKind = function
    | Grass -> {kind = Grass; defensiveBonus = 0.10; fgcol = None; bgcol = ConsoleColor.Green; symbol = None}
    | Water -> {kind = Water; defensiveBonus = 0.0; fgcol = None; bgcol = ConsoleColor.Blue; symbol = None};
    | Mountain -> {kind = Mountain; defensiveBonus = 0.30; fgcol = Some ConsoleColor.DarkGray; bgcol = ConsoleColor.Green; symbol = Some "^"};
    | Tree -> {kind = Tree; defensiveBonus = 0.15; fgcol = Some ConsoleColor.DarkGreen; bgcol = ConsoleColor.Green; symbol = Some "¥"}
    | RoadV -> {kind = RoadV; defensiveBonus = 0.0; fgcol = Some ConsoleColor.White; bgcol = ConsoleColor.Gray; symbol = Some "-"}
    | RoadH -> {kind = RoadH; defensiveBonus = 0.0; fgcol = Some ConsoleColor.White; bgcol = ConsoleColor.Gray; symbol = Some "¦"}


let getName (f : Field) =
    match f.kind with
    | Grass -> "Grass"
    | Water -> "Sea"
    | Mountain -> "Mountain"
    | Tree -> "Tree"
    | RoadV -> "Road"
    | RoadH -> "Road"