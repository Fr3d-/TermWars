module Fields

open System

type Field = {
    kind: FieldKind
    fgcol: System.ConsoleColor option
    bgcol: System.ConsoleColor
    symbol: string option
}

let getFieldFromKind = function
    | Grass -> {kind = Grass; fgcol = None; bgcol = ConsoleColor.Green; symbol = None}
    | Water -> {kind = Water; fgcol = None; bgcol = ConsoleColor.Blue; symbol = None};
    | Mountain -> {kind = Mountain; fgcol = Some ConsoleColor.DarkGray; bgcol = ConsoleColor.Green; symbol = Some "^"};
    | Tree -> {kind = Tree; fgcol = Some ConsoleColor.DarkGreen; bgcol = ConsoleColor.Green; symbol = Some "¥"}
    | RoadV -> {kind = RoadV; fgcol = Some ConsoleColor.White; bgcol = ConsoleColor.Gray; symbol = Some "-"}
    | RoadH -> {kind = RoadH; fgcol = Some ConsoleColor.White; bgcol = ConsoleColor.Gray; symbol = Some "¦"}


let getName (f : Field) =
    match f.kind with
    | Grass -> "Grass"
    | Water -> "Sea"
    | Mountain -> "Mountain"
    | Tree -> "Tree"
    | RoadV -> "Road"
    | RoadH -> "Road"