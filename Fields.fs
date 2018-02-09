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
    | Sea -> {kind = Sea; fgcol = None; bgcol = ConsoleColor.Blue; symbol = None};
    | Mountain -> {kind = Mountain; fgcol = Some ConsoleColor.DarkGray; bgcol = ConsoleColor.Green; symbol = Some "^"};
    | Tree -> {kind = Tree; fgcol = Some ConsoleColor.DarkGreen; bgcol = ConsoleColor.Green; symbol = Some "¥"}
    | RoadV -> {kind = RoadV; fgcol = Some ConsoleColor.White; bgcol = ConsoleColor.Gray; symbol = Some "-"}
    | RoadH -> {kind = RoadH; fgcol = Some ConsoleColor.White; bgcol = ConsoleColor.Gray; symbol = Some "¦"}