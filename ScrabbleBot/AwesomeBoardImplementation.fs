module internal AwesomeBoard

open ScrabbleUtil


type AwesomeBoard = Map<coord, uint32>


type Direction =
    | Down
    | Right

let tryGetTile (board: AwesomeBoard) coord = Map.tryFind coord board

let getTile (board: AwesomeBoard) coord =
    match tryGetTile board coord with
    | Some x -> x
    | None -> 0u
