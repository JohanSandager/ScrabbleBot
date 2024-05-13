module internal AwesomeBoard

open ScrabbleUtil


type AwesomeBoard = Map<coord, uint32>


type Direction =
    | Up
    | Down
    | Left
    | Right

let tryGetTile (board: AwesomeBoard) coord = Map.tryFind coord board
