module internal AwesomeBoard

open ScrabbleUtil


type AwesomeBoard = Map<coord, uint32>


type Direction =
    | Up
    | Down
    | Left
    | Right

let coordHasPlacedTile (coord: coord) (placedTiles: Map<coord, uint32>) =
    let result = placedTiles.TryFind coord

    match result with
    | Some _ -> true
    | None -> false

let tryGetTileFromCoordinate (coord: coord) (board: AwesomeBoard) = board.TryFind coord

