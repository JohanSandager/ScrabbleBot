module internal AwesomeBoard

open ScrabbleUtil


type AwesomeBoard = Map<coord, uint32>


type Direction =
    | Up
    | Down
    | Left
    | Right
