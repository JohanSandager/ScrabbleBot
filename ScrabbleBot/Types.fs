module internal Types

open StateMonad

type coord = int * int
type Placement = coord * (uint32 * (char * int))
type Move = list<Placement>
type word = (char * int) list
type tile = (char * int)

type squareFun = word -> int -> int -> Result<int, Error>
type square = Map<int, squareFun>

type boardFun = coord -> Result<square option, Error>

type board =
    { center: coord
      defaultSquare: square
      squares: boardFun }
