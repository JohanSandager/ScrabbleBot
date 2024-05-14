module internal Walker

open ScrabbleUtil.Dictionary
open PieceFormatter
open AwesomeBoard

let copyList lst =
    List.fold (fun acc element -> List.append acc [ element ]) lst

let getLastElement charLst = List.rev charLst |> List.head

let getLstWithoutLastElement charLst =
    List.rev charLst |> List.tail |> List.rev

let rec stepOverLst lst dict =
    match lst with
    | x :: xs ->
        match step x dict with
        | Some(_, dct) -> stepOverLst xs dct
        | None -> dict
    | [] -> dict

let getLargest (lst1: 'a list) (lst2: 'a list) =
    match lst1.Length with
    | x when x > lst2.Length -> lst1
    | _ -> lst2

let getLargestDirTouple ((a1, b1, c1): 'a * 'b * 'c list) ((a2, b2, c2): 'a * 'b * 'c list) =
    match c1.Length with
    | x when x > c2.Length -> (a1, b1, c1)
    | _ -> (a2, b2, c2)

let getLargest3 (lst1: 'a list) (lst2: 'a list) (lst3: 'a list) = getLargest lst1 (getLargest lst2 lst3)

let tryRemoveElement element (lst: 'a list) =
    match List.tryFindIndex (fun el -> el = element) lst with
    | Some x -> List.removeAt x lst
    | None -> lst

let getFirstWord (hand: uint32 list) dict pieces (trail: uint32 list) =
    let stepOverDct =
        stepOverLst (List.fold (fun acc x -> acc @ [ (getCharFromId x pieces) ]) [] trail) dict

    let rec aux (hand: uint32 list) dict pieces (trail: uint32 list) =
        List.fold
            (fun acc c ->
                let char = getCharFromId c pieces
                let hand' = tryRemoveElement c hand

                match step char dict with
                | Some(true, dct) -> getLargest3 (aux hand' dct pieces (trail @ [ c ])) (trail @ [ c ]) acc
                | Some(false, dct) -> getLargest (aux hand' dct pieces (trail @ [ c ])) acc
                | None -> acc)
            []
            hand

    aux hand stepOverDct pieces []

let getMove pieces id x y =
    let char = getCharFromId id pieces
    let pointValue = getPointValueFromId id pieces
    [ ((x, y), (id, (char, pointValue))) ]

let getMoves lst pieces coord dir =
    let mx, my = coord

    let (lst, _, _) =
        match dir with
        | Right -> List.fold (fun (lst, x, y) z -> (lst @ (getMove pieces z x y)), x + 1, y) ([], mx, my) lst
        | Down -> List.fold (fun (lst, x, y) z -> (lst @ (getMove pieces z x y)), x, y + 1) ([], mx, my) lst

    lst

let isLongEnough (str: string) = str.Length > 1


let isLetterValid board coord dict pieces =
    //printf "----------------------------IS VALID----------------------------\n"

    let rec goRight trail coord =
        let x, y = coord
        //printf "COORD RIGHT: %A\n" coord

        match tryGetTile board (x + 1, y) with
        | Some s -> goRight (trail @ [ s ]) (x + 1, y)
        | None ->
            //printf "RIGHT: %A\n" (idLstToString trail pieces)
            trail

    let rec goLeft trail coord =
        let x, y = coord
        //printf "COORD LEFT: %A\n" coord

        match tryGetTile board (x - 1, y) with
        | Some s -> goLeft ([ s ] @ trail) (x - 1, y)
        | None ->
            //printf "LEFT: %A\n" (idLstToString trail pieces)
            trail

    let rec goUp trail coord =
        let x, y = coord
        //printf "COORD UP: %A\n" coord

        match tryGetTile board (x, y - 1) with
        | Some s -> goUp ([ s ] @ trail) (x, y - 1)
        | None ->
            //printf "UP: %A\n" (idLstToString trail pieces)
            trail

    let rec goDown trail coord =
        let x, y = coord
        //printf "COORD DOWN: %A\n" coord

        match tryGetTile board (x, y + 1) with
        | Some s -> goDown (trail @ [ s ]) (x, y + 1)
        | None ->
            //printf "DOWN: %A\n" (idLstToString trail pieces)
            trail

    let xWord =
        idLstToString ((goLeft [] coord) @ [ (getTile board coord) ] @ (goRight [] coord)) pieces

    let yWord =
        idLstToString ((goUp [] coord) @ [ (getTile board coord) ] @ (goDown [] coord)) pieces

    //printf "X: %A; Y: %A;\n" xWord yWord
    //printf "--------------------------IS VALID END--------------------------\n"

    (lookup xWord dict || not (isLongEnough xWord))
    && (lookup yWord dict || not (isLongEnough yWord))

let isValid board cds dict pieces =
    List.forall (fun x -> isLetterValid board x dict pieces) cds

let updateBoard oldBoard ms =
    List.fold (fun acc x -> Map.add (fst x) (getIdFromPlacement x) acc) oldBoard ms

let getCoordsFromMoveLst lst =
    List.fold (fun acc mv -> acc @ [ fst mv ]) [] lst

let isValidElseEmpty board word dict pieces direction coord =
    let ms = getMoves word pieces coord direction

    let anyOccupiedTiles =
        List.forall (fun x -> Map.containsKey x board) (getCoordsFromMoveLst ms)

    match
        (isValid (updateBoard board ms) (getCoordsFromMoveLst ms) dict pieces)
        && not anyOccupiedTiles
    with
    | true -> word
    | false -> []

let walk (board: AwesomeBoard) (coord: int * int) hand dict pieces =
    let rec right (board: AwesomeBoard) (coord: int * int) trail hand dict pieces longestWord =
        let (x, y) = coord

        match tryGetTile board coord with
        | Some s ->
            //printf "Right %A\n" (idLstToString (getFirstWord hand dict pieces [ s ]) pieces)
            //printf "DOWN: %A\n" (right board (x, y + 1) (trail @ [ s ]) hand dict pieces longestWord)
            getLargestDirTouple
                (right board (x + 1, y) (trail @ [ s ]) hand dict pieces longestWord)
                (down board (x, y + 1) ([ s ]) hand dict pieces longestWord)
        | None ->
            getLargestDirTouple
                (coord, Right, (isValidElseEmpty (board) (getFirstWord hand dict pieces trail) dict pieces Right coord))
                longestWord

    and down (board: AwesomeBoard) (coord: int * int) trail hand dict pieces longestWord =
        let (x, y) = coord

        match tryGetTile board coord with
        | Some s ->
            //printf "DOWN: %A\n" (idLstToString (getFirstWord hand dict pieces [ s ]) pieces)
            //printf "DOWN: %A\n" (right board (x, y + 1) (trail @ [ s ]) hand dict pieces longestWord)

            getLargestDirTouple
                (right board (x + 1, y) ([ s ]) hand dict pieces longestWord)
                (down board (x, y + 1) (trail @ [ s ]) hand dict pieces longestWord)
        | None ->
            getLargestDirTouple
                (coord, Down, (isValidElseEmpty (board) (getFirstWord hand dict pieces trail) dict pieces Down coord))
                longestWord

    right board coord [] hand dict pieces ((0, 0), Right, [])
