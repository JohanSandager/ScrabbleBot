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

let getMoves lst pieces coord =
    let mx, my = coord

    let (lst, _, _) =
        List.fold (fun (lst, x, y) z -> (lst @ (getMove pieces z x y)), x + 1, y) ([], mx, my) lst

    lst

let rec walk (board: AwesomeBoard) (coord: int * int) trail hand dict pieces =
    let (x, y) = coord

    match tryGetTile board coord with
    | Some s -> walk board (x + 1, y) (trail @ [ s ]) hand dict pieces
    | None -> (coord, getFirstWord hand dict pieces trail)
