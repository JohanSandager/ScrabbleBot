module internal Walker

open ScrabbleUtil.Dictionary
open PieceFormatter

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
