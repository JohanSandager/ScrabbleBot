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
        match Dict.step x dict with
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

let rec getFirstWord (hand: uint32 list) dict pieces (trail: uint32 list) =
    List.fold
        (fun acc c ->

            let char = getCharFromId c pieces
            let hand' = tryRemoveElement c hand
            printf "---TRYING FOR %A; ACC %A---\n" char acc

            match step char dict with
            | Some(true, dct) ->
                printf ">>> TRUE %A%A <<<\n" (idLstToString trail pieces) char
                getLargest3 (getFirstWord hand' dct pieces (trail @ [ c ])) (trail @ [ c ]) acc
            | Some(false, dct) ->
                printf "ooo FALSE %A%A ooo\n" (idLstToString trail pieces) char
                getLargest (getFirstWord hand' dct pieces (trail @ [ c ])) acc
            | None -> acc)
        []
        hand





/// Tries the current word plus all letters in hand and return the index of one that makes a valid word
let rec tryMakeNewWordFromHand hand word index dict pieces =
    match hand with
    | [] -> -1
    | lst ->
        printf
            "Looking at index %A for length %A and word %A \n"
            index
            hand.Length
            (idLstToString (word @ [ hand.[index] ]) pieces)

        match lookup (idLstToString (word @ [ hand.[index] ]) pieces) dict with
        | false when index < hand.Length - 1 -> tryMakeNewWordFromHand hand word (index + 1) dict pieces
        | true ->
            printf "Found word word index %A \n" index
            index
        | _ -> -1
