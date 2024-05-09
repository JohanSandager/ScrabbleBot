module internal Walker

open ScrabbleUtil.Dictionary
open PieceFormatter

let copyList lst =
    List.fold (fun acc element -> List.append acc [ element ]) lst


(*let getFirstMove hand = 
        let hand' = copyList hand
        findLongestWord*)

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

//let rec findLongestWord hand word longestWordYet =
let rec findWordRecursive hand word dict pieces =
    match hand with
    | [] -> word
    | lst ->
        match tryMakeNewWordFromHand lst word 0 dict pieces with
        | -1 -> word
        | x ->
            printf "Index %A hand is %A \n" x lst
            let word' = (word @ [ hand.[x] ])
            let hand' = List.removeAt x lst
            findWordRecursive hand' word' dict pieces
