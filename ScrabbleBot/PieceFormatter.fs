module PieceFormatter

open Types

let getIdFromPlacement (placement: Placement) = fst (snd placement)

let getIdsFromMove (move: Move) =
    List.fold (fun acc placement -> List.append acc [ getIdFromPlacement placement ]) [] move

let getCharFromId id (pieces: Map<uint32, tile>) =
    let tl = Map.tryFind id pieces

    match tl with
    | Some x -> fst x
    | None -> failwith "No char here"

let idLstToString idLst pieces =
    List.fold (fun acc c -> acc + (getCharFromId c pieces).ToString()) "" idLst
