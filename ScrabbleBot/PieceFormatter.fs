module PieceFormatter

open Types
open ScrabbleUtil

let getIdFromPlacement (placement: Placement) = fst (snd placement)

let getIdsFromMove (move: Move) =
    List.fold (fun acc placement -> List.append acc [ getIdFromPlacement placement ]) [] move

let getPairFormTile (tile: tile) = Set.toList tile |> List.head

let getCharFromId id (pieces: Map<uint32, tile>) =
    let tl = Map.tryFind id pieces

    match tl with
    | Some x -> fst (getPairFormTile x)
    | None -> failwith "No char here"

let getPointValueFromId id (pieces: Map<uint32, tile>) =
    let tl = Map.tryFind id pieces

    match tl with
    | Some x -> snd (getPairFormTile x)
    | None -> failwith "No pv here"

let idLstToString idLst pieces =
    List.fold (fun acc c -> acc + (getCharFromId c pieces).ToString()) "" idLst
