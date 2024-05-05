namespace ScrabbleWrappble

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open AwesomeBoard

open System.IO

open ScrabbleUtil.DebugPrint

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)

        if m.Success then
            Some(List.tail [ for g in m.Groups -> g.Value ])
        else
            None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?"

        Regex.Matches(ts, pattern)
        |> Seq.cast<Match>
        |> Seq.map (fun t ->
            match t.Value with
            | Regex pattern [ x; y; id; c; p ] -> ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
            | _ -> failwith "Failed (should never happen)")
        |> Seq.toList

module Print =

    let printHand pieces hand =
        hand
        |> MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State =
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state =
        { board: Parser.board
          ourBoard: Map<coord, uint32> // Coordinate to tile id
          dict: Dictionary.Dict
          playerNumber: uint32
          hand: MultiSet.MultiSet<uint32>
          lastPlayed: coord }

    let mkState b d pn h oB lP =
        { board = b
          ourBoard = oB
          dict = d
          playerNumber = pn
          hand = h
          lastPlayed = lP }

    let board st = st.board
    let dict st = st.dict
    let playerNumber st = st.playerNumber
    let hand st = st.hand
    let ourBoard st = st.ourBoard

module Scrabble =
    open StateMonad


    let getPiece (pieces: Map<uint32, tile>) (id: uint32) = Map.find id pieces
    let getCharacter (piece: tile) = Set.toList piece |> List.head |> fst
    let getPointValue (piece: tile) = Set.toList piece |> List.head |> snd

    let rec stepOverList (lst: uint32 list) dict pieces =
        // debugPrint (lst.ToString() + "<- List should look like this \n")

        match lst with
        | x :: xs ->
            let char = getCharacter (getPiece pieces x)
            debugPrint ("Stepping over: " + char.ToString() + "\n")
            let step = Dictionary.step char dict

            match step with
            | Some(true, newDict) ->
                debugPrint "Found word \n"
                stepOverList xs newDict pieces
            | Some(false, newDict) ->
                debugPrint "No word here, moving on... \n"
                stepOverList xs newDict pieces
            | None -> dict
        | [] ->
            debugPrint "Done returning dict... \n"
            dict

    let getMove pieces id x y =
        let piece = getPiece pieces id
        let char = getCharacter piece
        let pointValue = getPointValue piece
        [ (coord (x, y), (id, (char, pointValue))) ]

    type internal Move = list<((int * int) * (uint32 * (char * int)))>
    let skip: Move = [ ((1, 1), (uint32 1, ('-', 1))) ]

    /// Recursively tries to build a combination of consecutive letters in the hand that match a word, returns an empty list if no word was found.
    let rec tryFindConsecutiveCombination
        (hand: uint32 list)
        (pieces: Map<uint32, tile>)
        (i: uint32)
        (move)
        (dict)
        (word)
        (stepOver)
        =
        let id = hand.[(int i)]
        let tile = getPiece pieces id
        let char = getCharacter tile
        let newMove = List.append move [ hand.[(int i)] ]
        let newWord = (word + (char.ToString()))
        let dictToUseTemp = stepOverList stepOver dict pieces

        // debugPrint ("Trying: " + newWord + "\n")
        // debugPrint ("Hand: " + hand.ToString() + "\n")

        match Dictionary.step char dictToUseTemp with
        | Some(true, _) ->
            // debugPrint ("Word found " + newWord + "\n")
            newMove
        | Some(false, newDict) ->
            // debugPrint ("Nothing found for char " + (char.ToString()) + "\n")

            match i with
            | i when (int i) < hand.Length - 1 ->
                // debugPrint "Doing this... \n"
                tryFindConsecutiveCombination hand pieces (i + 1u) newMove newDict newWord stepOver
            | _ ->
                // debugPrint "Returning... \n"
                []
        | None ->
            // debugPrint "No word down this path... \n"
            []

    /// Recursively loops over every tile in the hand returning an empty list if no move was found, or a list of uint32 ids mapping to placable tiles.
    let rec loopOverHand (st: State.state) (pieces: Map<uint32, tile>) (index) =
        let alreadInBoard = Map.fold (fun acc _ id -> List.append [ id ] acc) [] st.ourBoard
        // debugPrint ("Already in board: " + alreadInBoard.ToString() + "\n")
        let stepOverDict = stepOverList (List.rev alreadInBoard) st.dict pieces
        let hand = MultiSet.toList st.hand

        let result =
            tryFindConsecutiveCombination hand pieces index [] stepOverDict "" (List.rev alreadInBoard)

        match result with
        | [] ->
            match index with
            | index when (int index) < hand.Length - 1 -> loopOverHand st pieces (index + 1u)
            | _ -> []
        | _ -> result


    (*
    let rec walker
        (currentCoord: coord)
        placedTiles
        (direction: Direction)
        (trail: uint32 List)
        (hand: uint32 list)
        (pieces: Map<uint32, tile>)
        (i: uint32)
        (move)
        (dict)
        (word)
        =
        let (x, y) = currentCoord

        match direction with
        | Up ->
            match tryGetTileFromCoordinate (x, y + 1) placedTiles with
            | Some v -> walker (x, y + 1) placedTiles (trail :: v)
            | None -> tryFindConsecutiveCombination hand
        | Down -> coordHasPlacedTile (x, y - 1) placedTiles
        | Left -> coordHasPlacedTile (x - 1, y) placedTiles
        | Right -> coordHasPlacedTile (x + 1, y) placedTiles
        *)

    let getPlayableMove pieces movesLst centerPos =
        let x, y = centerPos

        let aux =
            fst (List.fold (fun (lst, a) id -> (List.append (getMove pieces id x a) lst, a + 1)) ([], y) movesLst)

        match aux with
        | [] -> SMPass
        | _ -> SMPlay aux


    let rec tempRem lst mtst =
        match lst with
        | x :: xs -> tempRem xs (MultiSet.removeSingle x mtst)
        | [] -> mtst

    let playGame cstream (pieces: Map<uint32, tile>) (st: State.state) =

        let rec aux (st: State.state) =
            Print.printHand pieces (State.hand st)
            let result = loopOverHand st pieces 0u

            let ourMove = getPlayableMove pieces result st.lastPlayed

            let newHand = tempRem result st.hand
            debugPrint ("\n-------------- DEBUG START -----------------\n")

            let testDic1 =
                match (Dictionary.step 'B' st.dict) with
                | Some(false, newDict) -> newDict

            let testDic2 =
                match (Dictionary.step 'E' testDic1) with
                | Some(false, newDict) -> newDict

            let testDic3 =
                match (Dictionary.step 'N' testDic2) with
                | Some(true, newDict) ->
                    debugPrint ("Yayy")
                    newDict

            let testDic4 =
                match (Dictionary.step 'N' testDic3) with
                | Some(true, newDict) ->
                    debugPrint "It is a word... \n"
                    newDict
                | Some(false, newDict) ->
                    debugPrint "WTF \n"
                    newDict
                | None ->
                    debugPrint "I don't even know no more... \n"
                    st.dict

            let testDic5 =
                match (Dictionary.step 'E' testDic4) with
                | Some(true, newDict) ->
                    debugPrint "It is a word... \n"
                    newDict
                | Some(false, newDict) ->
                    debugPrint "WTF \n"
                    newDict
                | None ->
                    debugPrint "I don't even know no more... \n"
                    st.dict


            debugPrint (result.ToString())

            debugPrint ("\n-------------- DEBUG END -----------------\n")

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint
                "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"

            let input = System.Console.ReadLine()

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) ourMove) // keep the debug lines. They are useful.
            send cstream (ourMove)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) ourMove) // keep the debug lines. They are useful.

            match msg with
            | RCM(CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let newMap =
                    List.fold (fun acc x -> Map.add (fst x) (fst (snd x)) acc) st.ourBoard ms

                debugPrint ((fst ms.[0]).ToString())

                let st' =
                    State.mkState
                        st.board
                        st.dict
                        st.playerNumber
                        (List.fold (fun acc (x, k) -> MultiSet.add x k acc) newHand newPieces)
                        newMap
                        (fst (fst ms.[0]), (snd (fst ms.[0])) + 1)

                debugPrint (newMap.ToString())
                aux st'
            | RCM(CMPlayed(pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM(CMPlayFailed(pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM(CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err ->
                printfn "Gameplay Error:\n%A" err
                aux st


        aux st

    let startGame
        (boardP: boardProg)
        (dictf: bool -> Dictionary.Dict)
        (numPlayers: uint32)
        (playerNumber: uint32)
        (playerTurn: uint32)
        (hand: (uint32 * uint32) list)
        (tiles: Map<uint32, tile>)
        (timeout: uint32 option)
        (cstream: Stream)
        =
        debugPrint (
            sprintf
                "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n"
                numPlayers
                playerNumber
                playerTurn
                hand
                timeout
        )

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP

        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet Map.empty board.center)
