namespace ScrabbleWrappble

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open AwesomeBoard
open MultiSet

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
          awesomeBoard: Map<coord, uint32> // Coordinate to tile id
          dict: Dictionary.Dict
          playerNumber: uint32
          hand: MultiSet.MultiSet<uint32>
          lastPlayed: coord
          isFirstMove: bool }

    let mkState b d pn h aB lP iFM =
        { board = b
          awesomeBoard = aB
          dict = d
          playerNumber = pn
          hand = h
          lastPlayed = lP
          isFirstMove = iFM }

    let board st = st.board
    let dict st = st.dict
    let playerNumber st = st.playerNumber
    let hand st = st.hand
    let ourBoard st = st.awesomeBoard

module Scrabble =
    open StateMonad

    let getPiece (pieces: Map<uint32, tile>) (id: uint32) = Map.find id pieces
    let getCharacter (piece: tile) = Set.toList piece |> List.head |> fst
    let getPointValue (piece: tile) = Set.toList piece |> List.head |> snd

    let rec stepOverList (lst: uint32 list) dict pieces =
        //debugPrint (lst.ToString() + "<- List should look like this \n")

        match lst with
        | x :: xs ->
            let char = getCharacter (getPiece pieces x)
            //debugPrint ("Stepping over: " + char.ToString() + "\n")
            let step = Dictionary.step char dict

            match step with
            | Some(true, newDict) ->
                //debugPrint "Found word \n"
                stepOverList xs newDict pieces
            | Some(false, newDict) ->
                //debugPrint "No word here, moving on... \n"
                stepOverList xs newDict pieces
            | None -> dict
        | [] ->
            //debugPrint "Done returning dict... \n"
            dict

    let getMove pieces id x y =
        let piece = getPiece pieces id
        let char = getCharacter piece
        let pointValue = getPointValue piece
        [ (coord (x, y), (id, (char, pointValue))) ]

    type internal Move = list<((int * int) * (uint32 * (char * int)))>

    /// Recursively tries to build a combination of consecutive letters in the hand that match a word, returns an empty list if no word was found.
    let rec tryFindConsecutiveCombination
        (handMultiset: MultiSet<uint32>)
        (pieces: Map<uint32, tile>)
        (i: uint32)
        (move)
        (dict)
        (word)
        (stepOver)
        =

        let hand = toList handMultiset
        let id = hand.[(int i)]
        let tile = getPiece pieces id
        let char = getCharacter tile
        let newMove = List.append move [ hand.[(int i)] ]
        let newWord = (word + (char.ToString()))
        let dictToUseTemp = stepOverList stepOver dict pieces

        let word =
            List.fold (fun acc x -> acc + (getCharacter (getPiece pieces x)).ToString()) "" stepOver

        //debugPrint ("WROD:: " + word + " trying:; " + word + char.ToString() + "\n")

        //debugPrint ("tryFindCon stepover : " + stepOver.ToString() + "\n")

        match Dictionary.step char dictToUseTemp with
        | Some(true, _) ->
            //debugPrint ("Word found " + word + char.ToString() + "\n")
            newMove
        | Some(false, _) ->
            //debugPrint ("Nothing found for char " + (char.ToString()) + "\n")

            match i with
            | i when (int i) < hand.Length - 1 ->
                //debugPrint "Doing this... \n"

                tryFindConsecutiveCombination handMultiset pieces (i + 1u) newMove dict newWord (stepOver @ [ id ])
            | _ ->
                //debugPrint "Returning... \n"
                []
        | None ->
            //debugPrint ("No word down this path... char: " + char.ToString() + " \n")
            []

    let rec loopOverHand2 (st: State.state) (pieces: Map<uint32, tile>) (index) (stepOver) =
        //debugPrint ("loopOverHand stepover : " + stepOver.ToString() + "\n")
        //let stepOverDict = stepOverList stepOver st.dict pieces
        let hand = toList st.hand

        let word =
            List.fold (fun acc c -> (getCharacter (getPiece pieces c)).ToString() + acc) "" stepOver

        let result =
            tryFindConsecutiveCombination st.hand pieces index [] st.dict word stepOver

        match result with
        | [] ->
            match index with
            | index when (int index) < hand.Length - 1 -> loopOverHand2 st pieces (index + 1u) stepOver
            | _ -> []
        | _ -> result


    let rec wordList
        (direction: Direction)
        (coord: coord)
        (st: State.state)
        (lst: char List)
        (pieces: Map<uint32, tile>)
        =
        let x, y = coord

        match direction with
        | Down ->
            match coordHasPlacedTile (x + 1, y) st.awesomeBoard with
            | true ->
                let tile = getTileFromCoordinate (x + 1, y) st.awesomeBoard
                wordList Down (x + 1, y) st (lst @ [ (getCharacter (getPiece pieces tile)) ]) pieces
            | false -> lst
        | Right ->
            match coordHasPlacedTile (x, y + 1) st.awesomeBoard with
            | true ->
                let tile = getTileFromCoordinate (x, y + 1) st.awesomeBoard
                wordList Right (x, y + 1) st (lst @ [ (getCharacter (getPiece pieces tile)) ]) pieces
            | false -> lst

    let getAppendedWordFromList (lst: char List) (x: char) =
        List.fold (fun acc c -> acc + c.ToString()) (x.ToString()) lst

    let rec isWordValidInAllDirections
        (direction: Direction)
        (coord: coord)
        (st: State.state)
        (lst: uint32 List)
        (pieces: Map<uint32, tile>)
        =
        let x, y = coord

        match direction with
        | Down ->
            match lst with
            | [] -> true
            | z :: zs ->
                let c = (getCharacter (getPiece pieces z))
                let charList = wordList direction (x, y) st [] pieces
                let word = getAppendedWordFromList charList c

                match Dictionary.lookup word st.dict with
                | true -> isWordValidInAllDirections direction (x, y + 1) st zs pieces
                | false -> false
        | Right ->
            match lst with
            | [] -> true
            | z :: zs ->
                let c = (getCharacter (getPiece pieces z))
                let charList = wordList direction (x, y) st [] pieces
                let word = getAppendedWordFromList charList c

                match Dictionary.lookup word st.dict with
                | true -> isWordValidInAllDirections direction (x + 1, y) st zs pieces
                | false -> false


    let rec walker
        (st: State.state)
        (currentCoord: coord)
        (direction: Direction)
        (trail: uint32 List)
        (pieces: Map<uint32, tile>)
        (word)
        (recursionLimit: int)
        =
        let x, y = currentCoord

        debugPrint (
            "Walker was called to action with trail: "
            + trail.ToString()
            + "with direction: "
            + direction.ToString()
            + "\n"
        )

        match direction with
        | Down ->
            //debugPrint ("Im going down, current coord: " + (x, y).ToString() + " \n")

            match coordHasPlacedTile (x, y + 1) st.awesomeBoard with
            | true ->
                //debugPrint ("Tile: " + (x, y + 1).ToString() + " has a placed tile \n")
                walker
                    st
                    (x, y + 1)
                    Down
                    (trail @ [ (getTileFromCoordinate currentCoord st.awesomeBoard) ])
                    pieces
                    word
                    0

            | false ->
                //debugPrint ("Tile: " + (x, y + 1).ToString() + " has no placed tile \n")
                match recursionLimit with
                | x when x > 1 ->
                    debugPrint ("You know the rules and so do I 1")
                    ((x, y), Down, [])
                | _ ->
                    let tempTrail =
                        match tryGetTileFromCoordinate currentCoord st.awesomeBoard with
                        | Some x -> (trail @ [ x ])
                        | None -> trail

                    match loopOverHand2 st pieces 0u tempTrail with
                    | [] -> walker st (x, y) Right [] pieces word (recursionLimit + 1)
                    | lst ->
                        match isWordValidInAllDirections direction (x, y) st lst pieces with
                        | true -> ((x, y), Down, lst)
                        | false -> walker st (x, y) Right [] pieces word (recursionLimit + 1)

        | Right ->
            //debugPrint ("Im going right, current coord: " + (x, y).ToString() + " \n")

            match coordHasPlacedTile (x + 1, y) st.awesomeBoard with
            | true ->
                //debugPrint ("Tile: " + (x + 1, y).ToString() + " has a placed tile \n")

                walker
                    st
                    (x + 1, y)
                    Right
                    (trail @ [ (getTileFromCoordinate currentCoord st.awesomeBoard) ])
                    pieces
                    word
                    0

            | false ->
                //debugPrint ("Tile: " + (x + 1, y).ToString() + " has no placed tile \n")
                match recursionLimit with
                | x when x > 1 ->
                    debugPrint ("You know the rules and so do I 2")
                    ((x, y), Down, [])
                | _ ->
                    let tempTrail =
                        match tryGetTileFromCoordinate currentCoord st.awesomeBoard with
                        | Some x -> (trail @ [ x ])
                        | None -> trail

                    match loopOverHand2 st pieces 0u tempTrail with
                    | [] -> walker st (x, y) Down [] pieces word (recursionLimit + 1)
                    | lst ->
                        match isWordValidInAllDirections direction (x, y) st lst pieces with
                        | true -> ((x, y), Right, lst)
                        | false -> walker st (x, y) Down [] pieces word (recursionLimit + 1)

    let getPlayableMove (direction: Direction) pieces movesLst fromPos (st: State.state) =
        let x, y = fromPos

        debugPrint (
            "Get playable move "
            + x.ToString()
            + " "
            + y.ToString()
            + " "
            + st.isFirstMove.ToString()
        )

        let (aux, _, _) =
            match direction with
            | Down ->
                match st.isFirstMove with
                | true ->
                    (List.fold
                        (fun (lst, a, b) id -> ((List.append (getMove pieces id a b) lst), a, b + 1))
                        ([], x, y)
                        movesLst)
                | false ->
                    (List.fold
                        (fun (lst, a, b) id -> ((List.append (getMove pieces id a b) lst), a, b + 1))
                        ([], x, y + 1)
                        movesLst)
            | Right ->
                match st.isFirstMove with
                | true ->
                    (List.fold
                        (fun (lst, a, b) id -> ((List.append (getMove pieces id a b) lst), a + 1, b))
                        ([], x, y)
                        movesLst)
                | false ->
                    (List.fold
                        (fun (lst, a, b) id -> ((List.append (getMove pieces id a b) lst), a + 1, b))
                        ([], x + 1, y)
                        movesLst)

        match aux with
        | [] -> SMPass
        | _ -> SMPlay aux


    let rec tempRem lst mtst =
        match lst with
        | x :: xs -> tempRem xs (MultiSet.removeSingle x mtst)
        | [] -> mtst

    let playGame cstream (pieces: Map<uint32, tile>) (st: State.state) =

        let fuck =
            match tryGetTileFromCoordinate st.board.center st.awesomeBoard with
            | Some x -> [ x ]
            | None -> []

        let rec aux (st: State.state) =
            Print.printHand pieces (State.hand st)
            //let result = loopOverHand st pieces 0u
            let (coord, dirction, result) = walker st st.board.center Down fuck pieces "" 0

            let ourMove = getPlayableMove dirction pieces result coord st

            let newHand = tempRem result st.hand
            debugPrint ("\n-------------- DEBUG START -----------------\n")
            debugPrint (dirction.ToString())
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
                    List.fold (fun acc x -> Map.add (fst x) (fst (snd x)) acc) st.awesomeBoard ms

                debugPrint ((fst ms.[0]).ToString())

                let st' =
                    State.mkState
                        st.board
                        st.dict
                        st.playerNumber
                        (List.fold (fun acc (x, k) -> MultiSet.add x k acc) newHand newPieces)
                        newMap
                        (fst (fst ms.[0]), (snd (fst ms.[0])) + 1)
                        false

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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet Map.empty board.center true)
