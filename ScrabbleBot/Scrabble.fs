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
          lastPlayed: coord }

    let mkState b d pn h aB lP =
        { board = b
          awesomeBoard = aB
          dict = d
          playerNumber = pn
          hand = h
          lastPlayed = lP }

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
    let skip: Move = [ ((1, 1), (uint32 1, ('-', 1))) ]

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
        let hand = MultiSet.toList handMultiset
        let id = hand.[(int i)]
        let tile = getPiece pieces id
        let char = getCharacter tile
        let newMove = List.append move [ hand.[(int i)] ]
        let newWord = (word + (char.ToString()))
        let dictToUseTemp = stepOverList stepOver dict pieces

        //debugPrint ("Trying: " + newWord + "\n")
        //ÆdebugPrint ("Hand: " + hand.ToString() + "\n")

        match Dictionary.step char dictToUseTemp with
        | Some(true, _) ->
            //debugPrint ("Word found " + newWord + "\n")
            newMove
        | Some(false, newDict) ->
            //debugPrint ("Nothing found for char " + (char.ToString()) + "\n")

            match i with
            | i when (int i) < hand.Length - 1 ->
                //debugPrint "Doing this... \n"
                tryFindConsecutiveCombination handMultiset pieces (i + 1u) newMove newDict newWord stepOver
            | _ ->
                //debugPrint "Returning... \n"
                []
        | None ->
            //debugPrint "No word down this path... \n"
            []

    /// Recursively loops over every tile in the hand returning an empty list if no move was found, or a list of uint32 ids mapping to placable tiles.
    let rec loopOverHand (st: State.state) (pieces: Map<uint32, tile>) (index) =
        let alreadInBoard =
            Map.fold (fun acc _ id -> List.append [ id ] acc) [] st.awesomeBoard

        debugPrint ("Already in board: " + alreadInBoard.ToString() + "\n")
        let stepOverDict = stepOverList (List.rev alreadInBoard) st.dict pieces
        let hand = MultiSet.toList st.hand

        let result =
            tryFindConsecutiveCombination st.hand pieces index [] stepOverDict "" (List.rev alreadInBoard)

        match result with
        | [] ->
            match index with
            | index when (int index) < hand.Length - 1 -> loopOverHand st pieces (index + 1u)
            | _ -> []
        | _ -> result

    let rec loopOverHand2 (st: State.state) (pieces: Map<uint32, tile>) (index) (stepOver) =
        let stepOverDict = stepOverList stepOver st.dict pieces
        let hand = MultiSet.toList st.hand

        let result =
            tryFindConsecutiveCombination st.hand pieces index [] stepOverDict "" stepOver

        match result with
        | [] ->
            match index with
            | index when (int index) < hand.Length - 1 -> loopOverHand st pieces (index + 1u)
            | _ -> []
        | _ -> result


    let largestOfThree (lst1: 'a list) (lst2: 'a list) (lst3: 'a list) =
        match lst1.Length with
        | x when x > lst2.Length && x > lst3.Length -> lst1
        | x when x > lst2.Length && x < lst3.Length -> lst3
        | _ -> lst2

    let partialTryFindConsecutive (st: State.state) (pieces) (move) =
        tryFindConsecutiveCombination st.hand pieces 0u move st.dict ""

    let rec goDown (direction: Direction) (trail: uint32 List) = []

    let rec walker
        (st: State.state)
        (currentCoord: coord)
        (direction: Direction)
        (trail: uint32 List)
        (pieces: Map<uint32, tile>)
        (word)
        (stepOver)
        =
        let x, y = currentCoord

        match direction with
        | Up ->
            debugPrint "Im going up... \n"

            match coordHasPlacedTile (x, y + 1) st.awesomeBoard with
            | true -> launchAndCompare st (x, y + 1) direction trail pieces word stepOver
            | false ->
                match loopOverHand2 st pieces 0u trail with
                | [] -> goDown Down trail
                | lst -> largestOfThree lst (goDown Down trail) []
        | Down ->
            debugPrint "Im going down... \n"

            match coordHasPlacedTile (x, y - 1) st.awesomeBoard with
            | true -> launchAndCompare st (x, y - 1) direction trail pieces word stepOver
            | false ->
                match loopOverHand2 st pieces 0u trail with
                | [] -> goDown Up trail
                | lst -> largestOfThree lst (goDown Up trail) []
        | Left ->
            debugPrint "Im going left... \n"

            match coordHasPlacedTile (x - 1, y) st.awesomeBoard with
            | true -> launchAndCompare st (x - 1, y) direction trail pieces word stepOver
            | false ->
                match loopOverHand2 st pieces 0u trail with
                | [] -> goDown Right trail
                | lst -> largestOfThree lst (goDown Right trail) []
        | Right ->
            debugPrint "Im going right... \n"

            match coordHasPlacedTile (x + 1, y) st.awesomeBoard with
            | true -> launchAndCompare st (x + 1, y) direction trail pieces word stepOver
            | false ->
                match loopOverHand2 st pieces 0u trail with
                | [] -> goDown Left trail
                | lst -> largestOfThree lst (goDown Left trail) []

    and launchAndCompare
        (st: State.state)
        (currentCoord: coord)
        (direction: Direction)
        (trail: uint32 List)
        (pieces: Map<uint32, tile>)
        (word)
        (stepOver)
        =
        match direction with
        | Up ->
            let up = walker st currentCoord Up [] pieces word stepOver
            let left = walker st currentCoord Left [] pieces word stepOver
            let right = walker st currentCoord Right [] pieces word stepOver
            largestOfThree up left right
        | Down ->
            let down = walker st currentCoord Down trail pieces word stepOver
            let left = walker st currentCoord Left [] pieces word stepOver
            let right = walker st currentCoord Right [] pieces word stepOver
            largestOfThree down left right
        | Right ->
            let right = walker st currentCoord Right trail pieces word stepOver
            let up = walker st currentCoord Up [] pieces word stepOver
            let down = walker st currentCoord Down [] pieces word stepOver
            largestOfThree right up down
        | Left ->
            let left = walker st currentCoord Left trail pieces word stepOver
            let up = walker st currentCoord Up [] pieces word stepOver
            let down = walker st currentCoord Down [] pieces word stepOver
            largestOfThree left up down

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
            //let result = loopOverHand st pieces 0u
            let result = walker st st.board.center Up [] pieces "" []
            let ourMove = getPlayableMove pieces result st.lastPlayed

            let newHand = tempRem result st.hand
            debugPrint ("\n-------------- DEBUG START -----------------\n")

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
