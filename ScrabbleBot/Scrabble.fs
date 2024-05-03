namespace ScrabbleWrappble

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.
// let bruteForce (hand: MultiSet<uint32>) (pieces: Map<uint32, 'a>) (st: State.state) =
//toList hand |> List.head |> getPiece pieces

//fold (fun _ x -> if (ScrabbleUtil.Dictionary.lookup (getCharacter x) st.dict) then forcePrint("match found!")) () hand


(*let rec aux c = 
            match ScrabbleUtil.Dictionary.lookup st.dict (getCharacter c ) with 
            | True -> forcePrint("Match found!!!")
            | False -> aux*)

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
          dict: ScrabbleUtil.Dictionary.Dict
          playerNumber: uint32
          hand: MultiSet.MultiSet<uint32> }

    let mkState b d pn h =
        { board = b
          dict = d
          playerNumber = pn
          hand = h }

    let board st = st.board
    let dict st = st.dict
    let playerNumber st = st.playerNumber
    let hand st = st.hand

module Scrabble =
    open StateMonad

    let getPiece (pieces: Map<uint32, tile>) (id: uint32) = Map.find id pieces
    let getCharacter (piece: tile) = Set.toList piece |> List.head |> fst
    let getPointValue (piece: tile) = Set.toList piece |> List.head |> snd

    let getMove pieces id x y =
        let piece = getPiece pieces id
        let char = getCharacter piece
        let pointValue = getPointValue piece
        [ (coord (x, y), (id, (char, pointValue))) ]

    type internal Move = list<((int * int) * (uint32 * (char * int)))>
    type internal Heuristic = Move -> Move -> bool
    type internal Algorithm<'a> = Heuristic -> State.state -> Map<uint32, 'a> -> Result<Move, Error>
    let skip: Move = [ ((1, 1), (uint32 1, ('-', 1))) ]

    let lastWord: Heuristic =
        fun (move_one: Move) (move_two: Move) -> failwith "Unimplemented"

    (*let bruteforce: Algorithm<'a> =
        fun (heuristic: Heuristic) (st: State.state) (pieces: Map<uint32, 'a>) ->
            let hand = MultiSet.toList st.hand
            let bestMove = skip
            let wordList = []

            for i in 0 .. hand.Length do
                let tile = getPiece pieces hand.[i]
                let char = getCharacter tile
                let tempHand = List.removeAt i hand

                for j in 0 .. tempHand.Length do
                    let tile2 = getPiece pieces hand.[j]
                    let char2 = getCharacter tile2
                    let tempHand2 = List.removeAt j hand

                    for k in 0 .. tempHand2.Length do
                        let tile3 = getPiece pieces hand.[k]
                        let char3 = getCharacter tile3
                        let word = char.ToString() + char2.ToString() + char3.ToString()
                        let isWord = Dictionary.lookup word st.dict

                        if isWord then 
                            

            Success bestMove*)

    let rec tryRec (st: State.state) (word: string) (pieces: Map<uint32, tile>) (i: uint32) (move) =
        let hand = MultiSet.toList st.hand
        let id = hand.[(int i)]
        let tile = getPiece pieces id
        let char = getCharacter tile
        let newWord = (word + (char.ToString()))

        let newMove = List.append move [ hand.[(int i)] ]

        match Dictionary.lookup newWord st.dict with
        | true -> newMove
        | false ->
            match i with
            | i when (int i) < hand.Length - 1 -> tryRec st newWord pieces (i + 1u) newMove
            | _ -> []

    let rec wrapTryRec (st: State.state) (pieces: Map<uint32, tile>) (index) =
        let hand = MultiSet.toList st.hand
        let result = tryRec st "" pieces index []

        match result with
        | [] ->
            match index with
            | index when (int index) < hand.Length - 1 -> wrapTryRec st pieces (index + 1u)
            | _ -> []
        | _ -> result


    let findBestMove
        (pieces: Map<uint32, 'a>)
        (st: State.state)
        (algorithm: Algorithm<'a>)
        (heuristic: Heuristic)
        : Result<Move, Error> =
        algorithm heuristic st pieces

    (*let findBestMoveOrSkip (pieces: Map<uint32, 'a>) (st: State.state) =
        match findBestMove pieces st bruteforce lastWord with
        | Success move -> move
        | Failure _ -> skip*)

    let rec tempRem lst mtst =
        match lst with
        | x :: xs -> tempRem xs (MultiSet.removeSingle x mtst)
        | [] -> mtst

    let playGame cstream (pieces: Map<uint32, tile>) (st: State.state) =

        let rec aux (st: State.state) =
            Print.printHand pieces (State.hand st)
            debugPrint ("\n-------------- DEBUG START -----------------\n")
            let result = wrapTryRec st pieces 0u

            let ourMove =
                fst (List.fold (fun (lst, a) id -> (List.append (getMove pieces id 0 a) lst, a + 1)) ([], 0) result)

            let newHand = tempRem result st.hand
            debugPrint (newHand.ToString())

            debugPrint (result.ToString())

            debugPrint ("\n-------------- DEBUG END -----------------\n")

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint
                "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"

            // let input = System.Console.ReadLine()
            let move =
                match ourMove with
                | [] -> SMPass
                | _ -> SMPlay ourMove

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM(CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' =
                    State.mkState
                        st.board
                        st.dict
                        st.playerNumber
                        (List.fold (fun acc (x, k) -> MultiSet.add x k acc) newHand newPieces)

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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet)
