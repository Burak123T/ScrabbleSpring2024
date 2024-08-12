namespace LexiIngenium

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open ScrabbleUtil.Dictionary
open Eval
open Parser


open System.IO

open ScrabbleUtil.DebugPrint



// The RegEx module is only used to parse human input. It is not used for the final product.
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
          dict: Dictionary.Dict
          playerNum: uint32
          playerCount: uint32
          hand: MultiSet.MultiSet<uint32>
          curPlayer: uint32
          playedLetters: Map<coord, (char * int)>
          timeout: uint32 option
          GoRight: bool
          GoDown: bool
          LastTile: (coord * uint32 * (char * int)) option
          FirstPlayerTurn: bool }

    // OLD (But keeping just in case we want it on one line): let mkState b d pn h = {board = b; dict = d;  playerNumber = pn; hand = h  }

    let mkState newBoard newDict newPlayerNum newNumPlayers newHand newPlaterTurn playedLetters timeout goRight goDown lastTile firstPlayerTurn =
        { board = newBoard
          dict = newDict
          playerNum = newPlayerNum
          playerCount = newNumPlayers
          hand = newHand
          curPlayer = newPlaterTurn
          playedLetters = playedLetters
          timeout = timeout
          GoRight = goRight
          GoDown = goDown
          LastTile = lastTile
          FirstPlayerTurn = firstPlayerTurn }

    type pieces = Map<uint32, tile>
    type coordinates = (int * int)

/// <summary>Our implementation for a move finding algorithm ;_;</summary>
module NextMoveFinder =
    type Move = (coord * (uint32 * (char * int))) list

    /// <summary>Generate the next move to be passed to the server as the next game move.</summary>
    let NextMove (pieces: State.pieces) (state: State.state): (bool * bool * Move) =
        ScrabbleUtil.DebugPrint.debugPrint (sprintf "- FirstMove (playerNum %d)\n" state.playerNum)
        // Check whether it is the first move of the game
        if Map.isEmpty state.playedLetters then
            // Depth-first search to find first valid word
            ScrabbleUtil.DebugPrint.debugPrint (sprintf "- finding first move (playerNum %d)\n" state.playerNum)
            let rec check (hand: MultiSet.MultiSet<uint32>) (move: Move) (dict: Dict) (x, y) (goRight: bool) (goDown: bool) : (Move * bool) =
                // We loop through all pieces in the hand until we find a valid word,
                // in which case we set finished to true and simply pass the move back up the stack.
                MultiSet.fold
                    (fun (move, finished) id _ -> // for each piece in hand
                        // Get the piece from the pieces map
                        // A piece is a set of variants, typically only one,
                        // but in case the piece is a wildcard piece, the set will contain all possible variants.
                        let piece = Map.find id pieces

                        Set.fold
                            (fun (move, finished) (char, points) -> // for each piece variant that the piece can be
                                if finished then
                                    // This is the branch we enter when we have already found a valid word
                                    (move, true)
                                else
                                    // Check if the current dictionary node has a child node with the current character
                                    match Dictionary.step char dict with
                                    | Some(finished, dict) ->
                                        let tile = (id, (char, points))
                                        let move = ((x, y), tile) :: move // add the current piece to the move (they don't have to be in order)

                                        if finished then
                                            // If this dictionary node is a word, we have found a valid word
                                            // and we can return the move
                                            (move, true)
                                        else
                                            // Otherwise, we continue the search
                                            if goRight then 
                                                check (MultiSet.removeSingle id hand) move dict (x + 1, y) true false
                                            else
                                                check (MultiSet.removeSingle id hand) move dict (x, y - 1) true false

                                    | None -> (move, false))
                            (move, finished)
                            piece)
                    (move, false)
                    hand

            let move, finished = check state.hand [] state.dict state.board.center true false
            ScrabbleUtil.DebugPrint.debugPrint (sprintf "- FIRST move generated (playerNum %d)\n" state.playerNum)
            (true, false, move)
        else
            // Depth-first search to find next valid word
            ScrabbleUtil.DebugPrint.debugPrint (sprintf "- finding next move (playerNum %d)\n" state.playerNum)
            let rec check (hand: MultiSet.MultiSet<uint32>) (move: Move) (dict: Dict) (x, y) (goRight: bool) (goDown: bool) : (Move * bool) =
                // We loop through all pieces in the hand until we find a valid word,
                // in which case we set finished to true and simply pass the move back up the stack.
                MultiSet.fold
                    (fun (move, finished) id _ -> // for each piece in hand
                        // Get the piece from the pieces map
                        // A piece is a set of variants, typically only one,
                        // but in case the piece is a wildcard piece, the set will contain all possible variants.
                        let piece = Map.find id pieces

                        Set.fold
                            (fun (move, finished) (char, points) -> // for each piece variant that the piece can be
                                if finished then
                                    // This is the branch we enter when we have already found a valid word
                                    (move, true)
                                else
                                    // Check if the current dictionary node has a child node with the current character
                                    match Dictionary.step char dict with
                                    | Some(finished, dict) ->
                                        let tile = (id, (char, points))
                                        let move = ((x, y), tile) :: move // add the current piece to the move (they don't have to be in order)

                                        if finished then
                                            // If this dictionary node is a word, we have found a valid word
                                            // and we can return the move
                                            (move, true)
                                        else
                                            // Otherwise, we continue the search
                                            if goRight then 
                                                check (MultiSet.removeSingle id hand) move dict (x + 1, y) true false
                                            else
                                                check (MultiSet.removeSingle id hand) move dict (x, y - 1) true false

                                    | None -> (move, false))
                            (move, finished)
                            piece)
                    (move, false)
                    hand
            let move, finished =
                match state.LastTile with
                | Some(lastCoord, _, (_, _)) -> check state.hand [] state.dict lastCoord true false
                | None -> check state.hand [] state.dict state.board.center true false
            ScrabbleUtil.DebugPrint.debugPrint (sprintf "- NEXT move generated (playerNum %d)\n" state.playerNum)
            ScrabbleUtil.DebugPrint.debugPrint (sprintf "- LastTile %A\n" state.LastTile)
            (true, false, move)


// match state.board.squares state.board.center with
// | StateMonad.Result.Success None -> GenerateFirstWord state
// | StateMonad.Result.Success(Some _) -> GenerateNextWord state
// | StateMonad.Result.Failure(_) -> GenerateNextWord state


module Scrabble =
    open System.Threading

    let playGame cstream pieces (st: State.state) =
        let rec aux (st: State.state) =
            ScrabbleUtil.DebugPrint.debugPrint (sprintf "\n \n Entered aux loop\n \n")
            //Print.printHand pieces (State.hand st)

            ScrabbleUtil.DebugPrint.debugPrint (sprintf "First turn \n")
            let right, down, nextMove = NextMoveFinder.NextMove pieces st

            if List.isEmpty nextMove then
                send cstream (SMPass)
            else
                send cstream (SMPlay nextMove)

            if st.curPlayer = st.playerNum then
                // remove the force print when you move on from manual input (or when you have learnt the format)
                // forcePrint
                //   "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"

                //let input = System.Console.ReadLine()

                //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

                match recv cstream with
                | RCM(CMPlaySuccess(move: (coord * (uint32 * (char * int))) list,
                                    points,
                                    newPieces: (uint32 * uint32) list)) ->
                    ScrabbleUtil.DebugPrint.debugPrint (
                        sprintf "Player %d played %A for %d points\n" st.playerNum move points
                    )
                    (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)

                    aux
                        { st with
                            hand =
                                let hand =
                                    move |> List.fold (fun acc (_, (id, _)) -> MultiSet.removeSingle id acc) st.hand

                                newPieces |> List.fold (fun acc (id, n) -> MultiSet.add id n acc) hand
                            playedLetters =
                                move
                                |> List.fold (fun acc (coord, (_, tile)) -> Map.add coord tile acc) st.playedLetters
                            curPlayer = (st.curPlayer + 1u) % st.playerCount
                            
                            // adding extra state updates
                            GoDown = down
                            GoRight = right
                            LastTile = 
                                match List.tryLast nextMove with
                                | Some (coord, (tileId, (char, points))) -> Some (coord, tileId, (char, points)) 
                                | None -> None
                            FirstPlayerTurn = false }
                | RCM(CMGameOver _) -> ()
                | RCM a ->
                    ScrabbleUtil.DebugPrint.debugPrint (sprintf "Player %d <- Server:\n%A\n" st.playerNum a)

                    aux
                        { st with
                            curPlayer = (st.curPlayer + 1u) % st.playerCount }
                | RGPE err ->
                    ScrabbleUtil.DebugPrint.debugPrint (sprintf "Player %d <- Server:\n%A\n" st.playerNum err)

                    aux
                        { st with
                            curPlayer = (st.curPlayer + 1u) % st.playerCount }
            else
                match recv cstream with
                | RCM(CMPlayed(pid, move: (coord * (uint32 * (char * int))) list, points)) ->
                    ScrabbleUtil.DebugPrint.debugPrint (sprintf "Player %d played %A for %d points\n" pid move points)

                    aux
                        { st with
                            playedLetters =
                                move
                                |> List.fold (fun acc (coord, (_, tile)) -> Map.add coord tile acc) st.playedLetters
                            curPlayer = (st.curPlayer + 1u) % st.playerCount }
                | RCM(CMPlayFailed(pid, ms)) ->
                    ScrabbleUtil.DebugPrint.debugPrint (sprintf "Player %d failed to play %A\n" pid ms)

                    aux
                        { st with
                            curPlayer = (st.curPlayer + 1u) % st.playerCount }
                | RCM(CMGameOver _) -> ()
                | RCM a ->
                    ScrabbleUtil.DebugPrint.debugPrint (sprintf "Player %d <- Server:\n%A\n" st.playerNum a)

                    aux
                        { st with
                            curPlayer = (st.curPlayer + 1u) % st.playerCount }
                | RGPE err ->
                    ScrabbleUtil.DebugPrint.debugPrint (sprintf "Player %d <- Server:\n%A\n" st.playerNum err)

                    aux
                        { st with
                            curPlayer = (st.curPlayer + 1u) % st.playerCount }

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

        fun () ->
            playGame
                cstream
                tiles
                (State.mkState board dict playerNumber numPlayers handSet playerTurn Map.empty timeout true false None true)
