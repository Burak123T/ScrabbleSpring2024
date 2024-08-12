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
    let dfsCheck (pieces: State.pieces) (state: State.state) (tile: (coord * uint32 * (char * int)) option) : Move =
            // Depth-first search to find first valid word
            ScrabbleUtil.DebugPrint.debugPrint (sprintf "-dfsCheck (playerNum %d)-" state.playerNum)
            let rec check (hand: MultiSet.MultiSet<uint32>) (move: Move) (dict: Dict) (x, y) : (Move * bool) =
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
                                            check (MultiSet.removeSingle id hand) move dict (x + 1, y)

                                    | None -> (move, false))
                            (move, finished)
                            piece)
                    (move, false)
                    hand
            let move, finished =
                match tile with
                | Some(coord, uint32, (c, i)) -> check state.hand [] state.dict coord
                | None -> check state.hand [] state.dict state.board.center
            move

    /// <summary>Staircase Method to find the next subsequent move.
    /// If the global state 'GoRight' is true, then it will go right to find the last letter,
    /// and then place words down. If the global state 'GoDown' is true, then it will do the
    /// opposite.
    /// </summary>
    let StaircaseNextMove (pieces: State.pieces) (state: State.state): bool * bool * option<coord * uint32 * (char * int)> * Move =
        let rec findLastLetter (goRight: bool) (goDown: bool) (lastTile: option<coord * uint32 * (char * int)>) =
            // Start by checking if we should go right
            ScrabbleUtil.DebugPrint.debugPrint (sprintf "-staircase (playerNum %d)-" state.playerNum)
            match goRight with
            | true -> 
                // From the last provided tile, check one tile to the right          
                match lastTile with
                | Some ((x, y), tileId, (character, score)) -> 
                    // If a letter to the right exists, then go one right again
                    ScrabbleUtil.DebugPrint.debugPrint (sprintf "-staircase right (playerNum %d)-" state.playerNum)
                    findLastLetter true false (Some ((x + 1, y), tileId, (character, score))) 
                // if no other letter exists, then try checking one down (returns 'false' for the 'state.ToRight')
                | None -> (false, true, lastTile, (dfsCheck pieces state lastTile))
            | false ->
                // check if we can go down instead
                match goDown with
                | true ->
                    // From the last provided tile, check one tile down        
                    match lastTile with
                    | Some ((x, y), tileId, (character, score)) -> 
                        // If a letter to under exists, then go one down again
                        ScrabbleUtil.DebugPrint.debugPrint (sprintf "-staircase down (playerNum %d)-" state.playerNum)
                        findLastLetter true false (Some ((x, y - 1), tileId, (character, score))) 
                    // if no letter exists under either, then we must have reached final letter
                    | None -> findLastLetter false false lastTile
                // we must have reached the last placed letter on the board
                | false -> // place new word
                    ScrabbleUtil.DebugPrint.debugPrint (sprintf "-staircase place word (playerNum %d)-" state.playerNum)
                    (true, false, lastTile, (dfsCheck pieces state lastTile))


        findLastLetter state.GoRight state.GoDown state.LastTile

    /// <summary>Generate the next move to be passed to the server as the next game move.</summary>
    let NextMove (pieces: State.pieces) (state: State.state) : Move =
        ScrabbleUtil.DebugPrint.debugPrint (sprintf "-NextMove (playerNum %d)-" state.playerNum)
        // Check whether it is the first move of the game
        if Map.isEmpty state.playedLetters then
            // Depth-first search to find first valid word
            ScrabbleUtil.DebugPrint.debugPrint (sprintf "- finding move (playerNum %d)-" state.playerNum)
            let rec check (hand: MultiSet.MultiSet<uint32>) (move: Move) (dict: Dict) (x, y) : (Move * bool) =
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
                                            check (MultiSet.removeSingle id hand) move dict (x + 1, y)

                                    | None -> (move, false))
                            (move, finished)
                            piece)
                    (move, false)
                    hand

            let move, finished = check state.hand [] state.dict state.board.center
            ScrabbleUtil.DebugPrint.debugPrint (sprintf "-move generated (playerNum %d)-" state.playerNum)
            move
        else
            ScrabbleUtil.DebugPrint.debugPrint (sprintf "- here ? (playerNum %d)-" state.playerNum)
            failwith "Should not happen?"



// match state.board.squares state.board.center with
// | StateMonad.Result.Success None -> GenerateFirstWord state
// | StateMonad.Result.Success(Some _) -> GenerateNextWord state
// | StateMonad.Result.Failure(_) -> GenerateNextWord state


module Scrabble =
    open System.Threading

    let playGame cstream pieces (st: State.state) =
        let rec aux (st: State.state) =
            //Print.printHand pieces (State.hand st)
            
            let firstMove = NextMoveFinder.NextMove pieces st
            let shouldGoRight, shouldGoDown, lastTile, nextMove = NextMoveFinder.StaircaseNextMove pieces st

            if st.curPlayer = st.playerNum then
                // remove the force print when you move on from manual input (or when you have learnt the format)
                // forcePrint
                //   "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"

                //let input = System.Console.ReadLine()

                if List.isEmpty firstMove then
                    if List.isEmpty nextMove then
                        send cstream (SMPass)
                    else 
                        send cstream (SMPlay nextMove)
                else
                    //debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                    if st.FirstPlayerTurn then
                        send cstream (SMPlay firstMove)

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
                            GoDown = shouldGoDown
                            GoRight = shouldGoRight
                            LastTile = lastTile
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
                            curPlayer = (st.curPlayer + 1u) % st.playerCount
                            
                            // adding extra state updates
                            GoDown = shouldGoDown
                            GoRight = shouldGoRight
                            LastTile = lastTile
                            FirstPlayerTurn = false }
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
