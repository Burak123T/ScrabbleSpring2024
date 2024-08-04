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
          playerNumber: uint32
          numPlayers: uint32
          hand: MultiSet.MultiSet<uint32>
          playerTurn: uint32
          playedLetters: Map<coord, (char * int)>
          timeout: uint32 option }

    // OLD (But keeping just in case we want it on one line): let mkState b d pn h = {board = b; dict = d;  playerNumber = pn; hand = h  }

    let mkState newBoard newDict newPlayerNum newNumPlayers newHand newPlaterTurn playedLetters timeout =
        { board = newBoard
          dict = newDict
          playerNumber = newPlayerNum
          numPlayers = newNumPlayers
          hand = newHand
          playerTurn = newPlaterTurn
          playedLetters = playedLetters
          timeout = timeout }


    let board st = st.board
    let dict st = st.dict
    let playerNumber st = st.playerNumber
    let hand st = st.hand
    let numPlayers st = st.numPlayers
    let playerTurn st = st.playerTurn
    let playedLetters st = st.playedLetters
    let timeout st = st.timeout

    //Takes a state and puts in the moves that were made
    let insertStateWithMove (moves: list<coord * (uint32 * (char * int))>) (state:state) =
        List.fold (fun acc move -> 
            let (coord , (_, (char, score))) = move
            let justPlayedLetters = acc.playedLetters |> Map.add coord (char, score)
            mkState acc.board acc.dict acc.playerNumber acc.numPlayers acc.hand acc.playerTurn justPlayedLetters acc.timeout
        ) state moves

    type pieces = Map<uint32, tile>
    type coordinates = (int * int)

/// <summary>Our implementation for a move finding algorithm ;_;</summary>
module NextMoveFinder =

    /// <summary>Generate the next move to be passed to the server as the next game move.</summary>
    let NextMove (state: State.state) : (coord * (uint32 * (char * int))) list = 
        // Check if center has a tile (i.e., if you are the first player to make a move)
        match state.board.squares (0,0) with
        | StateMonad.Result.Success None -> [(-1,0), (0u, ('B', 0)); (0,0), (0u, ('A', 0)); (1,0), (0u, ('R', 0))] // Placeholder move

    let rec GenerateFirstWord (state: State.state) (init: list<coord * (uint32 * (char * int))>) =
        match lookup "a" state.dict with // placeholder lookup
        | true -> GenerateFirstWord state [(init; (0,0), (0u, ('A', 0)))] // placeholder word generation
        | false -> init

module Scrabble =
    open System.Threading

    let playGame cstream pieces (st: State.state) =

        let rec aux (st: State.state) =
            //Print.printHand pieces (State.hand st)

            let newState = (State.mkState st.board st.dict st.playerNumber st.numPlayers st.hand st.playerTurn st.playedLetters st.timeout)

            // remove the force print when you move on from manual input (or when you have learnt the format)
           // forcePrint
             //   "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"

            //let input = System.Console.ReadLine()
            let move = NextMoveFinder.NextMove st

            //debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM(CMPlaySuccess(ms: (coord * (uint32 * (char * int))) list, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = newState // This state needs to be updated
                aux st'
            | RCM(CMPlayed(pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = newState // This state needs to be updated
                aux st'
            | RCM(CMPlayFailed(pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = newState // This state needs to be updated
                aux st'
            | RCM(CMGameOver _) -> ()
            | RCM a -> ()
            | RGPE err ->
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

        fun () ->
            playGame
                cstream
                tiles
                (State.mkState board dict playerNumber numPlayers handSet playerTurn Map.empty timeout)
