module internal MoveCalculator

    open ScrabbleUtil
    open ScrabbleUtil.Dictionary
    open Parser

    type pieces = Map<uint32, tile>
    type coordinates = (int * int)

    // Temporary way of saving the coord to be used
    let mutable tmp = (0, 0)

    //TODO: this is a simple implementation, try to make it better
    let getCoord (p: pieces) (w: string option) (b: Parser.board): (int * int) = tmp

    let mutable saveTileID: uint32 = 1u

    //TODO: should we use 'MaximumElement'
    let getLetter (p: pieces) (tid: uint32): (char * int) = 
        match p.ContainsKey(tid) with
        | true -> p.[tid].MaximumElement
        | false -> failwith "Tile not found."

    let findNextWordBacktrack (p: pieces) (dict: Dictionary.Dict) (word: string) =
        let rec aux (p: pieces) (d: Dictionary.Dict) (w: string) (visited: Set<uint32 * tile>) =
            match p.IsEmpty with
            | true ->
                match lookup w d with
                | true -> Some w
                | false -> None
            | false ->
                let MapToList = Map.toList p
                let nextLetter = MapToList |> List.head
                saveTileID <- fst nextLetter
                let addPieceToVisited = nextLetter |> visited.Add
                match visited.IsSupersetOf(addPieceToVisited) with
                | true -> None
                | false ->
                    let remainingHand = List.removeAt 0 MapToList
                    let checkIfCanFormWord = aux (Map.ofList remainingHand) d (w + string nextLetter) addPieceToVisited
                    match checkIfCanFormWord with
                    | Some(w1) -> Some(w1)
                    | None -> None
        aux p dict word Set.empty

    // [((x-coord, y-coord), (tile id, (letter, score)))]
    // hand: starting hand (tile id, number of tiles)
    let generateNextMove (dict: Dictionary.Dict) (p: pieces) (b: Parser.board) (timer: uint32 option): list<coord * (uint32 * (char * int))> = 
        match timer with
        | Some(_) ->
            match Map.isEmpty p with
            | true -> failwith "No move available. "
            | false ->
                let nextWord = findNextWordBacktrack p dict ""
                match tmp with
                | (a, b) -> 
                    tmp <- (a + 1, b + 1)
                let coords = getCoord p nextWord b 
                let tileID = saveTileID
                let letter = getLetter p saveTileID // TODO: get the score associated with the letter
                [ (coords), (tileID, (letter)) ]
        | None -> failwith "Timeout."
