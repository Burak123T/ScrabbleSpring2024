module internal MoveCalculator

    open ScrabbleUtil
    open ScrabbleUtil.Dictionary
    open Parser

    type pieces = Map<uint32, tile>
    type coordinates = (int * int)

    let mutable coordinates = (0, 0)

    let getCoord: (int * int) = 
        match coordinates with
        | (a, b) when a > b -> 
            coordinates <- (a, b + 1)
            coordinates
        | (a, b) when a < b -> 
            coordinates <- (a + 1, b)
            coordinates
        | (a, b) when a = b ->
            coordinates <- (a + 2, b - 2)
            coordinates
        | (a, b) -> 
            coordinates <- (a - 2, b + 2)
            coordinates

    let mutable saveTileID: uint32 = 1u

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
        | Some(t) -> // if playing with timer
            match (t > 0u) with
            | true ->
                match Map.isEmpty p with
                | true -> failwith "No move available. "
                | false ->
                    let nextWord = findNextWordBacktrack p dict ""
                    match coordinates with
                    | (a, b) -> 
                        coordinates <- (a + 1, b + 1)
                    let coords = getCoord
                    let tileID = saveTileID
                    let letter = getLetter p saveTileID // TODO: get the score associated with the letter
                    [ (coords), (tileID, (letter)) ]
            | false -> failwith "Timeout."
        | None -> // if not playing with timer
            match Map.isEmpty p with
            | true -> failwith "No move available. "
            | false ->
                let nextWord = findNextWordBacktrack p dict ""
                match coordinates with
                | (a, b) -> 
                    coordinates <- (a + 1, b + 1)
                let coords = getCoord
                let tileID = saveTileID
                let letter = getLetter p saveTileID // TODO: get the score associated with the letter
                [ (coords), (tileID, (letter)) ]
