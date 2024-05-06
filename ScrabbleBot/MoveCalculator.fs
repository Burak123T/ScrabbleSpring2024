module internal MoveCalculator

    open ScrabbleUtil
    open ScrabbleUtil.Dictionary
    open Parser

    type tile = (char * uint32)
    type pieces = Map<uint32, tile>
    type coordinates = (int * int)

    let getCoord: (int * int) = failwith "Not implemented"

    let getTileID: uint32 = failwith "Not implemented" 

    let getLetterAndScore: (char * uint32) = failwith "Not implemented" 

    // [((x-coord, y-coord), (tile id, (letter, score)))]
    let generateNextMove (b: board) (chars: char list): list<(int * int) * (uint32 * (char * uint32))> = 
        match chars with
        | [] -> failwith "No move available"
        | c::cs ->
            let coords = getCoord
            let tileID = getTileID
            let letterAndScore = getLetterAndScore // TODO: get the score associated with the letter
            [ (coords), (tileID, (letterAndScore)) ]


    let rec findNextWord (hand: uint32 list) (dict: Dictionary.Dict) (word: string) =
        match hand with
        | [] ->
            match lookup word dict with
            | true -> Some word
            | false -> None
        | c::rem ->
            // Backtracking 
            let nextResult = findNextWord rem dict (word + string c)
            match nextResult with
            | Some(w) -> Some(w)
            | None -> None