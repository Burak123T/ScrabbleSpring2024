module internal MoveCalculator

    open ScrabbleUtil
    open ScrabbleUtil.Dictionary

    let rec nextInput (hand: uint32 list) (dict: Dictionary.Dict) (word: string) =
        match hand with
        | [] ->
            match lookup word dict with
            | true -> Some word
            | false -> None
        | c::rem ->
            // Backtracking 
            let nextResult = nextInput rem dict (word + string c)
            match nextResult with
            | Some(w) -> Some(w)
            | None -> None