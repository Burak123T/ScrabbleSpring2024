module internal MoveCalculator

    open System 
    open Parser
    open ScrabbleUtil
    open ScrabbleUtil.Dictionary
    open ScrabbleUtil.ServerCommunication
    open MultiSet
    open System.Text.RegularExpressions


    // This function will be used to combine letetrs and find valid words within the dictionary
    let rec combineLetters (hand: uint32 list) (dict: Dictionary.Dict) (word: string) =
        match hand with
        | [] -> 
            match lookup word dict with
            | true -> Some word
            | false -> None
        | w::ws ->
            let a = combineLetters ws dict (word + string w)
            let b = combineLetters ws dict (word + string w)
            if (a = None) && (b = None) then None 
                else 
                    if (a.ToString().Length > b.ToString().Length) then a
                    else b
    
    let nextMove (hand: MultiSet.MultiSet<uint32>) (dict: Dictionary.Dict) (word: string) =
        match isEmpty hand with
        | true -> None
        | false -> 
            (combineLetters (toList hand) dict word)
