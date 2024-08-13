// Learn more about F# at http://fsharp.org

let time f =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start)

let readLines filePath = System.IO.File.ReadLines(filePath)

let spawnMultiples name dict bot =
    let rec aux =
        function
        | 0 -> []
        | x -> (sprintf "%s%d" name x, dict, bot) :: aux (x - 1)

    aux >> List.rev

[<EntryPoint>]
let main argv =
    ScrabbleUtil.DebugPrint.toggleDebugPrint true // Change to false to supress debug output

    System.Console.BackgroundColor <- System.ConsoleColor.White
    System.Console.ForegroundColor <- System.ConsoleColor.Black
    System.Console.Clear()

    let words =
        try
            readLines "../../../ScrabbleTemplate/Dictionaries/English.txt"
        with _ ->
            try
                readLines "../../ScrabbleTemplate/Dictionaries/English.txt"
            with _ ->
                try
                    readLines "../ScrabbleTemplate/Dictionaries/English.txt"
                with _ ->
                    readLines "./ScrabbleTemplate/Dictionaries/English.txt"

    let dict =
        List.fold
            (fun dict word ->
                let dict = Dictionary.insert word dict
                dict)
            (Dictionary.empty ())
            (List.ofSeq words)
    // for child_node in dict.child_nodes do
    //     ScrabbleUtil.DebugPrint.debugPrint <| sprintf "\n%c:" child_node.Key
    //     for child_child_node in child_node.Value.child_nodes do
    //         ScrabbleUtil.DebugPrint.debugPrint <| sprintf "\n|- %c %b" child_child_node.Key child_child_node.Value.is_word

    // match Dictionary.step 'E' dict with
    // | Some(is_word, node) ->
    //     printfn "%c %b\n" 'E' is_word

    //     match Dictionary.step 'I' node with
    //     | Some(is_word', node') ->
    //         printfn "%c %b\n" 'I' is_word'

    //         match Dictionary.step 'N' node' with
    //         | Some(is_word'', node'') ->
    //             printfn "%c %b\n" 'N' is_word''

    //             if is_word'' then
    //                 printfn "Word found\n"
    //                 exit 0
    //             else
    //                 printfn "Word not found\n"
    //         | None -> printfn "None''\n"
    //     | None -> printfn "None'\n"

    // | None -> printfn "None\n"

    let dictAPI =
        // Uncomment if you have implemented a dictionary. last element None if you have not implemented a GADDAG
        Some(Dictionary.empty, Dictionary.insert, Dictionary.step, None)

    // Uncomment this line to call your client
    let (dictionary, time) =
        time (fun () -> ScrabbleUtil.Dictionary.mkDict words dictAPI)

    let players =
        // [ "LexiIngenium", dictionary, LexiIngenium.Scrabble.startGame
        //   "OxyphenButazone", dictionary, Oxyphenbutazone.Scrabble.startGame ]
        spawnMultiples "LexiIngenium" dictionary LexiIngenium.Scrabble.startGame 1

    // Uncomment to test your dictionary
    let incorrectWords = ScrabbleUtil.Dictionary.test words 10 (dictionary false) // change the boolean to true if using a GADDAG

    match incorrectWords with
    | [] -> ScrabbleUtil.DebugPrint.debugPrint ("Dictionary test sucessful!\n")
    | incorrects ->
        ScrabbleUtil.DebugPrint.debugPrint ("Dictionary test failed for at least the following words: \n")
        List.iter (fun str -> ScrabbleUtil.DebugPrint.debugPrint (sprintf "%s\n" str)) incorrects

    // ---------------------------------------- Game Parameters ----------------------------------------

    let handSize = 7u
    let timeout = None
    let tiles = ScrabbleUtil.English.tiles 1u
    let seed = Some 0
    let port = 13001

    //let board = ScrabbleUtil.StandardBoard.standardBoard ()
    let board = ScrabbleUtil.InfiniteBoard.infiniteBoard ()
    //let board = ScrabbleUtil.RandomBoard.randomBoard ()
    //let board = ScrabbleUtil.RandomBoard.randomBoardSeed (Some 42)
    //let board = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoard ()
    //let board = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoardSeed (Some 42)
    //let board = ScrabbleUtil.HoleBoard.holeBoard ()
    //let board = ScrabbleUtil.InfiniteHoleBoard.infiniteHoleBoard ()

    //let players = spawnMultiples "OxyphenButazone" dictionary Oxyphenbutazone.Scrabble.startGame 2

    //let players = spawnMultiples "LexiIngenium" dictionary LexiIngenium.Scrabble.startGame 4

    do ScrabbleServer.Comm.startGame board dictionary handSize timeout tiles seed port players

    System.Console.ReadLine() |> ignore

    0
