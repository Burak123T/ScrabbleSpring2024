module internal MoveCalculator

    open ScrabbleUtil.Dictionary
    open Parser

    type tile = (char * uint32)
    type pieces = Map<uint32, tile>
    type coordinates = (int * int)

    /// <summary>Return the coordinate to place in the next move.</summary>
    val getCoord : coordinates

    /// <summary>Return the tile ID for the next move.</summary>
    val getTileID : uint32

    /// <summary>Return the letter and its score for the next move.</summary>
    val getLetterAndScore: (char * uint32)

    /// <summary>Generate the next move to be passed to the server as the next game move.</summary>
    val generateNextMove : board -> char list -> list<coordinates * (uint32 * tile)>

    /// <summary>Using backtracking, find the next word to be passed to the server.</summary>
    val findNextWord : list<uint32> -> Dict -> string -> option<string>