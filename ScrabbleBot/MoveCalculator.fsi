module internal MoveCalculator

    open ScrabbleUtil
    open ScrabbleUtil.Dictionary
    open Parser

    type pieces = Map<uint32, tile>
    type coordinates = (int * int)

    /// <summary>Return the coordinate to place in the next move.</summary>
    val getCoord: (int * int)

    /// <summary>Return the tile ID for the next move.</summary>
    val saveTileID: uint32

    /// <summary>Return the letter and its score for the next move.</summary>
    val getLetter: pieces -> uint32 -> (char * int)

    /// <summary>Generate the next move to be passed to the server as the next game move.</summary>
    val generateNextMove: Dict -> pieces -> board -> uint32 option -> list<coord * (uint32 * (char * int))>

    /// <summary>Using backtracking, find the next word to be passed to the server.</summary>
    val findNextWordBacktrack: pieces -> Dict -> string -> option<string>