module internal MoveCalculator

    open ScrabbleUtil.Dictionary
    open Parser

    /// <summary>Return the coordinate to place in the next move.</summary>
    val getCoord : board -> (int * int)

    /// <summary>Return the tile ID for the next move.</summary>
    val getTileID: board -> uint32

    /// <summary>Return the letter and its score for the next move.</summary>
    val getLetterAndScore: char -> uint32 -> (char * uint32)

    /// <summary>Generate the next move to be passed to the server as the next game move.</summary>
    val generateNextMove : board -> char list -> list<(int * int) * (uint32 * (char * uint32))>

    /// <summary>Using backtracking, find the next word to be passed to the server.</summary>
    val findNextWord : list<uint32> -> Dict -> string -> option<string>