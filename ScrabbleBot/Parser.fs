// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar = pstring "intToChar"

    let pPointValue = pstring "pointValue"

    let pCharToInt = pstring "charToInt"

    let pToUpper = pstring "toUpper"

    let pToLower = pstring "toLower"

    let pCharValue = pstring "charValue"

    let pTrue = pstring "true"

    let pFalse = pstring "false"

    let pIsDigit = pstring "isDigit"

    let pIsLetter = pstring "isLetter"

    let pIsVowel = pstring "isVowel"

    let pif = pstring "if"

    let pthen = pstring "then"

    let pelse = pstring "else"

    let pwhile = pstring "while"

    let pdo = pstring "do"

    let pdeclare = pstring "declare"

    let whitespaceChar = satisfy (System.Char.IsWhiteSpace)
    let pletter = satisfy (System.Char.IsLetter)


    let palphanumeric  = satisfy (System.Char.IsLetterOrDigit)

    let spaces = many whitespaceChar

    let spaces1 = many1 whitespaceChar

    let (.>*>.) a1 a2 = a1 .>> spaces >>. a2

    let (.>*>) a1 a2 = a1 .>> spaces .>> a2
    let (>*>.) a1 a2 = a1 >>. spaces >>. a2

    let parenthesise p = spaces >*>. p .>*> spaces

    let pid = pletter .>>. palphanumeric |>> fun (x, y) -> System.Char.ToString(x) + System.Char.ToString(y)
    
    let unop a b = a >*>. b 

    let binop (a: Parser<'a>) (b: Parser<'b>) (c: Parser<'c>) : Parser<'b * 'c> = a >*>. (b .>>. c)

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    do tref := choice [AddParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    do pref := choice [MulParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    do aref := choice [NParse; ParParse]

    let AexpParse = TermParse 

    let CexpParse = pstring "not implemented"

    let BexpParse = pstring "not implemented"

    let stmParse = pstring "not implemented"

    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
