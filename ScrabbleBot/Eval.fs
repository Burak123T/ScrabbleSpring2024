// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad
    
    let add (a: SM<int>) (b: SM<int>) = a >>= (fun a' -> b >>= (fun b' -> (ret (a' + b'))))

    let div (a : SM<int>) (b : SM<int>) : SM<int> = a >>= (fun a' -> b >>= (fun b' ->
        if b' <> 0 then
            ret (a' / b')
        else
            fail DivisionByZero))

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval a : SM<int> =
        match a with
        | N n -> ret n
        | V v -> lookup v
        | WL -> wordLength
        | PV pv -> (arithEval pv) >>= pointValue
        | Add(a', b') -> arithEval a' >>= (fun a'' -> arithEval b' >>= (fun b'' -> (ret (a'' + b''))))
        | Sub(a', b') -> arithEval a' >>= (fun a'' -> arithEval b' >>= (fun b'' -> (ret (a'' - b''))))
        | Mul(a', b') -> arithEval a' >>= (fun a'' -> arithEval b' >>= (fun b'' -> (ret (a'' * b''))))
        | Div(a', b') -> arithEval a' >>= (fun a'' -> arithEval b' >>= (fun b'' -> ( if b'' <> 0 then (ret (a'' / b'')) else fail DivisionByZero )))
        | Mod(a', b') -> arithEval a' >>= (fun a'' -> arithEval b' >>= (fun b'' -> ( if b'' <> 0 then (ret (a'' % b'')) else fail DivisionByZero )))
        | CharToInt(ci) -> charEval ci >>= ( fun r -> ret( int r ) )

    and charEval c : SM<char> = 
        match c with
        | C ch -> ret ch
        | IntToChar(aExpr) -> arithEval aExpr >>= (fun x -> ret (char x))
        | CV(aExpr) -> arithEval aExpr >>= characterValue
        | ToUpper(cExpr) -> charEval cExpr >>= ( fun r -> ret(System.Char.ToUpper r))
        | ToLower(cExpr) -> charEval cExpr >>= ( fun r -> ret(System.Char.ToLower r))

    let rec boolEval b : SM<bool> = 
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq(a1, a2) -> ret (a1 = a2)
        | ALt(a1, a2) -> ret (a1 < a2)
        | Not(bExpr) -> boolEval bExpr >>= (fun x -> ret (not x))
        | Conj(b1, b2) -> boolEval b1 >>= (fun b1' -> boolEval b2 >>= (fun b2' -> (ret (b1' && b2'))))
        | IsVowel(cExpr) -> 
            match cExpr with
            | C ch ->
                match ch with
                | 'a' | 'e' | 'i' | 'o' | 'u' -> ret true 
                | 'A' | 'E' | 'I' | 'O' | 'U' -> ret true 
                | _ -> ret false
            | _ -> ret false
        | IsLetter(cExpr) -> charEval cExpr >>= ( fun r -> ret(System.Char.IsLetter r))
        | IsDigit(cExpr) -> charEval cExpr >>= ( fun r -> ret(System.Char.IsDigit r))

    type stmnt =                  (* statements *)
    | Declare of string           (* variable declaration *)
    | Ass of string * aExp        (* variable assignment *)
    | Skip                        (* nop *)
    | Seq of stmnt * stmnt        (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt       (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    let stmntToSquareFun stm = failwith "Not implemented"

    let stmntToBoardFun stm m = failwith "Not implemented"

    type squareStmnt = Map<int, stmnt>
    let stmntsToSquare stms = failwith "Not implemented"

    let mkBoard c x boardStmnt ids = failwith "Not implemented"
    