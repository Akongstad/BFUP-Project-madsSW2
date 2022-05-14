module internal Eval

open Microsoft.FSharp.Collections
open StateMonad
(* Code for testing *)

let hello =
    [ ('H', 4)
      ('E', 1)
      ('L', 1)
      ('L', 1)
      ('O', 1) ]

let state =
    mkState [ ("x", 5); ("y", 42) ] hello [ "_pos_"; "_result_" ]

let emptyState = mkState [] [] []

let add (a: SM<int>) (b: SM<int>) : SM<int> =
    a >>= fun x -> b >>= fun y -> ret (x + y)

let sub (a: SM<int>) (b: SM<int>) : SM<int> =
    a >>= fun x -> b >>= fun y -> ret (x - y)

let mul (a: SM<int>) (b: SM<int>) : SM<int> =
    a >>= fun x -> b >>= fun y -> ret (x * y)

let div a b =
    a
    >>= fun x ->
            b
            >>= fun y ->
                    if y <> 0 then
                        ret (x / y)
                    else
                        fail DivisionByZero

let modulo a b =
    a
    >>= fun x ->
            b
            >>= fun y ->
                    if y <> 0 then
                        ret (x % y)
                    else
                        fail DivisionByZero

let vowels = Set [ 'a'; 'e'; 'i'; 'o'; 'u'; 'y' ]

let containsChar list =
    function
    | c -> Set.contains (System.Char.ToLower c) list

let isVowel c = containsChar vowels c

let charToInt c = int c - int '0'

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
    | C of char (* Character value *)
    | CV of aExp (* Character lookup at word index *)
    | ToUpper of cExp
    | ToLower of cExp
    | IntToChar of aExp

type bExp =
    | TT (* true *)
    | FF (* false *)

    | AEq of aExp * aExp (* numeric equality *)
    | ALt of aExp * aExp (* numeric less than *)

    | Not of bExp (* boolean not *)
    | Conj of bExp * bExp (* boolean conjunction *)

    | IsVowel of cExp (* check for vowel *)
    | IsLetter of cExp (* check for letter *)
    | IsDigit of cExp (* check for digit *)

let (.+.) a b = Add(a, b)
let (.-.) a b = Sub(a, b)
let (.*.) a b = Mul(a, b)
let (./.) a b = Div(a, b)
let (.%.) a b = Mod(a, b)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj(b1, b2)

let (.||.) b1 b2 =
    ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)

let (.->.) b1 b2 =
    ~~b1 .||. b2 (* boolean implication *)

let (.=.) a b = AEq(a, b)
let (.<.) a b = ALt(a, b)
let (.<>.) a b = ~~(a .=. b)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b)

let (.>=.) a b =
    ~~(a .<. b) (* numeric greater than or equal to *)

let (.>.) a b =
    ~~(a .=. b)
    .&&. (a .>=. b) (* numeric greater than *)

let rec arithEval a : SM<int> =
    match a with
    | N x -> ret x
    | V x -> lookup x
    | WL -> wordLength
    | PV x -> arithEval x >>= pointValue
    | Add (x, y) -> add (arithEval x) (arithEval y)
    | Sub (x, y) -> sub (arithEval x) (arithEval y)
    | Mul (x, y) -> mul (arithEval x) (arithEval y)
    | Div (x, y) -> div (arithEval x) (arithEval y)
    | Mod (x, y) -> modulo (arithEval x) (arithEval y)
    | CharToInt cExp -> (charEval cExp) >>= (fun f -> ret (int f))

and charEval c : SM<char> =
    match c with
    | C x -> ret x
    | CV x -> arithEval x >>= characterValue
    | ToUpper x ->
        charEval x
        >>= fun f -> ret (System.Char.ToUpper(f))
    | ToLower x ->
        charEval x
        >>= fun f -> ret (System.Char.ToLower(f))
    | IntToChar x -> arithEval x >>= fun f -> ret (char f)

let rec boolEval b : SM<bool> =
    match b with
    | TT -> ret true
    | FF -> ret false
    | AEq (x, y) ->
        arithEval x
        >>= fun x1 -> arithEval y >>= fun x2 -> ret (x1 = x2)
    | ALt (x, y) ->
        arithEval x
        >>= fun x1 -> arithEval y >>= fun x2 -> ret (x1 < x2)
    | Not x -> boolEval x >>= fun f -> ret (not f)
    | Conj (x, y) ->
        boolEval x
        >>= fun x1 -> boolEval y >>= fun x2 -> ret (x1 && x2)
    | IsDigit x ->
        charEval x
        >>= fun f -> ret (System.Char.IsDigit(f))
    | IsLetter x ->
        charEval x
        >>= fun f -> ret (System.Char.IsLetter(f))
    | IsVowel x -> charEval x >>= fun f -> ret (isVowel f)


type stm =
    (* statements *)
    | Declare of string (* variable declaration *)
    | Ass of string * aExp (* variable assignment *)
    | Skip (* nop *)
    | Seq of stm * stm (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm (* while statement *)

let rec stmntEval stmnt : SM<unit> =
    match stmnt with
    | Declare s -> declare s
    | Ass (x, a) -> arithEval a >>= update x
    | Skip -> ret ()
    | Seq (stmnt1, stmnt2) -> stmntEval stmnt1 >>>= stmntEval stmnt2 >>= ret
    | ITE (guard, stmnt1, stmnt2) ->

        push >>>= boolEval guard
        >>= fun g ->
                if g then
                    stmntEval stmnt1 >>>= pop
                else
                    stmntEval stmnt2 >>>= pop

    | While (guard, stmnt) ->
        push >>>= boolEval guard
        >>= fun b ->
                if b then
                    stmntEval stmnt
                    >>>= stmntEval (While(guard, stmnt))
                    >>>= pop
                else
                    ret () >>>= pop

(* Part 3 (Optional) *)

type StateBuilder() =

    member this.Bind(f, x) = f >>= x
    member this.Return(x) = ret x
    member this.ReturnFrom(x) = x
    member this.Delay(f) = f ()
    member this.Combine(a, b) = a >>= (fun _ -> b)

let prog = StateBuilder()

let rec arithEval2 a =
    prog {
        match a with
        | N x -> return x
        | V x -> return! lookup x
        | WL -> return! wordLength
        | PV x ->
            let! y = arithEval2 x
            return! pointValue y
        | Add (x, y) -> return! add (arithEval2 x) (arithEval2 y)
        | Sub (x, y) -> return! sub (arithEval2 x) (arithEval2 y)
        | Mul (x, y) -> return! mul (arithEval2 x) (arithEval2 y)
        | Div (x, y) -> return! div (arithEval2 x) (arithEval2 y)
        | Mod (x, y) -> return! modulo (arithEval2 x) (arithEval2 y)
        | CharToInt cExp ->
            let! x = (charEval2 cExp)
            return (int x)
    }

and charEval2 c =
    prog {
        match c with
        | C x -> return x
        | CV x ->
            let! f = arithEval2 x
            return! characterValue f
        | ToUpper x ->
            let! f = charEval2 x
            return System.Char.ToUpper(f)
        | ToLower x ->
            let! f = charEval2 x
            return System.Char.ToLower(f)
        | IntToChar x ->
            let! f = arithEval x
            return (char f)
    }

let rec boolEval2 b =
    prog {
        match b with
        | TT -> return true
        | FF -> return false
        | AEq (x, y) ->
            let! x1 = arithEval2 x
            let! x2 = arithEval2 y
            return (x1 = x2)
        | ALt (x, y) ->
            let! x1 = arithEval2 x
            let! x2 = arithEval2 y
            return (x1 < x2)
        | Not x ->
            let! f = boolEval2 x
            return (not f)
        | Conj (x, y) ->
            let! x1 = boolEval2 x
            let! x2 = boolEval2 y
            return (x1 && x2)
        | IsDigit x ->
            let! f = charEval2 x
            return System.Char.IsDigit(f)
        | IsLetter x ->
            let! f = charEval2 x
            return System.Char.IsLetter(f)
        | IsVowel x ->
            let! f = charEval2 x
            return (isVowel f)
    }

let rec stmntEval2 stm =
    prog {
        match stm with
        | Declare s -> return! declare s
        | Ass (x, a) ->
            let! a1 = arithEval2 a
            return! update x a1
        | Skip -> return ()
        | Seq (stmnt1, stmnt2) ->
            do! stmntEval2 stmnt1
            do! stmntEval2 stmnt2
            return ()
        | ITE (guard, stmnt1, stmnt2) ->
            do! push
            let! g = boolEval2 guard

            if g then
                do! stmntEval2 stmnt1
                do! pop
            else
                do! stmntEval2 stmnt2
                do! pop
        | While (guard, stmnt) ->
            do! push
            let! g = boolEval2 guard

            if g then
                do! stmntEval2 stmnt
                do! stmntEval2 (While(guard, stmnt))
                do! pop
            else
                return ()
                do! pop
    }

(* Part 4 (Optional) *)
type word = (char * int) list
type squareFun = word -> int -> int -> Result<int, Error>

let stmntToSquareFun2 stm : squareFun =
    (fun w pos acc ->
        (push
         >>>= declare "_pos_"
         >>>= update "_pos_" pos
         >>>= declare "_acc_"
         >>>= update "_acc_" acc
         >>>= declare "_result_"
         >>>= stmntEval stm
         >>>= (lookup "_result_")
         |> evalSM (mkState [] w [])))


let stmntToSquareFun stm : squareFun =
    (fun w pos acc ->
        let s = mkState [("_pos_",pos);("_acc_",acc);("_result_",0)] w ["_pos_";"_acc_";"_result_"]
        stmntEval2 stm 
        >>>= lookup "_result_" 
        |> evalSM s 
    )


type coord = int * int

type boardFun = coord -> Result<squareFun option, Error>

let stmntToBoardFun2 (stm: stm) m =
    (fun ((x, y): coord) ->
        match (push
               >>>= declare "_x_"
               >>>= update "_x_" x
               >>>= declare "_y_"
               >>>= update "_y_" y
               >>>= declare "_result_"
               >>>= stmntEval stm
               >>>= lookup "_result_"
               |> evalSM (mkState [] [] [])) with
        | Success i ->
            match Map.tryFind i m with
            | Some v -> Success(Some v)
            | None -> Success None
        | _ -> Success None)

let stmntToBoardFun (stm: stm) m =
    (
        fun ((x, y): coord) ->
        let s = mkState [("_x_",x);("_y_",y);("_result_",0)] [] ["_x_";"_y_";"_result_"]
        stmntEval2 stm
        >>>= lookup "_result_" 
        >>= (fun result -> 
            match Map.tryFind result m with
            | Some v -> ret(Some v)
            | None -> ret None
            )
        |> evalSM s     
    )


type board =
    { center: coord
      defaultSquare: squareFun
      squares: boardFun }

let mkBoard (c: coord) (defaultSq: stm) (boardStmnt: stm) (ids: (int * stm) list) : board =
    { squares =
          List.map (fun (k, sq) -> (k, stmntToSquareFun sq)) ids
          |> Map.ofList
          |> stmntToBoardFun boardStmnt
      center = c
      defaultSquare = stmntToSquareFun defaultSq }