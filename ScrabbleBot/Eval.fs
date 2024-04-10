module internal Eval

open StateMonad
open Types

(* Code for testing *)

let hello = [ ('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1) ]
let state = mkState [ ("x", 5); ("y", 42) ] hello [ "_pos_"; "_result_" ]
let emptyState = mkState [] [] []

let add (a: SM<int>) (b: SM<int>) =
    a >>= (fun resultA -> b >>= (fun resultB -> ret (resultA + resultB)))

let div (a: SM<int>) (b: SM<int>) =
    a
    >>= (fun resultA ->
        b
        >>= (fun resultB ->
            if resultB <> 0 then
                ret (resultA / resultB)
            else
                fail DivisionByZero))

let modul (a: SM<int>) (b: SM<int>) =
    a
    >>= (fun resultA ->
        b
        >>= (fun resultB ->
            if resultB <> 0 then
                ret (resultA % resultB)
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
    (~~b1) .||. b2 (* boolean implication *)

let (.=.) a b = AEq(a, b)
let (.<.) a b = ALt(a, b)
let (.<>.) a b = ~~(a .=. b)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b)

let (.>=.) a b =
    ~~(a .<. b) (* numeric greater than or equal to *)

let (.>.) a b =
    ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)


let rec arithEval a : SM<int> =
    match a with
    | N x -> ret x
    | V x -> lookup x
    | WL -> wordLength
    | PV x -> arithEval x >>= (fun x -> pointValue x)
    | Add(a, b) -> arithEval a >>= (fun a -> arithEval b >>= (fun b -> ret (a + b)))
    | Sub(a, b) -> arithEval a >>= (fun a -> arithEval b >>= (fun b -> ret (a - b)))
    | Mul(a, b) -> arithEval a >>= (fun a -> arithEval b >>= (fun b -> ret (a * b)))
    | Div(a, b) -> div (arithEval a) (arithEval b)
    | Mod(a, b) -> modul (arithEval a) (arithEval b)
    | CharToInt a -> charEval a >>= (fun a -> ret (int a))

and charEval c : SM<char> =
    match c with
    | C x -> ret x
    | CV x -> arithEval x >>= (fun x -> pointValue x >>= (fun x -> charEval (IntToChar(N x))))
    | ToUpper x -> charEval x >>= (fun x -> ret (System.Char.ToUpper x))
    | ToLower x -> charEval x >>= (fun x -> ret (System.Char.ToLower x))
    | IntToChar x -> arithEval x >>= (fun x -> ret (char x))

and boolEval b : SM<bool> =
    match b with
    | TT -> ret true
    | FF -> ret false
    | AEq(a, b) -> arithEval a >>= (fun a -> arithEval b >>= (fun b -> ret (a.Equals b)))
    | ALt(a, b) -> arithEval a >>= (fun a -> arithEval b >>= (fun b -> ret (a < b)))
    | Not a -> boolEval a >>= (fun a -> ret (not a))
    | Conj(a, b) -> boolEval a >>= (fun a -> boolEval b >>= (fun b -> ret (a && b)))
    | IsVowel a ->
        charEval a
        >>= (fun a ->
            match a with
            | 'E'
            | 'Y'
            | 'U'
            | 'I'
            | 'O'
            | 'A' -> ret true
            | _ -> ret false)
    | IsLetter a -> charEval a >>= (fun a -> ret (System.Char.IsLetter a))
    | IsDigit a -> charEval a >>= (fun a -> ret (System.Char.IsDigit a))


type stmnt = (* statements *)
    | Declare of string (* variable declaration *)
    | Ass of string * aExp (* variable assignment *)
    | Skip (* nop *)
    | Seq of stmnt * stmnt (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt (* while statement *)

let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

type StateBuilder() =

    member this.Bind(f, x) = f >>= x
    member this.Return(x) = ret x
    member this.ReturnFrom(x) = x
    member this.Delay(f) = f ()
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

type board =
    { center: coord
      defaultSquare: square
      squares: boardFun }

let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
