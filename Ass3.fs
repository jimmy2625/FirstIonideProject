module Ass3

type aExp =
| N of int              // Integer value
| V of string           // Variable
| WL                    // Length of the word
| PV of aExp            // Point value of character at specific word index
| Add of aExp * aExp    // Addition
| Sub of aExp * aExp    // Subtraction
| Mul of aExp * aExp    // Multiplication

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

let a1 = N 42
let a2 = N 4 .+. (N 5 .-. N 6)
let a3 = N 4 .*. N 2 .+. N 34
let a4 = (N 4 .+. N 2) .*. N 34
let a5 = N 4 .+. (N 2 .*. N 34)
let a6 = V "x"
let a7 = N 4 .+. (V "y" .-. V "z")

let rec arithEvalSimple a = 
    match a with
    |N n -> n
    |Add (a,b) -> arithEvalSimple a + arithEvalSimple b
    |Sub (a,b) -> arithEvalSimple a - arithEvalSimple b
    |Mul (a,b) -> arithEvalSimple a * arithEvalSimple b

let rec arithEvalState a s =
    match a with
    |N n -> n
    |V v -> match Map.tryFind(v) s with
            |Some n -> n
            |None -> 0
    |Add (a,b) -> arithEvalState a s + arithEvalState b s
    |Sub (a,b) -> arithEvalState a s - arithEvalState b s
    |Mul (a,b) -> arithEvalState a s * arithEvalState b s
    
type word = (char * int) list

let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_")
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_")
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_")
let arithDoubleWordScore = N 2 .*. V "_acc_"
let arithTripleWordScore = N 3 .*. V "_acc_"