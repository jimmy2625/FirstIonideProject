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

let hello: word = ['H', 4; 'E', 1; 'L', 1; 'L', 1; 'O', 1]

let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_")
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_")
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_")
let arithDoubleWordScore = N 2 .*. V "_acc_"
let arithTripleWordScore = N 3 .*. V "_acc_"

let rec arithEval a (w:word) s =
    match a with
    |N n -> n
    |V v -> match Map.tryFind(v) s with
            |Some n -> n
            |None -> 0
    |WL -> w.Length
    |PV a -> snd w.[arithEval a w s]
    |Add (a,b) -> arithEval a w s + arithEval b w s
    |Sub (a,b) -> arithEval a w s - arithEval b w s
    |Mul (a,b) -> arithEval a w s * arithEval b w s

type cExp =
| C of char (* Character value *)
| ToUpper of cExp (* Converts lower case to upper case character, non-letters are unchanged *)
| ToLower of cExp (* Converts upper case to lower case character, non-letters are unchanged *)
| CV of aExp (* Character lookup at word index *)

let rec charEval c (w:word) s = 
    match c with
    |C c -> c
    |ToUpper c -> System.Char.ToUpper(charEval c w s)
    |ToLower c -> System.Char.ToLower(charEval c w s)
    |CV a -> fst w.[arithEval a w s]

type bExp =
| TT (* true *)
| FF (* false *)
| AEq of aExp * aExp (* numeric equality *)
| ALt of aExp * aExp (* numeric less than *)
| Not of bExp (* boolean not *)
| Conj of bExp * bExp (* boolean conjunction *)
| IsDigit of cExp (* check for digit *)
| IsLetter of cExp (* check for letter *)
| IsVowel of cExp (* check for vowel *)
| IsConsonant of cExp

let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)

let (.=.) a b = AEq (a, b)
let (.<.) a b = ALt (a, b)
let (.<>.) a b = ~~(a .=. b) (* numeric inequality *)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b) (* numeric less than or equal to *)
let (.>=.) a b = ~~(a .<. b) (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

let vowels = ['a';'e';'i'; 'o'; 'u']

let rec boolEval b (w:word) s =
    match b with
    |TT -> true
    |FF -> false
    |AEq (a,b) -> arithEval a w s = arithEval b w s
    |ALt (a,b) -> arithEval a w s < arithEval b w s
    |Not b -> not (boolEval b w s)
    |Conj (a,b) ->  boolEval a w s && boolEval b w s
    |IsDigit c -> System.Char.IsDigit(charEval c w s)
    |IsLetter c -> System.Char.IsLetter(charEval c w s)
    |IsVowel c -> List.contains (System.Char.ToLower(charEval c w s)) vowels