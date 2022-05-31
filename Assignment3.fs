module FirstIonideProject.Assignment3
type aExp =
 | N of int // Integer value
 | V of string // Variable
 | WL // Length of the word
 | PV of aExp // Point value of character at specific word index
 | Add of aExp * aExp // Addition
 | Sub of aExp * aExp // Subtraction
 | Mul of aExp * aExp // Multiplicationtype

let (.+.) a b = Add (a,b)
let (.-.) a b = Sub (a,b)
let (.*.) a b = Mul (a,b)

//let a2 = N 4 .+. (N 5 .-. N 6);;

// 3.1
let rec arithEvalSimple = function
| N n -> n
| Add (a, b) -> arithEvalSimple a + arithEvalSimple b
| Sub (a,b) -> arithEvalSimple a - arithEvalSimple b
| Mul (a,b) -> arithEvalSimple a * arithEvalSimple b

// 3.2
let rec arithEvalState a s =
    match a with
    | N n  -> n
    | Add (a,b) -> (arithEvalState a s) + (arithEvalState b s)
    | Sub (a,b) -> (arithEvalState a s) - (arithEvalState b s)
    | Mul (a,b) -> (arithEvalState a s) * (arithEvalState b s)
    | V v  -> 
            match Map.tryFind(v) s with 
            |Some n -> n
            |None -> 0
// 3.3
let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_");;
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithDoubleWordScore = N 2 .*. V "_acc_";;
let arithTripleWordScore = N 3 .*. V "_acc_";;

type word = (char * int) list

let hello: word = ['H', 4; 'E', 1; 'L', 1; 'L', 1; 'O', 1];;
let rec arithEval a (w: word) s = 
 match a with 
 | N n  -> n
 | V v  -> 
     match Map.tryFind(v) s with 
     |Some a -> a
     |None -> 0
 | WL -> List.length w
 | PV (a) -> snd w.[arithEval a w s]
 | Add (a,b) -> (arithEval a w s) + (arithEval b w s)
 | Sub (a,b) -> (arithEval a w s) - (arithEval b w s)
 | Mul (a,b) -> (arithEval a w s) * (arithEval b w s);;
 
  //3.4
type cExp =
 | C of char (* Character value *)
 | ToUpper of cExp (* Converts lower case to upper case character,
 non-characters unchanged *)
 | ToLower of cExp (* Converts upper case to lower case character,
 non characters unchanged *)
 | CV of aExp (* Character lookup at word index *)

let rec charEval c (w:word) s =
  match c with
  | C c -> c
  | ToUpper (c) -> System.Char.ToUpper (charEval c w s)
  | ToLower (c) -> System.Char.ToLower (charEval c w s)
  | CV (a) -> fst w.[arithEval a w s];;
  
//3.5

type bExp =
  | TT (* true *)
  | FF (* false *)

  | AEq of aExp * aExp (* numeric equality *)
  | ALt of aExp * aExp (* numeric less than *)

  | Not of bExp (* boolean not *)
  | Conj of bExp * bExp (* boolean conjunction *)

  | IsLetter of cExp (* check for letter *)
  | IsDigit of cExp (* check for digit *)
  | IsVowel of cExp (* check for vowel *)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)
let (.=.) a b = AEq (a, b)
let (.<.) a b = ALt (a, b)
let (.<>.) a b = ~~(a .=. b) (* numeric inequality *)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b) (* numeric less than or equal to *)
let (.>=.) a b = ~~(a .<. b) (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b);; (* numeric greater than *)
  
let vowel = ['A'; 'E'; 'I'; 'O'; 'U'; 'a'; 'e'; 'i'; 'o'; 'u']

let isVowel (c: char) =
    match List.tryFindIndex (fun x -> x = c) vowel with
    | Some(_) -> true
    | None -> false;;
    
let rec boolEval b (w:word) s =
   match b with
   | TT           -> true
   | FF           -> false
   | AEq (a1, a2) -> (arithEval a1 w s) = (arithEval a2 w s)
   | ALt (a1, a2) -> (arithEval a1 w s) < (arithEval a2 w s)
   | Not (b)      -> not (boolEval b w s)
   | Conj (b1, b2)-> (boolEval b1 w s) && (boolEval b2 w s)
   | IsLetter (c) -> System.Char.IsLetter (charEval c w s)
   | IsDigit (c)  -> System.Char.IsDigit (charEval c w s)
   | IsVowel (c)  -> List.contains (charEval c w s) vowel
   
let isConsonant (c: cExp) = ~~(IsVowel c)
 
// A statement stmnt is a new type 
type stmnt =
| Skip                        (*Does nothing*)
| Ass of string * aExp        (*variable assignment*)
| Seq of stmnt * stmnt        (*sequential composition*)
| ITE of bExp * stmnt * stmnt (* if-then-else statement *)
| While of bExp * stmnt       (* while statement *)

//evaltStmnt takes a stmnt a word and a map and returns the map after updating it
let rec evalStmnt (st:stmnt) (w: word) (s: Map<string,int>) =
   match st with
   // skips simply return the map without doing anything
   | Skip -> s
   // Assign takes a pair of a string and aexp and adds the association to the map
   //Using the function arithEval on a, allows it to be evaluated before its value is added to the map as as the value of the key str. 
   | Ass (str, a) -> Map.add str (arithEval a w s) s
    //Whatever evalStmnt s1 w s is evaluated to, is passed on to evalStmnt s2
   // (evalStmnt s2 w) is missing a variable s, which is the one it gets from (evalStmnt s1 w s), since evalStmt returns a map
   | Seq (s1, s2) -> (evalStmnt s1 w s) |> (evalStmnt s2 w)
   | ITE (b, s1, s2) -> if (boolEval b w s) then (evalStmnt s1 w s) else (evalStmnt s2 w s)
   // The while case keeps evaluating while (boolEval b w s) returns true, then (evalStmnt s1 w s) is evaluated and given to evalStmnt (While(b, s1))
   | While (b, s1) -> if (boolEval b w s) then (evalStmnt s1 w s) |> evalStmnt (While(b, s1)) w else s;;
   
evalStmnt (Ass ("x", N 5)) [] Map.empty;;

evalStmnt (Seq (Ass ("x", WL), Ass ("y", N 7))) hello Map.empty;;

//If WL is bigger or equal to 5, then assign x to 1, else assign x to 2
evalStmnt (ITE (WL .>=. N 5, Ass ("x", N 1), Ass ("x", N 2))) hello Map.empty;;

// while whatever "x" is mapped to, is smaller than WL, which is 5, Seq (Ass ("y", V "y" .+. V "x"),Ass ("x", V "x" .+. N 1))
// Seq... first first runs Ass ("y", V "y" .+. V "x")
evalStmnt (While (V "x" .<=. WL, Seq (Ass ("y", V "y" .+. V "x"),Ass ("x", V "x" .+. N 1)))) hello Map.empty;; 

