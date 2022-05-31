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

//no logic has been made yet so this simply evaluates to an aExp = Add (N 4, Sub (N 5, N 6))
let a1 = N 42;;
let a2 = N 4 .+. (N 5 .-. N 6);;
let a3 = N 4 .*. N 2 .+. N 34;;
let a4 = (N 4 .+. N 2) .*. N 34;;
let a5 = N 4 .+. (N 2 .*. N 34);;



// 3.1
//now the aExp's can be evaluated, the function will recurse until an aExp is of type N
//Then the value of the aExp's will be calculated
let rec arithEvalSimple = function
    | N n -> n
    | Add (a, b) -> arithEvalSimple a + arithEvalSimple b
    | Sub (a,b) -> arithEvalSimple a - arithEvalSimple b
    | Mul (a,b) -> arithEvalSimple a * arithEvalSimple b

arithEvalSimple a1;;
arithEvalSimple a4;;

// 3.2
// With a State of type Map<String,Int> Variables can now be evaluated.
// When an aExp of type V is evaluated, it is looked up in the state with the Map.tryFind function
// Map.tryFind(v) will in case of Success return an a integer n that will result in a base case
// if the Variable V is not mapped to anything, it will return 0 which also results in a base case
// The same aExp can now be evaluated with different maps, where Variables are mapped to different integers
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

let a7 = N 4 .+. (V "y" .-. V "z");;
arithEvalState a7 (Map.ofList [("x", 4); ("y", 5)]);;
arithEvalState a7 (Map.ofList [("y", 4); ("z", 5)]);;

// 3.3
// A word is of type (char * int) list where is is a list of characters and their values
// To calculate the number of points a given character is worth on a given square on the board
// we must have the word, the position in the word the character is located, and an accumilator
// in our language, pos and acc will be stored in the map as variables
// The word will be a new parameter of the evaluation function
// and PV is added to the aExp
// PointValue(PV) has an aExp x which is the index in the word list that the character is located at
let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_");;
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithDoubleWordScore = N 2 .*. V "_acc_";;
let arithTripleWordScore = N 3 .*. V "_acc_";;

type word = (char * int) list

let hello: word = ['H', 4; 'E', 1; 'L', 1; 'L', 1; 'O', 1];;

// when evaluating a wordlength, List.length is used on the word
// when a point value is evaluated the index of word is used to return the exact character
// to get the int returned of that character, snd is used since snd ('H', 4) will return 4
// To account for double word scores above, the accumilator is simply muliplied
// To account for double letter scores above, the Point Value is multiplied
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
     
arithEval arithTripleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 42)]);;
// In the calculation, the aExp arithTripleLetterScore is evaluated with the word hello and a Map.ofList [("_pos_", 4); ("_acc_", 42)]
// The calculation will be ( 3 * (PV 4(1)) + 42)) which evaluates to 45 
  
// 3.4
// A new type cExp is introduced
type cExp =
     | C of char (* Character value *)
     | ToUpper of cExp (* Converts lower case to upper case character,
     non-characters unchanged *)
     | ToLower of cExp (* Converts upper case to lower case character,
     non characters unchanged *)
     | CV of aExp (* Character lookup at word index *)
 
// To evaluate characters, we make a function that returns a character instead of an integer
// The recursion of this function can end when the cExp is simply a character, or when evaluating the charavter value of an aExp
// To get the character returned from a word, fst is used since fst ('H', 4) will return H
// To get the index from a aExp we have to use the arithEval function inside or charEval function
// This is possible because a cExp of type Character Value (CV) takes an aExp as parameter
// This aExp is then used for evaluation of a Character value, since we need an integer to get the index of the word

let rec charEval c (w:word) s =
  match c with
  | C c -> c
  | ToUpper (c) -> System.Char.ToUpper (charEval c w s)
  | ToLower (c) -> System.Char.ToLower (charEval c w s)
  | CV (a) -> fst w.[arithEval a w s];;

// This evaluation takes the fst w.[(V "x" .-. N 1)] which in the map where x is mapped to 5 and then -1 is 4
// The 4th letter of Hello is o and thus O is returned. 
charEval (CV (V "x" .-. N 1)) hello (Map.ofList [("x", 5)]);;
  
//3.5
// boolean Expression are of type bExp and can take arguments of the other expressions
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
    
// Evaluating a bExps with boolEval will always return a boolean
// AEq (Arithmic equality) takes to Aexp that each evaluates to some int with the arithEval function, and then tests their equality with =
// ALT (Arithmic less than) does the same but with <
// Not takes the result of evaluating a bExp with boolEval and returns the opposite, so evalutating a bExp that returns true, will then return false
// Conj (Conjunction) evaluates 2 bExp and will only return true, if the both evaluate to true
// isLetter and isDigit takes the evaluation of a cExp and tests of the resulting Character is either a letter or a digit
// isVowel takes the evaluation of a cExp, and tests whether it is present in the list of vowels
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
 
// A statement stmnt is a new type that is used to manipulate the state or map used in the evaluating expressions

type stmnt =
| Skip                        (*Does nothing*)
| Ass of string * aExp        (*variable assignment*)
| Seq of stmnt * stmnt        (*sequential composition*)
| ITE of bExp * stmnt * stmnt (* if-then-else statement *)
| While of bExp * stmnt       (* while statement *)

//evaltStmnt takes a stmnt a word and a map and returns the map after updating it
// skip just returns the map unchanged
// Assign (Ass) assigns a String to some integer in form of a aExp that will evaluate to an int after using arithEval
// This is done with Map.add which takes the string, the int that the aExp evaluates to, and the map and updates it
// Sequence (Seq) takes 2 stmnt and uses the updated map that the first statement returns, and pipes it as an argument to the second evaluation
// See that the second stmt s2 does not have a map/state s cause it uses that which s1 returns
// If then Else, takes a bExp and 2 statements. If the evaluation of the bExp return true, then the first statement is evaluated, else the second
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

