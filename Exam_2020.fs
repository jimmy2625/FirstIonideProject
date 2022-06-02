module Exam202

//1 Insertion Sort

(*
    If the list is empty, simply insert the element in the list
    If the element is smaller than the head of the list, put it first at the list
    Otherwise, call insert recursively on the rest of the list until it is empty
*)

//1.1
let rec insert x lst =
    match lst with 
    |[] -> [x]
    |a::b when x < a -> x::a::b
    |a::b -> a::(insert x b)

insert true []
insert 5 [1; 3; 8; 9]

(*
    We assume that no empty list can be sorted, just return the same list
    Otherwise, call insert on the head of the list and recursively call insertionSort on the tail
    The logic behind this is to recursively call insert so the elements in the list always are sorted when inserted
*)

let rec insertionSort lst =
    match lst with 
    |[x] -> [x]
    |a::b -> (insert a) (insertionSort b)

insertionSort [5; 3; 1; 8; 9]

(*
    I use an auxiliary function to do the tail recursive method
    I use the same match cases as in insert, but instead i append the acc
    For the last case i use the head and cons operator for the recursive call to insertInner
*)

//1.2
let insertTail x lst =
    let rec inner lst' acc =
        match lst' with
        |[] -> x::acc
        |a::b when x < a -> x::a::b @ acc
        |a::b -> a::inner b acc
    inner lst []

insertTail 7 [1;3;5;8;9]
insertTail 5 [1; 3; 8; 9]

(*
    I do the same is in insertTail but in the last case i sort the rest of the list
    Then I insert the head in to the acc
*)

let insertionSortTail lst =
    let rec inner lst' acc =
        match lst' with 
        |[] -> acc
        |a::b -> inner b (insertTail a acc)
    inner lst []

insertionSortTail [5; 3; 1; 8; 9]

//1.3
(*
    The higher order functions is not a good fit for the insert method
    because the higher order functions has to run through the whole list and cannot return in the middle of a fold for example

    InsertionSort 2 can be implemented by using List.fold with the anonymous function that uses
    the insertTail on all the x-elements in the lst. The empty list is the inital state and the list returned is acc
*)
let insertionSort2 lst = List.fold (fun y x -> insertTail x y) [] lst

//1.4
(*
    I insert the f as a clause in my parameters
    To make the input respect the f, I call f on x and a in the comparison clause
*)

let insertBy f x lst =
    let rec insertInner lst' acc =
        match lst' with
        |[] -> x::acc
        |a :: b when f x < f a -> x::a::b @ acc
        |a :: b -> a:: insertInner b acc
    insertInner lst []

insertBy String.length "abc" ["q"; "bb"; "lbcd"]

let insertionSortBy f lst = List.fold (fun acc x -> insertBy f x acc) [] lst

insertionSortBy String.length ["bb"; "lbcd"; "q"; "abc"]

//2 Code Comprehension
let rec foo x =
 function
 | y :: ys when x = y -> ys
 | y :: ys -> y :: (foo x ys)

foo 12 [10;11;12]

(*
    The foo function takes an element x and checks if the head of the given list is the same as x
    If yes, then return the tail of the list, the list without the x
    If no, then concatenate the head of the list with the recursive call to foo on the rest of the list
*)

let rec bar x =
 function
 | [] -> []
 | xs :: xss -> (x :: xs) :: bar x xss

bar 10 [[10;11];[12;13]]

(*
    The bar function takes an element x and a list of lists
    If the list is empty, then simply return the empty list
    Otherwise, we add the x element to the head of the list and concatenates that to the list of lists 
    using a recursive call to bar
*)

let rec baz =
 function
 | [] -> []
 | [x] -> [[x]]
 | xs ->
    let rec aux =
        function
        | [] -> []
        | y :: ys -> ((foo y >> baz >> bar y) xs) @ (aux ys)
    aux xs

baz [1;2;3]

(*
    If the list is empty it simply returns the empty list
    If the list contains a single element then return a list of lists containing that element
    If there is a list, the recursive aux is called
    The aux returns the empty list if the list is empty
    Otherwise it calls foo(remove on the head) then compositioned to baz which returns both orders of ys
    Then it is compositioned to bar which adds the y back at the head of each list of lists
    This is essentially all permutations of the given list
*)

//2.1
(*
    The type of foo is 'a -> list<'a> -> list<'a>
    The type of bar is 'a -> list<list<'a>> -> list<list<'a>>
    The type of baz is list<'a> -> list<list<'a>>

    The foo function can be called remove, because it removes the x element from the list
    The bar function can be called addHeadToAllLists, since it adds the element at the head of all lists
    The baz function can be called permutations, because it results in all permutations of the given list
*)

//2.2
(*
    The warning comes from the fact that the foo function does not consider the case of the empty list
    
    In the function baz, the only instance of executing foo is when there is at least two elements in the list (line 128)
    Therefore foo will never execute with an empty list in baz

    If we want to create a foo2 that handles this incomplete pattern matching we would simply add the empty list -> empty list clause
*)

let rec foo2 x = 
    function
    | [] -> []
    | y :: ys when x = y -> ys
    | y :: ys -> y :: (foo x ys)

foo2 10 []


//2.3
(*
    The typing can be found using ionide and creating a new let-expression including the line of code
    Here the typing is: 'a -> (list<'a> -> list<list<'a>>)

    It first removes the element from the list
    It finds all permutations of the list without the element
    Then it adds back the element to all list of lists
*)

let example y = foo y >> baz >> bar y

//2.4
(*
    To make a bar2 using higher-order functions I will use the List.map function to map each list to the list that has x in front
*)

let bar2 x lstlst = List.map (fun lst -> x::lst) lstlst

//2.5
(*
    To make baz2 using higher-order functions I will use the List.collect method in the final match case of baz
    List.collect works like List.map except it maps every element to a list and concatenates all the sublist in the end
*)

let rec baz2 =
 function
 | [] -> []
 | [x] -> [[x]]
 | xs -> List.collect (fun y -> (foo y >> baz >> bar y) xs) xs


//2.6
 (*
     To make it a tail recursive solution I must make sure that any call to aux is made at the outermost point of the branch
     (right after the arrow) Also add the continuation call in every branch
 *)

let fooTail x lst =
    let rec aux c = 
        function
        | [] -> c []
        | y :: ys when x = y -> c ys
        | y :: ys -> aux (fun result -> c(y :: result)) ys
    aux id lst

//3 Rock Paper Scissors
(*
    I create my own types of shape of result
*)
//3.1

type shape = Rock | Paper | Scissors

type result = P1Win | P2Win | Draw

//I match with my moves and return the result of the moves

let rps s1 s2 =
    match s1,s2 with 
    |Paper, Rock -> P1Win
    |Rock, Scissors -> P1Win
    |Scissors, Paper -> P1Win
    |Rock, Paper -> P2Win
    |Paper, Scissors -> P2Win
    |Scissors, Rock -> P2Win
    |_ -> Draw

//3.2
(*
    Create a function parrot, that just mimics the same move
*)

let parrot s =
    function 
    |[] -> s
    |(_,m) :: _ -> m

(*
    Create a function beatingStrat that beats the shape the opponent has played the most
    To do this, first i create a let-binding on the opponentMoves that takes the snd of the moves list and uses List.map to get the values
    I then count the amount of rocks, papers and scissors in the opponentMoves by filtering out the shape and piping it to List.length
    I then do a simple if statement to check which shape i should return
*)

let beatingStrat moves =
    let opponentMoves = List.map snd moves
    let numRocks = opponentMoves |> List.filter (fun s -> s = Rock) |> List.length
    let numPapers = opponentMoves |> List.filter (fun s -> s = Paper) |> List.length
    let numScissors = opponentMoves |> List.filter (fun s -> s = Scissors) |> List.length

    if numScissors >= numPapers && numScissors >= numRocks then Rock
    else if numRocks >= numPapers && numRocks >= numScissors then Paper
    else Scissors

//4 RPN

//4.1
type stack = int list

let emptyStack : stack = []

type SM<'a> = S of (stack -> ('a * stack) option)

let ret x = S (fun s -> Some (x, s))

let fail = S (fun _ -> None)

let bind f (S a) : SM<'b> =
    S (fun s ->
        match a s with
        | Some (x, s') ->
            let (S g) = f x
            g s'
        | None -> None)

let (>>=) x f = bind f x
let (>>>=) x y = x >>= (fun _ -> y)

let evalSM (S f) = f emptyStack

let push x = S (fun stack -> Some((),x::stack))

let pop = S (fun stack -> if stack.Length > 0 then Some(stack.Head, stack.Tail) else None)

push 5 >>>= push 6 >>>= pop |> evalSM