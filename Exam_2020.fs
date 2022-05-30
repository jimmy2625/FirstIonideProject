module Exam2020

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

let rec bar x =
 function
 | [] -> []
 | xs :: xss -> (x :: xs) :: bar x xss

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