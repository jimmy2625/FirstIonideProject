module exam2021re

(* 1: Binary lists *)
type binList<'a, 'b> =
| Nil
| Cons1 of 'a * binList<'a, 'b>
| Cons2 of 'b * binList<'a, 'b>

(* Question 1.1
    If Nil I return 0
    If Cons1 I only require the second variable to recursively call to and add 1 everytime the case reaches Cons1
    I do the same with Cons2
    The underscore is used to declare that the variable is not used in the function
*)

let rec length lst =
    match lst with
    |Nil -> 0
    |Cons1(_,b) -> length b + 1
    |Cons2(_,b) -> length b + 1
    
(* Question 1.2
    If Nil I return the tuple of empty lists
    If Cons1 I append the element to the first list. Since split returns a tuple, I use the fst keyword to append the element to the first list
    I then recursively run split on the rest of the binary list on the second list with the keyword snd
    I do the same in the Cons2 case except i append to the second list of the tuple.
*)

let rec split lst =
    match lst with
    |Nil -> ([],[])
    |Cons1(a,b) -> (a :: fst (split b), snd (split b))
    |Cons2(a,b) -> (fst (split b), a :: snd (split b))
    
(*
    I do the same as in split except i add 1 to the list instead of appending the element
    Since the first variable in Cons1 and Cons2 are not needed, an underscore is used
*)

let rec length2 lst =
    match lst with
    |Nil -> (0,0)
    |Cons1(_,b) -> (fst (length2 b) + 1, snd (length2 b))
    |Cons2(_,b) -> (fst (length2 b), snd (length2 b) + 1)
    
(* Question 1.3
    If Nil i return Nil
    If Cons1 i create a new Cons1 with f has been applied to a and run the rest of the list recursively
    I do the same for Cons2 except with g applied to a
*)

let rec map f g lst =
    match lst with
    |Nil -> Nil
    |Cons1(a,b) -> Cons1(f a, map f g b)
    |Cons2(a,b) -> Cons2(g a, map f g b)
    
(* Question 1.4
    If Nil i return Nil
    If Cons1 i check if f a is true, if yes then return Cons1 and recursively call filter on the rest. If false then just recursively run filter
    I do the same in Cons2 except i check if g a is true
*)

let rec filter f g lst =
    match lst with
    |Nil -> Nil
    |Cons1(a,b) -> if f a then Cons1(a, filter f g b) else filter f g b
    |Cons2(a,b) -> if g a then Cons2(a, filter f g b) else filter f g b
    
(* Question 1.5
    If Nil i return the acc since this is the base case
    If Cons1 i call fold recursively but apply f with the acc on a
    I do the same in Cons2 except i apply g with the acc on a
*)

let rec fold f g acc lst =
    match lst with
    |Nil -> acc
    |Cons1(a,b) -> fold f g (f acc a) b
    |Cons2(a,b) -> fold f g (g acc a) b
    
(* Question 2 *)

let rec foo xs ys =
    match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | x :: xs, y :: ys when x < y ->
        x :: (foo xs (y :: ys))
    | x :: xs, y :: ys ->
        y :: (foo (x :: xs) ys)

and bar =
    function
    | [] -> []
    | [x] -> [x]
    | xs ->
    let (a, b) = List.splitAt (List.length xs / 2) xs
    foo (bar a) (bar b)
    
(* Question 2.1
    The type of foo is 'a list -> 'a list -> 'a list
    The type of bar is 'a list -> 'a list
    
    bar takes a list and sorts in ascending order
    
    name for foo: merge
    name for bar: mergeSort
    
    name for a and b in bar: firstHalf, secondHalf
*)

(* Question 2.2
    The "and" keyword serves to make the two bindings or records mutually recursive. It is used to call the other function
    and vice versa. Mutual recursion is useful to let the F# compiler know that each function is dependent or related to each other
    
    The program would still work as intended, since the "and" keyword only declares the two functions mutually recursive.
    However, since foo does not call bar, the functions dont need to be mutually recursive 
*)

(* Question 2.3
    I use List.unfold from the list library to construct foo2
    I use the two empty lists as the base case that indicates the end of the list generation
    I then use the same match cases as in foo, instead i include the first element of the lists
    I add the smallest value of the list to the result list and keep updating the state with the rest of the list
    At last the resulting list gives the same result as foo
*)

let foo2 xs ys =
        List.unfold
            (function
             | [], []                      -> None
             | [], y :: ys                 -> Some (y, ([], ys))
             | x :: xs, []                 -> Some (x, (xs, []))
             | x :: xs, y :: ys when x < y -> Some (x, (xs, y :: ys))
             | x :: xs, y :: ys            -> Some (y, (x::xs, ys)))
            (xs, ys)
            
(* Question 2.4
    Foo is not tail-recursive because the recursive call is NOT the last call in the function, since it has to append the x or y after the recursive call
    
    Evaluation of foo [1;2] [3;4;5]
    
    foo [1;2] [3;4;5]
    = 1 :: (foo [2] (3;4;5))
    = 1 :: 2 :: (foo [] [3;4;5])
    = 1 :: 2 :: [3;4;5]
    = [1;2;3;4;5]
    
    As the evaluation shows, the recursive call is NOT the last operation and the elements appended to the list, cannot be
    evaluated since they depend on the evaluation of the recursive call.
*)

(* Question 2.5
    I create the inner function aux that takes a continuation
    I use the same match cases and apply the continuation function to the rest of the list in the two first cases
    In the next two cases i call the aux function and use the anonymous function to append the x to the intermediate result
    I then call the aux with the id and the two lists 
*)

let fooTail xs ys =
    let rec aux c xs ys =
        match xs, ys with
        | [], ys -> c ys
        | xs, [] -> c xs
        | x :: xs, y :: ys when x < y ->
            aux (fun result -> c (x :: result)) xs (y::ys)
        | x :: xs, y :: ys ->
            aux (fun result -> c (y :: result)) (x :: xs) ys
    aux id xs ys
    
(* Question 3

*)

let isPerfectSquare n =
    let root = int (sqrt (float n))
    root * root = n

let closestPerfectSquare x =
    let rec findClosestPerfectSquare n =
        if isPerfectSquare n then n
        else findClosestPerfectSquare (n - 1)
    findClosestPerfectSquare x

let approxSquare x num =
    if x < 0 then
        failwith "x must be a non-negative integer."

    let y = closestPerfectSquare x
    if num = 0 then
        sqrt (float y)
    else
        let mutable r = y
        for _ in 1 .. num do
            r <- (x / r + r) / 2  // No need to convert the result back to int

        r




