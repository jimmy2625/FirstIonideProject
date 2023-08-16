module Recursion

//not tail-recursive implementation
let rec insert x lst =
    match lst with
    |[] -> [x]
    |a::b when x < a -> x::a::b
    |a::b -> a::(insert x b)

insert 5 [1; 3; 8; 9]

//tail-recursive implementation with aux function
let insertTail x lst =
    let rec aux lst' acc =
        match lst' with
        |[] -> acc
        |a::b when x < a -> x::a::b @ acc
        |a::b -> a::aux b acc
    aux lst []

insertTail 5 [1; 3; 8; 9]

//not tail-recursive implementation
let rec foo x =
 function
 | y :: ys when x = y -> ys
 | y :: ys -> y :: (foo x ys)

//tail-recursive implementation with continuation
let fooTail x lst =
    let rec aux c = 
        function
        | [] -> c []
        | y :: ys when x = y -> c ys
        | y :: ys -> aux (fun result -> c(y :: result)) ys
    aux id lst