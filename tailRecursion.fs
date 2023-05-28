module tailRecursion

let rec fact : int -> int =
    function
    | 0 -> 1
    | x -> x * fact (x - 1)
//same thing but with an accumilator
let rec factA (acc : int) : int -> int =
    function
    | 0 -> acc
    | x -> factA (acc * x) (x - 1)

//Hide the extra parameter
//Call the inner function with 1 (cannot * with 0)
let factB x =
    let rec factBInner acc = 
             function
             | 0 -> acc
             | x -> factBInner (acc * x) (x - 1)
    factBInner 1 x
    
//Without continuation
let rec append xs ys =
    match xs with
    | []      -> ys
    | x :: xs -> x :: (append xs ys)
//With continuation
//The continuation c is called after the recursive function is finished
//Since base case is c ys, it means c is a function that takes a list.
//user computation when we cannot reorder the way a function computes its results (e.g. the foldBack function)
//or when we have multiple recursive calls that cannot be combined into one (e.g. tree traversal)
//The C and the function returns a list
let rec appendC xs ys c =
    match xs with
    | []       -> c ys
    | x :: xs' -> appendC xs' ys
                      (fun r -> c (x :: r))        
                                    
let (@) xs ys = appendC xs ys

type BinTree =
    | Leaf
    | Node of BinTree * int * BinTree
let rec sum (t : BinTree) : int =
    match t with
    | Leaf -> 0
    | Node (l,n,r) -> sum l + sum r + n

//not good as it is not tail recursive when there are 2 recursive calls
let rec sumA (t : BinTree) (acc : int) : int =
    match t with
    | Leaf -> acc
    | Node (l,n,r) -> sumA r (sumA l (n + acc))
//this is good
let rec sumC (t : BinTree) (c : int -> int) : int =
  match t with
  | Leaf -> c 0
  | Node (l,n,r) ->
    sumC l (fun vl ->
           sumC r (fun vr -> c (vl + vr + n)))