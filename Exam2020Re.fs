module hej
(* 1: Binary search trees *)

    type 'a bintree = 
    | Leaf
    | Node of 'a bintree * 'a * 'a bintree

(* Question 1.1 *)

    let rec insert x bst =
        match bst with
        |Leaf -> Node(Leaf,x,Leaf)
        |Node (tree,a,tree2) -> if x < a then Node((insert x tree), a, tree2) else Node((tree, a, (insert x tree2)))
    
(* Question 1.2 *)

    let fromList lst =
        let rec inner lst' acc =
            match lst' with
            |[] -> acc
            |x::xs -> inner xs (insert x acc)
        inner lst Leaf

(* Question 1.3 *)

    let rec fold f acc t =
        match t with
        |Leaf -> acc
        |Node (tree,a,tree2) -> fold f (f (fold f acc tree) a) tree2
        
    fold (fun acc x -> x - acc) 0 (fromList [3;5;4;10])
    
    let rec foldBack f acc t =
        match t with
        |Leaf -> acc
        |Node (tree,a,tree2) -> foldBack f (f (foldBack f acc tree2) a) tree
        
    let rec inOrder2 t =
        let rec auxInOrder t acc =
            match t with
            |Leaf                -> acc
            |Node (tree,a,tree2) -> (foldBack (fun acc x -> x :: acc) acc t)
        auxInOrder t []
    
    let inOrder t = foldBack (fun acc x -> x :: acc) [] t

(* Question 1.4 *)

    (* 

    Q: Consider the following map function

    *)

    let rec badMap f =
      function
      | Leaf -> Leaf
      | Node (l, y, r) -> Node (badMap f l, f y, badMap f r)

    (*
    Even though the type of this function is `('a -> 'b) -> 'a bintree -> 'b bintree` 
    as we would expect from a map function, this  function does not do what
    we want it to do. What is the problem? Provide an example to demonstrate the problem.

    A: <Your answer goes here>
    *)

    let rec map _ = failwith "not implemented"

(* 2: Code Comprehension *)
    let rec foo =
        function
        | [x]                 -> [x]
        | x::y::xs when x > y -> y :: (foo (x::xs))
        | x::xs               -> x :: foo xs

    let rec bar =
        function
        | [x]          -> true
        | x :: y :: xs -> x <= y && bar (y :: xs)

    let rec baz =
        function
        | []               -> []
        | lst when bar lst -> lst
        | lst              -> baz (foo lst)
     
    foo [6;5;3;7;1]
    bar [1;5;3]
    baz [3;2;1]
    
(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo,  bar, and baz?

    A: foo is of type 'a list -> 'a list
       bar is of type 'a list -> bool
       baz is of type 'a list -> 'a list

    Q: What do functions ```bar```, and ```baz``` do 
       (not `foo`, we admit that it is a bit contrived)? 
       Focus on what they do rather than how they do it.

    A: bar compares every element in the list recursively and only returns True if all of the elements are in order in the list
       baz returns the list if bar is true (ordered) and if not sorts the list using foo


    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: foo: stepOfSort
       bar: testIfSorted
       baz: sortByFoo
    
    *)
        

(* Question 2.2 *)

 
    (* 
    The functions foo and bar generate a warning during compilation: 
    'Warning: Incomplete pattern matches on this expression.' 
    
    Q: Why does this happen, and where? 

    A: For both foo and bar it is indicative of a missing empty list case


    Q: For these particular three functions will this incomplete 
       pattern match ever cause problems for any possible execution of baz? 
       If yes, why; if no, why not.

    A: This will not cause any problems for the execution of baz since foo and bar's incomplete pattern matching regarding the empty list
    will never be reached in baz, since it is the case of "lst" which can not be empty.

    *)

    let rec foo2 =
        function
        | []                  -> []
        | [x]                 -> [x]
        | x::y::xs when x > y -> y :: (foo (x::xs))
        | x::xs               -> x :: foo xs

    let rec bar2 =
        function
        | []           -> true
        | [x]          -> true
        | x :: y :: xs -> x <= y && bar (y :: xs)

    (* Uncomment code to run after you have written foo2 and bar2 *)
    
    let rec baz2 =
      function
      | lst when bar2 lst -> lst
      | lst               -> baz2 (foo2 lst)
    
    baz2 [1;3;2]