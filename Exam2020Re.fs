module exam2020Re

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
        |Node (tree,a,tree2) -> fold f a tree
        
    fold (fun acc x -> x - acc) 0 (fromList [3;5;4;10])
    let foldBack _ = failwith "not implemented"
    let inOrder _ = failwith "not implemented"