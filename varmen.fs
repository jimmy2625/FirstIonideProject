module varmen

//1. Binary search trees

type 'a bintree =
| Leaf
| Node of 'a bintree * 'a * 'a bintree

let rec insert x t =
    match t with 
    |Leaf -> Node(Leaf,x,Leaf)
    |Node(tree1, y, tree2) -> if x <= y then Node((insert x tree1),x,tree2) else Node((insert x tree2), x, tree1)

let t1 = insert 5 Leaf
let t2 = insert 3 t1

let fromList lst =
    let rec inner lst' acc =
        match lst' with
        |[] -> acc 
        |x::xs -> inner xs (insert x acc)
    inner lst Leaf

fromList [5;3;4;10]