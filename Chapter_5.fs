module FirstIonideProject.Chapter_5

(*
map: (’a -> ’b) -> ’a list -> ’b list, where
map f xs = [f (x0 ); f (x1 ); . . . ; f (xn−1 )]
exists: (’a -> bool) -> ’a list -> bool, where
exists p xs = ∃x ∈ xs.p(x)
forall: (’a -> bool) -> ’a list -> bool, where
forall p xs = ∀x ∈ xs.p(x)
tryFind: (’a -> bool) -> ’a list -> ’a option, where
tryFind p xs is Some x for some x ∈ xs with p(x) = true or None if no such x exists
filter: (’a -> bool) -> ’a list -> ’a list, where
filter p xs = ys where ys is obtained from xs by deletion of elements xi : p(xi) = false
fold: (’a -> ’b -> ’a) -> ’a -> ’b list -> ’a, where
fold f a [b0;b1;...;bn−2;bn−1] = f(f(f(···f(f(a,b0),b1),...),bn−2),bn−1)
foldBack: (’a -> ’b -> ’b) -> ’a list -> ’b -> ’b, where
foldBack f [a0;a1;...;an−2;an−1] b = f(a0,f(a1,f(...,f(an−2,f(an−1,b))···)))
collect: (’a -> ’b list) -> ’a list -> ’b list, where collect f [a0;a1;...;an−1] = (f a0)@(f a1)@···@(f an−1)
*)

let addFsExt = List.map (fun s -> s + ".fs")

List.exists (fun x -> x>=2) [1;3;1;4]
// true since 3 and for are bigger than 2

List.forall (fun x -> x>=2) [1;3;1;4]
//false since not every number in the lister is bigger or equal to 2

List.tryFind (fun x -> x>3) [1;5;-2;8]
// Some 5; which is the first element that is bigger than 3

List.filter (fun x -> x>3) [1;5;-2;8]
// [5; 8]; which is the filtered list with elements bigger than 3

let isMember x xs = List.exists (fun y -> y=x) xs

isMember (2,3.0) [(2, 4.0) ; (3, 7.0)];;
//False, since 3.0 is not in the list

isMember "abc" [""; "a"; "ab"; "abc"];;
//True since abc is in the list

let norm(x:float,y:float) = sqrt(x*x+y*y)

//List.fold takes an accumulator type which i float, a list element type which is (float * float) a function and a start value 0.0
//vs is the list which has to be carried over at the end
let sumOfNorms vs = List.fold (fun s (x,y) -> s + norm(x,y)) 0.0 vs
let vs = [(1.0,2.0); (2.0,1.0); (2.0, 5.5)];;
printfn "The sum of the elements in the list vs is %f" (sumOfNorms vs)
// _ is used here because the value of the number or letter in the list is unused. it could be 1000 or 1. It does not matter. 
let length lst = List.fold (fun e _ -> e+1) 0 lst
printfn "%i" (length [[1;2];[];[3;5;8];[-2]])

let reverse xs = List.fold (fun rs x -> x::rs) [] xs

//List.foldback takes the input in reverse order 
let backSumOfNorms vs = List.foldBack (fun (x,y) s -> s + norm(x,y)) vs 0.0;;
printfn "The sum of the elements in the list vs is %f" (backSumOfNorms vs)

//list.foldback can be used on multiple lists
let app ys zs = List.foldBack (fun x xs -> x::xs) ys zs

let list = app [1;2;3] [4;5;6]
//[1;2;3;4;5;6]

let unzip zs = List.foldBack
                   (fun (x,y) (xs,ys) -> (x::xs,y::ys))
                    zs
                    ([],[]);;
let revUnzip zs = List.fold
                      (fun (xs,ys) (x,y) -> (x::xs,y::ys))
                      ([],[])
                      zs
let list2 = unzip [(1,"a");(2,"b")]
//([1; 2], ["a"; "b"])
let list3 = revUnzip [(1,"a");(2,"b")]
//([2; 1], ["b"; "a"])

let foldCheck = List.fold (+) 0 [1; 2; 3]
printfn "%i" (foldCheck)

let males = Set.ofList ["Bob"; "Bill"; "Ben"; "Bill"]
let set = set ["Bob"; "Bill"; "Ben"]
let set2 = Set.ofList [3; 1; 9; 5; 7; 9; 1]
let set3 = Set.remove 9 set2

Set.isSubset set (Set.ofList["Bob" ; "Bill"])
//true
printfn "%s" (Set.minElement (Set.ofList ["Bob"; "Bill"; "Ben"]))

let boardMembers = Set.ofList [ "Alice"; "Bill"; "Ann"]
Set.union males boardMembers
Set.difference males boardMembers;;
//["Ben"; "Bob"]

//
let setOfCounts s = Set.map Set.count s;;
let setOfCounts2 s = Set.fold
                        (fun sn se -> Set.add (Set.count se) sn)
                        Set.empty
                        s;;

let ss = Set.ofList [Set.ofList [1;3;5]; Set.ofList [2;4]; Set.ofList [7;8;9] ]

setOfCounts ss
//returns [2;3] as ss contains sets of sizes 2 and 3

let reg1 = Map.ofList [("a1",("cheese",25));
                         ("a2",("herring",4));
                         ("a3",("soft drink",5))]
let reg2 = Map.add "a4" ("bread", 6) reg1
Map.tryFind "a2" reg1
Map.exists (fun _ (_,p) -> p > 100) reg1

type BinTree<'a,'b> =
       | Leaf of 'a
       | Node of BinTree<'a,'b> * 'b * BinTree<'a,'b>;;
let rec depth = function
      | Leaf _        -> 0
      | Node(t1,_,t2) -> 1 + max (depth t1) (depth t2)

let t1 = Node(Node(Leaf 1,"cd",Leaf 2),"ab",Leaf 3);;
let t2 = Node(Node(Leaf [],[],Leaf []),[],Leaf [])
let dt1 = depth t1
printfn "%i" (dt1)

type BinTree2<'a> =
       | Leaf 
       | Node of BinTree2<'a> * 'a * BinTree2<'a>;;

//turns the binary tree into a list in different orders. @ concatenates the list.
// when preOrdering t4. It takes the leaf x and puts it first (5) then concatenates preorder tl and tr and puts the leafs first.
// Preorder tl which is the first branch of the node is 0
// it always takes the middle leaf

//inorder takes the leftmost leaf 

let rec preOrder = function
      | Leaf          -> []
      | Node(tl,x,tr) -> x :: (preOrder tl) @ (preOrder tr)
let rec inOrder = function
      | Leaf          -> []
      | Node(tl,x,tr) -> (inOrder tl) @ [x] @ (inOrder tr)
let rec postOrder = function
  | Leaf          -> []
  | Node(tl,x,tr) -> (postOrder tl) @ (postOrder tr) @ [x]
let t3 = Node(Node(Leaf, -3, Leaf), 0, Node(Leaf, 2, Leaf));;
let t4 = Node(t3, 5, Node(Leaf, 7, Leaf));;
preOrder t4;;
//val it : int list = [5; 0; -3; 2; 7]
inOrder t4;;
//val it : int list = [-3; 0; 2; 5; 7]
postOrder t4;;
//val it : int list = [-3; 2; 0; 7; 5]