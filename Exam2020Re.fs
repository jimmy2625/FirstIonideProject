module hej

(* 1. Binary search trees *)

//1.1
type 'a bintree =
| Leaf
| Node of 'a bintree * 'a * 'a bintree

let rec insert x t =
    match t with
    |Leaf -> Node(Leaf, x, Leaf)
    |Node(tl, y, tr) -> if x <= y then Node(insert x tl, y, tr) else Node(tl, y, insert x tr)
    
//1.2
let fromList lst =
    let rec aux acc lst =
        match lst with
        |[] -> acc
        |x::xs -> aux (insert x acc) xs
    aux Leaf lst
    
//1.3
let rec fold f acc t =
    match t with
    |Leaf -> acc
    |Node(tl, x, tr) -> fold f (f (fold f acc tl) x) tr
    
let rec foldBack f acc t =
    match t with
    |Leaf -> acc
    |Node(tl, x, tr) -> foldBack f (f (foldBack f acc tr) x) tl
    
let inOrder t = foldBack (fun acc x -> x :: acc) [] t

//1.4
let rec badMap f =
 function
 | Leaf -> Leaf
 | Node (l, y, r) -> Node (badMap f l, f y, badMap f r)
 
let t1 = insert 5 Leaf
let t2 = insert 3 t1

let addTwo x = x + 2

(*
    The badMap function does not respect the binary search tree property. Since applying the function f to the search tree and badMap allows any mapping of values, the tree order is not guaranteed after mapping
    
    An example can be:
    
    badMap (fun x -> -x) (Node (2, Node (1, Leaf, Leaf), Node(3, Leaf, Leaf))) which takes a valid search tree as argument, but which returns (Node (-2, Node (-1, Leaf, Leaf), Node(-3, Leaf, Leaf)))
    which has flipped the ordering of the tree.
*)

let rec map f tree = fold (fun acc x -> insert (f x) acc) Leaf tree

(* 2. Code Comprehension *)

let rec foo =
    function
    | [x] -> [x]
    | x::y::xs when x > y -> y :: (foo (x::xs))
    | x::xs -> x :: foo xs
let rec bar =
    function
    | [x] -> true
    | x :: y :: xs -> x <= y && bar (y :: xs)
let rec baz =
    function
    | [] -> []
    | lst when bar lst -> lst
    | lst -> baz (foo lst)
    
//2.1

(*
    The type of foo: 'a list -> 'a list
    The type of bar: 'a list -> bool
    The type of baz is 'a list -> 'a list
    
    bar checks if the list is sorted in ascending order, if yes it returns True, if no it returns False
    baz sorts the list in ascending order
    
    appropriate name for foo: removeLargerElements
    appropriate name for bar: isListSorted
    appropriate name for baz: sortList
*)

//2.2
(*
    The incomplete pattern matching for both foo and bar are both because the pattern match does not consider the empty list case
    No because baz calls both bar and foo in the match cases with a non-empty list, since the match case in baz for the empty list, simply returns the empty list. So these incomplete pattern matches will
    never cause any problems for the execution of baz.
*)

let rec foo2 =
    function
    | [] -> []
    | [x] -> [x]
    | x::y::xs when x > y -> y :: (foo2 (x::xs))
    | x::xs -> x :: foo2 xs
let rec bar2 =
    function
    | [] -> true
    | [x] -> true
    | x :: y :: xs -> x <= y && bar2 (y :: xs)

let rec baz2 =
    function
    | lst when bar2 lst -> lst
    | lst -> baz2 (foo2 lst)
    
//2.3
let rec foo3 =
    function
    | [x] -> [x]
    | x::xs -> x :: foo3 xs
    | x::y::xs when x > y -> y :: (foo3 (x::xs))
// They do not produce the same results for all possible inputs, since the last case of foo3 will never be matched, since the second case x::xs makes the third case unreachable

//2.4
let bar3 lst = lst = List.sort lst

//2.5
let fooTail lst =
    let rec aux c lst =
        match lst with
        | [] -> c []
        | [x] -> c [x]
        | x::y::xs when x > y -> aux (fun result -> c(y :: result)) (x::xs)
        | x::xs -> aux (fun result -> c(x :: result)) xs
    aux id lst
    
(* Big integers *)

//3.1

type bigInt = int array

let fromString (nums: string) : bigInt = Array.ofSeq (nums.ToCharArray() |> Seq.map (fun c -> int (string c)))

let toString (x: bigInt) = String.concat "" (Array.map string x)

//3.2
let add (x: bigInt) (y: bigInt) : bigInt =
    let maxLength = max x.Length y.Length + 1
    let result = Array.zeroCreate maxLength
    let mutable carry = 0

    for i = 0 to maxLength - 1 do
        let sum = carry +
                  (if i < x.Length then x.[i] else 0) +
                  (if i < y.Length then y.[i] else 0)
        result.[i] <- sum % 10
        carry <- sum / 10

    // Trim leading zeros
    let resultWithoutLeadingZeros =
        if result.[maxLength - 1] = 0 then
            Array.sub result 0 (maxLength - 1)
        else
            result

    resultWithoutLeadingZeros
    
//3.3
let multSingle (x: bigInt) (y: int) : bigInt =
    if y < 0 || y > 9 then
        failwith "y must be between 0 and 9"

    let maxLength = x.Length + 1
    let result = Array.zeroCreate maxLength
    let mutable carry = 0

    for i = 0 to x.Length - 1 do
        let product = (x.[i] * y) + carry
        result.[i] <- product % 10
        carry <- product / 10

    if carry > 0 then
        result.[x.Length] <- carry

    result
    
//3.4
let mult (x: bigInt) (y: bigInt) : bigInt =
    let resultLength = x.Length + y.Length
    let result = Array.zeroCreate resultLength

    for i = 0 to x.Length - 1 do
        let mutable carry = 0
        for j = 0 to y.Length - 1 do
            let product = (x.[i] * y.[j]) + result.[i + j] + carry
            result.[i + j] <- product % 10
            carry <- product / 10
        if carry > 0 then
            result.[i + y.Length] <- carry

    Array.rev result
    
//3.5
let fact (x: int) (numThreads: int) : bigInt =
    let segmentSize = x / numThreads

    let calculateSegment start endNum =
        async {
            let mutable result : bigInt = [| 1 |]
            for i = start to endNum do
                result <- mult result (multSingle (fromString (string i)) i)
            return result
        }

    let tasks =
        [|
            for i = 0 to numThreads - 1 do
                let start = i * segmentSize + 1
                let endNum = (i + 1) * segmentSize
                async {
                    let! result = calculateSegment start endNum
                    return result
                }
        |]

    let results = 
        tasks
        |> Async.Parallel
        |> Async.RunSynchronously

    let finalResult =
        results
        |> Array.fold (fun acc result -> mult acc result) [| 1 |]

    finalResult