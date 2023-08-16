module exam2021

//1 Dungeon Crawler

//1.1
type direction = North | East | South | West
type coord = C of int * int

//I match with the direction given and I write out the coordinate for readability

let move dist dir (C(x,y)) =
    match dir with
    |North -> C(x, y-dist)
    |South -> C(x, y+dist)
    |West -> C(x-dist, y)
    |East -> C(x+dist,y)
    
//I can now use the function keyword since i don't have to use a specific match with case
let turnRight dir =
    match dir with
    |North -> East
    |South -> West
    |West -> North
    |East -> South
    
let turnLeft dir =
    match dir with
    |North -> West
    |South -> East
    |West -> South
    |East -> North

//1.2
type position = P of (coord * direction)
type move = TurnLeft | TurnRight | Forward of int

(*
    I match with the move case and employ my previously created turnLeft and turnRight functions, 
    if the move is Forward, I will add a distance which I give as a parameter in move
*)
let step (P(coord, direction)) m =
    match m with
    |TurnLeft -> P(coord, turnLeft direction)
    |TurnRight -> P(coord, turnRight direction)
    |Forward x -> P(move x direction coord, direction)

//1.3
(*
    Instead of matching on the single move, I match on the move list
    I make an end case which is the empty list which just returns the start position
    The next case runs walk recursively with each step on the rest of the list
*)
let rec walk (P(coord, direction)) ms =
    match ms with
    |[] -> P(coord, direction)
    |x :: xs -> walk (step (P(coord,direction)) x) xs

//Since the only thing walk does is applying the step function, List.fold is preferred
let walk2 = List.fold step

//1.4
(*
    I match with the move list and create an end case which is the empty list that returns a list containing the start position
    The next case in the pattern is for the list, where I do a nested match x with. Since TurnLeft and TurnRight both don't add any coordinates to the coord list,
    i will just step towards the given direction.
    If the match case hits the Forward case, the starting position (coord) will be the head of the coord list while path runs recursively
    with the move function and recursively adds coordinates to the tail of the list if the Forward case is reached.
*)

let rec path (P(coord, direction)) ms =
    match ms with
    |[] -> [coord]
    |x :: xs ->
        match x with
        |TurnLeft |TurnRight -> path (step (P(coord, direction)) x) xs
        |Forward dist -> coord :: path (P(move dist direction coord, direction)) xs
        
//1.5
(*
    I create an inner function called aux and match on the movelist
    I do the same match cases but reverse the list to maintain the order and append the acc to the starting coord
    In the next case, since the turnLeft and turnRight does not change the acc, i just recursively run step as in path and leave the acc as it is
    In the forward case i have to move the coord like in path but i also need to append the acc to the coord
*)
let path2 (P(coord, direction)) ms =
    let rec aux (P(coord, direction)) ms acc =
        match ms with
        |[] -> List.rev (coord :: acc)
        |x :: xs ->
            match x with
            |TurnLeft |TurnRight -> aux (step (P(coord, direction)) x) xs acc
            |Forward dist -> aux (P(move dist direction coord, direction)) xs (coord :: acc)
    aux (P(coord, direction)) ms []
    
//1,6
(*
    My solution for path is not tail-recursive because the recursive call to path is NOT the last operation done
    The appending of the coord is the last operation done, since it requires a tail to append to which is only ready for appending AFTER the recursive call
    
    Evaluation of a call to path (P (C (0, 0), North)) [TurnRight; Forward 10; Forward 5]
    
    path (P (C (0,0), North)) [TurnRight; Forward 10; Forward 5]
    = path (step (P (0,0, North)) TurnRight) [Forward 10; Forward 5]
    = path (P (C (0,0), East) [Forward 10; Forward 5]
    = C(0,0) :: path (P (move 10 East C(0,0), East) [Forward 5]
    = C(0,0) :: path (P(C(10,0)), East) [Forward 5]
    = C(0,0) :: C(10,0) :: path (P (move 5 East 
*)
    
(*
    Continuation approach:
    Same match cases
    Just apply the continuation function (c) on each branch and in the forward case append the result to the coord with an anonymous function
*)
let path3 (P(coord, direction)) ms =
    let rec aux c (P(coord, direction)) ms =
        match ms with
        |[] -> c [coord]
        |x :: xs ->
            match x with
            |TurnLeft |TurnRight -> aux c (step (P(coord, direction)) x) xs
            |Forward dist -> aux (fun result -> c(coord :: result)) (P(move dist direction coord, direction)) xs
    aux id (P(coord, direction)) ms
    
//2 Code Comprehension

let x = [1;2;3]

let foo f =
    let mutable m = Map.empty
    let aux x =
        match Map.tryFind x m with
        | Some y when Map.containsKey x m -> y
        | None ->
        m <- Map.add x (f x) m; f x
    aux
let rec bar x =
    match x with
    | 0 -> 0
    | 1 -> 1
    | y -> baz (y - 1) + baz (y - 2)
and baz = foo bar

(* 2.1
    type of foo = ('a -> 'b) -> 'a -> 'b
    type of bar = int -> int
    type of baz = int -> int
    
    foo takes a function f and applies it on an input x
    baz takes an input x and gives the x'th number in the fibonacci sequence
    
    The mutable keyword in this example keeps a mutable map that is used in the function argument to foo
    The left arrow operator in the line m <- Map.add x (f x) m; f x is only used with mutable variable updates, therefore making the code not compile if I removed the mutable keyword
    
    appropriate name for foo = cache
    appropriate name for bar = fibAux
    appropriate name for baz = fib
*)

(* 2.2
    The 'and' keyword declares mutually recursive types, functions or values which ensures that for example functions can call each other which widens the scope of the function
    
    Nothing would change since bar does not call foo and foo does not call bar or baz
*)

(* 2.3
    This happens in the Some case of the aux function inside foo. This is because the pattern match is non-exhaustive because of the when clause, which excludes the case for Some but the guard is false
    
    If you match Map.tryFind x m then it will never return Some, if the key x was not in the map m which makes this when cause always be true
    
    The first redundant computation is the Some case since the guard will always be true, the when clause is redundant
    
    The second redundant computation is the None case, because it computes f x two times. To reduce this, I calculate the f x once and store it in the map afterwards
*)

let foo2 f =
    let mutable m = Map.empty
    let aux x =
        match Map.tryFind x m with
        | Some y -> y  // Use the cached result
        | None ->
            let result = f x  // Calculate the result
            m <- Map.add x result m  // Store in the cache
            result  // Return the calculated result
    aux

(* 2.4
    barbaz is slower since it has to initialize foo from scratch at every recursive call, the mutable map is reset which makes barbaz slower than baz
*)

let rec barbaz x =
    let baz = foo barbaz
    match x with 
    | 0 -> 0 
    | 1 -> 1
    | y -> baz (y - 1) + baz (y - 2)
    
//2.5
//I use Seq.initInfinite with the baz function provided. This implementation will calculate the sequence with baz and uses the memoization to ensure the bazSeq is efficient and close to instantaneous with large indexes.
let bazSeq = Seq.initInfinite baz

(* 3. Guess the next sequence element *)

//3.1
type element = E of char list

//3.2
let elToString (E lst:element) = List.map string lst |> String.concat ""

let elFromString (s:string) = E (Seq.toList s)

//3.3
let nextElement (E chars) =
    let rec processElement chars count acc =
        match chars with
        | [] -> (count, ' ') :: acc |> List.rev
        | hd :: tl ->
            match acc with
            | (prevCount, prevDigit) :: rest when prevDigit = hd ->
                processElement tl (count + 1) rest
            | _ ->
                processElement tl 1 ((count, hd) :: acc)

    let charPairs = processElement chars 1 []
    let newChars = charPairs |> List.collect (fun (count, digit) -> [count; int digit]) |> List.map char
    E newChars

//3.4
let elSeq startElement =
    Seq.unfold (fun current -> Some (current, nextElement current)) startElement
    
let elSeq2 startElement =
    seq {
        let mutable current = startElement
        while true do
            yield current
            current <- nextElement current
    }

(* 4. Rings *)

//4.1
type 'a ring = R of 'a list * 'a list

//4.2
let length (R(a,b)) = List.length a + List.length b

let ringFromList lst = R([],lst)

let ringToList (R(a,b)) = b @ List.rev a

//4.3
let empty = R([],[])

let push x (R(a,b)) = R(a,x::b)

let peek = function
    |R([],[]) -> None
    |R(_,b :: _) -> Some b
    |R(a,[]) -> Some (List.rev a).Head
    
let pop = function
    |R([],[]) -> None
    |R(a,[]) -> Some (R([], (List.rev a).Tail))
    |R(a,b) -> Some (R(a,b.Tail))
    
let cw = function
    |R([],[]) -> R([],[])
    |R([],b) -> R((List.rev b).Tail,[(List.rev b).Head])
    |R(a,b) -> R(a.Tail, a.Head :: b)
    
let ccw = function
    |R([],[]) -> R([],[])
    |R(a,[]) -> R([(List.rev a).Head], (List.rev a).Tail)
    |R(a,b) -> R(b.Head :: a, b.Tail)

//4.4
type StateMonad<'a, 'b> = SM of ('b ring -> ('a * 'b ring) option)
let ret x = SM (fun st -> Some (x, st))
let bind (SM m) f =
     SM (fun st ->
     match m st with
     | None -> None
     | Some (x, st') ->
     let (SM g) = f x
     g st')

let (>>=) m f = bind m f
let (>>>=) m n = m >>= (fun () -> n)
let evalSM (SM f) s = f s

let smLength = SM (fun state -> Some(length state, state))

let smPush x = SM (fun state -> Some((), push x state))

let smPop = SM (fun state -> if length state > 0 then Some (Option.get (peek ring), (Option.get (pop ring))) else None)

let smCW = SM (fun state -> Some((),cw state))

let smCCW = SM (fun state -> Some((), ccw state))