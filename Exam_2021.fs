module Exam202

//1 Dungeon Crawler

//1.1
type direction = North | East | South | West
type coord     = C of int * int

//I match with the direction given and I write out the coordinate for readability

let move (dist:int) (dir:direction) (C(x,y)) = 
    match dir with
    |North -> C(x,y-dist)
    |East -> C(x+dist,y)
    |South -> C(x,y+dist)
    |West -> C(x-dist,y)

move 10 North (C (0, 0))

//I can now use the function keyword since i don't have to use a specific match with case

let turnRight = function
|North -> East
|East -> South
|South -> West
|West -> North

let turnLeft = function
|North -> West
|East -> North
|South -> East
|West -> South

turnRight North;;

//1.2
type position = P of (coord * direction)
type move     = TurnLeft | TurnRight | Forward of int

(*
    I match with the move case and employ my previously created turnLeft and turnRight functions, 
    if the move is Forward, I will add a distance which I give as a parameter in move
*)

let step (P(coord,direction)) (m:move) = 
    match m with
    |TurnLeft -> (P(coord,turnLeft direction))
    |TurnRight -> (P(coord,turnRight direction))
    |Forward dist -> (P(move dist direction coord, direction))


step (P (C (0, 0), North)) TurnRight

step (P (C (0, 0), North)) TurnLeft

step (P (C (0, 0), North)) (Forward 10)

//1.3
(*
    Instead of matching on the single move, I match on the move list
    I make an end case which is the empty list which just returns the startposition
    The list case runs recursively on each step in the list's tail (xs)
*)
let rec walk (P(coord,direction)) (ms : move list) = 
    match ms with 
    |[] -> (P(coord,direction))
    |x::xs -> walk (step(P(coord,direction)) x) xs

walk (P (C (0, 0), North)) [TurnRight; Forward 10; TurnLeft]

walk (P (C (0, 0), North)) []

(*
    I do the same as in walk but instead use the List.fold with the folder function "step" on the whole list ms
*)

let walk2 (P(coord,direction)) (ms:move list) =
    match ms with
    |[] -> (P(coord,direction))
    |x::xs -> List.fold step (P(coord,direction)) ms 

walk2 (P (C (0, 0), North)) [TurnRight; Forward 10; TurnLeft]

//1.4
(*
    I match with the move list and create an end case which is the empty list that returns a list containing the start position
    The next case in the pattern is for the list, where I do a nested match x with. Since TurnLeft and TurnRight both don't add any coordinates to the coord list,
    i will just step towards the given direction.
    If the match case hits the Forward case, the starting position (coord) will be the head of the coord list while the path runs recursively
    with the move function and recursively adds coordinates to the tail of the list if the Forward case is reached.
*)
let rec path (P(coord,direction)) (ms : move list) : coord list =
        match ms with
        |[]     -> [coord] 
        |x::xs  ->  
        match x with
            |TurnLeft|TurnRight -> path (step (P(coord,direction)) x) xs
            |Forward dist       -> coord :: path (P(move dist direction coord,direction)) xs

path (P(C(1,0), North)) []
path (P (C (0, 0), North)) [TurnRight; Forward 10; TurnLeft]

//1.5
(*
    To implement a tail-recursive function of path, I will first make the outer function non recursive but the inner recursive.
    Afterwards, I will add an acc of type coord list to the pathInner and the end case and nested match cases are the same as in path
    The match x case now uses the pathInner recursive function instead and adds an accumulator at the end for both TurnLeft, TurnRight and Forward
*)

//mit eksempel:
let addOne (input : int list) : int list =
    let rec addOneInner (input : int list) (acc : int list) : int list = 
        match input with
        | [] -> acc
        | x :: xs -> addOneInner xs ((x + 1) :: acc)
    addOneInner input []

let path2 (P(coord,direction)) (ms : move list) : coord list =
    let rec pathInner (P(coord,direction)) (ms : move list) (acc : coord list) : coord list =
        match ms with
        |[]     -> [coord]
        |x::xs  -> match x with
                    |TurnLeft|TurnRight -> pathInner (step(P(coord,direction)) x) xs acc
                    |Forward dist -> coord :: pathInner (P(move dist direction coord,direction)) xs acc
    pathInner (P(coord,direction)) ms []

path2 (P(C(1,0), North)) []
path2 (P (C (0, 0), North)) [TurnRight; Forward 10; TurnLeft]

//2 Code Comprehension

let foo f =
  let mutable m = Map.empty
  let aux x =
    match Map.tryFind x m with
    | Some y when Map.containsKey x m -> y
    | None   ->
      m <- Map.add x (f x) m; f x

  aux

let rec bar x =
  match x with
  | 0 -> 0
  | 1 -> 1
  | y -> baz (y - 1) + baz (y - 2)

and baz = foo bar

(*
    The type of foo = ('a -> 'b) -> ('a -> 'b)
    The type of bar = int -> int
    The type of baz = int -> int

    The and keyword indicates that the two functions are mutually recursive which means they depend on each other to compile and run
    
*)

//4 Rings

//4.1
//I create the ring datatype by defining the 'a in both ring and the list tuples

type 'a ring = R of 'a list * 'a list

//4.2
(*
    Make sure to read the instructions carefully!!! Especially in ringToList, since it is a reversed
*)
let length (R(a,b)) = List.length a + List.length b

let ringFromList lst = R([],lst)

let ringToList (R(a,b)) = b @ List.rev a

ringToList (ringFromList [1;2;3;4;5])

length (ringFromList [1;2;3;4;5])

//4.3
(*
    Look at the complete list of algorithms and try your best!
    Make sure the type signatures are the same as the ones given in the exam
*)
let empty = R([],[])

let push x (R(a,b)) = R(a,x::b)

let peek = function
    |(R([],b)) -> Some b.Head
    |(R(a,[])) -> Some (List.rev a).Head
    |_ -> None 

[1;2;3;4;5] |> ringFromList |> peek

let pop = function
    |(R(a,[])) -> Some (R([], (List.rev a).Tail))
    |(R(a,b)) when b.Length > 0 -> Some (R(a,b.Tail))
    |_ -> None

[1;2;3;4;5] |> ringFromList |> pop |> Option.get |> ringToList

let cw = function
    |(R([],[])) -> (R([],[]))
    |(R([],b)) -> (R((List.rev b).Tail,([((List.rev b).Head)])))
    |(R(a,b)) -> (R(a.Tail,a.Head::b))

[1;2;3;4;5] |> ringFromList |> cw |> cw |> ringToList

let ccw = function
    |(R([],[])) -> (R([],[]))
    |(R(a,[])) -> (R(([(List.rev a).Head]),(List.rev a.Tail)))
    |(R(a,b)) -> (R(b.Head::a, b.Tail))

[1;2;3;4;5] |> ringFromList |> ccw |> ccw |> ringToList

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

(*
    Use the following template for completing the SM part of the exam
    The fst of the Some tuple is the returned element from the operation and the snd is the state of the ring after the operation
    Some = success
    None = failure
    Check type signatures in exam (SMPush), SM<unit, 'a> = ()
    Simply use the different functions from 4.3 to convert into SM functions instead, that is fail proof
*)

let smLength = SM (fun ring -> Some((length ring), (ring)))

let smPush x = SM (fun ring -> Some((), (push x ring)))

let smPop = SM (fun ring -> if length ring > 0 then Some ((peek ring), (Option.get (pop ring))) else None)

let smCW = SM (fun ring -> Some ((),(cw ring)))

let smCCW = SM (fun ring -> Some ((), (ccw ring)))

[1;2;3;4;5] |> ringFromList |> evalSM (smCW >>>= smCW) |>
 Option.get |> snd |> ringToList

[1;2;3;4;5] |> ringFromList |> evalSM (smCCW >>>= smCCW) |>
Option.get |> snd |> ringToList

//4.5
//Bullshit lidt om det her
