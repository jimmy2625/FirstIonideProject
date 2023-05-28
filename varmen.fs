module varmen

type direction = North | East | South | West
type coord = C of int * int

let move dist dir (C(x,y)) =
    match dir with
    |North -> C(x, y - dist)
    |South -> C(x, y + dist)
    |West -> C(x - dist, y)
    |East -> C(x + dist, y)

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

type position = P of (coord * direction)
type move = TurnLeft | TurnRight | Forward of int

let step (P(coord, direction)) m =
    match m with
    |TurnRight -> (P(coord, turnRight direction))
    |TurnLeft -> (P(coord, turnLeft direction))
    |Forward x -> (P(move x direction coord, direction))

let rec walk (P(coord, direction)) ms =
    match ms with
    |[] -> (P(coord, direction))
    |x::xs -> walk (step (P(coord,direction)) x) xs

let walk2 = List.fold step

let rec path (P(coord, direction)) ms =
    match ms with
    |[] -> [coord]
    |x::xs -> 
        match x with
        |TurnLeft|TurnRight -> path (step (P(coord, direction))x) xs
        |Forward x -> coord :: path (P(move x direction coord, direction)) xs

type ring<'a> = R of 'a list * 'a list

let length (R(a,b)) = List.length a + List.length b

let ringFromList a = R([], a)

let ringToList (R(a,b)) = b @ List.rev a

let empty = R([],[])

let push x (R(a,b)) = R(a, x::b)

let peek = function
    | R([],[]) -> None 
    | R(a,[]) -> Some (List.head (List.rev a))
    | R(_, y :: _) -> Some y