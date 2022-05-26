module Exam2021

//1 Dungeon Crawler

//1.1
type direction = North | East | South | West
type coord     = C of int * int

let move (dist:int) (dir:direction) (C(x,y)) = 
    match dir with
    |North -> C(x,y-dist)
    |East -> C(x+dist,y)
    |South -> C(x,y+dist)
    |West -> C(x-dist,y)

move 10 North (C (0, 0))

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

let step (P(coord,direction)) (m:move) = 
    match m with
    |TurnLeft -> (P(coord,turnLeft direction))
    |TurnRight -> (P(coord,turnRight direction))
    |Forward dist -> (P(move dist direction coord, direction))


step (P (C (0, 0), North)) TurnRight

step (P (C (0, 0), North)) TurnLeft

step (P (C (0, 0), North)) (Forward 10)

//1.3
let rec walk (P(coord,direction)) (ms : move list) = 
    match ms with 
    |[] -> (P(coord,direction))
    |x::xs -> walk (step(P(coord,direction)) x) xs

walk (P (C (0, 0), North)) [TurnRight; Forward 10; TurnLeft]

walk (P (C (0, 0), North)) []

let walk2 (P(coord,direction)) (ms:move list) =
    match ms with
    |[] -> (P(coord,direction))
    |x::xs -> List.fold step (P(coord,direction)) ms 

walk2 (P (C (0, 0), North)) [TurnRight; Forward 10; TurnLeft]

//1.4
let rec path (P(coord,direction)) (ms:move list) =
    match ms with 
    |[] -> [coord]
    //|x::xs -> path (walk(P(coord,direction)) x) xs

path (P(C(1,0), North)) []
path (P (C (0, 0), North)) [TurnRight; Forward 10; TurnLeft]