type direction = North | East | South | West
type coord = C of int * int

let move (dist: int) (dir:direction) (C(x,y): coord) : coord = 
    match dir with
    | North -> C(x,y-dist)
    | East -> C(x+dist,y)
    | South -> C(x,y+dist)
    | West -> C(x-dist,y)

let turnRight (dir: direction) : direction = 
    match dir with
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let turnLeft (dir: direction) : direction = 
    match dir with
    | North -> West
    | East -> North
    | South -> East
    | West -> South

type position = P of (coord * direction)
type move = TurnLeft | TurnRight | Forward of int

let step (p:position) (m:move) : position = 
    match p with 
    | P(x,y) -> 
        match m with
        | TurnLeft ->  P(x,turnLeft y)
        | TurnRight -> P(x,turnRight y)
        | Forward(dist) -> P(move dist y x,y)

let rec walk (p : position ) (ms : list<move>) = 
    match ms with
    | [] -> p
    | m::ms -> walk (step p m) ms

let rec walk2 (p : position ) (ms : list<move>) = List.fold (fun a b -> step a b) p ms

let rec path (P(c,_) as p: position ) (ms : list<move>)= 
    match ms with
    | [] -> [c]  
    | Forward _ as m::ms-> c::path (step p m) ms
    | m::ms-> path (step p m) ms  

let path2 (P(c,_) as p: position ) (ms : list<move>) = 
    let rec aux acc mlist  (P(c,_) as p) = 
        match mlist with
        | [] -> c::acc
        | m::mlist -> 
            match m with
            | Forward _ -> aux (c::acc) mlist (step p m)
            | _ -> aux acc mlist (step p m)
    aux [] ms p |> List.rev

let path3 (P(c,_) as p: position ) (ms : list<move>) = 
    let rec aux mlist  (P(c,_) as p) con = 
        match mlist with
        | [] -> con [c]
        | m::mlist ->
            match m with
            | Forward _ -> aux mlist (step p m) (fun f -> con (c::f))
            |_ -> aux mlist (step p m) con
    aux ms p id

type 'a ring = 'a list * 'a list

let length (a : 'a ring) : int = 
    match a with 
    |(a,b) -> List.length a + List.length b

let ringFromList (a : 'a list) : 'a ring = 
    match a with
    | a -> ring([],a)

let ringToList (a: 'a ring)    : 'a list =
    match a with
    | (a,b) -> b @ (List.rev a)



let empty: 'a ring = ([],[])

let push x (r : 'a ring) : 'a ring = 
    match r with 
    | (a,b) -> (a,x::b)

let peek (r :'a ring) : 'a option =
    match r with 
    | ([],[]) -> None 
    | (a,[]) -> Some (List.head (List.rev a))
    | (_, y :: _) -> Some y



let pop (r :'a ring) : ('a ring) option = 
    match r with
    |(a,b) when b.Length <> 0 -> Some (a,b.Tail)
    | (a,b) when b.IsEmpty && a.Length <> 0 ->
            let x::xs = List.rev a
            Some (List.rev xs,[])
    | _ -> None 

let cw (r : 'a ring) : 'a ring = 
    match r with
    | ([],[]) -> ([],[])
    | ([],b) -> 
            let x::xs = List.rev b
            (xs,[x])
    | (a,b) -> 
            let x::xs = a
            (xs,x::b)

let ccw (r : 'a ring) : 'a ring = 
    match r with
    | ([],[]) -> ([],[])
    | (a,[]) -> 
            let x::xs = List.rev a
            ([x],xs)
    | (a,b) -> 
            let x::xs = b
            (x::a,xs) 

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


let smLength = 
    SM (fun st -> Some (length st, st))

let smPush x = 
    SM (fun st ->  Some ((),  push x st))

let smPop : StateMonad<'a, 'a>= 
    SM( fun st -> 
        match (peek st ,pop st) with 
        | (Some a, Some r) -> Some (a, r)
        | (_,_) -> None)
                

let smCW : StateMonad<unit, 'a> = 
    SM( fun st -> Some((), cw st))


let smCCW : StateMonad<unit, 'a> = 
    SM( fun st -> Some((), ccw st)) 


let ringStep : StateMonad<unit, int> = 
    SM( fun st -> 
        if ((length st) <= 2) then Some ((), st) else
            let (a,x::y::b) = st
            match (((x+y)%2)=0) with 
            | true -> Some ((), ring(a,b))
            | false -> Some ((), ccw st))

let ringStep2 : StateMonad<unit, int> =
    smLength >>= (fun i -> if i < 2 then ret () else
        smPop >>= (fun x  ->
            smPop >>= (fun y ->
                if (x + y) % 2 = 0 then ret () else
                smPush y >>>= smPush x >>>= smCCW)))


type StateBuilder() =

    member this.Bind(f, x)    = f >>= x
    member this.Zero ()       = ret ()
    member this.Return(x)     = ret x
    member this.ReturnFrom(x) = x
    member this.Delay(f)      = f ()
    member this.Combine(a, b) = a >>= (fun _ -> b)
    
let prog = new StateBuilder()

let ringStep3 : StateMonad<unit, int> =
    prog {
        let! length = smLength
        if length >= 2 
        then 
            let! x = smPop
            let! y = smPop
            if (x + y) % 2 <> 0
            then
                do! smPush y
                do! smPush x
                do! smCCW
    }

let rec iterRemoveSumEven = function
    | 0u -> ret ()
    | x -> ringStep >>>= iterRemoveSumEven (x - 1u)

let rec iterRemoveSumEven2 x =
prog {
    if x > 0u
    then
        do! ringStep
        do! iterRemoveSumEven2 (x - 1u)
}