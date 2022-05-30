module Assignment6

type Result<'a, 'b> =
 | Success of 'a
 | Failure of 'b

type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero
        | ReservedName of string     

type State = { vars     : Map<string, int> list
               word     : (char * int) list 
               reserved : Set<string> }

type SM<'a> = S of (State -> Result<'a * State, Error>)

let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
 S (fun s ->
    match a s with
    | Success (av, s') ->
    let (S g) = f av
    g s'
    | Failure err -> Failure err)

let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
let fail err : SM<'a> = S (fun _ -> Failure err)

let (>>=) x f = bind f x
let (>>>=) x f = x >>= (fun () -> f)

let push : SM<unit> = S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))