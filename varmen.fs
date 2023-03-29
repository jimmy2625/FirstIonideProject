module varmen

type cmd = Push of int | Add | Mult
type stackProgram = cmd list

type stack = int list

let emptyStack () : stack = []

let runStackProg prog =
    if prog = [] then failwith "empty stack" else
        let rec inner prog' (acc:stack) =
            match prog' with 
            |[] -> acc.Head
            |x::xs -> match x with
                        |Push y -> inner xs (y::acc)
                        |Add -> match acc with
                                |[] -> failwith "empty stack"
                                |x::y::ys -> inner xs ((x+y)::ys)
                                |_ -> failwith "empty stack"
                        |Mult -> match acc with
                                |[] -> failwith "empty stack"
                                |x::y::ys -> inner xs ((x*y)::ys)
                                |_ -> failwith "empty stack"
        inner prog (emptyStack())

runStackProg [Push 5; Push 4; Add; Push 8; Mult]

type StateMonad<'a> = SM of (stack -> ('a * stack) option)
let ret x = SM (fun s -> Some (x, s))
let fail = SM (fun _ -> None)
let bind f (SM a) : StateMonad<'b> =
        SM (fun s ->
                match a s with
                | Some (x, s') ->
                let (SM g) = f x
                g s'
                | None -> None)
let (>>=) x f = bind f x
let (>>>=) x y = x >>= (fun _ -> y)
let evalSM (SM f) = f (emptyStack ())

let push x = SM (fun stack -> Some((),x::stack))

let pop = SM (fun stack -> if stack.Length > 0 then Some(stack.Head, stack.Tail) else None)

let runStackProg2 (prog: stackProgram) =
        if prog = [] then SM (fun _ -> None)
            else
                let rec aux sp (acc: stack) =
                    match sp with
                    | []       -> SM (fun s -> Some ((acc.Head), s))
                    | x::xs -> match x with
                                |Push n -> aux xs (n::acc)
                                |Add    -> match acc with
                                           | []        -> SM (fun _ -> None)
                                           | x::y::ys  -> aux xs ((x+y)::ys)
                                           | _         -> SM (fun _ -> None)
                                |Mult   -> match acc with
                                           | []        -> SM (fun _ -> None)
                                           | x::y::ys  -> aux xs ((x*y)::ys)
                                           | _         -> SM (fun _ -> None)
                aux prog (emptyStack ())

[Push 5; Push 4; Add; Push 8; Mult] |> runStackProg2 |> evalSM |> Option.map fst