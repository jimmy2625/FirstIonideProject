module FirstIonideProject

type direction = North | East | South | West
type coord = C of int * int


(* QUESTION 1: DUNGEON CRAWLER *)

(* 1.1 *)
let move dist dir (C (x, y)) =
    match dir with
    | North -> C (x, y - dist)
    | South -> C (x, y + dist)
    | East -> C (x + dist, y)
    | West -> C (x - dist, y)


// Two approaches
let turnRight = function
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let turnRight2 dir =
    match dir with
    | North -> East
    | East -> South
    | South -> West
    | West -> North    

// The "function"-keyword is simply shorthand for matching on new
// argument to the function. 

let turnLeft = function
    | North -> West
    | East -> North
    | South -> East
    | West -> South

(* 1.2 *)

type position = P of (coord * direction)
type move =
    | TurnLeft
    | TurnRight
    | Forward of int

let step (P (coord, dir)) = function
    | TurnLeft -> P (coord, turnLeft dir)
    | TurnRight -> P (coord, turnRight dir)
    | Forward n -> P (move n dir coord, dir)
    
(* 1.3 *)

let rec walk p = function
    | [] -> p
    | x :: xs -> walk (step p x) xs

let walk2 p ms = List.fold (fun pos move -> step pos move) p ms
// Which is identical to:
let walk3 = List.fold step

(* 1.4 *)

// non tail-recursive
let rec path (P (c, d)) = function
    | [] -> []
    | [ Forward x ] ->
        let (P (c', _)) = step (P (c, d)) (Forward x)
        [c; c']
    | Forward x :: xs ->
        let pos' = step (P (c, d)) (Forward x)
        c :: path pos' xs
    | m :: xs ->
        path (step (P (c, d)) m) xs

(* 1.5 *)
// tail-recursive
let path2 (P (c, d)) ms =
    let rec aux acc pos = function
        | [] -> List.rev acc
        | Forward x :: xs ->
            let (P (c', d')) = step pos (Forward x)
            aux (c' :: acc) (P (c', d')) xs
        | m :: xs ->
            aux acc (step pos m) xs
    aux [c] (P (c, d)) ms

(* 1.6 *)

(*

"Compelling" argument:
The last thing a tail-recursive function does is call itself.
"path" constructs a list after the recursive call returns making
it non-tail-recursive.


RECURSIVE STEPS OF "PATH"

path (P (0,0), North) [Forward 5; Forward; Forward 5]
-> (0,5) :: path (P (0,5), North) [Forward 5; Forward 5]
-> (0,5) :: ((0,10) :: path (P (0,10), North) [Forward 5])
-> (0,5) :: ((0,10) :: ((0, 15) :: path (P (0,15), North) []))
-> (0,5) :: ((0,10) :: ((0, 15) :: [])))
-> (0,5) :: ((0,10) :: ([(0, 15)])
-> (0,5) :: [(0,10); (0, 15)]
-> [(0,5); (0,10); (0, 15)]

RECURSIVE STEPS OF "PATH2" (tail-recursive)

path (P (0,0), North) [Forward 5; Forward; Forward 5]
-> aux [] (P ((0,0), North)) [Forward 5; Forward 5; Forward 5]
-> aux [(0,5)] (P ((0,5), North)) [Forward 5; Forward 5]
-> aux [(0, 10); (0,5)] (P ((0,10), North)) [Forward 5]
-> aux [(0, 15); (0, 10); (0,5)] (P ((0,15), North)) []
-> [(0,5); (0,10); (0, 15)] // acc is reversed

*)

(*
Tail recursive path using cps (continuation-passing-style)
Little tip:
CPS is truly mindbending
First exercise using the pattern before trying to understand how it works ¯\_(ツ)_/¯
It usually goes something like this:

    1. Initialise the continuation function using the identity function "id" when calling your
       auxiliary function "aux".
       
       In this instance we will use a slightly different initial function because we
       know that the first element of the list will be the initial position.
    
    2. At each recursive step you either:
        - Leave the continuation function as it is (as in third match case), or:
        - You expand upon the continuation function by creating a new function using
          lambda notation. The function should call the continuation function with some value.
          
          Explanation of this part:
          (fun list -> c' :: list |> cont)
          This essentially means:
          I still need to compute the rest of the coord list and I do not have it at this point.
          Once I do get it, I will prepend c' to the list and apply "cont" to it. Where "cont" is
          the previous recursive step that is also missing the tail of the list.
          
          This means that we can create an accumulator which is a long chain of functions (endofunctors specifically)
          that will call one another once a value is applied to the last "link" of the function chain.
        
        - Call "cont" in instances where you do not wish to call something recursively.
          This is the case in the first step, where we call "cont" with the value "[]" as this will
          be the tail of the list. 
          
*)

let path3 (P (c, d)) =
    let rec aux cont pos = function
        | [] -> cont []
        | Forward x :: xs ->
            let (P (c', d')) = step pos (Forward x)
            aux (fun ls -> cont (c' :: ls)) (P (c', d')) xs
        | m :: xs ->
            aux cont (step pos m) xs
    aux (fun ls -> c :: ls) (P (c, d))


// A more simple function that uses CPS, generates the list [n .. 0]:
// The continuation function is the identity function to begin with.
let downToZero n =
    let rec aux cont = function
        | 0 -> cont [0]
        | n -> aux (fun ls -> n :: ls |> cont) (n - 1)
    aux id n


(* QUESTION 2: CODE COMPREHENSION *)

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

let rec barbaz x =
    let baz = foo barbaz
    match x with
    | 0 -> 0
    | 1 -> 1
    | y -> baz (y - 1) + baz (y - 2)

(*
2.1

2.1.1: What are the types of functions foo, bar and baz?
(If you're using intellisense, this is done for you)
foo: ('a -> 'b) -> 'a -> 'b
bar: int -> int

2.1.2: What does the functions do?
foo: takes a function and an element and applies the function to the element.
    It does caches the value such that the same computation is not repeated
bar: returns the x'th number in the fibonacci sequence.

2.1.3: The function foo uses a mutable value
What functions does it serve (why is it there)?
    - It makes the map m mutable such that changes to the map can be used later on.
      It is seemingly useless as a call to the function foo would reinstantiate m
      as an empty map. However, in "baz" the function "foo" is partially applied with
      the function "bar". The function then ends up returning "aux : int -> int",
      and each result from "baz" will be cached.
What would happen if you removed the "mutable" keyword? Would it still work?
    - No, because the function uses the "<-" operator, which is strictly used on mutable values
      for reassigning.
    
2.1.4: What would be appropriate names?
foo: "map", "apply" or more importantly "cache"
bar: "fib"

*)

(*
2.2

2.2.1
What function does the "and" keyword serve in general (why would you use and when writing any program)?

    - The "and" keyword specifies that the functions are mutually recursive
      This makes it possible for them to call one another which is otherwise not possible.
      It requires that the first function is recursive
      You can make more that two functions mutually recursive with one another
      You would use it in any context where two functions relies on one another, as with ArithEval and CharEval.

2.2.2
What happens if we remove "and"

    - It won't compile because "bar" relies on "baz"

*)

(*
2.3 Warning: incomplete pattern matches

2.3.1:
    - It happens because the compiler does not look into "when"-clauses to see if a match
      statement has complete coverage. It only looks at the match cases.
       
      check for instance the following identity function which also produces the warning:

*)

let id x =
    match x with
    | n when true -> n


(*
2.3.2
Will the warning ever cause problems?:

    - No, because the when-clause is ALWAYS satisfied.

2.3.3:
What are the two redundant parts?

    - The when clause is redundant as m ALWAYS contains element x if matched on Some
      Furthermore, the map will always be empty so none of it makes sense.
    - f x is calculated twice.
*)

let foo2 f =
    let mutable m = Map.empty
    let aux x =
        match Map.tryFind x m with
        | Some y -> y
        | None ->
            let res = f x
            m <- Map.add x res m
            res
    aux

(*
2.4

barbaz is slower because it initializes foo at every recursive call
in "bar" the map is initialized just once due to partial application

*)


(*  2.5 *)

let bazSeq = Seq.initInfinite baz
let bazSeq2 =
    Seq.unfold (fun (a, b) -> Some (a, (b, a + b))) (0, 1)


(* QUESTION 3: GUESS THE NEXT SEQUENCE ELEMENT *)

(* 3.1 *)
type element = E of char list


(* 3.2 *)
let elToString (E ls) =
    List.map string ls |> String.concat ""
    
let elFromString (s: string) =
    List.ofSeq s |> E
    

(* 3.3 *)
let nextElement (E ls) =
    let rec aux (last : char) n = function
        | [] -> List.ofSeq (string n) @ [last]
        | x :: xs ->
            if last = x
            then aux last (n + 1) xs
            else (List.ofSeq (string n)) @ last :: aux x 1 xs      
    List.tryHead ls
    |> Option.map (fun c -> aux c 0 ls |> E)
    |> Option.defaultValue (E [])
    

(* 3.4 *)
let elSeq e = Seq.unfold (fun element ->
    let next = nextElement element
    Some (element, next)) e

let negativeNumbers = Seq.initInfinite (fun i -> i * -1)

let negativeNumbers2 = Seq.unfold (fun element ->
    Some (element, element - 1)) 0

let rec elSeq2 e =
    seq {
        let next = nextElement e
        yield e
        yield! elSeq2 next
    }
(*

Q: Why would initInfinite not be a good fit?

A:
    - We can't construct the n'th element without creating an auxiliary function to do this
    - It wouldn't cache intermediate elements making the computation slower.

*)

(* 3.5

NOTE:
the exercise asks you to create the parser of type elParse: string -> Parser<element>
This seems to be a mistake
We are creating a parser which may be run on multiple strings.

*)
(*let elParse =
    many digit .>> pchar '\n' |>> (fun digitlist -> E digitlist)

let elFromString2 str =
    run elParse str |> getSuccess*)
    
    
(* QUESTION 4: RINGS *)

(* 4.1 *)
type ring<'a> =
    | R of 'a list * 'a list
    

(* 4.2 *)
let length (R (l, r)) = List.length l + List.length r

let ringFromList ls = R ([], ls)

let ringToList (R (a, b)) = b @ List.rev a


(* 4.3 *)
let empty = R ([], [])

let push elem (R (a, b)) = R (a, elem :: b)


// Two approaches
let peek (R (a, b)) =
    List.tryHead b
    |> Option.orElse (List.tryLast a)
    
let peek2 (R (a, b)) =
    match a, b with
    | [], [] -> None
    | xs, y :: ys -> Some y
    | xs, [] -> List.tryLast a //List.rev xs |> List.tryHead


// Two approaches
let pop (R (a, b)) =
    List.tryHead b
    |> Option.map (fun _ -> R (a, List.tail b))
    |> Option.orElse 
        (List.tryHead a
        |> Option.map (fun _ -> R ([], List.rev a |> List.tail)))

let pop2 (R (a, b)) =
    match a, b with
    | [], [] -> None
    | x, _ :: ys -> Some (R (x, ys))
    | _ :: _, ys ->
        Some (R ([], (List.rev a |> List.tail)))

let cw (R (a, b)) =
    match a, b with
    | [], [] -> R ([], [])
    | [], xs ->
        let b' = List.rev xs
        R (List.tail b', [List.head b'])
    | x :: xs, ys -> R (xs, x :: ys)
    
let ccw (R (a, b)) =
    match a, b with
    | [], [] -> R ([], [])
    | xs, [] ->
        let a' = List.rev xs
        R ( [List.head a'], List.tail a')
    | xs, y :: ys -> R (y :: xs, ys)

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


(* 4.4 *)
let smLength = SM (fun ring -> Some (length ring, ring))

let smPush x = SM (fun ring -> Some ((), push x ring))

let smPop = SM (fun ring -> peek ring |> Option.map (fun res -> (res, pop ring |> Option.get)))

let smCW = SM (fun ring -> Some ((), cw ring))
let smCCW = SM (fun ring -> Some ((), ccw ring))

(*
4.5

SIDENOTE:
Something about monads:
A monad is a design pattern that wraps functions in a value and specifies the return type
For something to be a monad it must do two things:

    - Wrap a value in a monad, for instance "ret" for return
      ret: 'a -> SM<'a>
      
    - Compose two monads into a sequence of function where the second "link" uses the result
      from the first "link"
      
      bind: SM<'a> -> ('a -> SM<'b>) -> SM<'b>
      
      The ">>="-operator is usually called "bind" or "flatMap" and does exactly this
      
      >>>=: SM<unit> -> SM<'a> -> SM<'a>
      
      This operator does principally the same thing. But we are ensured that the
      result from the first monad is of type unit. This means that we can simplify
      the function given to the right side of the operator by discarding the result
      as it will never have any effect because it is simply "unit" or "essentially nothing".
      
      In "ringStep" the value i is the result of calling smLength which we unwrap
      from its monadic container to use it in a function on the right of the bind operator.
      
    - Option is for instance a monad as well
      It has "ret" which is just the constructor "Some"
      Some : 'a -> 'a option
      
      It has bind:
      Option.bind : ('a -> 'b option) -> 'a option -> 'b option

*)

let ringStep : StateMonad<unit, int> =
    smLength >>= (fun i -> if i < 2 then ret () else
        smPop >>= (fun x ->
            smPop >>= (fun y ->
                if (x + y) % 2 = 0 then ret () else
                smPush y >>>= smPush x >>>= smCCW)))



(* Ring step using computation expressions *)

type StateBuilder() =

    member this.Bind(f, x)    = f >>= x
    member this.Zero ()       = ret ()
    member this.Return(x)     = ret x
    member this.ReturnFrom(x) = x
    member this.Delay(f)      = f ()
    member this.Combine(a, b) = a >>= (fun _ -> b)
    
let prog = new StateBuilder()

// We are able to use if-then expressions without a then-branch
// Because this.Zero corresponds to "ret ()", which will
// be applied whenever a branch is missing.
let ringStep2 : StateMonad<unit, int> =
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
    
(* Same function using computational expressions *)
let rec iterRemoveSumEven2 x =
    prog {
        if x > 0u
        then
            do! ringStep
            do! iterRemoveSumEven2 (x - 1u)
    }

// That's it - Thanks! :)

