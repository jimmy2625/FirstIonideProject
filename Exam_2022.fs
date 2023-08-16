module Exam2022
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2022 = 
 *)

(* 1: Grayscale images *)

    type grayscale =
    | Square of uint8
    | Quad of grayscale * grayscale * grayscale * grayscale
    
    let img = 
      Quad (Square 255uy, 
            Square 128uy, 
            Quad(Square 255uy, 
                 Square 128uy, 
                 Square 192uy,
                 Square 64uy),
            Square 0uy)
    
(* Question 1.1 *)
    (*
        If square I return 1 if the square is white else 0
        If quad i recursively call countWhite on each quadrant and add the tally together
    *)
   
    let rec countWhite (img : grayscale) : int =
        match img with
        |Square x -> if x = 255uy then 1 else 0
        |Quad(a,b,c,d) -> countWhite a + countWhite b + countWhite c + countWhite d
    
(* Question 1.2 *)
    (*
        If Square I return the same square since rotating a square does not change it
        If Quad I call rotateRight recursively on each quadrant with the order changed to fit the description in the assignment
    *)
    let rec rotateRight img =
        match img with
        |Square x -> Square x
        |Quad(a,b,c,d) -> Quad(rotateRight d, rotateRight a, rotateRight b, rotateRight c)
            
(* Question 1.3 *)
    (*
        If it is a Square then run the mapper function on the x itself
        If it is a Quad then apply the recursive map with the mapper on each grayscale inside the Quad

        The bitmap function works by applying the map function with the lambda function inside to check if the
        uint is less than or equal to 127uy which indicates the square is black else return a white square (255uy)
    *)
    let rec map mapper img =
        match img with
        |Square x -> mapper x
        |Quad(a,b,c,d) -> Quad(map mapper a, map mapper b, map mapper c, map mapper d)
    
    let bitmap img = map (fun x -> if x <= 127uy then Square 0uy else Square 255uy) img

(* Question 1.4 *)
    (*
        Same case with Square as in 1.3 map, but this time with the added acc
        In the case of Quad apply the folder function and acc to the grayscale a inside the quad
        Then use this value as the acc for the calculation for the b grayscale and so on

        CountWhite2 is implemented with the help of the just created fold function
        I add an acc in the lambda function which represents the amount of white squares whereas the lambda function
        checks whether or not that particular square is white which increases the acc, else just return the same acc
    *)
    let rec fold folder acc img =
        match img with
        |Square x -> folder acc x
        |Quad(a,b,c,d) -> fold folder (fold folder (fold folder (fold folder acc a) b) c) d

    let countWhite2 img = fold (fun acc x -> if x = 255uy then 1 + acc else acc) 0 img

(* 2: Code Comprehension *)
    let rec foo =
        function
        | 0 -> ""
        | x when x % 2 = 0 -> foo (x / 2) + "0"
        | x when x % 2 = 1 -> foo (x / 2) + "1"

    let rec bar =
        function
        | []      -> []
        | x :: xs -> (foo x) :: (bar xs)

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo and bar?

    A: The type of foo is int -> string
       The type of bar is int list -> string list


    Q: What does the function bar and foo do.
       Focus on what it does rather than how it does it.

    A: Foo creates the string representation of a given int
    Bar creates a list of the binary string representations from a given list of ints
    
    Q: What would be appropriate names for functions 
       foo and bar?

    A: foo = intToBinary
       bar = intListToBinaryList
        
    Q: The function foo does not return reasonable results for all possible inputs.
       What requirements must we have on the input to foo in order to get reasonable results?
    
    A: The input must be a non-negative integer and inside the allowable range for 32-bit signed integers. 
        Furthermore, foo returns the wrong value for the input 0, since the binary representation of 0 is not "" "")
    *)
        

(* Question 2.2 *)

 
    (* 
    The function foo compiles with a warning. 

    
    Q: What warning and why?

    A: Since there is no non-constant match without a when clause the compiler cannot determine whether or not
       the when clauses are exhaustive or not.  
        
       to create foo2, I instead remove the second when-clause to make it exhaustive and also resulting in the exact same results for the same inputs as in foo.
    *)

    let rec foo2 =
        function
        | 0 -> ""
        | x when x % 2 = 0 -> foo (x / 2) + "0"
        | x when x % 2 = 1 -> foo (x / 2) + "1"
        | _ -> failwith "negative number"

(* Question 2.3 *) 

    //I simply use the List.map to run the foo function on each element of the input lst
    let bar2 lst = List.map foo lst

(* Question 2.4 *)

    (*

    Q: Neither foo nor bar is tail recursive. Pick one (not both) of them and explain why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
       You need to make clear what aspects of the evaluation tell you that the function is not tail recursive.
       Keep in mind that all steps in an evaluation chain must evaluate to the same value
       ((5 + 4) * 3 --> 9 * 3 --> 27, for instance).

    A: I have chosen foo to evaluate with the call foo 10

    foo 10 -->
    foo (10 / 2) + "0" -->
    foo 5 + "0" -->
    (foo (5 / 2) + "1") + "0" -->
    (foo 2 + "1") + "0" -->
    ((foo (2 / 2) + "0") + "1") + "0" -->
    ((foo 1 + "0") + "1") + "0" -->
    (((foo (1 / 2) + "1") + "0") + "1") + "0" -->
    (((foo 0 + "1") + "0") + "1") + "0" -->
    ((("" + "1") + "0") + "1") + "0" -->
    (("1" + "0") + "1") + "0" -->
    ("10" + "1") + "0" -->
    "101" + "0" -->
    "1010"
    
    Foo is not tail-recursive because it still has to do some computations after the recursive call "foo (x/2)" which is adding the + "0" or "1"
    
    Q: Even though neither `foo` nor `bar` is tail recursive only one of them runs the risk of overflowing the stack.
       Which one and why does  the other one not risk overflowing the stack?

    A: only bar risks overflowing the stacks as a list can be of arbitrary length and easily overflow the stack
       as you recurse over them. Foo will not overflow the stack as it's maximum number is 2^31 - 1. Since every 
       recursive call halves by two so the maximum number of recursive cals is 31 which will not overflow the stack.

    *)
(* Question 2.5
    I made it tail-recursive by adding an inner function that uses an accumulator to store my computed values and returning
    the accumulator when the 0 case is reached
*)
    
    let fooTail =
        let rec aux acc =
            function
            | 0 -> acc
            | x when x % 2 = 0 -> aux ("0" + acc) (x / 2)
            | x                -> aux ("1" + acc) (x / 2)
        aux ""

(* Question 2.6
    I used continuation by continually running fooTail on the x element inside my inner recursive call
    Furthermore, the continuation function is used on my result in the finishing case.
*)
    let barTail lst =
        let rec inner c =
            function
            |[] -> c []
            |x::xs -> inner(fun result -> c((fooTail x) :: result)) xs
        inner id lst

(* 3: Matrix operations *)

    type matrix = int[,]

    let init f rows cols = Array2D.init rows cols f

    let numRows (m : matrix) = Array2D.length1 m
    let numCols (m : matrix) = Array2D.length2 m

    let get (m : matrix) row col = m.[row, col]
    let set (m : matrix) row col v = m.[row, col] <- v

    let print (m : matrix) =
        for row in 0..numRows m - 1 do
            for col in 0..numCols m - 1 do
                printf "%d\t" (get m row col)
            printfn ""

(* Question 3.1 *)
    //I use an interpolated String to create my failwith message, and I use the %d format specifier to ensure it is an int
    let failDimensions (m1 : matrix) (m2 : matrix) =
        failwith
            $"Invalid matrix dimensions: m1 rows = %d{numRows m1}, m1 columns = %d{numCols m1}, m2 roms = %d{numRows m2}, m2 columns = %d{numCols m2}"

(* Question 3.2
    I check if the number of rows and columns are the same, if not then call faildimensions with said number of rows.
    If they are the same, use the init function from the definition with an anonymous function that adds the rows and cols from m1 with the rows and cols of m2
*)
    let add (m1 : matrix) (m2 : matrix) = if numRows m1 = numRows m2 && numCols m1 = numCols m2 then init (fun row col -> get m1 row col + get m2 row col) (numRows m1) (numCols m1) else failDimensions m1 m2
    
(* Question 3.3 *)

    let dotProduct m1 m2 row col =
        let b = numCols m1
        let lst = [0..b-1]
        List.fold 
            (fun acc j -> (get m1 row j) * (get m2 j col) + acc)
            0 lst

    let mult m1 m2 =
        if (numCols m1 <> numRows m2) then failDimensions m1 m2
        else
            let f row col = dotProduct m1 m2 row col
            init f (numRows m1) (numCols m2)

(* Question 3.4 *)
    open System.Threading.Tasks

    let parInit f rows cols = 
        let m = init (fun row col -> 0) rows cols
        let lst = List.init (rows*cols) 
                    (fun id -> (id/cols, id%cols))
        
        List.map
            (fun (row,col) ->
                async {
                    do set m row col (f row col)
                }
                ) lst
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
        m 


(* 4: Stack machines *)

    type cmd = Push of int | Add | Mult
    type stackProgram = cmd list

(* Question 4.1 *)

    type stack = int list (* replace this entire type with your own *)
    let emptyStack () : stack = []

(* Question 4.2 *)

    let runStackProg (prog: stackProgram) =
        if prog = [] then failwith "empty stack"
            else
                let rec aux sp (acc: stack) =
                    match sp with
                    | []       -> acc.Head
                    | x::xs -> match x with
                                |Push n -> aux xs (n::acc)
                                |Add    -> match acc with
                                           | []        -> failwith "empty stack"
                                           | x::y::ys  -> aux xs ((x+y)::ys)
                                           | _         -> failwith "empty stack"
                                |Mult   -> match acc with
                                           | []        -> failwith "empty stack"
                                           | x::y::ys  -> aux xs ((x*y)::ys)
                                           | _         -> failwith "empty stack"
                aux prog (emptyStack ())
    
(* Question 4.3 *)
    
    type StateMonad<'a> = SM of (stack -> ('a * stack) option)

    let ret x = SM (fun s -> Some (x, s))
    let fail  = SM (fun _ -> None)
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

    (*
    For my push, I just append the chosen integer x at the top of my stack as the changed SM and return unit (), since the type
    signature of push is int -> SM<unit>
    *)
    let push x = SM (fun stack -> Some((),x::stack))
   (*
   For my pop, I Just check if the stack is empty, if not then return success (Some) with the Head of the stack as return value
   and the changed SM now consists of the tail of that stack (the element is removed)
   *)
    let pop = SM (fun stack -> if stack.IsEmpty then None else Some(stack.Head, stack.Tail))

(* Question 4.4 *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.Zero ()       = ret ()
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()

    let rec runStackProg2 prog = 
        state {
            match prog with
            | [] -> return! pop
            | x :: xs ->
                match x with
                | Push a ->
                    do! push a
                    return! runStackProg2 xs
                | Add ->
                    let! first = pop
                    let! second = pop
                    do! push (first + second)
                    return! runStackProg2 xs
                | Mult ->
                    let! first = pop
                    let! second = pop
                    do! push (first * second)
                    return! runStackProg2 xs
        }
    
(* Question 4.5 *)

    open JParsec.TextParser

    let whitespaceChar = satisfy System.Char.IsWhiteSpace
    let spaces = many whitespaceChar

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2 // fjerner mellemrum mellem p1 og p2, returner (p1, p2)
    let (.>*>) p1 p2 = p1 .>> spaces .>> p2
    let (>*>.) p1 p2 = p1 .>> spaces >>. p2

    let pPush = spaces >>. pstring "PUSH" >*>. pint32 |>> (fun x -> Push x)
    let pAdd = spaces >>. pstring "ADD" |>> (fun x -> Add) // "ADD" -> Add
    let pMult = spaces >>. pstring "MULT" |>> (fun x -> Mult) // "MULT" -> Mult

    let parseStackProg (str: string) =
        let parser = many (pPush <|> pAdd <|> pMult)
        run parser str