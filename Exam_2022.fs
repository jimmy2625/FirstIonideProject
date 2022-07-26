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
    I have to recurse over the all of the grayscales inside of the Quad to make the code work.
    This is done by adding the results of the recursive calls to each grayscale inside the Quad
    My approach includes when matching a Square where the value is 255uy, the count will be increased by 1
    *)
   
    let rec countWhite (img : grayscale) : int =
        match img with
        |Square x when x = 255uy -> + 1
        |Quad(a,b,c,d) -> countWhite a + countWhite b + countWhite c + countWhite d
        |_ -> 0
    
(* Question 1.2 *)
    (*
    If it is a Square just return it
    If it is a Quad then run recursively the rotation on the grayscale
    I did not figure out how to make it run recursively on the other elements in the Quad
    *)
    let rec rotateRight img =
        match img with
        |Square x -> Square x
        |Quad(a,b,c,d) -> Quad(rotateRight d, rotateRight a, rotateRight b, rotateRight c)
            
(* Question 1.3 *)
    //almost
    let rec map mapper img =
        match img with
        |Square x -> mapper Square x
        |Quad(a,b,c,d) -> Quad(map mapper a, map mapper b, map mapper c, map mapper d)
    
    let bitmap _ = failwith "not implemented"

(* Question 1.4 *)
//Almost
    let rec fold folder acc img =
        match img with
        |Square x -> acc
        |Quad(a,b,c,d) -> folder(folder(folder(folder acc a)b)c) d
    
    let countWhite2 img = failwith "not implemented"

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
    Bar takes each element of the list and runs foo on it and returns the result in a string list
    
    Q: What would be appropriate names for functions 
       foo and bar?

    A: foo = intToBinary
       bar = intListToBinary
        
    Q: The function foo does not return reasonable results for all possible inputs.
       What requirements must we have on the input to foo in order to get reasonable results?
    
    A: The input must be a non-negative integer and inside the allowable range for 32-bit signed integers as well
    Furthermore, if the 
    *)
        

(* Question 2.2 *)

 
    (* 
    The function foo compiles with a warning. 

    
    Q: What warning and why?

    A: Foo has an incomplete pattern matching warning for the value "1".
    This warning occurs because the match case for the value "1" is not covered in the pattern matching.
    However, if 1 is input into the foo function, it will go to the last case and return "1", afterwards
    1 will be divided by 2 which results in 0 and returns the binary string representation of 1 which is 1.

    *)

    let rec foo2 =
        function
        | 0 -> ""
        | x when x % 2 = 0 -> foo (x / 2) + "0"
        | x when x % 2 = 1 -> foo (x / 2) + "1"

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

    A: I picked foo to analyze:
    foo 4
    = (foo 2) + "0" 
    = "0" + "0"
    = (foo 1) + "0" + "0"
    = "1" + "0" + "0"
    Foo is not tail-recursive because it still has to do some computations after the recursive call "foo (x/2)" which is adding the + "0" or "1"
    
    Q: Even though neither `foo` nor `bar` is tail recursive only one of them runs the risk of overflowing the stack.
       Which one and why does  the other one not risk overflowing the stack?

    A: bar runs the risk of overflowing the stack because it has to keep calculating using foo to concatenate onto the rest of the list

    *)
(* Question 2.5
    I made it tail-recursive by adding an inner function that uses an accumulator to store my computed values and returning
    the accumulator when the 0 case is reached
*)
    
    let fooTail x =
        let rec inner x' acc =
            match x' with
            | 0 -> acc
            | x when x % 2 = 0 -> foo (x / 2) + "0"
            | x when x % 2 = 1 -> foo (x / 2) + "1"
        inner x ""

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
    //this did not work
    let failDimensions m1 m2 = failwith "Invalid matrix dimensions: m1 rows =" numRows m1 "m1 columns = <number of
    columns in m1>, m2 roms = <number of rows in m2>, m2 columns = <number of columns in m2>"
    
(* Question 3.2 *)
    //almost
    let add m1 m2 = if numRows m1 = numRows m2 && numCols m1 = numCols m2 then (init (fun x y -> x+y) (numRows m1) (numCols m1)) else failDimensions m1 m2 
    
    add (init (fun x y -> x + y) 2 3) (init (fun x y -> x * y) 2 3) |> print

(* Question 3.3 *)
    
    let m1 = (init (fun i j -> i * 3 + j + 1) 2 3) 
    let m2 = (init (fun j k -> j * 2 + k + 1) 3 2)

    let dotProduct m1 m2 row col = failwith "not implemented"
    let mult _ = failwith "not implemented"

(* Question 3.4 *)
    let parInit _ = failwith "not implemented"

(* 4: Stack machines *)

    type cmd = Push of int | Add | Mult
    type stackProgram = cmd list

(* Question 4.1 *)

    type stack = int list (* replace this entire type with your own *)
    let emptyStack () : stack = []
    
    //I have made a helper length functions to use later on
    let length (a: stack) = List.length a

(* Question 4.2 *)

    let runStackProg (prog:stackProgram) : int = failwith "not implemented"
    
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
    
    //I also added the SM function of length as a helper function
    let smLength = SM (fun stack -> Some (length stack,stack))

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

    let runStackProg2 : StateMonad<unit> =
        state {
            let! length = smLength
            if length >= 2
            then
                let! x = pop
                let! y = pop
                if (x + y) % 2 <> 0
                then
                    do! push y
                    do! push x
        }
    
(* Question 4.5 *)

    let parseStackProg _ = failwith "not implemented"