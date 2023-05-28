module TAExam2022
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
    let rec countWhite img = 
        match img with
        | Square i -> if (i = 255uy) then 1 else 0
        | Quad (a,b,c,d) ->
            let whiteA = countWhite a
            let whiteB = countWhite b
            let whiteC = countWhite c
            let whiteD = countWhite d
            whiteA + whiteB + whiteC + whiteD
    
(* Question 1.2 *)
    let rec rotateRight img =
        match img with
        | Square i -> Square i
        | Quad (a,b,c,d) ->
            let rotateA = rotateRight a
            let rotateB = rotateRight b
            let rotateC = rotateRight c
            let rotateD = rotateRight d
            Quad (rotateD, rotateA, rotateB, rotateC)

(* Question 1.3 *)
    let rec map mapper img = 
        match img with
        | Square i -> mapper i
        | Quad (a,b,c,d) ->
            let mapA = map mapper a
            let mapB = map mapper b
            let mapC = map mapper c
            let mapD = map mapper d
            Quad (mapA, mapB, mapC, mapD)

    
    let bitmap img = 
        let mapper i = if (i <= 127uy) then Square 0uy else Square 255uy
        map mapper img

(* Question 1.4 *)

    let rec fold folder acc img =
        match img with
        | Square i -> folder acc i
        | Quad (a,b,c,d) ->
            let foldA = fold folder acc a
            let foldB = fold folder foldA b
            let foldC = fold folder foldB c
            let foldD = fold folder foldC d
            foldD
    
    let countWhite2 img =
        let folder acc i = if (i = 255uy) then acc + 1 else acc
        fold folder 0 img

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

    A: 
    foo : int -> string
    bar : list<int> -> list<string>


    Q: What does the function bar do.
       Focus on what it does rather than how it does it.

    A: <Your answer goes here>

    foo takes an int and converts it to its binary equivalent.
    bar takes int list and uses foo to convert int list to binary


    Q: What would be appropriate names for functions 
       foo and bar?

    A: <Your answer goes here>

    foo = intToBinary
    bar = listToBinary
        
    Q: The function foo does not return reasonable results for all possible inputs.
       What requirements must we have on the input to foo in order to get reasonable results?
    
    A: <Your answer goes here>
    *)
        

(* Question 2.2 *)

 
    (* 
    The function foo compiles with a warning. 

    
    Q: What warning and why?

    A: <Your answer goes here>

    *)

    let rec foo2 =
        function
        | 0 -> ""
        | x when x % 2 = 0 -> foo (x / 2) + "0"
        | x -> foo (x / 2) + "1" 

(* Question 2.3 *) 

    let bar2 lst =
        let mapper x = foo x
        List.map mapper lst

(* Question 2.4 *)

    (*

    Q: Neither foo nor bar is tail recursive. Pick one (not both) of them and explain why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
       You need to make clear what aspects of the evaluation tell you that the function is not tail recursive.
       Keep in mind that all steps in an evaluation chain must evaluate to the same value
       ((5 + 4) * 3 --> 9 * 3 --> 27, for instance).

    A: <Your answer goes here>

    let rec foo =
        function
        | 0 -> ""
        | x when x % 2 = 0 -> foo (x / 2) + "0"
        | x when x % 2 = 1 -> foo (x / 2) + "1"


    foo (5)
    = foo (5 / 2) + "1"
    = (foo (2)) + "1"
    = (foo (2/2) + "0") + "1")
    = (foo 1) + "0" + "1")
    = (foo 1/2) + "1" + "0" + "1"
    = foo (0) + "1" + "0" + "1"
    = "" + "1" + "0" + "1" ()

    
    Q: Even though neither `foo` nor `bar` is tail recursive only one of them runs the risk of overflowing the stack.
       Which one and why does  the other one not risk overflowing the stack?

    A: <Your answer goes here>

    *)
(* Question 2.5 *)

    let fooTail x =
        let rec aux x acc =
            match x with
            | 0 -> acc
            | x when x % 2 = 0 -> aux (x/2) ("0" + acc)
            | x -> aux (x/2) ("1" + acc)
        aux x ""

    


    // let rec bar =
    //     function
    //     | []      -> []
    //     | x :: xs -> (foo x) :: (bar xs)


(* Question 2.6 *)
    let barTail lst =
        let rec aux lst c =
            match lst with
            | [] -> c []
            | x :: xs -> aux xs (fun r -> c (foo x :: r))
        aux lst id

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

    let failDimensions m1 m2 = "Invalid matrix dimensions: m1 rows = " + string(numRows m1) + ", m1 columns = <number of columns in m1>, m2 roms = <number of rows in m2>, m2 columns = <number of columns in m2>" |> failwith


(* Question 3.2 *)

    let add m1 m2 = 
        if (numRows m1 <> numRows m2 || numCols m1 <> numCols m2) then failDimensions m1 m2
        else 
            let f row col = (get m1 row col) + (get m2 row col)
            init f (numRows m1) (numCols m1)

(* Question 3.3 *)
    
    let m1 = (init (fun i j -> i * 3 + j + 1) 2 3) 
    let m2 = (init (fun j k -> j * 2 + k + 1) 3 2)

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

    type stack = int list
    let emptyStack () : stack = List.empty<int>

(* Question 4.2 *)

    let runStackProg (prog: stackProgram) =
        let rec aux prog (acc: stack) =
                match prog with
                | [] -> 
                    match acc with
                    | [] -> failwith "empty stack"
                    | x :: _ -> x
                | x :: xs ->
                    match x with
                    | Push a -> aux xs (a :: acc)
                    | Add -> 
                        match acc with
                        | a :: b :: rest -> aux xs (a + b :: rest)
                        | _ -> failwith "empty stack"
                    | Mult ->
                        match acc with
                            | a :: b :: rest -> aux xs (a * b :: rest)
                            | _ -> failwith "empty stack"
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

    let push x = SM (fun stack -> Some((), x :: stack))
    let pop = SM (fun stack ->
        match stack with
        | [] -> None
        | x :: xs -> Some(x, xs)
        )

(* Question 4.4 *)

    type StateBuilder() =
 
        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
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