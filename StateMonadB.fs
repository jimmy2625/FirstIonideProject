module FirstIonideProject.StateMonadB

type Result<'TSuccess,'TFailure> =
    | Success of 'TSuccess
    | Failure of 'TFailure
    
type Request = {name:string; email:string}
let bind switchFunction twoTrackInput =
    match twoTrackInput with
    | Success s -> switchFunction s
    | Failure f -> Failure f
//Funktionen gør bare så switchfunktionen ikke kører hvis den får failure, men altså stadig tager imod det
(*The function takes a switch function ('a -> Result<'b,'c>) just like the validate functions
and returns a function (Result<'a,'c> -> Result<'a,'c> so it changes the input of the function
meaning they can be piped together)
The switchFunction paramenter is the validate
and the twoTrackInput is the Result Type*)
    
(*The parameter (switchFunction) of bind takes some type 'a and emits a Result of type 'b (for the success track) and 'c (for the failure track)
The returned function itself has a parameter (twoTrackInput) which is a Result of type 'a (for success) and 'c (for failure). The type 'a has to be the same as what the switchFunction is expecting on its one track.
The output of the returned function is another Result, this time of type 'b (for success) and 'c (for failure) – the same type as the switch function output.*)

let validate1 input =
   if input.name = "" then Failure "Name must not be blank"
   else Success input

let validate2 input =
   if input.name.Length > 50 then Failure "Name must not be longer than 50 chars"
   else Success input

let validate3 input =
   if input.email = "" then Failure "Email must not be blank"
   else Success input
//Connot use >> since the output on the left has to match the input on the right, which is does not in the start, when it is not a some of none yet
// the validate function takes a request and returns a result
// the bind >>= does so that they can be piped together.  
// glue the three validation functions together
// After using the function bind on validate2, it now takes a result instead of a request
let combinedValidation =
    // convert from switch to two-track input
    let validate2' = bind validate2
    let validate3' = bind validate3
    // connect the two-tracks together
    validate1 >> validate2' >> validate3'
// test 1
let input1 = {name=""; email=""}
combinedValidation input1
|> printfn "Result1=%A"

// ==> Result1=Failure "Name must not be blank"

// test 2
let input2 = {name="Alice"; email=""}
combinedValidation input2
|> printfn "Result2=%A"

// ==> Result2=Failure "Email must not be blank"

// test 3
let input3 = {name="Alice"; email="good"}
combinedValidation input3
|> printfn "Result3=%A"

// ==> Result3=Success {name = "Alice"; email = "good";}

/// create an infix operator
/// Left hand side is is a two-track value (Result) and right hand side is a switch function 
let (>>=) twoTrackInput switchFunction =
    bind switchFunction twoTrackInput

let combinedValidation2 x =
    x
    |> validate1   // normal pipe because validate1 has a one-track input
                   // but validate1 results in a two track output...
    >>= validate2  // ... so use "bind pipe". Again the result is a two track output
    >>= validate3   // ... so use "bind pipe" again.
//we want to bind the 2nd and 3hd functions so they can take either a none and pass it along, or take a some and turn it into some or none
//we dont have to bind the first function, as it should not take a none. 
let combinedValidation3 =
    validate1
    >> bind validate2
    >> bind validate3
(*Bind has one switch function parameter. It is an adapter that converts the switch function into a fully two-track function (with two-track input and two-track output).
Switch composition has two switch function parameters. It combines them in series to make another switch function.*)

let (>=>) switch1 switch2 x =
    match switch1 x with
    | Success s -> switch2 s
    | Failure f -> Failure f
(*
Tager et input x og giver til den første switchfunction, 
hvis den passer så giv videre til den anden
hvis den failer så smid den ud
*)
let combinedValidation4 =
    validate1
    >=> validate2
    >=> validate3
//hvis validate 1 giver success bliver output givet til validate 2, ellers bliver det smidt ud

//FORSKELLEN PÅ BIND(==>) OG SWITCH(>=>)
(*
Bind has one switch function parameter. It is an adapter that converts the switch function into a fully two-track function (with two-track input and two-track output).
Switch composition has two switch function parameters. It combines them in series to make another switch function.
altså, bind tager en switchfunction og laver den om så den tager et 2-trackinput
switch tager en 2 switchfunktioner og binder dem sammen
*)
let canonicalizeEmail input =
   { input with email = input.email.Trim().ToLower() }
   
   
// convert a normal function into a switch
let switch f x =
    f x |> Success
    
//når man både bruger switch til at få canonicalizeEmail til at outputte et result
// og >=> til at binde den sammen med andre switch funktioner
let usecase =
    validate1
    >=> validate2
    >=> validate3
    >=> switch canonicalizeEmail
    
// convert a normal function into a two-track function
let map oneTrackFunction twoTrackInput =
    match twoTrackInput with
    | Success s -> Success (oneTrackFunction s)
    | Failure f -> Failure f

//tee er en dead-end og dens output bliver ignoreret. 
let tee f x =
    f x |> ignore
    x
    
// kan både sende success og failure videre. 
let doubleMap successFunc failureFunc twoTrackInput =
    match twoTrackInput with
    | Success s -> Success (successFunc s)
    | Failure f -> Failure (failureFunc f)