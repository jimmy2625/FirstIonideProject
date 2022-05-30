module StateMonad

(*
    Connect the steps of getting a request and receiving the Response into a single unit
    The validate step takes an input and outputs something dependent on whether it was a Success or Failure
    Normally functions can only have one output, but by creating our own Result type, the validate step can now basically have two different outputs
    If we want to link a lot of functions together, it could be interpreted as a Railway track - if the train gets on the bad path, it wont ever go back to the right track
*)

type Request = {name:string; email:string}

type Result<'TSuccess,'TFailure> =
    | Success of 'TSuccess
    | Failure of 'TFailure

(*
    Normally use >> operator to function composition, but this only works when a function's output is the same as the compositioned function's input
    To solve this, instead use an adapter function that can change the type signature of the function so it can be compositioned
    The bind function takes a switch function and creates a new function that matches the twoTrackInput(Result) to Success which runs the function with s
    If it is a Failure, the train will go the bad track
    This can be interpreted as a the switchfunction being transformed from a one-tracked function to a two-tracked one
    Can be written in three ways:
*)

let bind switchFunction =
    fun twoTrackInput ->
        match twoTrackInput with
        | Success s -> switchFunction s
        | Failure f -> Failure f

let bind2 switchFunction twoTrackInput =
    match twoTrackInput with
    | Success s -> switchFunction s
    | Failure f -> Failure f

let bind3 switchFunction =
    function
    | Success s -> switchFunction s
    | Failure f -> Failure f

(*
    I will make some test cases to test the railway oriented programming style in f#
    These three test validates all take a Request input and outputs a Result
*)

let validate1 input =
   if input.name = "" then Failure "Name must not be blank"
   else Success input

let validate2 input =
   if input.name.Length > 50 then Failure "Name must not be longer than 50 chars"
   else Success input

let validate3 input =
   if input.email = "" then Failure "Email must not be blank"
   else Success input

(*
    If we want to use standard >> function composition then we have to bind the second and third validate functions
    The validate2' and validate3' functions now both take a two tracked input - this can be seen using ionide also
*)

// convert from switch to two-track input
let validate2' = bind validate2
let validate3' = bind validate3
// connect the two-tracks together
let combinedValidation = validate1 >> validate2' >> validate3'

//Testing

// test 1
let input1 = {name=""; email=""}
combinedValidation input1

// ==> Result1=Failure "Name must not be blank"

// test 2
let input2 = {name="Alice"; email=""}
combinedValidation input2

// ==> Result2=Failure "Email must not be blank"

// test 3
let input3 = {name="Alice"; email="good"}
combinedValidation input3

// ==> Result3=Success {name = "Alice"; email = "good";}

(*
    If I want to create an infix operator for binding it should be like this: (>>=)
    This approach is data-oriented rather than function-oriented since there is an explicit value x as the Request
*)

let (>>=) twoTrackInput switchFunction =
    bind switchFunction twoTrackInput

let combinedValidation2 x =
    x
    |> validate1   // normal pipe because validate1 has a one-track input
                    // but validate1 results in a two track output...
    >>= validate2  // ... so use "bind pipe". Again the result is a two track output
    >>= validate3   // ... so use "bind pipe" again.

