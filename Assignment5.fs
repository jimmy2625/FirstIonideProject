module FirstIonideProject.Assignment5

//exercise 5.1
// the tail recursive version of sum, takes a inner aux function with another parameter acc (accumilator)
// this accumilator stops stack overflow by doing calculations before each recursive call
// sumAux is called with 0 and n the first time where 0 is the start of the accumilator

let sum m n =      
    let rec sumAux acc = 
        function
        | 0 -> acc + m
        | n -> sumAux (acc + m + n) (n - 1)
    sumAux 0 n  
        
//exercise 5.2
// lenght calculates the length of a list by adding 1 to the accumilator for each recursive call
// the accumilator is returned when the list is empty
let length lst =
    let rec lengthAux acc lst =
        match lst with
        |[] -> acc
        |_::tail -> lengthAux (acc+1) tail
    lengthAux 0 lst

//Exercise 5.3
//foldback takes a folding function f, a list and a state
// the aux function takes another parameter c that is also a function
// the aux function is called recursively on a function that takes some parameter res
let foldBack f lst state =
    let rec foldbackAux c = function
        | [] -> c state
        | x::xs -> foldbackAux (fun res -> c (f x res)) xs
    foldbackAux id lst

//Exercise 5.4
let factC x =
    let rec aux x c = 
        match x with
        |0 -> c 1
        |x -> aux (x-1) (fun res -> c (x * res))
    aux x id