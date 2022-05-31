module FirstIonideProject.Assignment5

//exercise 5.1        
let sum m n =      
    let rec sumAux acc = 
        function
        | 0 -> acc + m
        | n -> sumAux (acc + m + n) (n - 1)
    sumAux 0 n
        
//exercise 5.2
let length lst =
    let rec lengthAux acc lst =
        match lst with
        |[] -> acc
        |_::tail -> lengthAux (acc+1) tail
    lengthAux 0 lst

//Exercise 5.3
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