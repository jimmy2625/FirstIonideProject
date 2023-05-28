module bogen

//1.1
let g n = n + 4

//1.2
let h (x,y) = System.Math.Sqrt x*x + y*y

//1.3
let g2 = function
    |n -> n + 4

let h2 = function
    |(x,y) -> System.Math.Sqrt x*x + y*y

//1.4
let rec f n = 
    match n with
    |0 -> 0
    |n -> n + f(n-1)

//1.5
let rec fib n =
    match n with
    |0 -> 0
    |1 -> 1
    |n -> fib(n-1) + fib(n-2)

//1.6
let rec sum (m, n) =  
    match (m, n) with
    | (m, 0) -> m
    | (m, n) -> (m + n) + sum(m, n-1)

//1.7
(*
    1: (float, int)
    2: int
    3: float
    4: (float * int -> float) * (int -> int)
*)

//2.1
let f3 n = 
    match n with
    |n when n % 5 = 0 -> false
    |n when n % 2 = 0 -> true
    |n when n % 3 = 0 -> true
    |_ -> false

//2.2
let rec pow (s,n) =
    match (s,n) with
    |(_,0) -> ""
    |(s,1) -> s
    |(s,n) -> s + pow(s,n-1) 

//2.3
let isIthChar (str:string,i:int,ch:char) = if str.[i] = ch then true else false

//2.4
let occFromIth (str:string, i:int, ch:char) = if i >= str.Length then 0 else str.Substring i |> Seq.filter(fun charAtIndex -> charAtIndex = ch)|> Seq.length

//2.5
let occInString (str:string, ch:char) = occFromIth (str, 0, ch)

//2.6
let notDivisible (d,n) = if n % d <> 0 then true else false

//2.11
let VAT x n = x + x * float n/100.0

//3.1
