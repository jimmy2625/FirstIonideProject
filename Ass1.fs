module Ass1

let sqr x = x * x

let pow x n = System.Math.Pow(x,n)

let rec sum n =
    match n with
    |0 -> 0
    |n -> sum (n-1) + n

let rec fib n =
    match n with
    |0 -> 0
    |1 -> 1
    |n -> fib(n-1) + fib(n-2)

let dup s = s ^ s

let rec dupn s n =
    match n with
    |0 -> ""
    |_ -> s + (dupn s (n-1))

let rec bin = function
    |(_,0) -> 1
    |(n,k) when n = k -> 1
    |(n,k) -> bin(n-1, k-1) + bin(n-1,k)