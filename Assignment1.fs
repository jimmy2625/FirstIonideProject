module Assignment1

//Exercise 1.1 = Write a function sqr : int -> int that given an integer x returns x squared
let sqr x = x * x

sqr 2

//Exercise 1.2 = Write a function pow : float -> float -> float that given two floating point numbers x and n returns x to the power of n
let pow x n = System.Math.Pow(x,n)

pow 10 2

//Exercise 1.3 = Write a recursive function sum : int -> int such that given an integer n such that n sum of all integers from 0 to n inclusive.
let rec sum = function
|0 -> 0
|n -> sum(n-1) + n

sum 1

//Exercise 1.4 = lav fibonacci
let rec fib = function
|0 -> 0
|1 -> 1
|n -> fib(n-1) + fib(n-2)

fib 5

//Exercise 1.5 = Write a function dup : string -> string that given a string s concatenates s with itself
let dup s:string = s ^ s

dup "fuck"

//Exercise 1.6 = Write a function dupn : string -> int -> string that given a string s and an integer n concatenates s with itself n times.
let rec dupn s n = 
    match n with
    |_ when n <= 0 -> ""
    |_ -> s + (dupn s (n-1))

dupn "fuck" 3

//Exercise 1.7 = pascal's triangle
let rec bin = function
|n,0 -> 1
|n,k when k = n -> 1
|n,k -> bin(n-1,k-1) + bin(n-1,k)

bin (4,2)

//Exercise 1.8 = timediff
let timediff (h1,m1) (h2,m2) = h2*60+m2 - h1*60-m1

timediff (12,34) (13,35)

//Exercise 1.9 = Write a function minutes : int * int -> int that computes the number of minutes since midnight.
let minutes (h,m) = timediff (0,0) (h,m)

minutes (23,1)

//Exercise 1.10 = defintions of curry and uncurry
let curry f a b = f (a,b)
let uncurry f (a,b) = f a b

//Assignment 1.11
let empty (letter, pointValue) = fun pos -> letter, pointValue
let theLetterA : int -> char * int = empty ('A',1)

theLetterA 123123