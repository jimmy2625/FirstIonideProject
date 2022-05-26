module chapter1
//1.1 Declare a function g: int -> int, where g(n) = n + 4.

let g n:int = n + 4;;

g 3;;

//1.2 Declare a function h: float * float -> float, where h(x,y) = squareroot(x2 + y2)

let h (x:float,y:float) = System.Math.Sqrt((System.Math.Pow(x,2))+(System.Math.Pow(y,2)))

h (3.0,1.0);;

//1.3