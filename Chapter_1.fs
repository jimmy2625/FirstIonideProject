module FirstIonideProject.Chapter_1

let circleArea r = System.Math.PI * r * r;;
fun r -> System.Math.PI * r * r;;
let circleArea2 = fun r ->  System.Math.PI * r * r;;

  let daysOfMonth = function
      | 2        -> 28  // February
      | 4|6|9|11 -> 30  // April, June, September, November
      | _        -> 31  // All other months
      ;;

let rec fact = function
      | 0 -> 1
      | n -> n * fact(n-1);;
 

let rec power = function
      | (x,0) -> 1.0                
      | (x,n) -> x * power(x,n-1);;
(*power(4.0,2)
Y 4.0 * power(4.0,2-1)
Y 4.0 * power(4.0,1)
Y 4.0 * (4.0 * power(4.0,1-1))
Y 4.0 * (4.0 * power(4.0,0))
Y 4.0 * (4.0 * 1.0)
Y 16.0 *)

// This does NOT work
  let rec powerNo = function
      | (x, n) -> x * powerNo(x,n-1)   
      | (x, 0) -> 1.0;;
      
let rec gcd = function
      | (0,n) -> n
      | (m,n) -> gcd(n % m,m);;
      
   


printfn "A string: %s. An int: %i. A float: %f. A bool: %b" "hello" 42 3.14 true
printfn "area of circle with radius 10: %f" (circleArea 10)
printfn "there are %i days in february" (daysOfMonth 2)
printfn "10 factorial is %i" (fact 10)
printfn "3 to the power of 4 is %f" (power (3,4))
printfn "the greatest common divisor of 3768 and 1701 is %i" (gcd (3768, 1701))