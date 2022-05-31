module FirstIonideProject.Chapter_2

let even n = n % 2 = 0;;
let isLowerCaseVowel ch = ch='a' || ch='e' || ch='i' || ch='o' || ch='u';;
let isLowerCaseConsonant ch = System.Char.IsLower ch && not (isLowerCaseVowel ch);;
let aString = @"123sdfs\\""\\df4"

let nameAge(name,age) = name + " is " + (string age) + " years old";;

let adjString s = if even(String.length s)
                    then s else " " + s
let square x = x * x
let square2 (x:float) = x * x
let square3 x = x * x : float
let plusSomething = (+)
let three = 3
let plusThree = plusSomething three
let twoPlusThree = (+) 2 3

let f = fun y -> y+3;;
let g = fun x -> x*x
let h = f << g;;



// h 4 = 4 * 4 + 3

let weight ro = fun s -> ro * s ** 3.0;;
let waterWeight = weight 1000.0 
//waterWeight 2.0 = 2.0 * 1000.0 ** 3.0
// weight tager 2 variable men får kun 1 i waterweight, det vil sige waterweight bliver en funktion der tager det sidste variable
let (.||.) p q = (p || q) && not(p && q)
let a = (1 > 2) .||. (2 + 3 < 5);;

type Person = {age : int; birthday : int * int; name : string; sex : string};;
let john = {name =  "John"; age = 29; sex = "M"; birthday = (2,11)};;
let solve1(a,b,c) =
      if b*b-4.0*a*c < 0.0 || a = 0.0
      then failwith "discriminant is negative or a=0.0"
      else ((-b + sqrt(b*b-4.0*a*c))/(2.0*a),
            (-b - sqrt(b*b-4.0*a*c))/(2.0*a));;
//solve1 returns a pair if the else branch is satisfied      

let solve2(a,b,c) =
      let d = b*b-4.0*a*c
      if d < 0.0 || a = 0.0
      then failwith "discriminant is negative or a=0.0"
      else ((-b + sqrt d)/(2.0*a),(-b - sqrt d)/(2.0*a));;
//solve 2 introducerer d for at skrive udregningen færre gange. 
  
let solve3(a,b,c) =
  let sqrtD =
      let d = b*b-4.0*a*c
      if d < 0.0 || a = 0.0
      then failwith "discriminant is negative or a=0.0"
      else sqrt d
  ((-b + sqrtD)/(2.0*a),(-b - sqrtD)/(2.0*a));;
  
//de nedadgående linjer er scopes. det inderste scope der begynder med let d = ... stopper når en linje er mindre indenteret hvilket linje 52 er. 
  
let solve4(a,b,c) =
  let d = b*b-4.0*a*c
  if d < 0.0 || a = 0.0
  then failwith "discriminant is negative or a=0.0"
  else let sqrtD = sqrt d
       ((-b + sqrtD)/(2.0*a),(-b - sqrtD)/(2.0*a));;
       
type Shape = | Circle of float
             | Square of float
             | Triangle of float*float*float;;
let area1 = function
      | Circle r        -> System.Math.PI * r * r
      | Square a        -> a * a
      | Triangle(a,b,c) ->
            let s = (a + b + c)/2.0
            sqrt(s*(s-a)*(s-b)*(s-c))
let isShape = function
      | Circle r        -> r > 0.0
      | Square a        -> a > 0.0
      | Triangle(a,b,c) ->
          a > 0.0 && b > 0.0 && c > 0.0
          && a < b + c && b < c + a && c < a + b
let area2 x =
      if not (isShape x)
      then failwith "not a legal shape" raise
      else match x with
           | Circle r        -> System.Math.PI * r * r
           | Square a        -> a * a
           | Triangle(a,b,c) ->
               let s = (a + b + c)/2.0
               sqrt(s*(s-a)*(s-b)*(s-c))

printfn "A string: %s. An int: %i. A float: %f. A bool: %b" "hello" 42 3.14 true
printfn "The number 2468 is an even number, is %b" (even 2468)
printfn "i, o and n are lower case vowels %b , %b, %b" (isLowerCaseVowel 'i')(isLowerCaseVowel 'o')(isLowerCaseVowel 'n')
printfn "This is a string %s and it is %i long and %s is the 6th character" (aString + aString) (String.length (aString + aString)) ( string aString.[5])
printfn "%s" (nameAge("Diana",15+4))
printfn "%s" (adjString "teste")
printfn "%i" (plusThree 5)
printfn "%b" ((1 > 2) .||. (2 + 3 < 5))
printfn "johns birthday: %i / %i" (fst john.birthday) (snd john.birthday)
printfn "the area of a circle with radius 5 is %f" (area2 (Circle 5.0))