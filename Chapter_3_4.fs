module FirstIonideProject.Chapter_3_4

let xs = [2;3;2]
let x = 2::[3;4;5];;
let y = 5::x;;
let z = [2 .. 30]
let z2 = [2 .. 3 .. 100]
let ys = ["Big"; "Mac"]
let zs = [("b",2);("c",3);("e",5)]

type P = {name: string; age: int}
let qs = [{name = "Brown"; age = 25}; {name = "Cook"; age = 45}];;

let fs = [sin; cos;]
let ls = [[2;3];[3];[2;3;3]]


// x::xs matches any non empty list in pattern matching where x is the head(the front/first element of the list and xs is the rest)

(*let x::xs = [1;2;3];;
  val xs : int list = [2; 3]
  val x : int = 1*)
let rec suml = function
      | []    -> 0
      | x::xs -> x + suml xs
      
let rec altsum = function
| [] ->0
| [x] -> x
| x0::x1::xs -> x0 - x1 + altsum xs;;

let rec succPairs = function
      | x0 :: x1 :: xs -> (x0,x1) :: succPairs(x1::xs)
      | _              -> [];;
//this is layered patterns?
let rec succPairs2 = function
    | x0::(x1::_ as xs) -> (x0,x1) :: succPairs xs
    | _                  -> [];;
    
//sumProd returns a pair where the first is the sum and the seconds is the product. 
let rec sumProd = function
      | []   -> (0,1)
      | x::xs ->
            let (rSum,rProd) = sumProd xs
            (x+rSum,x*rProd)
            
//unzip [(1,"a");(2,"b")] -> [1; 2], ["a"; "b"]
let rec unzip = function
| []          -> ([],[])
| (x,y)::rest ->
      let (xs,ys) = unzip rest
      (x::xs,y::ys)
            
let rec mix = function
| (x::xs,y::ys) -> x::y::(mix (xs,ys))
| ([],[]) -> []
|  _ -> failwith "mix: parameter error";;
//Hvis ikke de 2 lister slutter på ([],[]) betyder det at de ikke er lige lange.
let rec mix2 xlst ylst =
      match (xlst,ylst) with
      | (x::xs,y::ys) -> x::y::(mix2 xs ys)
      | ([],[])       -> []
      | _             -> failwith "mix: parameter error"
let rec isMember x = function
      | y::ys -> x=y || (isMember x ys)
      | []    -> false;;
let xxs = [1;2] @ [3;4];;
let sxx = List.rev xxs

type ArticleCode = string
type ArticleName = string
type Price       = int
// a register is the list of available items with their code name and price
type Register    = (ArticleCode * (ArticleName*Price)) list

type NumberPieces= int
type Item        = (NumberPieces * ArticleCode)
type Purchase    = Item list

type Info        = NumberPieces * ArticleName * Price
type Infoseq     = Info list
type Bill        = Infoseq * Price
// A bill is a list of purchased items and a grand total price

//in this example, 37 is the grand total of 3 herrings and 1 cheese, where 12 is the grand total of 3 herrings at the cost of 4 each
let BillExample : Bill = ([(3,"herring",12); (1,"cheese",25)],    37)

let reg = [("a1",("cheese",25));
            ("a2",("herring",4));
             ("a3",("soft drink",5)) ]
let pur : Purchase = [(3,"a2"); (1,"a1")]

//adesc is the article name and price in the register, these are returned when the articlecode is found ac=ac'
// when they are not equal, the function will go to the next line _::(reg: Register) and try with the tail of the reg 
let rec findArticle articleCode = function
    | (aritcleCode',namePrice)::_ when articleCode=aritcleCode' -> namePrice
    | _::(reg: Register) -> findArticle articleCode reg
    | _ -> failwith(articleCode + " is an unknown article code")
   
// firstly the function finds the articles name and price in the register with the findarticle function
// the variable names aname and aprice are the results of the findarticle function on ac and rec. 
// then it calculates the price of the first item by multiplying the price with the quantitiy np
// then it adds it to the list of purchases and adds the price to the sum. 
let rec makeBill reg = function
    | []           -> ([],0)
    | (np,ac)::(pur: Purchase) -> let (aname,aprice) = findArticle ac reg
                                  let tprice         = np*aprice
                                  let (billtl,sumtl) = makeBill reg pur
                                  ((np,aname,tprice)::billtl,tprice+sumtl)
let test = makeBill reg pur


type Country = string
type Map     = (Country * Country) list

type Colour     = Country list
type Colouring  = Colour list

let exMap = [("a","b"); ("c","d"); ("d","a")]

//check whether 2 countries are neighbours in the forms (c1,c2) or (c2,c1) in the map which is a list of pairs of countries.
let areNb (m: Map) c1 c2 = isMember (c1,c2) m || isMember (c2,c1) m;;

//not(areNb) since they can NOT be neighbours.
//check every country c' in the color, if the country c' is neighbour with the country c in the Map m
let rec canBeExtBy (m: Map) (col:Colour) (c:Country) =
        match  col with
        | []       -> true
        | c'::col' -> not(areNb m c' c) && canBeExtBy m col' c;;
   
 canBeExtBy exMap ["c"] "a"
 //TRUE

 canBeExtBy exMap ["a"; "c"] "b";;
 //FALSE since b is neighbours with a and therefore can not be in the same color.
 
 
 //checks if the country c can be added to the colour cols, and adds it if possible.
 //If the color list is empty, the country is added to the list.
 //if not, there are 2 options. our country is checked with the first color in the coloring list. If they are not neightbours, our country will be added to that list
 // if they are neighbours, a recursion will happen, to check the next color list in the coloring, until it either finds a neighbor or makes a new color. 
let rec extColouring m cols c =
      match cols with
      | []         -> [[c]]
      | col::cols' -> if canBeExtBy m col c
                      then (c::col)::cols'
                      else col::extColouring m cols' c

//extColouring exMap [] "a";;
//val it : string list list = [["a"]]
//extColouring exMap [["c"]] "a";;
//val it : string list list = [["a"; "c"]]
//extColouring exMap [["b"]] "a";;
//val it : string list list = [["b"]; ["a"]]
//Note that the first of the three examples exercises the base case of the declaration, the second example the then-branch, and the last example the else-branch (the recursion and the base case).

//__
let addElem x ys = if isMember x ys then ys else x::ys;;
  let rec countries = function
      | []           -> []
      | (c1,c2)::m -> addElem c1 (addElem c2 (countries m));;
  let rec colCntrs m = function
      | []    -> []
      | c::cs -> extColouring m (colCntrs m cs) c;;


printfn "A string: %s. An int: %i. A float: %f. A bool: %b" "hello" 42 3.14 true
printfn "The sum of the list x is: %i" (suml (x)) 
printfn "The altsum of the list x is: %i" (altsum (x))
printfn "The sum and the product of the list xs is: %i %i" (fst (sumProd (xs))) (snd (sumProd(xs)))
printfn "2 is part of the list xs: %b" (isMember 2 xxs)