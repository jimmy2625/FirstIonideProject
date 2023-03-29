module Assignment2

//Exercise 2.1 = Write a function downto1 : int -> int list that given an integer returns the -element list [n; n-1;
let rec downto1 n = if n <= 0 then [] else n::downto1(n-1)

downto1 10

let rec downto2 = function
|0 -> []
|n -> n::downto2(n-1)

downto2 10

//Exercise 2.2 = Write a function removeOddIdx : 'a list -> 'a list that given a list xs returns a list where all odd- indexed elements of xs have been removed
let removeOddIdx xs = xs |> List.mapi(fun i el -> el,i) |> List.filter(fun (_,i) -> i%2 = 0) |> List.map fst
removeOddIdx ["hej"; "med"; "dig"]
//tag listen xs - pipe det og brug mapi så jeg mapper alle index på elementerne med et integer - filter det så lige tal bliver fjernet - brug List.map og tag første element da vi ikke vil have index men kun string

//Exercise 2.3 = Write a function combinePair : 'a list -> ('a * 'a) list that given a list xs returns the list with elements from xs combined into pairs. If xs contains an odd number of elements, then the last element is thrown away.
let rec combinePair = function
|[] -> []
|x::xs when xs.IsEmpty -> []
|x::xs -> (x,xs.Head) :: combinePair xs.Tail

combinePair ["Marry"; "had"; "a"; "little"; "lamb"; "its"; "fleece";
                "was"; "white"; "as"; "snow"];;

//lav pattern matching, hvis tom liste -> tom liste - hvis listen har ulige antal strings, dvs tail er empty -> tom liste - ellers så lav par med to elementer ad gangen, derefter kør rekursivt på xs' tail

//Exercise 2.4 = nye typer
type complex = float * float

let mkComplex x y = (x,y) : complex

let complexToPair (x : complex) = fst x, snd x

let (|+|) (c1:complex) (c2:complex) = 
    let a,b = c1
    let c,d = c2
    a+c, b+d

let (|*|) (c1:complex) (c2:complex) =
    let a,b = c1
    let c,d = c2
    (a*c-b*d, b*c+a*d)

let (|-|) (c1:complex) (c2:complex) =
    let a,b = c2
    c1 |+| (-a,-b)

let (|/|) (c1:complex) (c2:complex) =
    let a,b = c2
    let d = a**2 + b**2
    c1 |*| (a/d,-b/d)

//Exercise 2.5 = Write a non-recursive function explode1 : string -> char list that given a string s returns the list of characters in s .
let explode1 = function 
|"" -> []
|s -> s.ToCharArray() |> List.ofArray

//brug s.ToCharArray() og pipe det til List.ofArray

explode1 "hej fede"

let rec explode2 = function
|"" -> []
|s -> s.[0] :: explode2 (s.Remove(0,1))

//brug s.[0] til at få første element i char listen, kør rekursivt på s.Remove(0,1) som fjerner 1 element ad gangen indtil listen er tom

explode2 "hej fede"

//Exercise 2.6 = Write a function implode : char list -> string that given a list of characters cs returns a string with all characters of cs in the same order.
let implode (cs: char list) = List.foldBack (fun (a:char ) (b:string) -> System.Char.ToString(a) + b) cs ""