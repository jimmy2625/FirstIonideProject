module exam2021re

type binList<'a, 'b> =
| Nil
| Cons1 of 'a * binList<'a, 'b>
| Cons2 of 'b * binList<'a, 'b>

//1.1
(*
  I match the binlst with the three cases Nil, Cons1 and Cons2
  If Nil then just return the length 0
  If Cons1, then I dont care about the first element in the Cons1, I only care about the binList(b). I add 1 to the length and call recursively on the rest of the binlist
  Same with Cons2
*)
let rec length binlst = 
    match binlst with
    |Nil -> 0
    |Cons1 (_,b) -> 1 + length b
    |Cons2 (_,b) -> 1 + length b 

length (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil))))))

//1.2
(*
  If Nil then return two empty lists
  If Cons1 then append the element a to the first list. Because split returns two lists use the fst keyword on the split recursive call, the second list is just run recursively
  If Cons2 then just run recursively on the first list, but append to the second list using the snd keyword. 
  This in turn will append to the first list if Cons1 type and append to the second list if Cons2.
*)

let rec split binlst =
  match binlst with
  |Nil -> ([],[])
  |Cons1 (a,b) -> (a:: fst (split b), snd(split b))
  |Cons2 (a,b) -> (fst (split b)), a::snd (split b)

split (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil))))))

(*
  If Nul then return a tuple of ints of (0,0)
  I do the same as in split but instead of appending, I add 1 to the fst in Cons1 and add 1 to the snd if Cons2
*)
let rec length2 binlst = 
  match binlst with
  |Nil -> (0,0)
  |Cons1 (_,b) -> (1 + fst (length2 b), snd (length2 b))
  |Cons2 (_,b) -> (fst(length2 b), 1 + snd (length2 b))

length2 (Nil : binList<int, bool>)

length2 (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil))))))

//1.3
(*
  I return Nil if Nil
  If Cons1 then create a new Cons1, where the function f is called on a and the map is called recursively on the rest of the list.
  If Cons2 then create a new Cons2, where the function g is called on a and the map is called recursively on the rest of the list.
*)
let rec map f g binlst = 
  match binlst with 
  |Nil -> Nil
  |Cons1 (a,b) -> Cons1(f a, map f g b)
  |Cons2 (a,b) -> Cons2(g a, map f g b)

map (fun x -> x % 2 = 0) 
    (function | true -> 0 | false -> 1)
    (Nil : binList<int, bool>)

map (fun x -> x % 2 = 0) 
    (function | true -> 0 | false -> 1) 
    (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil))))))

//1.4
(*
  If Nil then return Nil
  If Cons1 then make an if-statement that checks if f has been run on a is true, then return a new Cons1 with a and a recursive call to b. If false just call recursively on the rest of the list
  If Cons2 then make an if-statement that checks if g has been run on a is true, then return a new Cons2 with a and a recursive call to b. If false just call recursively on the rest of the list

*)
let rec filter f g binlst =
  match binlst with
  |Nil -> Nil
  |Cons1 (a,b) -> if f a then Cons1(a,filter f g b) else filter f g b
  |Cons2 (a,b) -> if g a then Cons2(a,filter f g b) else filter f g b

filter (fun x -> x % 2 = 0) 
       id 
       (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil))))))

//1.5
(*
  If Nil then return the acc
  If Cons1 then run fold recursively where the acc is updated with f run on the acc and element (a)
  If Cons2 then run fold recursively where the acc is updated with g run on the acc and element (a)
  This in turn will update the acc one Cons at a time by either applying f or g on each Cons. 
*)
let rec fold f g acc binlst =
  match binlst with 
  |Nil -> acc
  |Cons1 (a,b) -> fold f g (f acc a) b
  |Cons2 (a,b) -> fold f g (g acc a) b

fold (+) 
     (fun acc -> function | true -> acc | false -> -acc) 
     0 
     (Nil : binList<int, bool>)

fold (+) 
     (fun acc -> function | true -> acc | false -> -acc) 
     0 
     (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil))))))

//2 Code Comprehension
(*
  2.1
  Type of foo = list<'a> -> list<'a> -> list<'a>
  Type of bar = list<'a> -> list<'a>

  bar is sorting the list

  foo can be called indexSort
  bar can be called mergeSort

  a = mergeList1
  b = mergeList2
*)

let rec foo xs ys =
  match xs, ys with
  | [], ys -> ys
  | xs, [] -> xs
  | x :: xs, y :: ys when x < y -> x :: (foo xs (y :: ys))
  | x :: xs, y :: ys -> y :: (foo (x :: xs) ys)

and bar =
  function
  | [] -> []
  | [x] -> [x]
  | xs ->
    let (a, b) = List.splitAt (List.length xs / 2) xs
    foo (bar a) (bar b)

bar [8;1;2;3;6;4]

(*
  2.2
  The and keyword serves as the keyword for mutual recursion and widening the scope of the two functions
  If I removed the And keyword then foo inside of the bar function would not be able to run bar
*)

(*
  2.3
  
*)

//3 Approximating square roots
//3.1

//4 Rational numbers

//4.1
type rat = R of int * int

//4.2
//helper function to calculate g
let rec helper n d = 
    if d = 0 then n
    else helper d (n % d)

let mkRat n d = 
  if n < 0 && d < 0 
    then Some (R(n/helper n d, d/helper n d))
      elif n < 0 && d > 0 
        then Some (R(-n/helper n d, +d/helper n d))
          elif n > 0 && d < 0 
            then Some (R(-n/helper n d, +d/helper n d))
              elif d <> 0 
                then Some (R(n/helper n d, d/helper n d))
                  else None

let ratToString (R(a,b)) = string a + " / " + string b

mkRat 15 -10 |> Option.get |> ratToString