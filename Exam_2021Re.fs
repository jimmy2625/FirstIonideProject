module exam2021re

type binList<'a, 'b> =
| Nil
| Cons1 of 'a * binList<'a, 'b>
| Cons2 of 'b * binList<'a, 'b>

//1.1
let rec length binlst = 
    match binlst with
    |Nil -> 0
    |Cons1 -> length(+1)
    |Cons2 -> length(+1)

length (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil))))))

//2 Code Comprehension

let rec foo xs ys =
  match xs, ys with
  | [], ys -> ys
  | xs, [] -> xs
  | x :: xs, y :: ys when x < y ->
  	x :: (foo xs (y :: ys))
  | x :: xs, y :: ys ->
  	y :: (foo (x :: xs) ys)

and bar =
  function
  | [] -> []
  | [x] -> [x]
  | xs ->
    let (a, b) = List.splitAt (List.length xs / 2) xs
    foo (bar a) (bar b)