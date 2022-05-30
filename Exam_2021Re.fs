module exam2021re

type binList<'a, 'b> =
| Nil
| Cons1 of 'a * binList<'a, 'b>
| Cons2 of 'b * binList<'a, 'b>

//1.1
let rec length binlst = 
    match binlst with
    |Nil -> 0
    |Cons1  -> length()

length (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil))))))