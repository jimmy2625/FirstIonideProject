module øvelse

//1 Binary Lists

type binList<'a, 'b> =
| Nil
| Cons1 of 'a * binList<'a, 'b>
| Cons2 of 'b * binList<'a, 'b>

let rec length binlst =
    match binlst with
    |Nil -> 0
    |Cons1(_,b) -> length b + 1
    |Cons2(_,b) -> length b + 1

length (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil))))))

let rec split binlst =
    match binlst with
    |Nil -> ([],[])
    |Cons1(a,b) -> (a:: fst (split b), snd (split b))
    |Cons2(a,b) -> (fst (split b), a:: snd (split b))

split (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil))))))

let rec length2 binlst =
    match binlst with
    |Nil -> (0,0)
    |Cons1(_,b) -> (fst (length2 b) + 1, snd (length2 b))
    |Cons2(_,b) -> (fst (length2 b), snd (length2 b) + 1)

length2 (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil))))))

let rec map f g binlst =
    match binlst with
    |Nil -> Nil
    |Cons1(a,b) -> Cons1(f a, map f g b)
    |Cons2(a,b) -> Cons2(g a, map f g b)

map (fun x -> x % 2 = 0) 
    (function | true -> 0 | false -> 1) 
    (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil))))))

let rec filter f g binlst =
    match binlst with
    |Nil -> Nil
    |Cons1(a,b) -> if f a then Cons1(a, filter f g b) else filter f g b
    |Cons2(a,b) -> if g a then Cons1(a, filter f g b) else filter f g b

filter (fun x -> x % 2 = 0) 
       id 
       (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil))))))