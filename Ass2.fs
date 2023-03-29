module Ass2

let rec downto1 n = if n<=0 then [] else n::downto1 (n-1)

let rec downto2 n = 
    match n with
    |n when n <= 0 -> []
    |_ -> n::downto2(n-1)

let removeOddIdx xs = xs |> List.mapi(fun i el -> el,i) |> List.filter(fun (_,i) -> i%2=0) |> List.map fst

let rec combinePair xs =
    match xs with
    |[] -> []
    |_::xs when xs.IsEmpty -> []
    |x::xs -> (x,xs.Head) :: combinePair(xs.Tail)

let explode1 (s:string) = s.ToCharArray() |> List.ofArray

let rec explode2 (s:string) = if s.Length > 0 then s.[0] :: explode2 (s.Remove(0,1)) else []

let implode (cs: char list) = List.foldBack (fun a b -> System.Char.ToString(a) + b) cs ""

let implodeRev (cs: char list) = List.fold (fun acc x ->  System.Char.ToString(x) + acc) "" cs

let toUpper (s:string) = s |> explode1 |> List.map System.Char.ToUpper |> implode

let rec ack = function
    |0,n -> n+1
    |m,0 when m > 0 -> ack(m-1,1)
    |m,n -> ack(m-1, ack(m,n-1))