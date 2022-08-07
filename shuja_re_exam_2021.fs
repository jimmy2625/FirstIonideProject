
type binList<'a, 'b> =
| Nil
| Cons1 of 'a * binList<'a, 'b>
| Cons2 of 'b * binList<'a, 'b>

//binList length language: F#
// Path: exam/Program.fs
let rec length (c: binList<'a, 'b>) =
    match c with 
    | Nil -> 0
    | Cons1(_,b) -> 1 + length b
    | Cons2(_,b) -> 1 + length b


let rec split (bin: binList<'a, 'b>) = 
    match bin with 
    | Nil -> ([], [])
    | Cons1(a,b) -> (a::fst(split b), snd(split b))
    | Cons2(a,b) -> (fst(split b), a::snd(split b))
                                

let rec length2 = function
    | Nil -> (0,0)
    | Cons1(_,b) -> (1 + fst(length2 b), snd(length2 b))
    | Cons2(_,b) ->(fst(length2 b), 1 + snd(length2 b))

let rec map f g (bin: binList<'a, 'b>)  : binList<'b, 'd>= 
    match bin with 
    | Nil -> Nil
    | Cons1(a,b) -> Cons1(f a, map f g b)
    | Cons2(c,d) -> Cons2(g c, map f g d)


let rec filter f g (bin: binList<'a, 'b>) : binList<'a, 'b> = 
    match bin with 
    | Nil -> Nil
    | Cons1(a,b) -> if f a then Cons1(a, filter f g b) else filter f g b
    | Cons2(c,d) -> if g c then Cons2(c, filter f g d) else filter f g d

filter (fun x -> x % 2 = 0) 
       id 
       (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil))))))

let fold f g acc (bin: binList<'a, 'c>) : 'b = 
    let rec aux acc = function
        | Nil -> acc
        | Cons1(a,b) -> aux (f acc a) b
        | Cons2(c,d) -> aux (g acc c) d
    aux acc bin
 
let rec fold2 f g acc lst :'b = 
    match lst with 
    | Nil -> acc
    | Cons1(a,b) -> fold2 f g (f acc a) b
    | Cons2(c,d) -> fold2 f g (g acc c) d


let rec foo xs ys =
    match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | x :: xs, y :: ys when x < y ->
        x :: (foo xs (y :: ys))
    | x :: xs, y :: ys ->
        y :: (foo (x :: xs) ys)

let foo2 xs ys =
        List.unfold
            (function
             | [], []                      -> None
             | [], y :: ys                 -> Some (y, ([], ys))
             | x :: xs, []                 -> Some (x, (xs, []))
             | x :: xs, y :: ys when x < y -> Some (x, (xs, y :: ys))
             | x :: xs, y :: ys            -> Some (y, (x::xs, ys)))
            (xs, ys)
 


let fooTail xs ys =
    let rec aux xs ys acc =
        match xs, ys with
        | [], ys -> acc
        | xs, [] -> acc 
        | x :: xs, y :: ys when x < y ->
            aux xs (y :: ys) (x :: acc)
        | x :: xs, y :: ys ->
            aux (x :: xs) ys (y :: acc)
    aux xs ys [] |> List.rev


let barTail xs =
    let rec aux xs =
        match xs with
        | [] -> []
        | x :: xs ->
            x :: (aux xs)
    aux xs


let perfectSquare (x : int) = 
    let rec auxDecreasing x =
        if x <= 0 
            then 0
        else
            let d = System.Math.Sqrt (float x)
            //Khoth's pattern is followed (found on stack overflow); Math.Abs (d - Math.Floor(d + 0.001)) < 0.001
            if (System.Math.Abs (d - System.Math.Floor(d + 0.001)) < 0.001) 
                then x 
            else
                auxDecreasing (x-1)
    let rec auxIncreasing x =
            let d = System.Math.Sqrt (float x)
            if (System.Math.Abs (d - System.Math.Floor(d + 0.001)) < 0.001) 
                then x 
            else
                auxIncreasing (x+1)
    let a = auxDecreasing x
    let b = auxIncreasing x

    let differenceBetweenAandX = x - a
    let differenceBetweenBandX = b - x
    if (differenceBetweenAandX < differenceBetweenBandX && a <> 0)
        then a
    elif (differenceBetweenAandX > differenceBetweenBandX)
        then b
    else a

let approxSquare x num = 
    let y = perfectSquare x
    let mutable r = System.Math.Sqrt y
    let rec aux num =
        match num with
        | 0 -> r
        | a when a > 0 -> 
            r <- (((float x / r) + r) / 2.0)
            aux (num - 1)
    aux num


let quadratic a b c num   = 
    let rec aux a b c d =
        match a, b, c, d with
        | _, _, _, _ ->
            let x = (float -b + approxSquare(b*b - 4*a*c) num) / ( 2*a |> float)
            let y = (float -b - approxSquare(b*b - 4*a*c) num) / ( 2*a |> float)
            (x, y) 
    aux a b c num



let rec parQuadratic2 eqs  numprocesses num  : (float * float) list= 
    match eqs with 
    | [] -> []
    | eq :: eqs -> 
        match eq with 
            | (a,b,c) ->  
                let (x,y) = quadratic a b c num
                (x,y) :: parQuadratic2 eqs numprocesses num




type SM<'a> = SM of (rat -> ('a * rat) option)
let ret x = SM (fun st -> Some (x, st))
let bind (SM m) f =
    SM (fun st ->
    match m st with
    | None -> None
    | Some (x, st') ->
        let (SM g) = f x
        g st')
let (>>=) m f = bind m f
let (>>>=) m n = m >>= (fun () -> n)
let evalSM (SM f) s = f s


let smPlus x =
    SM (fun st ->
        match plus x st with
        | None -> None
        | Some (st') -> Some ((), st'))



type rat = 
    | RAT of int * int

let rec gcd (x : int) (y : int)  =
    match x, y with
    | 0, _ -> y
    | _, 0 -> x
    | x, y -> gcd (y % x) (x % y)

let mkRat n d  : rat option = 
    match n,d with 
    | n,d when d=0 -> None
    | n,d when n=0 -> let g = gcd n d in Some (RAT(n/g,d/g))
    | n,d  when n>0 && d>0 -> let g = gcd n d in Some (RAT(n/g,d/g))
    | n,d  when n<0 && d>0 -> let g = gcd -n d in Some (RAT(n/g,d/g))
    | n,d  when n>0 && d<0 -> let g = gcd n d in Some (RAT(-n/g,-d/g))
    | n,d  when n<0 && d<0 -> let g = gcd -n -d in Some (RAT(-n/g,-d/g))
    | _ -> failwith "None"



let ratToString (RAT(n,d)) : string = (string n) + " / " + (string d)

let plus (rat1 : rat) (rat2 : rat )  : rat option =
    match rat1, rat2 with
    | RAT(a,b), RAT(c,d) -> mkRat(a*d + b*c) (b*d)

let minus (rat1 : rat) (rat2 : rat )  : rat option =
    match rat1, rat2 with
    | RAT(a,b), RAT(c,d) -> mkRat(a*d - b*c) (b*d)

let mult (rat1 : rat) (rat2 : rat )  : rat option =
    match rat1, rat2 with
    | RAT(a,b), RAT(c,d) -> mkRat(a*c) (b*d)

let div (rat1 : rat) (rat2 : rat )  : rat option =
    match rat1, rat2 with
    | RAT(a,b), RAT(c,d) -> mkRat(a*d) (b*c)





let smMinus x =
    SM (fun st ->
        match minus st x with
        | None -> None
        | Some (st') -> Some ((), st'))

let smMult x =
    SM (fun st ->
        match mult x st with
        | None -> None
        | Some (st') -> Some ((), st'))

let smDiv x =
    SM (fun st ->
        match div st x with
        | None -> None
        | Some (st') -> Some ((), st'))

type StateBuilder() =
    member this.Bind(x, f) = bind x f
    member this.Zero () = ret ()
    member this.Return(x) = ret x
    member this.ReturnFrom(x) = x
    member this.Combine(a, b) = a >>= (fun _ -> b)

let state = new StateBuilder()

//Create the function calculate : (rat * (rat -> SM<unit>)) list -> SM<unit> that given a list [(r1,
//op1); (r2, op2); ...; (rn-1, opn-1); (rn, opn)] returns op1 r1 >>>= (op2 r2 >>>= ... >>>=
//(opn-1 rn-1 >>>= opn rn)...).
let rec calculate (list : (rat * (rat -> SM<unit>)) list  ) : SM<unit> = 
    match list with 
    | [] -> ret ()
    | (x : rat ,f ) :: xs ->
          f x  >>>= calculate xs
