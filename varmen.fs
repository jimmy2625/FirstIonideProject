module varmen

type grayscale =
| Square of uint8
| Quad of grayscale * grayscale * grayscale * grayscale

let img =
 Quad (Square 255uy,
        Square 128uy,
        Quad(Square 255uy,
            Square 128uy,
            Square 192uy,
            Square 64uy),
        Square 0uy)

let rec countWhite img =   
    match img with
    |Square x -> if x = 255uy then + 1 else 0
    |Quad(a,b,c,d) -> countWhite a + countWhite b + countWhite c + countWhite d

let rec rotateRight img =
    match img with
    |Square x -> Square x
    |Quad(a,b,c,d) -> Quad(rotateRight d, rotateRight a, rotateRight b, rotateRight c)

let rec map mapper img =
    match img with
    |Square x -> mapper x
    |Quad(a,b,c,d) -> Quad(map mapper a, map mapper b, map mapper c, map mapper d)

let bitmap img = map (fun x -> if x <= 127uy then Square 0uy else Square 255uy) img

let rec fold folder acc img =
    match img with
    |Square x -> folder acc x
    |Quad(a,b,c,d) -> fold folder (fold folder (fold folder (fold folder acc a) b) c) d

let countWhite2 img = fold (fun acc x -> if x = 255uy then acc + 1 else acc) 0 img

let rec foo =
    function
    | 0 -> ""
    | x when x % 2 = 0 -> foo (x / 2) + "0"
    | x when x % 2 = 1 -> foo (x / 2) + "1"

let rec bar =
    function
    | [] -> []
    | x :: xs -> (foo x) :: (bar xs)

(*
    The type of foo is: int -> string
    The type of bar is: list<int> -> list<string>

    foo returns the binary representation of the given int in a string format
    bar returns the list of binary representations as a a string from the given list of ints

    foo: intToBinary
    bar: intListToBinaryList

    the input to foo has to be a non-negative integer for it to compile, furthermore foo 0 returns the wrong result
*)

(*
    Foo throws an incomplete pattern matching warning because the when-clauses are not exhaustive because there is no non-constant match without a when-clause.
*)