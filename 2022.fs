//1.1

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
    |Square x when x = 255uy -> +1
    |Quad(a,b,c,d) -> countWhite a + countWhite b + countWhite c + countWhite d
    |_ -> 0

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
        |Quad(a,b,c,d) -> fold  folder (fold folder (fold folder (fold folder acc a) b) c) d

let countWhite2 img = fold (fun acc x -> if x = 255uy then 1 + acc else acc) 0 img