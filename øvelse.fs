module øvelse

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
    |Square x when x = 255uy -> + 1
    |Quad(a,b,c,d) -> countWhite a + countWhite b + countWhite c + countWhite d
    |_ -> 0

countWhite img

