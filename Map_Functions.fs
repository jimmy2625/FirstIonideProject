module map

let testMap = Map [ (1, "a"); (2, "b")]

//Map.add returns a new map with the new binding, if the key already exists in the old map, the old binding will be replaced by the new one
Map.add 3 "c" testMap

Map.add 2 "d" testMap

//Map.change returns a new map with the value changed with the function given
Map.change 1 (fun x -> 
    match x with
    |Some s -> Some(s + "z")
    |None -> None
) testMap

//Map.tryFind looks up an element in the map returning Some if it's found and None if not (a match case can be created if Some or None has to return something special)
Map.tryFind(1) testMap

match Map.tryFind(3) testMap with
    |Some a -> a
    |None -> "does not exist"