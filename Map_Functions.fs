module map

let testMap = Map [ (1, "a"); (2, "b")]

//Map.add returns a new map with the new binding, if the key already exists in the old map, the old binding will be replaced by the new one
Map.add 3 "c" testMap

Map.add 2 "d" testMap

//Map.