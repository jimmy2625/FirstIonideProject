module FirstIonideProject.Assignment4

type MultiSet<'a when 'a : comparison> = 
    |MS of Map<'a , uint32> * uint32 

let empty<'a when 'a : comparison> = MS ((Map.empty<'a, uint32>), 0u)

let isEmpty (MS(map, size)) = if (size = 0u) then true else false

let size (MS(map, size)) = size

let contains a (MS(map, size)) = Map.containsKey a map

let numItems a (MS(map, size)) =
    if (contains a (MS(map, size)))
        then Map.find a map
            else 0u

let add a n (MS(map, size)) =
    if (contains a (MS(map, size)))
        then (MS(Map.add a (Map.find a map + n) map, size + n))
            else (MS(Map.add a n map, size + n))

let addSingle a (MS(map, size)) = add a 1u (MS(map, size))

let remove a n (MS(map, size)) = 
    if (numItems a (MS(map, size)) < n) 
        then (MS(Map.remove a map, size - numItems a (MS(map, size))))
            else (MS(Map.add a (numItems a (MS(map, size)) - n) map, size - n))

let removeSingle a (MS(map, size)) =
    if (contains a (MS(map, size)))
        then remove a 1u (MS(map, size))
            else (MS(map, size))

let fold f acc (MS(map, size)) = Map.fold f acc map

let foldBack f (MS(map, size)) acc = Map.foldBack f map acc

let map f (MS(map, size)) = Map.fold (fun acc key value -> add (f key) value acc ) empty map

let rec ofList = function
    |[] -> empty
    |x::xs -> addSingle x (ofList xs) 
     
let toList (MS(map, size)) = foldBack (fun key value acc -> List.replicate (int value) key @ acc) (MS(map, size)) [] 
 
let union (MS(map1, size1)) (MS(map2, size2)) =
    fold (fun acc key value ->
        if contains key (MS(map2, size2))
            then add key (uint32(abs (int value) - (int (numItems key (MS(map2, size2)))))) acc
                else add key value acc) (MS(map2, size2)) (MS(map1, size1))

let sum (MS(map1, size1)) (MS(map2, size2)) = ofList (toList (MS(map1, size1)) @ toList (MS(map2, size2)))

let subtract (MS(map1, size1)) (MS(map2, size2)) = fold (fun acc key value -> remove key value acc) (MS(map1, size1)) (MS(map2, size2))

let intersection (MS(map1, size1)) (MS(map2, size2)) = 
    fold (fun acc key value -> 
        if contains key (MS(map2, size2)) 
            then add key (min value (numItems key (MS(map2, size2)))) acc 
                else acc) empty (MS(map1, size1))

