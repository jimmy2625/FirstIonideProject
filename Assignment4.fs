module FirstIonideProject.Assignment4

// Vores multiset består af et map der mapper strings eller a' til integers samt en integer der repræsenterer størrelse på mappet
// Den integer eller uint32 som a mapper til, repræsenterer antal af gange som a optræder i mappet
type MultiSet<'a when 'a : comparison> = 
    |MS of Map<'a , uint32> * uint32 

let empty<'a when 'a : comparison> = MS ((Map.empty<'a, uint32>), 0u)

let isEmpty (MS(map, size)) = if (size = 0u) then true else false

let size (MS(map, size)) = size

// Map.containskey returnerer true hvis a findes i mappet
let contains a (MS(map, size)) = Map.containsKey a map

// numitems returnerer 0 hvis keyen a ikke findes i mappet
// Map.find returnerer den value som keyen a mapper til
// Da den integer eller uint32 som a' mapper til, repræsenterer antal af gange som a optræder i mappet
let numItems a (MS(map, size)) =
    if (contains a (MS(map, size)))
        then Map.find a map
            else 0u

// med add kan et a' tilføjet til multisettet n antal gange
// Hvis a allerede findes i mappet, skal det nuværende n(Map.find a map) lægges oveni den nye n
// size skal altid opdateres med n
let add a n (MS(map, size)) =
    if (contains a (MS(map, size)))
        then (MS(Map.add a (Map.find a map + n) map, size + n))
            else (MS(Map.add a n map, size + n))

// addSingle kører add funktionen 1 gang
let addSingle a (MS(map, size)) = add a 1u (MS(map, size))

// Remove works kinda the same way as add.
// If a already exists in the map, but there are fewer than we want to remove
// Then a is completely removed from the map, and the size of the MS is
// its current size minus the number of a's previously in the map
// else twe use Map.add to update the MS with a smaller number of a's
let remove a n (MS(map, size)) = 
    if (numItems a (MS(map, size)) < n) 
        then (MS(Map.remove a map, size - numItems a (MS(map, size))))
            else (MS(Map.add a (numItems a (MS(map, size)) - n) map, size - n))

let removeSingle a (MS(map, size)) =
    if (contains a (MS(map, size)))
        then remove a 1u (MS(map, size))
            else (MS(map, size))

// the fold function simply takes the Map.fold higher order function
// And uses the function f on the map, this allows us to use Map.fold directly on a multiset
// Since Map.fold usually takes a map and not a Multiset
let fold f acc (MS(map, size)) = Map.fold f acc map

let foldBack f (MS(map, size)) acc = Map.foldBack f map acc

let map f (MS(map, size)) = Map.fold (fun acc key value -> add (f key) value acc ) empty map

let rec ofList = function
    |[] -> empty
    |x::xs -> addSingle x (ofList xs) 

// The toList function takes a multiset and uses our foldback function
// Which takes some function that we define here
// Our function uses List.replicate which takes an int as a count for the number of times it should put something in the list
// here we use (int val) which turns our uin32 into a regular int
// Then it takes a key, which is the item to put in the list, and the it concatenates it with the accumilator
let toList (MS(map, size)) = foldBack (fun key value acc -> List.replicate (int value) key @ acc) (MS(map, size)) [] 
 
// The union functino takes 2 MultiSets and return a set with the elements combined, where the largest containment of an element is the one to stay
// The function fold over the multisets(where ms1 is the accumilator/initial state and ms2 is the input map.
// if the key is in the second multiset, then add the key, with the abs value, to the accumilator ms1
let union (MS(map1, size1)) (MS(map2, size2)) =
    fold (fun acc key value ->
        if contains key (MS(map2, size2))
            then add key (uint32(abs (int value) - (int (numItems key (MS(map2, size2)))))) acc
                else add key value acc)
                (MS(map2, size2)) (MS(map1, size1))

// the sum takes 2 MS's and sum them together to a total of 2 MS's.
// since we have a toList function, we can concatenate them, and them put them back together. 
let sum (MS(map1, size1)) (MS(map2, size2)) = ofList (toList (MS(map1, size1)) @ toList (MS(map2, size2)))

let subtract (MS(map1, size1)) (MS(map2, size2)) = fold (fun acc key value -> remove key value acc) (MS(map1, size1)) (MS(map2, size2))

// in intersection, we use min value to find the least amount of accorences between ms1 and ms2 and the add the key that amount of times
let intersection (MS(map1, size1)) (MS(map2, size2)) = 
    fold (fun acc key value -> 
        if contains key (MS(map2, size2)) 
            then add key (min value (numItems key (MS(map2, size2)))) acc 
                else acc) empty (MS(map1, size1))

