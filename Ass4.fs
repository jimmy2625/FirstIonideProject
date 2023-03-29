module Ass4

//Exercise 1
type MultiSet<'a when 'a : comparison> = 
|MS of Map<'a,uint32> * uint32

let empty = MS ((Map.empty<'a, uint32>),0u)

let isEmpty (MS(_,size)) = if size = 0u then true else false

let size (MS(_,size)) = size 

let contains a (MS(map,_)) = map.ContainsKey a

let numItems a (MS(map,size)) = if contains a (MS(map,size)) 
                                    then Map.find a map 
                                        else 0u

let add a n (MS(map,size)) = if contains a (MS(map,size)) 
                                then (MS(Map.add a (Map.find a map + n)map,size + n))
                                    else (MS(Map.add a n map, size + n))

let addSingle a (MS(map,size)) = add a 1u (MS(map,size))

let remove a n (MS(map,size)) = if contains a (MS(map,size)) 
                                    then (MS(Map.remove a map,size-n))
                                        else (MS(map,size))

let ms = add 2 3u empty

remove 2 3u ms