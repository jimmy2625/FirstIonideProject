module varmen

type MultiSet<'a when 'a : comparison> = 
    |MS of Map<'a , uint32> * uint32 

let empty = MS (Map.empty, 0u)

let isEmpty (MS(_,size)) = if size = 0u then true else false

let size (MS(Map,size))