module list

let testStringList = ["hej"; "ko"]
let testIntegerList = [1;2;3;4]

//List.map applies the function f to every element in the list
List.map (fun s -> s + ".fs") testStringList

//List.exists returns true if the function given holds true for at least one element in the list, and returns false otherwise
List.exists (fun x -> x>3) testIntegerList

//List.forall returns true if the function given holds true for all the elements in the list, and returns false otherwise
List.forall (fun x -> x<4) testIntegerList

//List.tryFind returns Some(x) if the function given holds true, and returns None otherwise
List.tryFind (fun x -> x > 2) testIntegerList

//List.filter returns a new list with all the elements where the function given holds true
List.filter (fun x -> x<4) testIntegerList

//List.fold accumulates the elements of the list with the accumulation function f (folder) starting with the value e (initialState) - the function f is folded over the list
//Adds all even numbers in the list together
//The x represents the accumulator
//The y represents the element from the list
List.fold (fun x y -> if y % 2 = 0 then x + y else x) 0 testIntegerList