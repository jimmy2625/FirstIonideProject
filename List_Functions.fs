module list

let testStringList = ["hej"; "ko"]
let testIntegerList = [10;20;30]

//List.map applies the function f to every element in the list
List.map (fun s -> s + ".fs") testStringList

List.map (fun s -> s + 2) testIntegerList

//List.exists returns true if the function given holds true for at least one element in the list, and returns false otherwise
List.exists (fun x -> x>4) testIntegerList

//List.forall returns true if the function given holds true for all the elements in the list, and returns false otherwise
List.forall (fun x -> x<4) testIntegerList

//List.tryFind returns Some(x) if the function given holds true, and returns None otherwise
List.tryFind (fun x -> x > 2) testIntegerList

//List.filter returns a new list with all the elements where the function given holds true
List.filter (fun x -> x<4) testIntegerList

//List.fold (folder acc list) accumulates the elements of the list with the accumulation function f (folder) starting with the value e (initialState) - the function f is folded over the list
//Adds all even numbers in the list together
//The acc represents the accumulator
//The y represents the element from the list
List.fold (fun acc x -> if x % 2 = 0 then acc+x else acc) 0 testIntegerList

//Usin List.fold to create a new list, returns the reversed list since the element is put in front of the list as it folds
List.fold(fun acc x -> x::acc) [] testIntegerList

//List.foldback (folder list acc) returns the list in the same order since the input is fed in reverse order
List.foldBack (fun x acc -> x::acc) testIntegerList [] 

//List.collect takes a function that applies to all the elements in the list and concatenates the results into one big list
//Creates 3 separate lists that is [10;20;30] [20;40;60] and [30;60;90] because of the function and concatenates into one final list
List.collect (fun x -> [for i in 1..3 -> x * i]) testIntegerList

//List.splitAt splits a list into two lists at the given index
List.splitAt 2 testIntegerList