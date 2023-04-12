module set

let testSetList = ["Jimmy" ; "Benjamin" ; "Benjamin"; "Lukas" ; "Lauge" ; "Shuja"]
let deSejeList = ["Jimmy" ; "Benjamin" ; "Sam"]

//Set.ofList creates a set from a list, since it is now a Set, repeated elements are removed and the Set is now ordered
let testSet = Set.ofList testSetList
let deSeje = Set.ofList deSejeList

//Set.toList creates a list from a set, the list is now ordered since it has been converted to a Set before
Set.toList testSet

//Set.add adds an element to the Set, the original Set is not changed
Set.add "Harry" testSet

//Set.remove removes an element from the Set, the original Set is not changed
Set.remove "Jimmy" testSet

//Set.contains checks if the Set contains a specific element
Set.contains "Jimmy" testSet

//Set.isSubset checks if given Set is a subset of another Set
Set.isSubset testSet (set ["Bums"; "Jimmy" ; "Benjamin"; "Lukas" ; "Lauge" ; "Shuja"])

//Set.minElement returns the smallest element in the Set
Set.minElement testSet

//Set.maxElement returns the biggest element in the Set
Set.maxElement testSet

//Set.count returns the amount of elements in the Set
Set.count testSet

//Set.union unions two sets, all of the elements combined in a new Set
Set.union testSet deSeje

//Set.intersect intersects two sets, those who are in both sets
Set.intersect testSet deSeje

//Set.difference creates a Set with the elements not in deSeje but is in TestSet
Set.difference testSet deSeje

//The rest of the Set functions work in the same fashion as the List functions