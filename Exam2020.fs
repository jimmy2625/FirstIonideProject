type 'a bintree =
| Leaf
| Node of 'a bintree * 'a * 'a bintree

let insert