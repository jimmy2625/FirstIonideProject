module exam2020

type 'a bintree =
| Leaf
| Node of 'a bintree * 'a * 'a bintree
