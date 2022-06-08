type color = Red | Black
type 'a rbt = Leaf | Node of color * 'a rbt * 'a * 'a rbt

let rec mem v = function
| Leaf -> false
| Node (c, g, a, d) -> if c = v then true else mem v g;;

let rec min = function
| Leaf -> failwith "[min] arbre vide"
| Node (_, Leaf, a, Leaf) -> a
| Node (_, g, _, _) -> min g;;

let rec insert_leaf a = function
| Leaf -> Node(Red, Leaf, a, Leaf)
| Node(c, g, v, d) -> if a = v then Node(c, g, v, d) 
else if a > v then Node(c, g, v, insert_leaf a d)
else Node(c, insert_leaf a g, v, d);;

let rec update_conflict = function
| Node (Black, Node(Red, Node(Red, a, x, b), y, c), z, d)
| Node (Black, a, x, Node(Red, b, y, Node(Red, c, z, d)))
| Node (Black, Node(Red, a, x, Node(Red, b, y, c)), z, d)
| Node (Black, a, x, Node(Red,  Node(Red, b, y, c), z, d))
  -> Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
| a -> a
;;

let insert a arbre = 
  let rec aux a = function
  | Leaf -> insert_leaf a Leaf
  | Node(c, g, v, d) -> if a = v then Node(c, g, v, d)
  else if a > v then update_conflict(Node(c, g, v, aux a d))
  else update_conflict(Node(c,aux a g, v, d))
in match aux a arbre with
| Node(Red, g, v, d) -> Node(Black, g, v, d)
|a -> a;;

(* Sachant que la complexité de `update_conflict` est en O(1), 
`insert` est en complexité O(ln(n)), soit O(ln(t)), 
avec n le nombre de noeuds et t temps d'exécution *)