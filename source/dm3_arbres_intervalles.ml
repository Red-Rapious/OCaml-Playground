type arbre = 
| V
| N of (int * int) * arbre * arbre

let a = N(
  (0, 1), 
  N((-21, -19), N((-26, -24), V, V), N((-16, -14), V, V)), 
  N((10, 12), N((5, 7), V, V), N((15, 17), V, V))
  );;

(* Complexité en O(h) dans le pire cas *)
let rec appartient n = function 
| V -> false
| N ((a, b), g, d) -> 
  n >= a && n <= b || if n < a then appartient n g else appartient n d;;

(* Complexité en O(h) dans le pire cas *)
let rec intervalles_gauche n = function
| V -> V
| N ((a, b), g, d) -> 
  if n >= a && n <= b then g
  else if n > b then N ((a, b), g, intervalles_gauche n d) 
  else intervalles_gauche n g;;

(* Complexité en O(h) dans le pire cas *)
let rec decoupe_gauche n = function
| V -> (V, n)
| N ((a, b), g, d) -> 
  if n >= a && n <= b then (g, a)
  else if n > b then 
    let i, t = decoupe_gauche n d in (N((a, b), g, i), t) 
  else decoupe_gauche n g;;

(* Complexité en O(h) dans le pire cas *)
let rec decoupe_droite n = function
| V -> (V, n)
| N ((a, b), g, d) -> 
  if n >= a && n <= b then (d, a)
  else if n < b then 
    let i, t = decoupe_droite n g in (N((a, b), d, i), t) 
  else decoupe_droite n d;;

(* Complexité en O(h) dans le pire cas *)
let insere x y a = 
  let ag, tg = decoupe_gauche x a in 
  let ad, td = decoupe_droite y a in 
  N ((tg, td), ag, ad);;

(* Complexité en O(n^2) dans le pire cas ? *)
let rec arbre_of_list = function
| [] -> V
| (x, y) :: l -> insere x y (arbre_of_list l);;

(*let l = [(0,1) ; (5, 7) ; (4, 9) ; (6, 10)];;
let a = arbre_of_list l;;*)

(* Complexité dégeulasse *)
let rec list_of_arbre = function
| V -> []
| N(s, g, d) -> s :: list_of_arbre g @ list_of_arbre d;;

(* Complexité O(n), avec n le nombre de noeuds *)
let list_of_arbre_2 a = 
  let l = ref [] in
  let rec aux = function
  | V -> ()
  | N(i, g, d) -> l := i :: !l ; aux g ; aux d
in aux a ; !l;;

(* Complexité en max(C_liste_of_arbre, C_arbre_of_list) *)
let normalise l = list_of_arbre (arbre_of_list l);;

let rec retrait = function
| V -> failwith "erreur: retrait d'un arbre vide"
| N((a, b), V, d) -> a, (if a != b then N((a+1, b), V, d) else d)
| N(i, g, d) -> let n, g' = retrait g in n, N(i, g', d);;

