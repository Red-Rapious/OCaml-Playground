type 'a arbre =
| V
| N of 'a arbre * 'a * 'a arbre

(* Exercice 1 *)
let rec nb_noeuds_feuilles = function
| V -> (0, 0)
| N(V, _, V) -> (1, 1)
| N(g, _, d) -> 
  let (n1, f1), (n2, f2) = nb_noeuds_feuilles g, nb_noeuds_feuilles d 
  in (n1 + n2, f1 + f2);;

(* Exercice 2 *)
let rec est_strict = function
| V -> false
| N(V, _, V) -> true
| N(g, _, d) -> est_strict g && est_strict d;;

(* Exercice 3 *)
let est_parfait a = 
  let rec aux acc = function
  | V -> (false, acc)
  | N(V, _, V) -> (true, acc)
  | N(g, _, d) -> 
    let (b1, p1), (b2, p2) = aux (acc+1) g, aux (acc + 1) d in
    (b1 && b2 && p1 = p2, p1)
  in fst (aux 0 a);;

(* Exercice 4 *)
let rec miroir = function
| V -> V
| N(g, w, d) -> N(miroir d, w, miroir g);;

(* Exercice 5*)
let rec contient e = function 
| V -> false
| N(g, w, d) -> w = e || contient e g || contient e d ;;

(* Exercice 6 *)
let ppsac a x y = 
  let rec aux a x y = match a with
  | V -> (None, false, false)
  | N (g, e, d) -> 
    let (a1, cx1, cy1), (a2, cx2, cy2) = aux g x y, aux d x y in 
    if a1 != None then (a1, true, true) 
    else if a2 != None then (a2, true, true)
    else 
      let cx, cy = cx1 || cx2 || x=e, cy1 || cy2 || y=e in 
      ((if not cx || not cy then None else Some a), cx, cy)
      
  in match (aux a x y) with
  | None, _, _ -> failwith "ppsac: erreur"
  | Some a, _, _ -> a;;

(* Exercice 7 *)
let rec parcours_prefixe f = function
| V -> ()
| N(g, w, d) -> f w ; parcours_prefixe f g ; parcours_prefixe f d;;

let rec parcours_infixe f = function
| V -> ()
| N(g, w, d) -> parcours_infixe f g ; f w ; parcours_infixe f d;;

let rec parcours_postfixe f = function
| V -> ()
| N(g, w, d) -> parcours_postfixe f g ; parcours_postfixe f d ; f w;;


(* Exercice 8 *)
let rec liste_prefixe = function
| V -> []
| N(g, w, d) -> w :: (liste_prefixe g @ liste_prefixe d);;

let rec liste_infixe = function
| V -> []
| N(g, w, d) -> liste_infixe g @ (w :: liste_infixe d);;

let rec liste_postfixe = function
| V -> []
| N(g, w, d) -> (liste_postfixe g @ liste_postfixe d) @ [w];;

(* Exercice 9 *)
let arbre_max = N(
  N(N(V, 3, V), 5, N(V, 1, V)), 
  18, 
  N( N(N(V, 2, V),  4, N(V, 0, V)), 10, V) 
);;

(* Exercice 10 *)
let rec ajoute x = function
| V -> N(V, x, V)
| N(g, w, d) -> 
  if x > w then N(V, x, N(g, w, d))
  else N(ajoute x g, w, d);;

(* Dans le pire cas, la liste est triée et x en est le ppe *)
(* Ainsi la complexité est en O(n) avec n la taille de la liste*)
(* L'arbre correspondant est un peigne gauche *)

(* Exercice 11 *)
let ind_max_liste l = 
  let rec aux l (i, m) = 
    if l = [] then (i, m)
    else match m with
    | None ->  aux (List.tl l) (1, Some (List.hd l))
    | Some a -> if a < (List.hd l) then aux (List.tl l) (i + 1, Some (List.hd l))
                else aux (List.tl l) (i, m)

  in match aux l (0, None) with
  | (i, None) -> failwith "erreur"
  | (i, Some a) -> (i, a);;

let liste = [3 ; 7; 6; 8; 9 ; 8 ; 5];;
ind_max_liste liste;;