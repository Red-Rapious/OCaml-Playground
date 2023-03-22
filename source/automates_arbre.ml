(* Linéaire en la taille de la liste*)
let rec contient l e = 
  match l with 
  | a :: l' -> a = e || contient l' e 
  | [] -> false
;;

let rec union l1 l2 = 
  if l1 = [] then l2 else 
    (if contient l2 (List.hd l1)
      then union (List.tl l1) l2
    else
      List.hd l1 :: union (List.tl l1) l2)
;;

(* Complexité en le produit des tailles des deux listes *)
let rec union_mieux l1 l2 =
  match l1 with
  | [] -> l2
  | a:: l1' -> 
    if contient l2 a
      then union l1' l2
    else
      a :: union l1' l2
;;

(* Complexité O(L^2), car C_k = C_k-1 + O(L * |l_k|) *)
let rec fusion = function
| [] -> []
| l1 :: l2 -> union l1 (fusion l2)
;;

let produit l1 l2 =
  let couples = ref [] in 
  List.iter (fun a ->
    List.iter (fun b -> couples := (a, b) :: !couples) l2
  ) l1;
  !couples
;;


type arbre = Noeud of noeud | Vide
and noeud = { 
  etiquette: int; 
  gauche: arbre; 
  droit: arbre 
} ;;

let arbre x ag ad =
Noeud ({ etiquette = x ; gauche = ag ; droit = ad })
;;

let rec taille_arbre = function 
| Vide -> 0
| Noeud n -> 
  1 + taille_arbre n.gauche + taille_arbre n.droit
;;

type automate_Descendant_Deterministe = 
{ 
  finals_desc: bool array; 
  transitions_desc: (int * int) array array
} ;;

let rec applique_desc add q t =
  match t with
  | Vide -> add.finals_desc.(q)
  | Noeud n -> 
    let qg, qd = add.transitions_desc.(q).(n.etiquette) in
    applique_desc add qg n.gauche && applique_desc add qd n.droit
;;