(*------------------------------------*)
(* PARTIE 1 : ALGORITHME DE KARATSUBA *)
(*------------------------------------*)

type poly = int array ;;

let max a b = if a>b then a else b;;

let print_int_array a = 
  for i = 0 to (Array.length a) - 1 do
    print_int a.(i) ; print_string " " ; 
  done;
  print_string "\n";;

let somme pol1 pol2 =
  let pol = Array.make (max (Array.length pol1) (Array.length pol2)) 0 in
  for i = 0 to Array.length pol - 1 do
    if Array.length pol1 > i then pol.(i) <- pol.(i) + pol1.(i) ;
    if Array.length pol2 > i then pol.(i) <- pol.(i) + pol2.(i) ;
  done;
  pol;;

let soustraction pol1 pol2 =
  let pol = Array.make (max (Array.length pol1) (Array.length pol2)) 0 in
  for i = 0 to Array.length pol - 1 do
    if Array.length pol1 > i then pol.(i) <- pol.(i) - pol1.(i) ;
    if Array.length pol2 > i then pol.(i) <- pol.(i) - pol2.(i) ;
  done;
  pol;;

let produit_naif pol1 pol2 =
  let pol = Array.make (Array.length pol1 + Array.length pol2 - 1) 0 in
  for i = 0 to Array.length pol1 - 1 do
    for j = 0 to Array.length pol2 - 1 do
      pol.(i+j) <- pol.(i+j) + (pol1.(i) * pol2.(j));
    done;
  done;
  pol;;

let sous_tableau t deb fin =
  let pol = Array.make (fin - deb) 0 in
  for i = 0 to fin - deb - 1 do
    pol.(i) <- t.(i + deb);
  done;
  pol;;

let rec kara pol1 pol2 = 
  if Array.length pol1 != Array.length pol2
    then failwith "[Erreur] Les tailles de tableaux sont différentes";
  let n = Array.length pol1 in
  if n = 1 
    then [|0 ; pol1.(0) * pol2.(0)|]
  else
    let a, b = sous_tableau pol1 0 (n/2), sous_tableau pol1 (n/2) n in
    let c, d = sous_tableau pol2 0 (n/2), sous_tableau pol2 (n/2) n in
    let pol = Array.make (2*n) 0 in
    
    let ac, bd = kara a c, kara b d in
    let expr = soustraction (soustraction (kara (somme a b) (somme c d)) ac) bd in

    for i = 0 to n - 1 do
      pol.(i) <- pol.(i) + bd.(i);
    done;
    for i = 0 to n - 1 do
      pol.(i + n/2) <- pol.(i + n/2) + expr.(i);
    done;
    for i = 0 to n - 1 do
      pol.(i + n) <- pol.(i + n) + ac.(i);
    done;

    pol;;

kara [|1;2;3;4|] [|1;2;3;4|];;

(*---------------------------------------------*)
(* PARTIE 2 : NOMBRE D'INVERSIONS D'UN TABLEAU *)
(*---------------------------------------------*)


(* Complexité en O(n^2) *)
(* On a n(n+1)/2*2 lectures du tableau (en temps constant) *)
let inversions_naive t =
  let tot = ref 0 in
  for i = 0 to Array.length t - 2 do
    for j = i + 1 to Array.length t - 1 do
      if t.(i) > t.(j) then incr tot
    done;
  done;
  !tot;;

(* Complexité en O(l1 +l2) avec li la longueur de la liste i *)
let pre_inversions liste1 liste2 = 
  let rec aux l1 l2 acc ltot =
    match l1, l2 with
    | [], _ -> acc
    | l, [] -> acc + (List.length l) * ltot
    | a :: l1', b :: l2'
    -> if a > b 
      then aux l1 l2' acc ltot
      else aux l1' l2 (acc + (ltot - List.length l2)) ltot
  in aux liste1 liste2 0 (List.length liste2);;

(* TODO: DEUXIEME COMPTEUR *)

(*print_int (pre_inversions [3;7;8;15;17] [2;4;5;14;16]);;*)
 
(* Idée 1 : séparer le tableau en deux partie égales ?, 
  puis fusion sort chaque partie et enfin appliquer pre_inversions
  sur les deux demi-tableaux convertis en listes  *)
let rec partitionner l = 
  match l with
  | a :: b :: l' ->
    let l1, l2 = partitionner l' in (a::l1, b::l2)
  | _ -> (l, [])
;;

(*let partitionner_2 l =
  let n = List.length l in
  let a, a1, a2 = Array.of_list l, Array.make (n/2) 0, Array.make (n-(n/2)) 0 in
  for i = 0 to n - 1 do
    if i < n/2 
      then a1.(i) <- a.(i)
      else a2.(i-n/2) <- a.(i)
    done;
  Array.to_list a1, Array.to_list a2;;*)

(*let fusionner_2 l1 l2 =
  let n1, n2 = List.length l1, List.length l2 in
  let a, a1, a2 = Array.make (n1+n2) 0, Array.of_list l1, Array.of_list l2 in
  for i = 0 to n1 - 1 do
    a.(i) <- a1.(i)
  done;
  for i = 0 to n2 - 2 do
    a.(i+n1) <- a2.(i)
  done;
  Array.to_list a;;*)


let rec fusionner l1 l2 =
  match (l1, l2) with
  |[], _ -> l2
  |_, [] -> l1
  |a :: l1', b :: l2' ->
    if a <= b then 
      a :: fusionner l1' l2
    else
      b :: fusionner l1 l2'
;;

let rec tri_fusion_mod l =
  match l with
  | _ :: _ :: _ -> 
    let l1, l2 = partitionner l in 
    let (l1', a1), (l2', a2) = tri_fusion_mod l1, tri_fusion_mod l2 in
  (fusionner l1' l2', (a1 + a2 + pre_inversions l1' l2'))
  | _ -> l, 0
;;

let inversions_old tab = 
  let l1, l2 = partitionner (Array.to_list tab) in
  pre_inversions l1 l2;;

let inversions tab =
  snd (tri_fusion_mod (Array.to_list tab));;

let tableau_aleatoire taille bound = 
  Random.init 12345;
  let tab = Array.make taille 0 in
  for i=0 to taille-1 do
    tab.(i) <- Random.int bound
  done;
  tab;;

let tab = tableau_aleatoire 100 100;;
print_int (inversions_naive tab);;
print_string "\n";;
print_int (inversions tab);;