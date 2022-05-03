(* PARTIE 1 : ALGORITHME DE KARATSUBA *)

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

(* PARTIE 2 : NOMBRE D'INVERSIONS D'UN TABLEAU *)

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

