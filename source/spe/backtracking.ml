let rec nombre_liste p liste =
  match liste with 
    | [] -> 0
    | a :: l' -> (if p a then 1 else 0) + nombre_liste p l'
;;

let existence_tableau p tab =
  let existe = ref false in 
  for i = 0 to Array.length tab - 1 do 
    if p tab.(i) then existe := true 
  done;
  !existe
;;

(* Somme d'un sous-ensemble *)
let rec coups_valides (cible, candidats, selection) =
  match candidats with
  | [] -> []
  | a :: l -> 
    if a <= cible then (a, l) :: coups_valides (cible, l, selection) 
    else coups_valides (cible, l, selection)
;;

let applique (cible, candidats, selection) (valeur, reste) =
  (cible - valeur, reste, valeur :: selection);;

let position_initiale = (100, List.init 100 (fun i -> i+1), []);;

let est_finale (cible, _, _) = cible = 0;;

let compte_solutions =
  let compte = ref 0 in 
  let rec aux position = 
    if est_finale position then
      incr compte
    else
      List.iter
      (fun coup -> aux (applique position coup))
      (coups_valides position)
  in 
  aux position_initiale ;
  !compte
;;


(* Problème des reines *)
let rec coups_valides_2 (taille, ligne, reines) =
  let coups = ref [] in
  for x = 0 to taille-1 do
      let possible = ref true in
      List.iter (fun (reine_x, reine_y) -> 
        if x = reine_x then possible := false ;
        if ligne = reine_y then possible := false ;
        if reine_x + reine_y = x + ligne then possible := false ;
        if taille - reine_x + reine_y = taille - x + ligne then possible := false
      ) reines;
      if !possible then coups := (x, ligne) :: !coups
  done;
  !coups
;;

let applique_2 (taille, ligne, reines) (x, y) = (taille, ligne+1, (x, y) :: reines);;

let position_initiale_2 taille = (taille, 0, []);;

let est_finale_2 (taille, ligne, _) = ligne = taille;;

let compte_solutions_2 =
  let compte = ref 0 in 
  let rec aux position = 
    if est_finale_2 position then
      incr compte
    else
      List.iter
      (fun coup -> aux (applique_2 position coup))
      (coups_valides_2 position)
  in 
  aux (position_initiale_2 10) ;
  !compte
;;

print_int compte_solutions_2;;


(* Suites de Langford *)
let rec coups_valides_3 (taille, ligne, reines) =
  let coups = ref [] in
  for x = 0 to taille-1 do
      let possible = ref true in
      List.iter (fun (reine_x, reine_y) -> 
        if x = reine_x then possible := false ;
        if ligne = reine_y then possible := false ;
        if reine_x + reine_y = x + ligne then possible := false ;
        if taille - reine_x + reine_y = taille - x + ligne then possible := false
      ) reines;
      if !possible then coups := (x, ligne) :: !coups
  done;
  !coups
;;

let applique_3 (taille, ligne, reines) (x, y) = (taille, ligne+1, (x, y) :: reines);;

let position_initiale_3 taille = (taille, 0, []);;

let est_finale_3 (taille, ligne, _) = ligne = taille;;

let compte_solutions_3 =
  let compte = ref 0 in 
  let rec aux position = 
    if est_finale_2 position then
      incr compte
    else
      List.iter
      (fun coup -> aux (applique_2 position coup))
      (coups_valides_2 position)
  in 
  aux (position_initiale_2 10) ;
  !compte
;;