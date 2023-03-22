(* Problème des reines *)
let rec coups_valides_2 (taille, (i, ligne), reines) =
  let coups = ref [] in
  for x = 0 to taille-1 do
      let possible = ref true in
      List.iter (fun (reine_x, reine_y) -> 
        if x = reine_x then possible := false ;
        if reine_x + reine_y = x + ligne then possible := false ;
        if taille - reine_x + reine_y = taille - x + ligne then possible := false
      ) reines;
      if !possible then coups := (x, ligne) :: !coups
  done;
  !coups
;;

let ordre_ligne taille i = 
  (taille / 2) + (if i mod 2 = 0 then 1 else -1) * ((i+1)/2)
  (*i*)
;;

let applique_2 (taille, (i, ligne), reines) (x, y) = (taille, (i+1, ordre_ligne taille (i+1)), (x, y) :: reines);;

let position_initiale_2 taille = (taille, (0, ordre_ligne taille 0), []);;

let est_finale_2 (taille, (i, ligne), _) = i = taille;;


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