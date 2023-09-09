(* Implémentation classique du parcours en profondeur *)
let parcours_en_profondeur g = (* g est un tableau de listes contenants les voisins *)
  let n = Array.length g in 
  let etats = Array.make n 0 in 

  let rec traiter sommet = 
    if etats.(sommet) = 0 then begin
      (* pré-traitement *)
      etats.(sommet) <- 1 ;

      List.iter traiter g.(sommet) ;
      (* post-traitement *)

      etats.(sommet) <- 2
    end
  in 
  for s = 0 to n - 1 do 
    traiter s
  done
;;

let tri_topologique g =
  let n = Array.length g in 
  let etats = Array.make n 0 in 
  let tri = ref [] in

  let rec traiter sommet = 
    if etats.(sommet) = 0 then begin 
      (* pré-traitement *)
      etats.(sommet) <- 1 ;
      
      List.iter traiter g.(sommet) ;
      tri := sommet :: !tri ;

      (* post-traitement *)
      (*etats.(sommet) <- 2;*)
    end 
  in 
  for i = 0 to n-1 do 
    traiter i
  done ;
  !tri
;;

let detection_de_cycle g =
  let n = Array.length g in 
  let etats = Array.make n 0 in 
  let cycle = ref false in

  let rec traiter sommet = 
    if !cycle || etats.(sommet) = 1 then
      cycle := true
    else
      if etats.(sommet) = 0 then begin
        etats.(sommet) <- 1 ;

        List.iter traiter g.(sommet) ;

        etats.(sommet) <- 2
      end
  in 
  for s = 0 to n - 1 do 
    traiter s
  done ;
  !cycle
;;

(* Algorithme de Kosaraju *)
let parcours1 g =
  let n = Array.length g in 
  let a_traiter = Array.make n true in 
  let l = ref [] in
  
  let rec traiter sommet = 
    if a_traiter.(sommet) then begin
      a_traiter.(sommet) <- false ; 
      l := sommet :: !l ;

      List.iter traiter g.(sommet);
    end
  in 

  for s = 0 to n-1 do 
    traiter s
  done;
  !l
;;

let transpose g = 
  let n = Array.length g in 
  let gt = Array.make n [] in 

  for i = 0 to n-1 do 
    List.iter (fun j -> gt.(j) <- i :: gt.(j)) gt.(i)
  done;
  gt
;;

let parcours2 g ordre = 
  let n = Array.length g in 
  let a_traiter = Array.make n true in 

  let rec traiter sommet composante = 
    if a_traiter.(sommet) then begin 
      a_traiter.(sommet) <- false ;
      List.iter (fun s -> traiter s composante) g.(sommet);
      composante := sommet :: !composante
    end
  in 
  let composantes = ref [] in 

  let ajout_composante sommet = 
    if a_traiter.(sommet) then begin 
      let composante = ref [] in 
        traiter sommet composante ;
        composantes := composante :: !composantes 
    end
  in

  List.iter ajout_composante ordre;
  !composantes
;;

let kosaraju g = 
  let ordre = parcours1 g in 
  let gt = transpose g in 
  parcours2 gt ordre
;;