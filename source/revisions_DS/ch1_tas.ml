type 'a tas = {
  mutable taille : int ;
  contenu : (int * 'a) array
}

let creer_tas capacite elt =
  { taille = 0 ; contenu = Array.make capacite (0, elt) };;

let est_vide tas = tas.taille = 0;;

let echanger t i j =
  let cache = t.(i) in 
  t.(i) <- t.(j) ;
  t.(j) <- cache
;;

(* Percolation vers le haut, pour corriger l'absence de caractère tournoi de l'arbre lors d'un ajout *)
let rec remontee tab i =
  if i > 0 then begin
    let parent = (i + 1) / 2 - 1 in begin
        if tab.(i) < tab.(parent) then begin
          echanger tab i parent ;
          remontee tab parent
        end
    end
  end;;

(* Percolation vers le bas, pour s'assurer que la nouvelle racine soit bien mise à sa place lors d'une extraction*)
let rec descente tab courant taille = 
  (* Définition des futurs candidats *)
  let candidat = ref courant in 
  let fils_gauche = (2 * courant) + 1 in 
  let fils_droit = fils_gauche + 1 in

  (* Analyse du fils gauche *)
  if fils_gauche < taille then begin
    if tab.(fils_gauche) < tab.(!candidat) then
      candidat := fils_gauche ;
    
    (* Analyse du fils droit*)
    if fils_droit < taille then begin
      if tab.(fils_droit) < tab.(!candidat) then
        candidat := fils_droit
    end;
    
    (* En cas de changement, on réalise l'échange et on continue récursivement *)
    if !candidat <> courant then begin
      echanger tab courant !candidat;
      descente tab !candidat taille
    end
  end
;;

(* Ajout d'un élément dans le tas *)
let ajout tas elt priorite =
  if tas.taille >= Array.length tas.contenu then
    failwith "ajout : tas plein" ;
  tas.contenu.(tas.taille) <- (priorite, elt) ;
  remontee tas.contenu tas.taille;
  tas.taille <- tas.taille + 1
;;

let extraction tas =
  if tas.taille = 0 then
    failwith "extraction : tas vide" ;
  
  let _, racine = tas.contenu.(0) in 
  tas.taille <- tas.taille - 1 ;
  tas.contenu.(0) <- tas.contenu.(tas.taille) ;
  descente tas.contenu 0 tas.taille ;
  racine
;;

let tri_par_tas t =
  let n = Array.length t in 
  for i = 1 to n-1 do 
    remontee t i 
  done ;
  for i = n - 1 downto 1 do 
    echanger t 0 i ;
    descente t 0 i
  done
;;