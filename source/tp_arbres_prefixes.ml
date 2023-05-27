module CharMap = Map.Make (Char)

type trie = Node of trie CharMap.t
let tl s = List.of_seq (String.to_seq s);;
let fl l = String.of_seq (List.to_seq l);;

let trie_vide = Node CharMap.empty;;
(* let Node dico_vide = trie_vide *)
let trie_mot_vide = Node (CharMap.add '$' trie_vide CharMap.empty);;

let trie_singleton c = Node (CharMap.add c trie_mot_vide CharMap.empty);;

let rec trie_contient m (Node dico) = 
  match m with
  |[] -> CharMap.mem '$' dico
  |l :: m' -> CharMap.mem l dico && trie_contient m' (CharMap.find l dico);;

let rec trie_ajouter m (Node dico) =
  match m with
  |[] -> Node (CharMap.add '$' (Node dico) dico)
  |l :: m' -> Node (CharMap.add l (trie_ajouter m' (Node dico)) dico);;

let pouleto = tl "pouleto";;
let patato = tl "patato";;
let t = trie_ajouter pouleto trie_vide;;
let t = trie_ajouter patato t;;
let b = trie_contient pouleto t && trie_contient patato t;;

let rec liste_mots (Node dico) = 
  let liste = ref [] in 
  CharMap.iter (fun key value -> 
    if key <> '$' then liste := key :: !liste ; 
                       liste := List.append !liste (liste_mots value)) dico;
  !liste
;;

let rec liste_mots_2 (Node dico) = 
  let liste = ref [] in 
  CharMap.iter (fun key value -> 
    if key = '$' then ())
  !liste
  ;;

let l = liste_mots t;;
print_int (List.length l);;