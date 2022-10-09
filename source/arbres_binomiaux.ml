type 'a arbre = A of int * 'a * 'a arbre list

let lier a1 a2 =
  let A(r1, v1, f1) = a1
  and A(r2, v2, f2) = a2 in assert(r1 = r2);
  if v1 < v2 then 
    A(r1 + 1, v1, a2::f1)
  else
    A(r2 + 1, v2, a1::f2)
;;

let rec ajout p l =
  match l with
  | [] -> [p]
  | a :: l' when a = p -> ajout (p+1) l'
  | a :: l' when a > p -> p :: l
;;

let rec addition l1 l2 =
  match (l1, l2) with
  | [], _ -> l2
  | _, [] -> l1
  | a :: l1', b :: l2' when a < b -> a :: (addition l1' l2)
  | a :: l1', b :: l2' when a > b -> b :: (addition l1 l2')
  | a :: l1', b :: l2' when a = b -> addition (ajout b l1) l2'
;;

addition [1 ; 2 ; 7] [1 ; 4 ; 6];;

type 'a tas = 'a arbre list

let rec minimum = function
| [] -> failwith "minimum : tas vide"
| A(_, a, _) :: [] -> a
| A(_, a, _) :: l -> let m = minimum l in if a > m then m else a;;

(* Tentative de rec term *)
(*let minimum tas = 
  let rec aux min = function
  | [] -> failwith "minimum : tas vide"
  | a :: [] -> if a < min then a else min
  | a :: l -> aux (if a < min then a else min) l
in aux tas 
;;*)

let rec ajout_arbre a l =
  let A(r1, _, _) = a in
  match l with
  | [] -> [a]
  | A (r2, n, f) :: l' when r1 = r2 -> ajout_arbre (lier a (A(r2, n, f))) l'
  | A (r2, n, f) :: l' when r1 < r2 -> a :: l
;;

let rec fusion t1 t2 =
  match (t1, t2) with
  | [], _ -> t2
  | _, [] -> t1
  | a :: t1', b :: t2' when a < b -> a :: (fusion t1' t2)
  | a :: t1', b :: t2' when a > b -> b :: (fusion t1 t2')
  | a :: t1', b :: t2' when a = b -> fusion (ajout_arbre b t1) t2'
;;

let inserer x a = ajout_arbre (A(0, x, [])) a;;

let extraire_minimum tas = 
  (* Extraction du minimum, 'min', et de l'arbre le contenant, 'tarbre'*)
  let rec tarbre_et_min = function
  | [] -> failwith "extraire_minimum : tas vide"
  | A(r, a, l) :: [] -> a, l, []
  | A(r, a, l) :: l' -> let min, enf, t = tarbre_et_min l' in 
    if a > min 
      then min, enf, A(r, a, l) :: t 
    else 
      a, l, l'
  in 
  let min, tarbre, t = tarbre_et_min tas in
  min, fusion t (List.rev tarbre)
;;