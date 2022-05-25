let rec est_present l e = 
  match l with
  | [] -> false
  | a :: l' -> e = a || est_present l' e;;

let rec ieme l n =
  match n, l with 
  | _, [] -> None
  | 0, a :: l' -> Some a
  | _, _ :: l' -> ieme l' (n-1)
;;

let rec maximum l =
  match l with
  | [] -> - 10000000
  | a :: l' -> let m = maximum l' in if a > m then a else m
;;

let rec concat l1 l2 =
  match l1, l2 with
  | [], _ -> l2
  | a :: l', l2 -> a :: (concat l' l2);;

let rec miroir l =
  match l with 
  | [] -> []
  | a :: l' -> concat (miroir l') [a];;

let fac_ter n =
  let rec aux n acc = 
  match n with
  | 0 -> 1
  | _ -> aux (n-1) (acc*n)
in aux n 1;;