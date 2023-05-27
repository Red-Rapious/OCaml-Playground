let rec merge l1 l2 = 
  match (l1, l2) with
  | ([], _) -> l2
  | (_, []) -> l1
  | (h1::t1, h2::t2) -> if h1 < h2 then h1 :: merge t1 l2 else h2 :: merge l1 t2
;;

let rec split x y z =
  match x with
  | [] -> (y, z)
  | e :: x' -> split x' (e::z) y
;;

let rec tri_fusion l = 
  match l with
  | ([] | _::[]) -> l
  | _ -> let l1, l2 = split l [] [] in merge (tri_fusion l1) (tri_fusion l2)
;;