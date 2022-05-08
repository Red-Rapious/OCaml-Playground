let partitionner2 l =
  let rec auxi l1 l2 n =
    if n = 0 then (l1, l2) else
      match l2 with
      | a :: l2' -> auxi (a :: l1) l2' (n - 1)
  in let (l1, l2) = auxi [] l (List.length l / 2)
  in (List.rev l1, l2);;

let rec fusionner2 l1 l2 =
  match (l1, l2) with
  | ([], _) -> (l2, 0)
  | (_, []) -> (l1, 0)
  | (a :: l1', b :: l2') -> if a <= b
      then let (l, n) = fusionner2 l1' l2 in (a :: l, n)
      else let (l, n) = fusionner2 l1 l2'
        in (b :: l, n + List.length l1);;

let rec tri_fusion2 l =
  if List.length l > 1 then
    let (l1, l2) = partitionner2 l in
    let ((l1', n1), (l2', n2)) = (tri_fusion2 l1, tri_fusion2 l2) in
    let (l', n) = fusionner2 l1' l2' in
    (l', n1 + n2 + n)
  else (l, 0);;

print_int (snd (tri_fusion2  [[2; 4; 8; 16; 15; 13; 9; 1]]));;