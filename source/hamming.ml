let rec hamming n = 
  if n = 2 || n = 3 || n = 5 then true
  else 
    if n mod 2 = 0 then (hamming (n/2))
    else if n mod 3 = 0 then (hamming (n/3))
    else if n mod 5 = 0 then (hamming (n/5))
    else false;;

print_int (Bool.to_int (hamming 9001));;

let rec suite_recurrente (f, n, a) =
  if n=0 then a
  else f (suite_recurrente (f, n-1, a));;

let f = fun x ->2*x;;
print_int (suite_recurrente (f, 10, 1));;


let rec hamming2 n =
  if n = 3 || n = 2 || n = 5 then true else
  if n mod 2 = 0 then hamming2 (n/2)
  else if n mod 3 = 0 then hamming2 (n/3)
  else if n mod 5 = 0 then hamming2 (n/5)
  else false;;