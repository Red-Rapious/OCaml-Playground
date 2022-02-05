let rec factorielle = function
  |0|1 -> 1
  |n -> n*factorielle(n-1);;

print_endline "Hello World!";;
(* let string = input_line stdin;; *)
let x = factorielle 10;;
print_int x;;