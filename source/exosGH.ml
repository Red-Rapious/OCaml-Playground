(* Exercice 1 - opérateurs *)

(* 1 *)
let x = 42*10;;

(* 2 *)
let y = 3.14 /. 2.0;;

(* 3 *)
let z = 4.2 ** 7.;;
(*print_float z;; *)

let rec puissance a b = match b with
  |0 -> 1
  |1 -> a
  |_ -> a*(puissance a (b-1));;

let z = puissance 5 5;;
(*print_int z;; *)

(* Exercice 2 - égailté *)
(* 1 *)
let b = true;;

(* Exercice 5 - double fun*)
let double n = 2*n;;

(* Exercice 6 - more fun *)
let cube n = n**3.;;

let signe n = if n>0 then 1 else (if n<0 then -1 else 0);;
(*print_int (signe ~-10);;*)

let aire_cercle r = (3.14 *. r) *. r;;

(* Exercice 7 *)
let root_square_mean x y = ((x*.x +. y*.y) /. 2.0) ** 0.5;;

(* Exercice 8 *)
let rec fibonacci n = match n with
  | 0 | 1 -> 1
  | n -> ((fibonacci (n-1)) + (fibonacci (n-2)));;

(* Exercice 9 *)
let (+/.) x y = x+.y /. 2.;;
print_float (2.0 +/. 7.5);;