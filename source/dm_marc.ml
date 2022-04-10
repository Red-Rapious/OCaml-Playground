(* 1 *)
let rec termes f u0 n =
  if n = 0 then []
  else u0 :: termes f (f u0) (n - 1);;
(* 7 *)
let calculer_nu f u0 =
  let rec auxi_nu u_k u_2k nu =
    if nu <> 0 && (u_k = u_2k) then (nu, u_k)
    else auxi_nu (f u_k) (f (f u_2k)) (nu + 1)
  in auxi_nu u0 u0 0;;
(* 8 *)
let calculer_mu f u0 u_nu =
  let rec auxi_mu u_k u_nuplusk mu =
    if u_k = u_nuplusk then (mu, u_k)
    else auxi_mu (f u_k) (f u_nuplusk) (mu + 1)
  in auxi_mu u0 u_nu 0;;
(* 9 *)
let calculer_lambda f u_mu =
  let rec auxi_lambda u_muplusk lambda =
    if lambda <> 0 && u_mu = u_muplusk then lambda
    else auxi_lambda (f u_muplusk) (lambda + 1)
  in auxi_lambda u_mu 0;;
(* 10 *)
let cycle f u0 =
  let paire_mu = calculer_mu f u0 (snd (calculer_nu f u0)) in
  (termes f u0 (fst paire_mu), termes f (snd paire_mu) (calculer_lambda f (snd paire_mu)));;
let cycle_explicite f u0 =
  let u_nu = snd (calculer_nu f u0) in
  let (mu, u_mu) = calculer_mu f u0 u_nu in
  let lambda = calculer_lambda f u_mu in
  (termes f u0 mu, termes f u_mu lambda);;
(* II *)
let modulo k n = fun x -> (k * x) mod n;;

let sy n =
  let rec auxi n = if n mod 2 = 0 then auxi (n / 2) else n
  in auxi (if n mod 2 = 0 then n else 3 * n + 1);;
(* 12 *)
let rec p n =
  if n / 10 = 0 then n * n
  else (n mod 10) * (n mod 10) + p (n / 10);;
(* 14 *)
let u b = fun a -> ((10 * a) mod b);; (* cycle (fun x -> (10 * (u a) x) / b) b;; *) 
(* cycle (fun x -> ((10 * x) mod 28)) 19*)

let decomp base a b =
  let l = cycle (fun x -> ((base * x) mod b)) a and f x = (base * x) / b in
  (List.map f (fst l), List.map f (snd l));;
let decomp2 a b = decomp 10 a b;;
(* 15 *)
let longueur a b = List.length (snd (decomp2 a b));;
(* 16 *)
let maxi borne = (*4215*)
  let rec auxi b record acc =
    let l = longueur 1 b and b1 = b + 1 in
    if b > borne then acc
    else if l > record then auxi b1 l [b]
    else if l = record then auxi b1 l (b :: acc)
    else auxi b1 record acc
  in auxi 1 0 [];;

let maxi2 borne = (*4216*)
  let rec auxi b record acc =
    let l = longueur 1 b and b1 = b + 1 in
    if l > record then auxi b1 l b
    else if b >= borne then acc
    else auxi b1 record acc
  in auxi 1 0 0;;

let maxi3 borne = (*4216*)
  let paire = ref (0, 0) in (*record, acc*)
  for b = 1 to borne do
    let l = longueur 1 b in
    if l > fst !paire then
      paire := (l, b)
  done;
  snd !paire;;

