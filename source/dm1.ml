let termes2 f u0 n =
	let l = ref [u0] in
	for i = 1 to n-1 do
		l := f (List.hd !l) :: !l
	done;
	List.rev !l;;


let rec termes1 f u0 n =
  if n = 0 then []
  else u0 :: termes1 f (f u0) (n-1);;

let termes3 f u0 n =
  let rec aux f u0 n acc =
    if n = 0 then acc
    else aux f (f u0) (n-1) (u0 :: acc) 
  in List.rev (aux f u0 n []);;

let calculer_nu f u0 =
	let rec aux f k uk u2k =
		if uk = u2k then k, uk
		else aux f (k+1) (f uk) (f(f u2k));
	in aux f 1 (f u0) (f (f u0));;



let calculer_mu f u0 uNu =
    let rec aux f k uk ukNu =
      if uk = ukNu then k, uk
      else aux f (k+1) (f uk) (f ukNu);
    in aux f 0 u0 uNu;;


let calculer_lambda f uMu =
	let rec aux f uMu n un =
		if uMu = un then n
		else aux f uMu (n+1) (f un);
	in aux f uMu 1 (f uMu);;

  

let cycle f u0 =
	let nu = calculer_nu f u0
in let mu = calculer_mu f u0 (snd nu)
in let lambda = calculer_lambda f (snd mu) in
	
	termes1 f u0 (fst mu), 
	termes1 f (snd mu) lambda;;

  


let prabekhar n =
   let p = ref 0 and i = ref n in
   while !i != 0 do
   	p := !p + (!i mod 10)*(!i mod 10);
   	i := !i/10
   done;
   !p;;

let decomp a b = 
	let v x = 10*x / b 
	and c = cycle (fun x -> (10*x) mod b) a
	in List.map v (fst c), List.map v (snd c) ;;

	let plus_grande_partie_cyclique =
    let m = ref (0, 0) and l = ref 0 in
    for i = 1 to 10000 do
        l := List.length (snd (decomp 1 i));
        if !l > (fst !m) then m := (i, !l) else ()
    done;
    fst !m;; 