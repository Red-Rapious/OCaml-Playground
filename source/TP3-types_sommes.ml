type ternaire =
| Vrai
| Faux
| Inconnu;;

let negation t = match t with
| Vrai -> Faux
| Faux -> Vrai
| Inconnu -> Inconnu;;

let conjonction t1 t2 =
  match (t1, t2) with
  | (Vrai, Vrai) -> Vrai
  | (Faux, _) -> Faux
  | (_, Faux) -> Faux
  | (_, _) -> Inconnu;;

let rec assoc_opt cle liste =
  match liste with
  | [] -> None
  | a :: l' -> if (fst a) = cle then Some (snd a) else assoc_opt cle l';;

let premiere_occurence t a =
  let p = ref None in
  for i = 0 to Array.length t - 1 do
    if t.(i) = a && !p = None then p := Some i
  done;
  !p;;

let premiere_occurence_2 t a =
  let i = ref 0 and p = ref None in
  while !i < Array.length t && !p = None do
    if t.(!i) = a then p := Some !i ; incr i
    done;
  !p;;

let appli_option f x = 
  match x with 
  | None -> None
  | Some a -> Some (f a);;

let rec filtre_option l = 
  match l with 
  | [] -> []
  | a :: l' -> if a = None then l' else a :: filtre_option l';;

(*let rec filtre_option_term l =
  let rec aux l acc = 
    match l with
    | [] -> acc
    | a :: l' -> if a = None then l' else aux l' a::acc
  in aux l [];;*)

type expr =
| Consti of int
| Constf of float
| Var of string
| Add of expr*expr
| Sub of expr*expr
| Mult of expr*expr
| Div of expr*expr
| Ln of expr
| Exp of expr
| Cos of expr
| Sin of expr
;;

let rec subst e1 v e2 =
  match e1 with
  | Var a -> if a = v then e2 else Var a
  | Add (a, b) -> Add ((subst a v e2), (subst b v e2))
  | Sub (a, b) -> Sub ((subst a v e2), (subst b v e2))
  | Mult (a, b) -> Mult ((subst a v e2), (subst b v e2))
  | Div (a, b) -> Div ((subst a v e2), (subst b v e2))
  | a -> a
;;

let rec eval e =
  match e with
  | Add (Consti a, Consti b) -> Consti (a+b)
  | Sub (Consti a, Consti b) -> Consti (a-b)
  | Mult (Consti a, Consti b) -> Consti (a*b)
  | Div (Consti a, Consti b) -> if b=0 then failwith "Erreur : division par 0" else Consti (a/b)
  
  | Add (Constf a, Constf b) -> Constf (a+.b)
  | Sub (Constf a, Constf b) -> Constf (a-.b)
  | Mult (Constf a, Constf b) -> Constf (a*.b)
  | Div (Constf a, Constf b) -> if b=0.0 then failwith "Erreur : division par 0" else Constf (a/.b)

  | Add (Ln a, Ln b) -> Ln(Mult(a, b))
  | Sub (Ln a, Ln b) -> Ln(Div(a, b))
  | Mult (Exp a, Exp b) -> Exp(Add(a, b))
  | Div (Exp a, Exp b) -> Exp(Sub(a, b))

  | Add (a, b) -> Add (eval a, eval b)
  | Sub (a, b) -> Sub (eval a, eval b)
  | Mult (a, b) -> Mult (eval a, eval b)
  | Div (a, b) -> Div (eval a, eval b)
  | a -> a
;;

let rec derivation e v =
  match e with
  | Add(a, b) -> Add(derivation a v, derivation b v)
  | Sub(a, b) -> Sub(derivation a v, derivation b v)
  | Mult(a, b) -> Add(Mult(derivation a v, b),  Mult(a, derivation b v))
  | Div(a, b) -> Div(Sub(Mult(derivation a v, b),  Mult(a, derivation b v)), Mult(b, b))
  | Consti a -> Consti 0
  | Constf a -> Constf 0.0
  | Var a -> if a = v then Consti 1 else Consti 0
  | Ln a -> Mult(Div(Consti 1, a), derivation a v)
  | Exp a -> Mult(Exp a, derivation a v)
  | Cos a -> Sub(Consti 0, Mult(Sin a, derivation a v))
  | Sin a -> Mult(Cos a, derivation a v)
;;

let der_et_eval e v = eval (derivation e v);;