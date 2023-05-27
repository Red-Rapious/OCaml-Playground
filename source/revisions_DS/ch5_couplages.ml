let rec existe p = function
| [] -> None 
| a :: l -> if p a then Some a else existe p l
;;

type biparti = {
  u : int list array ;
  v : int list array
}

let couplage_maximal g = 
  let nu = Array.length g.u in 
  let nv = Array.length g.v in 

  let vus = Array.make nu false in 
  let couplage = Array.make nv None in 

  let rec visite_sommet s = 
    if vus.(s) then 
      false 
    else begin 
      vus.(s) <- true ;
      match existe bon_voisin g.u.(s) with
      | None -> false 
      | Some v -> couplage.(v) <- Some s ; true
    end 
  
  and bon_voisin v = 
    match couplage.(v) with
    | None -> true 
    | Some s' -> visite_sommet s'
  in 

  for s = 0 to nu-1 do 
    for i = 0 to nu-1 do 
      vus.(i) <- false 
    done ; 
    ignore (visite_sommet s)
  done ; 
  couplage
;;
