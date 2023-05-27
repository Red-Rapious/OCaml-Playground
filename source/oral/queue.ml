type 'a queue = {tab : 'a array ; mutable deb : int ; mutable fin : int ; mutable vide : bool};;

let vide cap el_init = {tab = Array.make cap el_init ; deb = 0 ; fin = 0 ; vide = true};;

let est_vide q = q.vide;;

let insertion q el = 
  if q.fin mod Array.length q.tab = q.deb && (not q.vide) then failwith "queue pleine"
  else begin
    q.tab.(q.fin) <- el ;
    q.fin <- q.fin + 1 mod Array.length q.tab ;
    q.vide <- false
  end
;;

let suppression q el =
  if q.vide then failwith "queue vide"
  else begin
    q.deb <- q.deb + 1 mod Array.length q.tab ;
    if q.fin = q.deb then q.vide <- true
  end
;;