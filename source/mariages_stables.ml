(* Q13 : hommes célibataires : référence liste 'hommes_celibataires' *)
(* Q14 : liste des femmes à qui il a déjà fait une proposition : liste des femmes dans l'ordre
   de préférence, celles n'étant plus dans la liste ayant déjà été proposées *)
(* (Q15 : utilisation d'un tableau de taille n contenant un type option : None si elle n'est pas
   fiancées, le fiancé si elle l'est) *)
(* Q15 : autre option, utiliser deux tableaux, un tableau des fiancés, celui retourné à la fin,
   et un tableau de booléens pour savoir si une femme est fiancée *)
(* Q16 : on créé un tableau de préférence des femmes, un tableau de tableaux, qui contient pour chaque
   femme et pour chaque homme son classement dans les préférences de la femme *)

let gale_shapley pref_h pref_f =
   let n = Array.length pref_h in
   let propositions = Array.copy pref_h in 
   let couplage = Array.make n 0 in 
   let hommes_celibataires = ref (List.init n (fun i -> i)) in
   let est_fiancee = Array.make n false in
   let classement_h = Array.make n (Array.make n 0) in

   for f = 0 to n-1 do (* pour chaque femme, on prétraite *)
      let pref = ref pref_f.(f) in (* le classement par ordre de préférence des hommes pour la femme f *)
      for rang = 0 to n-1 do (* pour chaque position du classement *)
         classement_h.(f).(List.hd !pref) <- rang ; (* on associe le rang à l'homme choisi *)
         pref := List.tl !pref (* on enlève l'homme du classement *)
      done
   done ;

   while List.length !hommes_celibataires <> 0 do (* tant qu'il reste des célibataires *)
      let h = List.hd !hommes_celibataires in  (* on prend un homme quelconque *)
      let f = List.hd propositions.(h) in (* on choisit la femme non proposée qu'il préfère *)
      
      if not est_fiancee.(f) then begin  (* si elle n'est pas encore fiancée *)
         couplage.(f) <- h ; (* il se fiance à elle *)
         hommes_celibataires := List.tl !hommes_celibataires ; (* il n'est donc plus célibataire *)
         est_fiancee.(f) <- true (* elle est donc fiancée *)
      end
      
      else begin (* elle est déjà fiancée *)
         let h' = couplage.(f) in (* on fait venir son fiancé *)
         if classement_h.(f).(h) < classement_h.(f).(h') then begin(* si le nouveau est plus bg *)
            couplage.(f) <- h ; (* elle le quite pour le nouveau boug *)
            hommes_celibataires := h' :: !hommes_celibataires (* l'ancien redevient célibataire *)
         end
      end ;

      propositions.(h) <- List.tl propositions.(h) (* il a proposé *)
   done ;
   couplage
;;

let pref_h = [|
   [1 ; 0 ; 2];
   [0 ; 2 ; 1];
   [0 ; 1 ; 2]
|] ;;

let pref_f = [|
   [0 ; 2 ; 1];
   [2 ; 0 ; 1];
   [2 ; 1 ; 0]
|];;

gale_shapley pref_h pref_f;;