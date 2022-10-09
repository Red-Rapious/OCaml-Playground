module UF = struct 
  type t = int array

  let init n = Array.init n (fun i -> i)
  let find t a = t.(a)
  let union t a b = 
    let fa = find t a
    and fb = find t b in 
    if fa = fb then false
    else begin 
      for i = 0 to Array.length t - 1 do 
        if t.(i) = fa then t.(i) <- fb 
      done;
      true
    end
    ;;
end