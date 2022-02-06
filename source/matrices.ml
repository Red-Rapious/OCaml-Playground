let matrice = [|[|0; 1|]; [|1 ; 1|]|];;
let det2 mat = mat.(0).(0) * mat.(1).(1) - mat.(0).(1)*mat.(1).(0);;
