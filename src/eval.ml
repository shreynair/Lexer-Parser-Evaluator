open LccTypes 

let cntr = ref (-1)

let fresh () =
  cntr := !cntr + 1 ;
  !cntr

let rec lookup env var = match env with 
  [] -> None
  |(v,e)::t -> if v = var then e else lookup t var

let rec alpha_convert e = failwith ("Unimplemented; Optional")

let isalpha e1 e2 = failwith ("Unimplemented")

let rec reduce env e = failwith ("Unimplemented")

let rec laze env e = failwith ("Unimplemented") 

let rec eager env e = failwith ("Unimplemented")

let rec convert tree = failwith ("Unimplemented")

let rec readable tree = failwith ("Unimplemented")
