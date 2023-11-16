open LccTypes 

let cntr = ref (-1)

let fresh () =
  cntr := !cntr + 1 ;
  !cntr

let rec lookup env var = match env with 
  [] -> None
  |(v,e)::t -> if v = var then e else lookup t var

let rec alpha_convert e = failwith ("Unimplemented")
  (* let rec alpha_convert_helper env e =
    match e with 
      | Var x ->
        let sub = lookup env x in
        match sub with
          | Some i -> Var i
          | None -> Var x
      | Func (x,y) -> 
        let a = "v" ^ (fresh ()) in
        Func (a, alpha_convert_helper ((x, Some (Var a)) :: env) y )

      | Application x,y -> Application(alpha_convert_helper env x, alpha_convert_helper env y)
  in

  alpha_convert_helper [] e
  *)
let isalpha e1 e2 = failwith ("Unimplemented")

let rec reduce env e = failwith ("Unimplemented")

let rec laze env e = failwith ("Unimplemented") 

let rec eager env e = failwith ("Unimplemented")

let rec convert tree = failwith ("Unimplemented")

let rec readable tree = failwith ("Unimplemented")
