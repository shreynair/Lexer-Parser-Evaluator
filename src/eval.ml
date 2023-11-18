open LccTypes 
open String
open Str

let cntr = ref (-1)

let fresh () =
  cntr := !cntr + 1 ;
  !cntr

let rec lookup env var = match env with 
  [] -> None
  |(v,e)::t -> if v = var then e else lookup t var

let rec alpha_convert e = 
  cntr := -1 ;
  let rec alpha_convert_helper env e =
    match e with
    | Var x ->
      let sub = lookup env x in
      (match sub with
      | Some i -> i
      | None -> Var x)
    | Func (x, y) ->
      let alpha = "v" ^ (string_of_int (fresh ())) in
      Func (alpha, alpha_convert_helper ((x, Some (Var alpha)) :: env) y)
    | Application (x, y) ->
      let alpha_x = alpha_convert_helper env x in
      let alpha_y = alpha_convert_helper env y in
      Application (alpha_x, alpha_y)
  in

  alpha_convert_helper [] e

let isalpha e1 e2 = 
  let alpha_e1 = alpha_convert e1 in
  cntr := -1 ;
  let alpha_e2 = alpha_convert e2 in
  cntr := -1 ;

  let rec tokenizer lst e =
    match e with
      | Var x -> lst @ [Lambda_Var x]
      | Func (x,y) -> tokenizer (lst @ [Lambda_Lambda] @ [Lambda_Var x] @ [Lambda_Dot] ) y
      | Application (x,y) -> tokenizer (tokenizer lst x) y
  in

  let toks1 = tokenizer [] alpha_e1 in
  let toks2 = tokenizer [] alpha_e2 in

  let rec tok_compare toks1 toks2 =
    match (toks1, toks2) with
      | ([],[]) -> true
      | (h1 :: t1,h2 :: t2) when h1 = h2 -> tok_compare t1 t2
      | _ -> false
  in

  tok_compare toks1 toks2

let rec reduce env e = 
  cntr := -1 ;
  let alpha_e = alpha_convert e in
  match alpha_e with
    | Var a -> 
      (match (lookup env a) with
        | None -> Var a
        | Some b -> b)
    | Func (a,b) ->  
      (match (lookup env a) with
        | None -> Func(a, reduce env b)
        | Some _ -> reduce env b)
    | Application (Func(a,b), c) -> 
      let c_reduce = reduce env c in
      reduce ((a, Some c_reduce)::env) (Func(a,b))
    | Application (a,b) -> Application (reduce env a, reduce env b)


let rec laze env e = 
  cntr := -1 ;
  let alpha_e = alpha_convert e in
  let rec laze_helper env e =
    match e with
    | Var a -> 
      (match (lookup env a) with
        | None -> Var a
        | Some b -> b)
    | Func (a,b) -> 
      (match (lookup env a) with
        | None -> Func(a, laze_helper env b)
        | Some _ -> laze_helper env b)
    | Application (Func(a,b), c) -> laze_helper ((a, Some c) :: env) (Func(a,b))
    | Application (a,b) when (isalpha (laze_helper env a) a) -> Application (a, laze_helper env b)
    | Application (a,b) when not (isalpha (laze_helper env a) a) -> Application (laze_helper env a, b)
    (*
    | Application (Application(Func(a,b), c), Application(Func(d,e), f)) -> laze_helper env (Application(Func(d,e), f))
    | Application (Application (a,b), c) -> Application (laze_helper env (Application (a,b)), c)
    | Application (a, Application (b,c)) -> Application (a, laze_helper env (Application (b,c)))
    *)
    (* "((Lx.(((Ly.y) a) x)) b)" *)
    in laze_helper env e
  

let rec eager env e = 
  cntr := -1 ;
  let alpha_e = alpha_convert e in
  match alpha_e with
    | Var a -> 
      (match (lookup env a) with
        | None -> Var a
        | Some b -> b)
    | Func (a,b) -> 
      (match (lookup env a) with
        | None -> Func(a, eager env b)
        | Some _ -> eager env b)
    | Application (Func (a,b), c) when (isalpha (eager env c) c) -> eager ((a, Some c) :: env) (Func (a,b))
    | Application (Func (a,b), c) when not(isalpha (eager env c) c) -> Application (Func (a,b), eager env c)
    | Application (a, Application (b,c)) -> Application (a, eager env (Application (b,c)))
    | Application (a,b) -> Application (eager env a, eager env b)
    
    
let rec convert tree = 
  match tree with
    | Bool true -> "(Lx.(Ly.x))"
    | Bool false -> "(Lx.(Ly.y))"
    | If (a,b,c) -> "((" ^ convert a ^ " " ^ convert b ^ ") " ^ convert c ^ ")"
    | Not a -> "((Lx.((x (Lx.(Ly.y))) (Lx.(Ly.x)))) " ^ convert a ^ ")"
    | And (a,b) -> "(((Lx.(Ly.((x y) (Lx.(Ly.y))))) " ^ convert a ^ ") " ^ convert b ^ ")"
    | Or (a,b) -> "(((Lx.(Ly.((x (Lx.(Ly.x))) y))) " ^ convert a ^ ") " ^ convert b ^ ")"
  

let rec readable tree = 
  match tree with
  | Func(v0, Func(v1, Var v0')) when v0 = v0' -> "true"
  | Func(v0, Func(v1, Var v1')) when v1 = v1' -> "false"
  | Application (Application (Func (v0, Func (v1, Application (Application (Var v0', Var v1'), Func (v2, Func (v3, Var v3'))))), a), b) when v0 = v0' && v1 = v1' && v3 = v3' -> 
    "(" ^ (readable a) ^ " and " ^ (readable b) ^ ")"
  | Application(Application(Func (v0 ,Func (v1 ,Application (Application (Var v0', Func (v2, Func (v3, Var v2'))),Var v1'))), a), b) when v0 = v0' && v1 = v1' && v2 = v2' -> 
    "(" ^ (readable a) ^ " or " ^ (readable b) ^ ")"
  | Application(Application(a, b), c) -> "(if " ^ (readable a) ^ " then " ^ (readable b) ^ " else " ^ (readable c) ^ ")"
  | Application(Func(v0, Application(Application(Var v0', Func(v1, Func(v2, Var v2'))), Func(v3, Func(v4, Var v3')))), a) when v0 = v0' && v2 = v2' && v3 = v3' -> 
    "(not " ^ (readable a) ^ ")"
  
  
      