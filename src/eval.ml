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
  match alpha_e with
    | Var a -> 
      (match (lookup env a) with
        | None -> Var a
        | Some b -> b)
    | Func (a,b) -> 
      (match (lookup env a) with
        | None -> Func(a, laze env b)
        | Some _ -> laze env b)
    | Application (Func(a,b), c) -> laze ((a, Some c) :: env) (Func(a,b))
    | Application (a, Func(b,c)) -> Application (a, laze env (Func(b,c)))
    | Application (Application (a,b), c) -> Application (laze env (Application (a,b)), c)
    | Application (a, Application (b,c)) -> Application (a, laze env (Application (b,c)))
    | Application (a,b) -> Application (laze env a, laze env b)

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
    | Application (a, Application (b,c)) -> Application (a, eager env (Application (b,c)))
    | Application (Func (a,b), c) -> eager ((a, Some c) :: env) (Func (a,b))
    | Application (a,b) -> Application (eager env a, eager env b)
    
    
let rec convert tree = 
  match tree with
    | Bool true -> "(Lx.(Ly.x))"
    | Bool false -> "(Lx.(Ly.y))"
    | If (a,b,c) -> "((" ^ (convert a) ^ " " ^ (convert b) ^ ") " ^ (convert c) ^ ")"
    | Not a -> "((Lx.((x (Lx.(Ly.y))) (Lx.(Ly.x)))) " ^ (convert a) ^ ")"
    | And (a,b) -> "(((Lx.(Ly.((x y) (Lx.(Ly.y))))) " ^ (convert a) ^ ") " ^ (convert b) ^ ")"
    | Or (a,b) -> "(((Lx.(Ly.((x (Lx.(Ly.x))) y))) " ^ (convert a) ^ ") " ^ (convert b) ^ ")"
  

let rec readable tree = 
  match tree with
  | Func(v0, Func(v1, Var v0')) when v0 = v0' -> "true"
  | Func(v0, Func(v1, Var v1')) when v1 = v1' -> "false"
  | Application(Application(a, b), c) -> "(if " ^ (readable a) ^ " then " ^ (readable b) ^ " else " ^ (readable c) ^ ")"
  | Application(Func(v0, Application(Application(Var v0', Func(v1, Func(v2, Var v2'))), Func(v3, Func(v4, Var v3')))), a) -> 
    if (v0 = v0' && v2 = v2' && v3 = v3') then
      "(not " ^ (readable a) ^ ")"
    else 
      ""
  | Application (Application (Func (v0, Func (v1, Application (Application (Var v0', Var v1'), Func (v2, Func (v3, Var v3'))))), a), b) -> 
    (*Application(Application(Func ("v0",Func ("v1",Application (Application (Var "v0", Var "v1"),Func ("v2", Func ("v3", Var "v3"))))), Var "a"),Var "b")*)
    (*Application(Application(Func ("v0",Func ("v1",Application (Application (Var "v0", Var "v1"),Func ("v2", Func ("v3", Var "v3"))))),Func ("v4", Func ("v5", Var "v4"))),Func ("v6", Func ("v7", Var "v7")))*)
    if (v0 = v0' && v1 = v1' && v3 = v3') then
      "(" ^ (readable a) ^ " and " ^ (readable b) ^ ")"
    else 
      ""
  | Application(Application(Func (v0 ,Func (v1 ,Application (Application (Var v0', Func (v2, Func (v3, Var v2'))),Var v1'))), a), b) -> 
    (*Application(Application(Func ("v0",Func ("v1",Application (Application (Var "v0", Func ("v2", Func ("v3", Var "v2"))),Var "v1"))),Var "a"),Var "b")*)
    if (v0 = v0' && v1 = v1' && v2 = v2') then
      "(" ^ (readable a) ^ " or " ^ (readable b) ^ ")"
    else
      ""
      