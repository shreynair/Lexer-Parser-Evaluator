open LccTypes 

let match_token (toks : 'a list) (tok : 'a) : 'a list =
  match toks with
  | [] -> raise (Failure("List was empty"))
  | h::t when h = tok -> t
  | h::_ -> raise (Failure( 
      Printf.sprintf "Token passed in does not match first token in list"
    ))

let lookahead toks = match toks with
   h::t -> h
  | _ -> raise (Failure("Empty input to lookahead"))


(* Write your code below *)



let parse_lambda toks = 
  let rec parse_lambda_helper toks =
    match lookahead toks with
      | Lambda_Var x ->
        let t = match_token toks (Lambda_Var x) in
        (t, Var x)
      | Lambda_LParen ->
        let t = match_token toks Lambda_LParen in
        (match lookahead t with
          | Lambda_Lambda ->
            let t' = match_token t Lambda_Lambda in
            (match parse_lambda_helper t' with
              | (t'', Var x) ->
                let t''' = match_token t'' Lambda_Dot in
                let (t'''', y) = parse_lambda_helper t''' in
                let t''''' = match_token t'''' Lambda_RParen in
                (t''''', Func (x, y))
              | _ -> raise (Failure "parsing failed"))
          | _ ->
            let (t', x) = parse_lambda_helper t in
            let (t'', y) = parse_lambda_helper t' in
            let t''' = match_token t'' Lambda_RParen in
            (t''', Application (x, y)))
      | Lambda_EOF -> raise (Failure "List was empty")
      | _ -> raise (Failure "parsing failed")
  in

  let (t, exp) = parse_lambda_helper toks in
  if t <> [Lambda_EOF] then
    raise (Failure "parsing failed")
  else
    exp

  

let rec parse_engl toks = 
  let (t, exp) = parse_engl_junc toks in
  if t <> [Engl_EOF] then
    raise (Failure "parsing failed")
  else
    exp
    
and parse_engl_junc toks  =
  let (t,b) = parse_engl_bool toks in
  match lookahead t with
    | Engl_And ->
      let t' = match_token t (Engl_And) in
      let (t'', j) = parse_engl_junc t' in
      (t'', And(b, j))
    | Engl_Or ->
      let t' = match_token t (Engl_Or) in
      let (t'', j) = parse_engl_junc t' in
      (t'', Or(b, j))
    | _ -> t,b

and parse_engl_bool toks =
  match lookahead toks with
    | Engl_True ->
      let t = match_token toks (Engl_True) in
      (t, Bool true)
    | Engl_False ->
      let t = match_token toks (Engl_False) in
      (t, Bool false)
    | Engl_LParen ->
      let t = match_token toks (Engl_LParen) in
      let (t',c) = parse_engl_junc t in
      let t'' = match_token t' (Engl_RParen) in
      (t'', c)
    | Engl_Not ->
      let t = match_token toks (Engl_Not) in
      let (t', c) = parse_engl_junc t in
      (t', Not c)
    | Engl_If ->
      let t = match_token toks (Engl_If) in
      let (t', c) = parse_engl_junc t in
      let t'' = match_token t' (Engl_Then) in
      let (t''', d) = parse_engl_junc t'' in
      let t'''' = match_token t''' (Engl_Else) in
      let (t''''', e) = parse_engl_junc t'''' in
      (t''''', If(c,d,e))
    | Engl_EOF -> raise (Failure "List was empty")
    | _ -> raise (Failure "parsing failed")
