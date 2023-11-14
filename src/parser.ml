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

let parse_lambda toks = failwith ("Unimplemented")

let parse_engl toks = failwith ("Unimplemented")
