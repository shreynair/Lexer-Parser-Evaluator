open LccTypes
open String
open Str
open List

let lex_lambda input = 
  let rec string_to_list input =
    match (String.length input) with
      | 0 -> []
      | _ -> input.[0] :: string_to_list (String.sub input 1 (String.length input - 1))
  in

  let lst = string_to_list input in

  let rec tokenizer lst =
    let regex_var = regexp "[a-z]" in
    match lst with
      | [] -> [Lambda_EOF]
      | '(' :: tail -> Lambda_LParen :: (tokenizer tail)
      | ')' :: tail -> Lambda_RParen :: (tokenizer tail)
      | 'L' :: tail -> Lambda_Lambda :: (tokenizer tail)
      | '.' :: tail -> Lambda_Dot :: (tokenizer tail)
      | ' ' :: tail | '\t' :: tail | '\n' :: tail  -> tokenizer tail
      | var :: tail when string_match regex_var (String.make 1 var) 0 -> (Lambda_Var (String.make 1 var)) :: (tokenizer tail)
      | _ :: tail -> raise (Failure "tokenizing failed")
  in

  tokenizer lst

let lex_engl input = 
  let lst = List.fold_left (fun acc x -> acc @ (full_split (regexp "[\\(\\)]") x) ) [] (Str.split (regexp " +") input) in

  let rec tokenizer lst =
    match lst with 
      | [] -> [Engl_EOF]
      | Delim "(" :: tail -> Engl_LParen :: (tokenizer tail)
      | Delim ")" :: tail -> Engl_RParen :: (tokenizer tail)
      | Text "if" :: tail -> Engl_If :: (tokenizer tail)
      | Text "then" :: tail -> Engl_Then :: (tokenizer tail)
      | Text "else" :: tail -> Engl_Else :: (tokenizer tail)
      | Text "true" :: tail -> Engl_True :: (tokenizer tail)
      | Text "false" :: tail -> Engl_False :: (tokenizer tail)
      | Text "not" :: tail -> Engl_Not :: (tokenizer tail)
      | Text "and" :: tail -> Engl_And :: (tokenizer tail)
      | Text "or" :: tail -> Engl_Or :: (tokenizer tail)
      | _ :: tail -> raise (Failure "tokenizing failed")
  in

  tokenizer lst
