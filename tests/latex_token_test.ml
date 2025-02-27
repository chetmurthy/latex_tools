(**pp -syntax camlp5o -package pa_ppx_regexp,pa_ppx.runtime,pa_ppx.runtime_fat,pa_ppx.testutils,latex_tools,bos *)
open OUnit2
open Pa_ppx_testutils
open Latex_tools
open Latex_tokens
open Latex_lexer

Pa_ppx_runtime.Exceptions.Ploc.pp_loc_verbose := true ;;
Pa_ppx_runtime_fat.Exceptions.Ploc.pp_loc_verbose := true ;;

let python_quote s =
  let contains_squote = String.contains s '\'' in
  let contains_dquote = String.contains s '"' in
  let b = Buffer.create (String.length s) in
  (match (contains_squote, contains_dquote) with

     (false, _) ->
      Buffer.add_char b '\'' ;
      s |> String.iter (fun c ->
               if c = '"' then Buffer.add_char b c
               else Buffer.add_string b (Char.escaped c)) ;
      Buffer.add_char b '\''

   | (true, true) ->
      Buffer.add_char b '\'' ;
      s |> String.iter (fun c ->
               if c = '\'' then Buffer.add_string b "\\'"
               else if c = '"' then Buffer.add_char b c
               else Buffer.add_string b (Char.escaped c)) ;
      Buffer.add_char b '\''

   | (true, false) ->
      Buffer.add_char b '"' ;
      s |> String.iter (fun c ->
               if c = '\'' then Buffer.add_char b c
               else Buffer.add_string b (Char.escaped c)) ;
      Buffer.add_char b '"'
  ) ;
  Buffer.contents b

let list_of_tokens_eof lexbuf =
  let rec lrec acc =
    match token lexbuf with
      [(EOF, _, _)] -> List.rev acc
    | t -> lrec ((List.rev t) @ acc)
  in lrec []

let fmt1 (t,s,_) =
  let s = Fmt.(str "(%a, %a)@." string (python_quote s) (quote ~mark:"'" pp) t) in
  [%subst {|\n|} / "" / g m] s

let latex_tokens ~roundtrip fname =
  let lexbuf = Sedlexing.Utf8.from_channel (open_in fname) in
  let toks = list_of_tokens_eof lexbuf in
  if roundtrip then
    toks |> List.iter (fun (_,s,_) -> print_string s)
  else
    let l = List.map fmt1 toks in
    Fmt.(pf stdout "%a@." (list ~sep:(const string "\n") string) l)

let _ = 
if not !Sys.interactive then
  let fname = ref "" in
  let roundtrip = ref false in
  begin
    Arg.(parse [
             "-roundtrip", Set roundtrip, "roundtrip (dump back text of tokens)"
           ]
           (fun s -> fname := s)
           "latex_token_test.exe <fname>");
    let roundtrip = !roundtrip in
    latex_tokens ~roundtrip !fname
  end
else ()

