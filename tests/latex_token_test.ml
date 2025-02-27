(**pp -syntax camlp5o -package pa_ppx.testutils,latex_tools,bos *)
open OUnit2
open Pa_ppx_testutils
open Latex_tools
open Latex_tokens
open Latex_lexer

let list_of_tokens_eof lexbuf =
  let rec lrec acc =
    match token lexbuf with
      (EOF, _, _) -> List.rev acc
    | t -> lrec (t::acc)
  in lrec []

let latex_tokens fname =
  let lexbuf = Sedlexing.Utf8.from_channel (open_in fname) in
  let toks = list_of_tokens_eof lexbuf in
  toks |> List.iter (fun (t, s, _) ->
              Fmt.(pf stdout "(%a, %a)@." (quote ~mark:"'" string) (String.escaped s) (quote ~mark:"'" pp) t) ;
            ) ;
  Fmt.(pf stdout "@.")

let _ = 
if not !Sys.interactive then
  let fname = ref "" in
  begin
    Arg.(parse [
           ]
           (fun s -> fname := s)
           "latex_token_test.exe <fname>");
    latex_tokens !fname
  end
else ()

