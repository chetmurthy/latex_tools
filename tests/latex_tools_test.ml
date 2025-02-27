(**pp -syntax camlp5o -package pa_ppx.testutils,latex_tools *)
open OUnit2
open Pa_ppx_testutils
open Latex_tools
open Environments
open Latex_tokens

let printer s = s

let test_tokens ctxt =
  ()
  ; assert_equal "" ""

let printer s = Fmt.(str "<<%s>>" s)

let test_strip_spaces ctxt =
  ()
  ; assert_equal ~printer
      {|\begin{foo}\end{foo}|}
      ({|\begin{foo}\end{foo}|}
       |> Tools.stream_of_string
       |> StripSpaceAfterBeginEnd.stream
       |> stream_to_string pp_tex)
  ; assert_equal ~printer
      {|\begin{foo}\end{foo} 
|}
      ({|\begin {foo}\end
{foo} 
|}
       |> Tools.stream_of_string
       |> StripSpaceAfterBeginEnd.stream
       |> stream_to_string pp_tex)

let suite = "Test latex_tools" >::: [
      "tokens"   >:: test_tokens
    ; "strip spaces after begin/end"   >:: test_strip_spaces
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

