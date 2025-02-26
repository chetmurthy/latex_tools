(**pp -syntax camlp5o -package pa_ppx.testutils,latex_tools *)
open OUnit2
open Pa_ppx_testutils
open Latex_tools
open Latex_tokens

let printer s = s

let test_tokens ctxt =
  ()
  ; assert_equal "" ""

let suite = "Test latex_tools" >::: [
      "tokens"   >:: test_tokens
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

