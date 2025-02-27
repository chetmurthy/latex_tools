(**pp -syntax camlp5o -package pa_ppx.testutils,pa_ppx.utils,latex_tools,pa_ppx.deriving_plugins.std *)
open OUnit2
open Pa_ppx_testutils
open Pa_ppx_utils
open Latex_tools
open Environments
open Latex_tokens

let printer s = s

let test_tokens ctxt =
  ()
  ; assert_equal "" ""

let printer s = Fmt.(str "<<%s>>" s)

let token_dummy_loc t = { t with loc = Ploc.dummy }

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

let test_begin_end ctxt =
  ()
  ; assert_equal ~printer:[%show: MarkEnvironmentBeginEnd.t token list]
      [{ it = `EnvironBegin ("foo"); text = "\\begin{foo}"; loc = Ploc.dummy };
       { it = `EnvironEnd ("foo"); text = "\\end{foo}"; loc = Ploc.dummy }]
      ({|\begin{foo}\end{foo}|}
       |> Tools.stream_of_string
       |> StripSpaceAfterBeginEnd.stream
       |> MarkEnvironmentBeginEnd.stream
       |> Std.stream_map token_dummy_loc
       |> Std.list_of_stream)
  ; assert_equal ~printer:[%show: MarkEnvironmentBeginEnd.t token list]
      [{ it = `EnvironBegin ("foo"); text = "\\begin{foo}"; loc = Ploc.dummy };
       { it = `Text; text = "..."; loc = Ploc.dummy };
       { it = `EnvironEnd ("foo"); text = "\\end{foo}"; loc = Ploc.dummy }]
      ({|\begin{foo}...\end{foo}|}
       |> Tools.stream_of_string
       |> StripSpaceAfterBeginEnd.stream
       |> MarkEnvironmentBeginEnd.stream
       |> Std.stream_map token_dummy_loc
       |> Std.list_of_stream)


let suite = "Test latex_tools" >::: [
      "tokens"   >:: test_tokens
    ; "strip spaces after begin/end"   >:: test_strip_spaces
    ; "marking begin/end of environments" >:: test_begin_end
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

