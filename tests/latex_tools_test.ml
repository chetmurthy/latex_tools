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
  let cmp = [%eq: MarkEnvironmentBeginEnd.t token list] in
  let printer = [%show: MarkEnvironmentBeginEnd.t token list] in
  ()
  ; assert_equal ~cmp ~printer
      [{ it = `EnvironBegin ("foo"); text = "\\begin{foo}"; loc = Ploc.dummy };
       { it = `EnvironEnd ("foo"); text = "\\end{foo}"; loc = Ploc.dummy }]
      ({|\begin{foo}\end{foo}|}
       |> Tools.stream_of_string
       |> StripSpaceAfterBeginEnd.stream
       |> MarkEnvironmentBeginEnd.stream
       |> Std.list_of_stream)
  ; assert_equal ~cmp ~printer
      [{ it = `EnvironBegin ("foo"); text = "\\begin{foo}"; loc = Ploc.dummy };
       { it = `Text; text = "..."; loc = Ploc.dummy };
       { it = `EnvironEnd ("foo"); text = "\\end{foo}"; loc = Ploc.dummy }]
      ({|\begin{foo}...\end{foo}|}
       |> Tools.stream_of_string
       |> StripSpaceAfterBeginEnd.stream
       |> MarkEnvironmentBeginEnd.stream
       |> Std.list_of_stream)

let test_coalesce ctxt =
  let cmp = [%eq: CoalesceEnvironments.t token list] in
  let printer = [%show: CoalesceEnvironments.t token list] in
  ()
  ; assert_equal ~cmp ~printer
      [{ it = `Environment (("foo", [])); text = "\\begin{foo}\\end{foo}"; loc = Ploc.dummy }]
      ({|\begin{foo}\end{foo}|}
       |> Tools.stream_of_string
       |> StripSpaceAfterBeginEnd.stream
       |> MarkEnvironmentBeginEnd.stream
       |> CoalesceEnvironments.stream
       |> Std.list_of_stream)
  ; assert_equal ~cmp ~printer
      [{ it = `Environment (("foo", [{ it = `Text; text = "..."; loc = Ploc.dummy }]));
         text = "\\begin{foo}...\\end{foo}"; loc = Ploc.dummy }]
      ({|\begin{foo}...\end{foo}|}
       |> Tools.stream_of_string
       |> StripSpaceAfterBeginEnd.stream
       |> MarkEnvironmentBeginEnd.stream
       |> CoalesceEnvironments.stream
       |> Std.list_of_stream)
  ; assert_equal ~cmp ~printer
      [{ it = `Environment (("foo",
                             [{ it = `Text; text = ".."; loc = Ploc.dummy };
                              { it = `Environment (("bar", [{ it = `Text; text = ".."; loc = Ploc.dummy }]));
                                text = "\\begin{bar}..\\end{bar}"; loc = Ploc.dummy };
                              { it = `Text; text = ".."; loc = Ploc.dummy }]
                ));
         text = "\\begin{foo}..\\begin{bar}..\\end{bar}..\\end{foo}"; loc = Ploc.dummy }]
      ({|\begin{foo}..\begin{bar}..\end{bar}..\end{foo}|}
       |> Tools.stream_of_string
       |> StripSpaceAfterBeginEnd.stream
       |> MarkEnvironmentBeginEnd.stream
       |> CoalesceEnvironments.stream
       |> Std.list_of_stream)

let suite = "Test latex_tools" >::: [
      "tokens"   >:: test_tokens
    ; "strip spaces after begin/end"   >:: test_strip_spaces
    ; "marking begin/end of environments" >:: test_begin_end
    ; "coalesce begin/end of environments" >:: test_coalesce
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

