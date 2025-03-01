(**pp -syntax camlp5o -package pa_ppx.testutils,pa_ppx.utils,latex_tools,pa_ppx.deriving_plugins.std *)
open OUnit2
open Pa_ppx_testutils
open Pa_ppx_utils
open Latex_tools
open Tools
open Texparse
(*
exception ReportedStreamError of (string * (int * int)) option
*)
open Pa_ppx_runtime.Exceptions
type t +=
  ReportedStreamError of string * (string * (int * int)) option[@name "ReportedStreamError"]
  [@@deriving show]


let report_token_transducer_error name ~underlying strm =
  let report_underlying () =
    match Stream.peek underlying with
      Some tok ->
       let bp = Ploc.first_pos tok.loc in
       let ep = Ploc.last_pos tok.loc in
       raise (ReportedStreamError (name, Some (tok.text, (bp, ep))))
    | None -> raise (ReportedStreamError (name, None)) in
  Utils.report_transducer_stream_error report_underlying strm

let transduce name transformer strm =
  report_token_transducer_error name ~underlying:strm [< (transformer strm) >]

let printer s = s

let test_tokens ctxt =
  ()
  ; assert_equal "" ""

let printer s = Fmt.(str "<<%s>>" s)

let test_strip_spaces ~list ctxt =
  let doit_stream s =
    s
    |> Tools.stream_of_string
    |> transduce "strip-spaces" StripSpaceAfterBeginEnd.stream
    |> stream_to_string pp_tex in
  let doit_list s =
    s
    |> Tools.list_of_string
    |> StripSpaceAfterBeginEnd.list
    |> list_to_string pp_tex in
  let doit = if list then doit_list else doit_stream in

  ()
  ; assert_equal ~printer
      {|\begin{foo}\end{foo}|}
      (doit {|\begin{foo}\end{foo}|})
  ; assert_equal ~printer
      {|\begin{foo}\end{foo} 
|}
      (doit {|\begin {foo}\end
{foo} 
|})
  ; assert_equal ~printer
      {|\argle{foo}|}
      (doit {|\argle{foo}|})

let test_begin_end ~list ctxt =
  let cmp = [%eq: MarkEnvironmentBeginEnd.t token list] in
  let printer = [%show: MarkEnvironmentBeginEnd.t token list] in
  let doit_stream s =
    s
    |> Tools.stream_of_string
    |> transduce "strip-spaces" StripSpaceAfterBeginEnd.stream
    |> transduce "mark-environments" MarkEnvironmentBeginEnd.stream
    |> Std.list_of_stream in
  let doit_list s =
    s
    |> Tools.list_of_string
    |> StripSpaceAfterBeginEnd.list
    |> MarkEnvironmentBeginEnd.list in
  let doit = if list then doit_list else doit_stream in
  ()
  ; assert_equal ~cmp ~printer
      [{ it = `EnvironBegin ("foo"); text = "\\begin{foo}"; loc = Ploc.dummy };
       { it = `EnvironEnd ("foo"); text = "\\end{foo}"; loc = Ploc.dummy }]
      (doit {|\begin{foo}\end{foo}|})
  ; assert_equal ~cmp ~printer
      [{ it = `EnvironBegin ("foo"); text = "\\begin{foo}"; loc = Ploc.dummy };
       { it = `Text; text = "..."; loc = Ploc.dummy };
       { it = `EnvironEnd ("foo"); text = "\\end{foo}"; loc = Ploc.dummy }]
      (doit {|\begin{foo}...\end{foo}|})
  ; assert_equal ~cmp ~printer
      [{ it = `Escape; text = "\\"; loc = Ploc.dummy };
       { it = `CommandName; text = "foo"; loc = Ploc.dummy };
       { it = `GroupBegin; text = "{"; loc = Ploc.dummy };
       { it = `Text; text = "foo"; loc = Ploc.dummy };
       { it = `GroupEnd; text = "}"; loc = Ploc.dummy }]
      (doit {|\foo{foo}|})
  ; assert_equal ~cmp ~printer
      [{ it = `Escape; text = "\\"; loc = Ploc.dummy };
       { it = `CommandName; text = "begin"; loc = Ploc.dummy };
       { it = `Text; text = "foo"; loc = Ploc.dummy }]
      (doit {|\begin foo|})
  ; assert_equal ~cmp ~printer
      [{ it = `Escape; text = "\\"; loc = Ploc.dummy };
       { it = `CommandName; text = "end"; loc = Ploc.dummy };
       { it = `Text; text = "foo"; loc = Ploc.dummy }]
      (doit {|\end foo|})
  ; assert_equal ~cmp ~printer
      [{ it = `Escape; text = "\\"; loc = Ploc.dummy };
       { it = `CommandName; text = "begin"; loc = Ploc.dummy };
       { it = `GroupBegin; text = "{"; loc = Ploc.dummy };
       { it = `GroupEnd; text = "}"; loc = Ploc.dummy }]
      (doit {|\begin {}|})
  ; assert_equal ~cmp ~printer
      [{ it = `Escape; text = "\\"; loc = Ploc.dummy };
       { it = `CommandName; text = "begin"; loc = Ploc.dummy };
       { it = `GroupBegin; text = "{"; loc = Ploc.dummy };
       { it = `Text; text = "a "; loc = Ploc.dummy };
       { it = `GroupBegin; text = "{"; loc = Ploc.dummy };
       { it = `GroupEnd; text = "}"; loc = Ploc.dummy };
       { it = `GroupEnd; text = "}"; loc = Ploc.dummy }]
      (doit {|\begin {a {}}|})

let test_partial_begin_end ~list ctxt =
  let cmp = [%eq: MarkEnvironmentBeginEnd.t token list] in
  let printer = [%show: MarkEnvironmentBeginEnd.t token list] in
  let doit_stream environs s =
    s
    |> Tools.stream_of_string
    |> transduce "strip-spaces" StripSpaceAfterBeginEnd.stream
    |> transduce "mark-environments" (MarkEnvironmentBeginEnd.stream ~environs)
    |> Std.list_of_stream in
  let doit_list environs s =
    s
    |> Tools.list_of_string
    |> StripSpaceAfterBeginEnd.list
    |> MarkEnvironmentBeginEnd.list ~environs in
  let doit = if list then doit_list else doit_stream in
  ()
  ; assert_equal ~cmp ~printer
      [{ it = `EnvironBegin ("foo"); text = "\\begin{foo}"; loc = Ploc.dummy };
       { it = `Text; text = "..."; loc = Ploc.dummy };
       { it = `EnvironEnd ("foo"); text = "\\end{foo}"; loc = Ploc.dummy }]
      (doit [] {|\begin{foo}...\end{foo}|})
  ; assert_equal ~cmp ~printer
      [{ it = `Escape; text = "\\"; loc = Ploc.dummy };
       { it = `CommandName; text = "begin"; loc = Ploc.dummy };
       { it = `GroupBegin; text = "{"; loc = Ploc.dummy };
       { it = `Text; text = "foo"; loc = Ploc.dummy };
       { it = `GroupEnd; text = "}"; loc = Ploc.dummy };
       { it = `Escape; text = "\\"; loc = Ploc.dummy };
       { it = `CommandName; text = "end"; loc = Ploc.dummy };
       { it = `GroupBegin; text = "{"; loc = Ploc.dummy };
       { it = `Text; text = "foo"; loc = Ploc.dummy };
       { it = `GroupEnd; text = "}"; loc = Ploc.dummy }]
      (doit ["bar"] {|\begin{foo}\end{foo}|})
  ; assert_equal ~cmp ~printer
      [{ it = `Escape; text = "\\"; loc = Ploc.dummy };
       { it = `CommandName; text = "begin"; loc = Ploc.dummy };
       { it = `GroupBegin; text = "{"; loc = Ploc.dummy };
       { it = `Text; text = "foo"; loc = Ploc.dummy };
       { it = `GroupEnd; text = "}"; loc = Ploc.dummy };
       { it = `Escape; text = "\\"; loc = Ploc.dummy };
       { it = `CommandName; text = "end"; loc = Ploc.dummy };
       { it = `GroupBegin; text = "{"; loc = Ploc.dummy };
       { it = `Text; text = "foo"; loc = Ploc.dummy };
       { it = `GroupEnd; text = "}"; loc = Ploc.dummy };
       { it = `EnvironBegin ("bar"); text = "\\begin{bar}"; loc = Ploc.dummy };
       { it = `EnvironEnd ("bar"); text = "\\end{bar}"; loc = Ploc.dummy }]
      (doit ["bar"] {|\begin{foo}\end{foo}\begin{bar}\end{bar}|})

let test_coalesce ~list ctxt =
  let cmp = [%eq: CoalesceEnvironments.t token list] in
  let printer = [%show: CoalesceEnvironments.t token list] in
  let doit_stream s =
    s
    |> Tools.stream_of_string
    |> StripSpaceAfterBeginEnd.stream
    |> MarkEnvironmentBeginEnd.stream
    |> CoalesceEnvironments.stream
    |> Std.list_of_stream in
  let doit_list s =
    s
    |> Tools.list_of_string
    |> StripSpaceAfterBeginEnd.list
    |> MarkEnvironmentBeginEnd.list
    |> CoalesceEnvironments.list in
  let doit = if list then doit_list else doit_stream in
  ()
  ; assert_equal ~cmp ~printer
      [{ it = `Environment (("foo", [])); text = "\\begin{foo}\\end{foo}"; loc = Ploc.dummy }]
      (doit {|\begin{foo}\end{foo}|})
  ; assert_equal ~cmp ~printer
      [{ it = `Environment (("foo", [{ it = `Text; text = "..."; loc = Ploc.dummy }]));
         text = "\\begin{foo}...\\end{foo}"; loc = Ploc.dummy }]
      (doit {|\begin{foo}...\end{foo}|})
  ; assert_equal ~cmp ~printer
      [{ it = `Environment (("foo",
                             [{ it = `Text; text = ".."; loc = Ploc.dummy };
                              { it = `Environment (("bar", [{ it = `Text; text = ".."; loc = Ploc.dummy }]));
                                text = "\\begin{bar}..\\end{bar}"; loc = Ploc.dummy };
                              { it = `Text; text = ".."; loc = Ploc.dummy }]
                ));
         text = "\\begin{foo}..\\begin{bar}..\\end{bar}..\\end{foo}"; loc = Ploc.dummy }]
      (doit {|\begin{foo}..\begin{bar}..\end{bar}..\end{foo}|})

let test_partial_coalesce ~list ctxt =
  let cmp = [%eq: CoalesceEnvironments.t token list] in
  let printer = [%show: CoalesceEnvironments.t token list] in
  let doit_stream environs s =
    s
    |> Tools.stream_of_string
    |> StripSpaceAfterBeginEnd.stream
    |> MarkEnvironmentBeginEnd.stream
    |> CoalesceEnvironments.stream ~environs
    |> Std.list_of_stream in
  let doit_list environs s =
    s
    |> Tools.list_of_string
    |> StripSpaceAfterBeginEnd.list
    |> MarkEnvironmentBeginEnd.list
    |> CoalesceEnvironments.list ~environs in
  let doit = if list then doit_list else doit_stream in
  ()
  ; assert_equal ~cmp ~printer
      [{ it = `EnvironBegin ("foo"); text = "\\begin{foo}"; loc = Ploc.dummy };
       { it = `EnvironEnd ("foo"); text = "\\end{foo}"; loc = Ploc.dummy }]
      (doit ["bar"] {|\begin{foo}\end{foo}|})
  ; assert_equal ~cmp ~printer
      [{ it = `Environment (("foo", [])); text = "\\begin{foo}\\end{foo}"; loc = Ploc.dummy }]
      (doit ["foo"] {|\begin{foo}\end{foo}|})
  ; assert_equal ~cmp ~printer
      [{ it = `Environment (
                  ("foo",
                   [{ it = `Text; text = ".."; loc = Ploc.dummy };
                    { it = `EnvironBegin ("bar"); text = "\\begin{bar}"; loc = Ploc.dummy };
                    { it = `Text; text = ".."; loc = Ploc.dummy };
                    { it = `EnvironEnd ("bar"); text = "\\end{bar}"; loc = Ploc.dummy };
                    { it = `Text; text = ".."; loc = Ploc.dummy }])
                );
         text = "\\begin{foo}..\\begin{bar}..\\end{bar}..\\end{foo}"; loc = Ploc.dummy }
      ]
      (doit ["foo"] {|\begin{foo}..\begin{bar}..\end{bar}..\end{foo}|})
  ; assert_equal ~cmp ~printer
      [{ it = `EnvironBegin ("foo"); text = "\\begin{foo}"; loc = Ploc.dummy };
       { it = `Text; text = ".."; loc = Ploc.dummy };
       { it = `Environment (("bar", [{ it = `Text; text = ".."; loc = Ploc.dummy }]));
         text = "\\begin{bar}..\\end{bar}"; loc = Ploc.dummy };
       { it = `Text; text = ".."; loc = Ploc.dummy };
       { it = `EnvironEnd ("foo"); text = "\\end{foo}"; loc = Ploc.dummy }]
      (doit ["bar"] {|\begin{foo}..\begin{bar}..\end{bar}..\end{foo}|})
  ; assert_equal ~cmp ~printer
      [{ it = `Environment (("foo",
                             [{ it = `Text; text = ".."; loc = Ploc.dummy };
                              { it = `Environment (("bar", [{ it = `Text; text = ".."; loc = Ploc.dummy }]));
                                text = "\\begin{bar}..\\end{bar}"; loc = Ploc.dummy };
                              { it = `Text; text = ".."; loc = Ploc.dummy }]
                ));
         text = "\\begin{foo}..\\begin{bar}..\\end{bar}..\\end{foo}"; loc = Ploc.dummy }]
      (doit [] {|\begin{foo}..\begin{bar}..\end{bar}..\end{foo}|})

let extract_environments strm =
  let acc = ref [] in
  let open Visitors.CoalesceEnvironments in
  let dt = make_dt () in
  let old_migrate_t_token = dt.migrate_t_token in
  let migrate_t_token dt tok =
    let tok = old_migrate_t_token dt tok in
    match tok.it with
      `Environment (name, cl) ->
       Std.push acc tok ;
       tok
    | _ -> tok in
  let dt = { dt with migrate_t_token = migrate_t_token } in
  Stream.iter (fun tok -> ignore(dt.migrate_t_token dt tok)) strm ;
  List.rev !acc

let test_extract_environments ~list ctxt =
  let cmp = [%eq: string list] in
  let printer = [%show: string list] in
  let doit_stream s =
    s
    |> Tools.stream_of_string
    |> StripSpaceAfterBeginEnd.stream
    |> MarkEnvironmentBeginEnd.stream
    |> CoalesceEnvironments.stream
    |> extract_environments
    |> List.map (fun tok -> tok.text) in 
  let doit_list s =
    s
    |> Tools.list_of_string
    |> StripSpaceAfterBeginEnd.list
    |> MarkEnvironmentBeginEnd.list
    |> CoalesceEnvironments.list
    |> Std.stream_of_list
    |> extract_environments
    |> List.map (fun tok -> tok.text) in 
  let doit = if list then doit_list else doit_stream in
  ()
  ; assert_equal ~cmp ~printer
      ["\\begin{foo}\\end{foo}"]
      (doit {|\begin{foo}\end{foo}|})
  ; assert_equal ~cmp ~printer
      ["\\begin{foo}...\\end{foo}"]
      (doit {|\begin{foo}...\end{foo}|})
  ; assert_equal ~cmp ~printer
      ["\\begin{bar}..\\end{bar}";
  "\\begin{foo}..\\begin{bar}..\\end{bar}..\\end{foo}"]
      (doit {|\begin{foo}..\begin{bar}..\end{bar}..\end{foo}|})
  ; assert_equal ~cmp ~printer
      []
      (doit {|\foo|})

let test_partial_extract_environments ~list ctxt =
  let cmp = [%eq: string list] in
  let printer = [%show: string list] in
  let doit_stream environs s =
    s
    |> Tools.stream_of_string
    |> StripSpaceAfterBeginEnd.stream
    |> MarkEnvironmentBeginEnd.stream ~environs
    |> CoalesceEnvironments.stream ~environs
    |> extract_environments
    |> List.map (fun tok -> tok.text) in 
  let doit_list environs s =
    s
    |> Tools.list_of_string
    |> StripSpaceAfterBeginEnd.list
    |> MarkEnvironmentBeginEnd.list ~environs
    |> CoalesceEnvironments.list ~environs
    |> Std.stream_of_list
    |> extract_environments
    |> List.map (fun tok -> tok.text) in 
  let doit = if list then doit_list else doit_stream in
  ()
  ; assert_equal ~cmp ~printer
      ["\\begin{foo}\\end{foo}"]
      (doit ["foo"] {|\begin{foo}\end{foo}|})
  ; assert_equal ~cmp ~printer
      ["\\begin{foo}...\\end{foo}"]
      (doit ["foo"] {|\begin{foo}...\end{foo}|})
  ; assert_equal ~cmp ~printer
      ["\\begin{foo}..\\begin{bar}..\\end{bar}..\\end{foo}"]
      (doit ["foo"] {|\begin{foo}..\begin{bar}..\end{bar}..\end{foo}|})

let suite = "Test latex_tools" >::: [
      "tokens"   >:: test_tokens
    ; "strip spaces after begin/end (stream)"   >:: test_strip_spaces ~list:false
    ; "strip spaces after begin/end (list)"   >:: test_strip_spaces ~list:true
    ; "marking begin/end of environments (stream)" >:: test_begin_end ~list:false
    ; "marking begin/end of environments (list)" >:: test_begin_end ~list:true
    ; "partial marking begin/end of environments (stream)" >:: test_partial_begin_end ~list:false
    ; "partial marking begin/end of environments (list)" >:: test_partial_begin_end ~list:true
    ; "coalesce begin/end of environments (stream)" >:: test_coalesce ~list:false
    ; "coalesce begin/end of environments (list)" >:: test_coalesce ~list:true
    ; "partial coalesce begin/end of environments (stream)" >:: test_partial_coalesce ~list:false
    ; "partial coalesce begin/end of environments (list)" >:: test_partial_coalesce ~list:true
    ; "extract environments (stream)" >:: test_extract_environments ~list:false
    ; "extract environments (list)" >:: test_extract_environments ~list:true
    ; "partial extract environments (stream)" >:: test_partial_extract_environments ~list:false
    ; "partial extract environments (list)" >:: test_partial_extract_environments ~list:true
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

