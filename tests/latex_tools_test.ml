(**pp -syntax camlp5o -package pa_ppx.testutils,pa_ppx.utils,latex_tools,pa_ppx.deriving_plugins.std *)
open OUnit2
open Pa_ppx_testutils
open Pa_ppx_utils
open Latex_tools
open Tools
open Environments

let printer s = s

let test_tokens ctxt =
  ()
  ; assert_equal "" ""

let printer s = Fmt.(str "<<%s>>" s)

let test_strip_spaces ctxt =
  let doit_stream s =
    s
    |> Tools.stream_of_string
    |> StripSpaceAfterBeginEnd.stream
    |> stream_to_string pp_tex in
  let doit_list s =
    s
    |> Tools.list_of_string
    |> StripSpaceAfterBeginEnd.list
    |> list_to_string pp_tex in

  ()
  ; assert_equal ~printer
      {|\begin{foo}\end{foo}|}
      (doit_stream {|\begin{foo}\end{foo}|})
  ; assert_equal ~printer
      {|\begin{foo}\end{foo} 
|}
      (doit_stream {|\begin {foo}\end
{foo} 
|})
  ; assert_equal ~printer
      {|\begin{foo}\end{foo}|}
      (doit_list {|\begin{foo}\end{foo}|})
  ; assert_equal ~printer
      {|\begin{foo}\end{foo} 
|}
      (doit_list {|\begin {foo}\end
{foo} 
|})

let test_begin_end ctxt =
  let cmp = [%eq: MarkEnvironmentBeginEnd.t token list] in
  let printer = [%show: MarkEnvironmentBeginEnd.t token list] in
  let doit_stream s =
    s
    |> Tools.stream_of_string
    |> StripSpaceAfterBeginEnd.stream
    |> MarkEnvironmentBeginEnd.stream
    |> Std.list_of_stream in
  let doit_list s =
    s
    |> Tools.list_of_string
    |> StripSpaceAfterBeginEnd.list
    |> MarkEnvironmentBeginEnd.list in
  ()
  ; assert_equal ~cmp ~printer
      [{ it = `EnvironBegin ("foo"); text = "\\begin{foo}"; loc = Ploc.dummy };
       { it = `EnvironEnd ("foo"); text = "\\end{foo}"; loc = Ploc.dummy }]
      (doit_stream {|\begin{foo}\end{foo}|})
  ; assert_equal ~cmp ~printer
      [{ it = `EnvironBegin ("foo"); text = "\\begin{foo}"; loc = Ploc.dummy };
       { it = `Text; text = "..."; loc = Ploc.dummy };
       { it = `EnvironEnd ("foo"); text = "\\end{foo}"; loc = Ploc.dummy }]
      (doit_stream {|\begin{foo}...\end{foo}|})
  ; assert_equal ~cmp ~printer
      [{ it = `EnvironBegin ("foo"); text = "\\begin{foo}"; loc = Ploc.dummy };
       { it = `EnvironEnd ("foo"); text = "\\end{foo}"; loc = Ploc.dummy }]
      (doit_list {|\begin{foo}\end{foo}|})
  ; assert_equal ~cmp ~printer
      [{ it = `EnvironBegin ("foo"); text = "\\begin{foo}"; loc = Ploc.dummy };
       { it = `Text; text = "..."; loc = Ploc.dummy };
       { it = `EnvironEnd ("foo"); text = "\\end{foo}"; loc = Ploc.dummy }]
      (doit_list {|\begin{foo}...\end{foo}|})

let test_coalesce ctxt =
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
  ()
  ; assert_equal ~cmp ~printer
      [{ it = `Environment (("foo", [])); text = "\\begin{foo}\\end{foo}"; loc = Ploc.dummy }]
      (doit_stream {|\begin{foo}\end{foo}|})
  ; assert_equal ~cmp ~printer
      [{ it = `Environment (("foo", [{ it = `Text; text = "..."; loc = Ploc.dummy }]));
         text = "\\begin{foo}...\\end{foo}"; loc = Ploc.dummy }]
      (doit_stream {|\begin{foo}...\end{foo}|})
  ; assert_equal ~cmp ~printer
      [{ it = `Environment (("foo",
                             [{ it = `Text; text = ".."; loc = Ploc.dummy };
                              { it = `Environment (("bar", [{ it = `Text; text = ".."; loc = Ploc.dummy }]));
                                text = "\\begin{bar}..\\end{bar}"; loc = Ploc.dummy };
                              { it = `Text; text = ".."; loc = Ploc.dummy }]
                ));
         text = "\\begin{foo}..\\begin{bar}..\\end{bar}..\\end{foo}"; loc = Ploc.dummy }]
      (doit_stream {|\begin{foo}..\begin{bar}..\end{bar}..\end{foo}|})
  ; assert_equal ~cmp ~printer
      [{ it = `Environment (("foo", [])); text = "\\begin{foo}\\end{foo}"; loc = Ploc.dummy }]
      (doit_list {|\begin{foo}\end{foo}|})
  ; assert_equal ~cmp ~printer
      [{ it = `Environment (("foo", [{ it = `Text; text = "..."; loc = Ploc.dummy }]));
         text = "\\begin{foo}...\\end{foo}"; loc = Ploc.dummy }]
      (doit_list {|\begin{foo}...\end{foo}|})
  ; assert_equal ~cmp ~printer
      [{ it = `Environment (("foo",
                             [{ it = `Text; text = ".."; loc = Ploc.dummy };
                              { it = `Environment (("bar", [{ it = `Text; text = ".."; loc = Ploc.dummy }]));
                                text = "\\begin{bar}..\\end{bar}"; loc = Ploc.dummy };
                              { it = `Text; text = ".."; loc = Ploc.dummy }]
                ));
         text = "\\begin{foo}..\\begin{bar}..\\end{bar}..\\end{foo}"; loc = Ploc.dummy }]
      (doit_list {|\begin{foo}..\begin{bar}..\end{bar}..\end{foo}|})

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

let test_extract_environments ctxt =
  let cmp = [%eq: string list] in
  let printer = [%show: string list] in
  ()
  ; assert_equal ~cmp ~printer
      ["\\begin{foo}\\end{foo}"]
      ({|\begin{foo}\end{foo}|}
       |> Tools.stream_of_string
       |> StripSpaceAfterBeginEnd.stream
       |> MarkEnvironmentBeginEnd.stream
       |> CoalesceEnvironments.stream
       |> extract_environments
       |> List.map (fun tok -> tok.text))
  ; assert_equal ~cmp ~printer
      ["\\begin{foo}...\\end{foo}"]
      ({|\begin{foo}...\end{foo}|}
       |> Tools.stream_of_string
       |> StripSpaceAfterBeginEnd.stream
       |> MarkEnvironmentBeginEnd.stream
       |> CoalesceEnvironments.stream
       |> extract_environments
       |> List.map (fun tok -> tok.text))
  ; assert_equal ~cmp ~printer
      ["\\begin{bar}..\\end{bar}";
  "\\begin{foo}..\\begin{bar}..\\end{bar}..\\end{foo}"]
      ({|\begin{foo}..\begin{bar}..\end{bar}..\end{foo}|}
       |> Tools.stream_of_string
       |> StripSpaceAfterBeginEnd.stream
       |> MarkEnvironmentBeginEnd.stream
       |> CoalesceEnvironments.stream
       |> extract_environments
       |> List.map (fun tok -> tok.text))

let suite = "Test latex_tools" >::: [
      "tokens"   >:: test_tokens
    ; "strip spaces after begin/end"   >:: test_strip_spaces
    ; "marking begin/end of environments" >:: test_begin_end
    ; "coalesce begin/end of environments" >:: test_coalesce
    ; "extract environments" >:: test_extract_environments
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

