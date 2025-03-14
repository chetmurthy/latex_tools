(**pp -syntax camlp5o -package pa_ppx.testutils,pa_ppx.utils,latex_tools,pa_ppx.deriving_plugins.std *)
open OUnit2
open Pa_ppx_testutils
open Pa_ppx_base
open Pa_ppx_utils
open Latex_tools
open Tools
open Texparse
open Parser_utils

Pa_ppx_runtime.Exceptions.Ploc.pp_loc_verbose := true ;;
Pa_ppx_runtime_fat.Exceptions.Ploc.pp_loc_verbose := true ;;

let eq_loc loc1 loc2 =
  Ploc.first_pos loc1 = Ploc.first_pos loc2 &&
  Ploc.last_pos loc1 = Ploc.last_pos loc2

let assert_raises_exc_at loc f =
  Testutil.assert_raises_exn_pred
    (function
       ReportedStreamError(_,_, Some(_,loc')) as exc ->
        if not(eq_loc loc loc') then begin
            Fmt.(pf stderr "Locations didn't match: %a <> %a@."
                   Pp_MLast.Ploc.pp loc Pp_MLast.Ploc.pp loc') ;
            false
          end
        else true
     | _ -> false)
    f

let printer s = s

let test_tokens ctxt =
  ()
  ; assert_equal "" ""

let printer s = Fmt.(str "<<%s>>" s)

let test_strip_spaces ctxt =
  let doit s =
    s
    |> Tools.stream_of_string
    |> transduce "strip-spaces" StripSpaceAfterBeginEnd.stream
    |> stream_to_string pp_tex in
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

let test_begin_end ctxt =
  let cmp = [%eq: MarkEnvironmentBeginEnd.t token list] in
  let printer = [%show: MarkEnvironmentBeginEnd.t token list] in
  let doit s =
    s
    |> Tools.stream_of_string
    |> transduce "strip-spaces" StripSpaceAfterBeginEnd.stream
    |> transduce "mark-environments" MarkEnvironmentBeginEnd.stream
    |> Std.list_of_stream in
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

let test_partial_begin_end ctxt =
  let cmp = [%eq: MarkEnvironmentBeginEnd.t token list] in
  let printer = [%show: MarkEnvironmentBeginEnd.t token list] in
  let doit environs s =
    s
    |> Tools.stream_of_string
    |> transduce "strip-spaces" StripSpaceAfterBeginEnd.stream
    |> transduce "mark-environments" (MarkEnvironmentBeginEnd.stream ~environs)
    |> Std.list_of_stream in
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

let test_coalesce ctxt =
  let cmp = [%eq: CoalesceEnvironments.t token list] in
  let printer = [%show: CoalesceEnvironments.t token list] in
  let doit s =
    s
    |> Tools.stream_of_string
    |> StripSpaceAfterBeginEnd.stream
    |> MarkEnvironmentBeginEnd.stream
    |> CoalesceEnvironments.stream
    |> Std.list_of_stream in
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

let test_partial_coalesce ctxt =
  let cmp = [%eq: CoalesceEnvironments.t token list] in
  let printer = [%show: CoalesceEnvironments.t token list] in
  let doit environs s =
    s
    |> Tools.stream_of_string
    |> StripSpaceAfterBeginEnd.stream
    |> MarkEnvironmentBeginEnd.stream
    |> CoalesceEnvironments.stream ~environs
    |> Std.list_of_stream in
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

let test_extract_environments ctxt =
  let cmp = [%eq: string list] in
  let printer = [%show: string list] in
  let doit s =
    s
    |> Tools.stream_of_string
    |> StripSpaceAfterBeginEnd.stream
    |> MarkEnvironmentBeginEnd.stream
    |> CoalesceEnvironments.stream
    |> extract_environments
    |> List.map (fun tok -> tok.text) in 
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

let test_partial_extract_environments ctxt =
  let cmp = [%eq: string list] in
  let printer = [%show: string list] in
  let doit environs s =
    s
    |> Tools.stream_of_string
    |> StripSpaceAfterBeginEnd.stream
    |> MarkEnvironmentBeginEnd.stream ~environs
    |> CoalesceEnvironments.stream ~environs
    |> extract_environments
    |> List.map (fun tok -> tok.text) in 
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


let test_coalesce_group ctxt =
  let cmp = [%eq: CoalesceGroups.t token list] in
  let printer = [%show: CoalesceGroups.t token list] in
  let doit s =
    s
    |> Tools.stream_of_string
    |> StripSpaceAfterBeginEnd.stream
    |> MarkEnvironmentBeginEnd.stream
    |> CoalesceGroups.stream
    |> Std.list_of_stream in
  ()
  ; assert_equal ~cmp ~printer
      [{ it = `Group ([{ it = `Text; text = "foo bar buzz"; loc = Ploc.dummy }]);
         text = "{foo bar buzz}"; loc = Ploc.dummy };
       { it =
           `Bracket (
               [{ it = `Text; text = "yadda "; loc = Ploc.dummy };
                { it = `Group ([{ it = `Text; text = "meh"; loc = Ploc.dummy }]);
                  text = "{meh}"; loc = Ploc.dummy };
                { it = `MergedSpacer; text = " "; loc = Ploc.dummy };
                { it = `Text; text = "yadda"; loc = Ploc.dummy }]
             );
         text = "{yadda {meh} yadda}"; loc = Ploc.dummy }
      ]
      (doit {|{foo bar buzz}[yadda {meh} yadda]|})
  ; assert_equal ~cmp ~printer
      [{ it = `Escape; text = "\\"; loc = Ploc.dummy };
       { it = `CommandName; text = "ref"; loc = Ploc.dummy };
       { it = `Group ([{ it = `Text; text = "foo"; loc = Ploc.dummy }]);
         text = "{foo}"; loc = Ploc.dummy };
       { it = `Escape; text = "\\"; loc = Ploc.dummy };
       { it = `CommandName; text = "label"; loc = Ploc.dummy };
       { it = `Group ([{ it = `Text; text = "bar"; loc = Ploc.dummy }]);
         text = "{bar}"; loc = Ploc.dummy }
      ]
      (doit {|\ref{foo}\label{bar}|})

let test_parse_commands ctxt =
  let cmp = [%eq: Commands.t token list] in
  let printer = [%show: Commands.t token list] in
  let doit cmdmap s =
    s
    |> Tools.stream_of_string
    |> StripSpaceAfterBeginEnd.stream
    |> MarkEnvironmentBeginEnd.stream
    |> CoalesceGroups.stream
    |> transduce "commands" (Commands.stream ~cmdmap)
    |> Std.list_of_stream in
  ()
  ; assert_equal ~cmp ~printer
      [{ it =
           `Command (
               ("ref", [],
                [{ it = `CommandGroup ([{ it = `Text; text = "a"; loc = Ploc.dummy }]);
                   text = "{a}"; loc = Ploc.dummy }
               ])
             );
         text = "\\ref{a}"; loc = Ploc.dummy };
       { it = `Escape; text = "\\"; loc = Ploc.dummy };
       { it = `CommandName; text = "label"; loc = Ploc.dummy };
       { it = `CommandGroup ([{ it = `Text; text = "b"; loc = Ploc.dummy }]);
         text = "{b}"; loc = Ploc.dummy }
      ]
      (doit [("ref",(1,0))] {|\ref{a}\label{b}|})
  ; assert_equal ~cmp ~printer
      [{ it =
           `Command (
               ("ref", [],
                [{ it = `CommandGroup ([{ it = `Text; text = "a"; loc = Ploc.dummy }]);
                   text = "{a}"; loc = Ploc.dummy }
               ])
             );
         text = "\\ref{a}"; loc = Ploc.dummy };
       { it =
           `Command (
               ("label", [],
                [{ it = `CommandGroup ([{ it = `Text; text = "b"; loc = Ploc.dummy }]);
                   text = "{b}"; loc = Ploc.dummy }
               ])
             );
         text = "\\label{b}"; loc = Ploc.dummy }
      ]
      (doit [("ref",(1,0)); ("label",(1,0))] {|\ref{a}\label{b}|})
  ; assert_equal ~cmp ~printer
      [{ it =
           `Command (
               ("foo",
                [{ it = `CommandBracket ([{ it = `Text; text = "x"; loc = Ploc.dummy }]);
                   text = "{x}"; loc = Ploc.dummy }
                ],
                [{ it = `CommandGroup ([{ it = `Text; text = "a"; loc = Ploc.dummy }]);
                   text = "{a}"; loc = Ploc.dummy }
               ])
             );
         text = "\\foo[x]{a}"; loc = Ploc.dummy }
      ]
      (doit [("foo",(2,1))] {|\foo[x]{a}|})
  ; assert_raises_exc_at (Ploc.make_unlined (12,12))
      (fun _ -> doit [("foo",(3,1))] {|abc{\foo{a}}|})
  ; assert_raises_exc_at (Ploc.make_unlined (10,13))
      (fun _ -> doit [("foo",(3,1))] {|abc\foo{a}def|})
  ; assert_equal ~cmp ~printer
      [{ it =
           `Command (
               ("x", [],
                [{ it =
                     `CommandGroup (
                         [{ it =
                              `Command (
                                  ("ref", [],
                                   [{ it =
                                        `CommandGroup ([{ it = `Text; text = "a"; loc = Ploc.dummy }]);
                                      text = "{a}"; loc = Ploc.dummy }
                                  ])
                                );
                            text = "\\ref{a}"; loc = Ploc.dummy };
                          { it = `Escape; text = "\\"; loc = Ploc.dummy };
                          { it = `CommandName; text = "label"; loc = Ploc.dummy };
                          { it = `CommandGroup ([{ it = `Text; text = "b"; loc = Ploc.dummy }]);
                            text = "{b}"; loc = Ploc.dummy }
                         ]
                       );
                   text = "{\\ref{a}\\label{b}}"; loc = Ploc.dummy }
               ])
             );
         text = "\\x{\\ref{a}\\label{b}}"; loc = Ploc.dummy }
      ]
      (doit [("x",(1,0));("ref",(1,0))] {|\x{\ref{a}\label{b}}|})
  ; assert_equal ~cmp ~printer
      [{ it =
           `Command (
               ("of", [],
                [{ it = `CommandGroup ([{ it = `Text; text = "xx"; loc = Ploc.dummy }]);
                   text = "{xx}"; loc = Ploc.dummy }
               ])
             );
         text = "\\of{xx}"; loc = Ploc.dummy }
      ]
      (doit [("of",(1,0))] {|\of{xx}|})

let suite = "Test latex_tools" >::: [
      "tokens"   >:: test_tokens
    ; "strip spaces after begin/end (stream)"   >:: test_strip_spaces
    ; "marking begin/end of environments (stream)" >:: test_begin_end
    ; "partial marking begin/end of environments (stream)" >:: test_partial_begin_end
    ; "coalesce begin/end of environments (stream)" >:: test_coalesce
    ; "partial coalesce begin/end of environments (stream)" >:: test_partial_coalesce
    ; "extract environments (stream)" >:: test_extract_environments
    ; "partial extract environments (stream)" >:: test_partial_extract_environments
    ; "coalesce groups (stream)" >:: test_coalesce_group
    ; "parse commands (stream)" >:: test_parse_commands
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

