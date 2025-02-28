(**pp -syntax camlp5o -package pa_ppx_regexp,pa_ppx.runtime,pa_ppx.runtime_fat,pa_ppx.testutils,pa_ppx.utils,latex_tools,bos *)

open OUnit2
open Pa_ppx_testutils
open Pa_ppx_utils
open Latex_tools
open Environments
open Tools

let comments = ref false
let check_dollar = ref false
let check_percent = ref false
let check_caret = ref false
let check_backslash = ref false
let check_doublebackslash = ref false
let environments = ref []
let rev_extra_args = ref []
;;

Pa_ppx_runtime.Exceptions.Ploc.pp_loc_verbose := true ;;
Pa_ppx_runtime_fat.Exceptions.Ploc.pp_loc_verbose := true ;;

let filter_environments pred strm =
  let acc = ref [] in
  let open Visitors.CoalesceEnvironments in
  let dt = make_dt () in
  let old_migrate_t_token = dt.migrate_t_token in
  let migrate_t_token dt tok =
    let tok = old_migrate_t_token dt tok in
    match tok.it with
      `Environment (name, cl) ->
       if pred tok then Std.push acc tok ;
       tok
    | _ -> tok in
  let dt = { dt with migrate_t_token = migrate_t_token } in
  Stream.iter (fun tok -> ignore(dt.migrate_t_token dt tok)) strm ;
  List.rev !acc

let read_latex_environments fname =
  fname
  |> open_in
  |> Tools.list_of_channel ~fname:fname
  |> StripSpaceAfterBeginEnd.list
  |> MarkEnvironmentBeginEnd.list
  |> CoalesceEnvironments.list
  |> Std.stream_of_list

let pp_environment tok =
  Fmt.(pf stdout "%s\n" tok.text)

let extract_environments el f =
  let pred = function
      {it=`Environment (name, cl)} -> List.mem name el
    | _ -> false in
  let strm = read_latex_environments f in
  let envs = filter_environments pred strm in
  Fmt.(pf stdout "==== %s ====\n" f) ;
  List.iter pp_environment envs

if not !Sys.interactive then begin
    Arg.(parse [
             "-with-comments", (Arg.Set comments),
             "check comments as well"
           ; "-check-all", (Arg.Unit (fun s ->
                                check_dollar := true
                                      ; check_percent := true
                                      ; check_backslash := true
                           )),
             "check for $, %, \\"
           ; "-check-dollar", (Arg.Unit (fun s ->
                                   check_dollar := true
                              )),
             "check for $"
           ; "-check-percent", (Arg.Unit (fun s ->
                                    check_percent := true
                               )),
             "check for %"
           ; "-check-caret", (Arg.Unit (fun s ->
                                    check_caret := true
                               )),
             "check for ^"
           ; "-check-backslash", (Arg.Unit (fun s ->
                                      check_backslash := true
                                 )),
             "check for \\"
           ; "-check-doublebackslash", (Arg.Unit (fun s ->
                                            check_doublebackslash := true
                                       )),
             "check for \\\\"
           ; "-environment", (Arg.String (fun s -> Std.push environments s)),
             "<name>    extract named environment"
           ]
           (Std.push rev_extra_args)
           "extract_environments <args> <input-file>") ;

    let el = List.rev !environments in
    let input_files = List.rev !rev_extra_args  in
    input_files |> List.iter (fun f -> extract_environments el f)
  end
;;
