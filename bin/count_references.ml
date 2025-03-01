(**pp -syntax camlp5o -package pa_ppx_regexp,pa_ppx.runtime,pa_ppx.runtime_fat,pa_ppx.testutils,pa_ppx.utils,latex_tools,bos *)

open OUnit2
open Pa_ppx_testutils
open Pa_ppx_utils
open Latex_tools
open Texparse
open Parser_utils
open Tools

let verbose = ref false ;;
let rev_extra_args = ref [] ;;

Pa_ppx_runtime.Exceptions.Ploc.pp_loc_verbose := true ;;
Pa_ppx_runtime_fat.Exceptions.Ploc.pp_loc_verbose := true ;;

let filter_commands strm =
  let acc = ref [] in
  let open Visitors.Commands in
  let dt = make_dt () in
  let old_migrate_t_token = dt.migrate_t_token in
  let migrate_t_token dt tok =
    let tok = old_migrate_t_token dt tok in
    match tok.it with
      `Command _ ->
       Std.push acc tok ;
       tok
    | _ -> tok in
  let dt = { dt with migrate_t_token = migrate_t_token } in
  Stream.iter (fun tok -> ignore(dt.migrate_t_token dt tok)) strm ;
  List.rev !acc

let cmdmap = [("ref",(1,0)); ("label",(1,0))]

let read_latex fname =
  fname
  |> open_in
  |> Tools.stream_of_channel ~fname:fname
  |> transduce "strip-spaces" StripSpaceAfterBeginEnd.stream
  |> transduce "mark-environments" (MarkEnvironmentBeginEnd.stream)
  |> transduce "coalesce-groups" CoalesceGroups.stream
  |> transduce "commands" (Commands.stream ~cmdmap)

let pp_command tok =
  Fmt.(pf stdout "%s\n" tok.text)

let extract_commands f =
  let strm = read_latex f in
  let cmds = filter_commands strm in
  Fmt.(pf stdout "==== %s ====\n" f) ;
  List.iter pp_command cmds

if not !Sys.interactive then begin
    Arg.(parse [
             "-verbose", (Arg.Set verbose),
             "verbose"
           ]
           (Std.push rev_extra_args)
           "count_references <args> <input-files>") ;

    let input_files = List.rev !rev_extra_args  in
    input_files |> List.iter (fun f ->
                       extract_commands f)
  end
;;
