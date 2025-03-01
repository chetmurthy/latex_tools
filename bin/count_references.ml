(**pp -syntax camlp5o -package pa_ppx_regexp,pa_ppx.runtime,pa_ppx.runtime_fat,pa_ppx.base,pa_ppx.testutils,pa_ppx.utils,latex_tools,bos *)

open OUnit2
open Pa_ppx_base
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

let iter_commands f strm =
  let open Visitors.Commands in
  let dt = make_dt () in
  let old_migrate_t_token = dt.migrate_t_token in
  let migrate_t_token dt tok =
    let tok = old_migrate_t_token dt tok in
    match tok.it with
      `Command _ ->
       f tok ;
       tok
    | _ -> tok in
  let dt = { dt with migrate_t_token = migrate_t_token } in
  Stream.iter (fun tok -> ignore(dt.migrate_t_token dt tok)) strm

let read_latex ~cmdmap (fname,ic) =
  ic
  |> Tools.stream_of_channel ~fname:fname
  |> transduce "strip-spaces" StripSpaceAfterBeginEnd.stream
  |> transduce "mark-environments" (MarkEnvironmentBeginEnd.stream)
  |> transduce "coalesce-groups" CoalesceGroups.stream
  |> transduce "commands" (Commands.stream ~cmdmap)

let pp_command tok =
  Fmt.(pf stdout "%s\n" tok.text)

let process_latex_file ~cmdmap f fname =
  let ic = open_in fname in
  let strm = read_latex ~cmdmap (fname,ic) in
  let rv = f strm in
  close_in ic ;
  rv

let add_labels_from_file h f =
  process_latex_file ~cmdmap:[("label",(1,0))] (fun strm ->
      let add_one = function
          {it=`Command("label",[],[{it=`CommandGroup [{it=`Text; text=name}]}])} ->
          Hashtbl.add h name (ref 0)
        | t -> Fmt.(pf stderr "%a unrecognized command: %a@."
                    string (Ploc.string_of_location t.loc)
                    Commands.pp_tex t) in
      iter_commands add_one strm)  f

let build_labels fl =
  let h = Hashtbl.create 23 in
  fl |> List.iter (fun f -> add_labels_from_file h f) ;
  h

let count_refs_from_file h f =
  process_latex_file ~cmdmap:[("ref",(1,0))] (fun strm ->
      let add_one = function
          {it=`Command("ref",[],[{it=`CommandGroup [{it=`Text; text=name}]}])} as t ->
           if Hashtbl.mem h name then
             incr (Hashtbl.find h name)
           else
             Fmt.(pf stderr "%a ref without label: %a@."
                    string (Ploc.string_of_location t.loc)
                    Commands.pp_tex t)
        | t -> Fmt.(pf stderr "%a unrecognized command: %a@."
                    string (Ploc.string_of_location t.loc)
                    Commands.pp_tex t) in
      iter_commands add_one strm) f

let count_refs h fl =
  fl |> List.iter (fun f -> count_refs_from_file h f)

let dump_count h =
  let l = Hashtbl.fold (fun k v acc -> (k,v)::acc) h [] in
  let l = List.stable_sort (fun (k1, _) (k2,_) -> Stdlib.compare k1 k2) l in
  l |> List.iter (fun (k,v) ->
      Fmt.(pf stdout "%s: %d\n" k !v))

if not !Sys.interactive then begin
    Arg.(parse [
             "-verbose", (Arg.Set verbose),
             "verbose"
           ]
           (Std.push rev_extra_args)
           "count_references <args> <input-files>") ;

    let input_files = List.rev !rev_extra_args  in
    let h = build_labels input_files in
    count_refs h input_files ;
    dump_count h
  end
;;
