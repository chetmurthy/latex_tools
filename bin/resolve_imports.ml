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
let input_file = ref "" ;;

Pa_ppx_runtime.Exceptions.Ploc.pp_loc_verbose := true ;;
Pa_ppx_runtime_fat.Exceptions.Ploc.pp_loc_verbose := true ;;

let read_latex ~cmdmap (fname,ic) =
  ic
  |> Tools.stream_of_channel ~fname:fname
  |> transduce "strip-spaces" StripSpaceAfterBeginEnd.stream
  |> transduce "mark-environments" (MarkEnvironmentBeginEnd.stream)
  |> transduce "coalesce-groups" CoalesceGroups.stream
  |> transduce "commands" (Commands.stream ~cmdmap)

let process_latex_file ~cmdmap f fname =
  let ic = open_in fname in
  let strm = read_latex ~cmdmap (fname,ic) in
  let rv = f strm in
  close_in ic ;
  rv

let rec pp_tex_expanding pps tok =
  match (tok : Commands.t token) with
    {it=`Command("include", [], [{it=`CommandGroup [{it=`Text; text=fname}]}])} ->
     let fname = fname^".tex" in
    process_latex_file ~cmdmap:[] (fun strm ->
        Stream.iter (pp_tex_expanding pps) strm) fname
  | tok -> Commands.pp_tex pps tok

let resolve_imports fname =
  process_latex_file ~cmdmap:[("include",(1,0))] (fun strm ->
        Stream.iter (pp_tex_expanding Fmt.stdout) strm) fname
;;

if not !Sys.interactive then begin
    Arg.(parse [
             "-verbose", (Arg.Set verbose),
             "verbose"
           ]
           (fun s -> input_file := s)
           "resolve_imports <args> <input-file>") ;

    let input_file = !input_file  in
    resolve_imports input_file
  end
;;
