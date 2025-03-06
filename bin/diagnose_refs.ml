(**pp -syntax camlp5o -package pa_ppx_regexp,pa_ppx.runtime,pa_ppx.runtime_fat,pa_ppx.testutils,pa_ppx.utils,latex_tools,bos,markup *)

open OUnit2
open Pa_ppx_testutils
open Pa_ppx_utils
open Latex_tools
open Texparse
open Tools

let verbose = ref false
let fixup = ref false
let rev_extra_args = ref []
;;

Pa_ppx_runtime.Exceptions.Ploc.pp_loc_verbose := true ;;
Pa_ppx_runtime_fat.Exceptions.Ploc.pp_loc_verbose := true ;;

if not !Sys.interactive then begin
    Arg.(parse [
             "-verbose", (Arg.Set verbose),
             "verbose"
           ; "-fixup", (Arg.Set verbose),
             "fixup the files (in-place update) for those problems that this tool thinks are fixable"
           ]
           (Std.push rev_extra_args)
           "diagnose_refs <args> <input-files>") ;

    let input_files = List.rev !rev_extra_args  in
    let verbose = !verbose in
    let fixup = !fixup in
    Html.DiagnoseRefs.diagnose ~verbose ~fixup input_files
  end
;;
