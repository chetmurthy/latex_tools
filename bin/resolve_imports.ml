(**pp -syntax camlp5o -package pa_ppx_regexp,pa_ppx.runtime,pa_ppx.runtime_fat,pa_ppx.base,pa_ppx.testutils,pa_ppx.utils,latex_tools,bos,rresult *)

open Pa_ppx_base
open Pa_ppx_testutils
open Pa_ppx_utils
open Ppxutil
open Latex_tools
open Texparse
open Parser_utils
open Tools
open Transforms

Pa_ppx_runtime.Exceptions.Ploc.pp_loc_verbose := true ;;
Pa_ppx_runtime_fat.Exceptions.Ploc.pp_loc_verbose := true ;;

let verbose = ref false ;;
let recursively = ref false ;;
let inplace = ref false ;;
let input_file = ref "" ;;
let output_file = ref "" ;;
let only_expand = ref [] ;;
let exclude = ref [] ;;

let resolve_imports fname =
  let new_fname = fname^".NEW" in
  let (must_close, oc, pps) = match (!inplace, !output_file) with
      (false, "") ->
       (false, stdout, Fmt.stdout)
    | (false, f) ->
       let oc = f |> open_out in
       (true, oc, oc |> Format.formatter_of_out_channel)
    | (true, "") ->
       let oc = new_fname |> open_out in
       (true, oc, oc |> Format.formatter_of_out_channel)
    | (true, f) -> failwith "Cannot specify in-place ('-i') and an output file '-o')" in
  ExpandImports.stream
    ~verbose:!verbose
    ~recursively:!recursively
    ~exclude:!exclude
    ~only_expand:!only_expand
    fname
  |> Stream.iter (Commands.pp_tex pps) ;
  Format.pp_print_flush pps () ;
  flush oc;
  if must_close then close_out oc ;
  if !inplace then
    Bos.OS.U.(rename (Fpath.v new_fname) (Fpath.v fname) |> error_to_msg |> Rresult.R.failwith_error_msg)

if not !Sys.interactive then begin
    Arg.(parse [
             "-verbose", (Arg.Set verbose),
             "verbose"
           ; "-recursively", (Arg.Set recursively),
             "expand include/import recursively"
           ; "-i", (Arg.Set inplace),
             "replace the file in-place with expanded version"
           ; "-o", (Arg.Set_string output_file),
             "<filename> write expanded TeX to this file (conflicts with -i)"
           ; "-exclude", (Arg.String (Std.push exclude)),
             "<filename-regexp> exclude imports matching this regexp (conflicts with -only-expand)"
           ; "-only-expand", (Arg.String (Std.push only_expand)),
             "<filename-regexp> only expand imports matching this regexp (conflicts with -exclude)"
           ]
           (fun s -> input_file := s)
           "resolve_imports <args> <input-file>") ;
    let input_file = !input_file  in
    resolve_imports input_file
  end
;;
