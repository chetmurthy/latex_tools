(**pp -syntax camlp5o -package pa_ppx_regexp,pa_ppx.runtime,pa_ppx.runtime_fat,pa_ppx.base,pa_ppx.testutils,pa_ppx.utils,latex_tools,bos,rresult *)

open OUnit2
open Pa_ppx_base
open Pa_ppx_testutils
open Pa_ppx_utils
open Ppxutil
open Latex_tools
open Texparse
open Parser_utils
open Tools

Pa_ppx_runtime.Exceptions.Ploc.pp_loc_verbose := true ;;
Pa_ppx_runtime_fat.Exceptions.Ploc.pp_loc_verbose := true ;;

type select_imports_t =
  All
| Exclude of string list
| Only of string list

let verbose = ref false ;;
let recursively = ref false ;;
let inplace = ref false ;;
let input_file = ref "" ;;
let output_file = ref "" ;;
let select_imports = ref All ;;

let exclude_imports pat =
  match !select_imports with
    All ->
    select_imports := Exclude [pat]
  | Exclude l -> select_imports := Exclude (l@[pat])
  | Only l ->
     Fmt.(failwithf "exclude: cannot exclude '%S' when we're already only expanding %a" pat
          (list ~sep:(const string " ") Dump.string) l)
;;
let only_expand pat =
  match !select_imports with
    All ->
    select_imports := Only [pat]
  | Only l -> select_imports := Only (l@[pat])
  | Exclude l ->
     Fmt.(failwithf "exclude: cannot only expand '%S' when we're already excluding %a" pat
          (list ~sep:(const string " ") Dump.string) l)
;;

let exclude_regexps = ref None
let only_expand_regexps = ref None

let build_regexps () =
  match !select_imports with
    All -> ()
  | Exclude l -> exclude_regexps := Some (List.map Pcre2.regexp l)
  | Only l -> only_expand_regexps := Some (List.map Pcre2.regexp l)

let process_arguments () =
  build_regexps () ;
  if !verbose then
    Fmt.(pf stderr "[Processed cmdline arguments]@.")

let filename_passes_selection fname =
  let rv =
    match (!exclude_regexps, !only_expand_regexps) with
      (None, None) -> true
    | (Some l, None) ->
       not(List.exists (fun rex -> Pcre2.pmatch ~rex fname) l)
    | (None, Some l) ->
       List.exists (fun rex -> Pcre2.pmatch ~rex fname) l
    | (Some _, Some _) -> assert false in
  if !verbose then
    Fmt.(pf stderr "%s %a@."
           (if rv then "expand" else "reject")
           Dump.string fname
    ) ;
  rv
;;

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

let cmdmap = [("include",(1,0)); ("import",(1,0))]

let rec pp_tex_expanding pps tok =
  let cmdmap = if !recursively then cmdmap else [] in
  match (tok : Commands.t token) with
    {it=`Command(("include"|"input"), [], [{it=`CommandGroup [{it=`Text; text=fname}]}])} ->
     let fname = if not (Std.ends_with ~pat:".tex" fname) then fname^".tex" else fname in
     if filename_passes_selection fname then
       process_latex_file ~cmdmap (fun strm ->
           Stream.iter (pp_tex_expanding pps) strm) fname
     else Commands.pp_tex pps tok
  | tok -> Commands.pp_tex pps tok

let expand_imports fname pps =
  process_latex_file ~cmdmap (fun strm ->
        Stream.iter (pp_tex_expanding pps) strm) fname

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
  expand_imports fname pps ;
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
           ; "-exclude", (Arg.String exclude_imports),
             "<filename-regexp> exclude imports matching this regexp (conflicts with -only-expand)"
           ; "-only-expand", (Arg.String only_expand),
             "<filename-regexp> only expand imports matching this regexp (conflicts with -exclude)"
           ]
           (fun s -> input_file := s)
           "resolve_imports <args> <input-file>") ;
    process_arguments () ;
    let input_file = !input_file  in
    resolve_imports input_file
  end
;;
