(**pp -syntax camlp5o -package pa_ppx_regexp,pa_ppx.runtime,pa_ppx.runtime_fat,pa_ppx.base,pa_ppx.testutils,pa_ppx.utils,bos,rresult *)

open Pa_ppx_base
open Pa_ppx_testutils
open Pa_ppx_utils
open Ppxutil
open Texparse
open Parser_utils
open Tools

module ExpandImports = struct

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

let filename_acceptor ?(verbose=false) ~only_expand ~exclude () =
  if only_expand <> [] && exclude <> [] then
    failwith "exclude: cannot only -either- expand or exclude, not both" ;
  let only_expand_regexps = List.map Pcre2.regexp only_expand in
  let exclude_regexps = List.map Pcre2.regexp exclude in
  (fun fname ->
    let rv =
      match (exclude_regexps, only_expand_regexps) with
        ([], []) -> true
      | (l, []) ->
         not(List.exists (fun rex -> Pcre2.pmatch ~rex fname) l)
      | ([], l) ->
         List.exists (fun rex -> Pcre2.pmatch ~rex fname) l
      | (_, _) -> assert false in
    if verbose then
      Fmt.(pf stderr "%s %a@."
             (if rv then "expand" else "reject")
             Dump.string fname
      ) ;
    rv)
;;

let cmdmap = [("include",(1,0)); ("import",(1,0))]

let rec pp_tex_expanding ?(verbose=false) ~recursively ~filename_acceptor pps tok =
  let cmdmap = if recursively then cmdmap else [] in
  match (tok : Commands.t token) with
    {it=`Command(("include"|"input"), [], [{it=`CommandGroup [{it=`Text; text=fname}]}])} ->
     let fname = if not (Std.ends_with ~pat:".tex" fname) then fname^".tex" else fname in
     if filename_acceptor fname then
       process_latex_file ~cmdmap (fun strm ->
           Stream.iter (pp_tex_expanding ~verbose ~recursively ~filename_acceptor pps) strm) fname
     else Commands.pp_tex pps tok
  | tok -> Commands.pp_tex pps tok

let file ?(verbose=false) ~recursively ~only_expand ~exclude fname pps =
  let filename_acceptor = filename_acceptor ~verbose ~only_expand ~exclude () in
  process_latex_file ~cmdmap (fun strm ->
        Stream.iter (pp_tex_expanding ~verbose ~recursively ~filename_acceptor pps) strm) fname
end
