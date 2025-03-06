(**pp -syntax camlp5o -package pa_ppx_regexp,pa_ppx.runtime,pa_ppx.runtime_fat,pa_ppx.base,pa_ppx.testutils,pa_ppx.utils,bos,rresult,markup *)

open Pa_ppx_base
open Pa_ppx_testutils
open Pa_ppx_utils
open Coll
open Ppxutil
open Texparse
open Parser_utils
open Tools

module DiagnoseRefs = struct

let stream_of_kstream ks = Stream.from (fun _ -> Markup.next ks)

let ids_of strm =
  let rec hrec = parser
    [< ' `Start_element (_,args) ; strm >] ->
      begin
        match args |> List.find_map (function ((_,"id"),_) as x -> Some x | _ -> None) with
          Some (_,rhs) -> [< 'rhs ; hrec strm >]
        | None -> hrec strm
      end
  | [< '_ ; strm >] -> hrec strm
  | [< >] -> [< >]
  in hrec strm

let hrefs_of strm =
  let rec hrec = parser
    [< ' `Start_element ((_,"a"),args) ; strm >] ->
      begin
        match args |> List.find_map (function ((_,"href"),_) as x -> Some x | _ -> None) with
          Some (_,rhs) -> [< 'rhs ; hrec strm >]
        | None -> hrec strm
      end
  | [< '_ ; strm >] -> hrec strm
  | [< >] -> [< >]
  in hrec strm

let extract_hrefs f =
  let open Markup in
  let (cs, closer) = file f in
  let strm = cs |> parse_html |> signals |> stream_of_kstream in
  let hrefs = strm |> hrefs_of |> Std.list_of_stream in
  closer() ;
  hrefs

let extract_ids f =
  let open Markup in
  let (cs, closer) = file f in
  let strm = cs |> parse_html |> signals |> stream_of_kstream in
  let ids = strm |> ids_of |> Std.list_of_stream in
  closer() ;
  ids

(** How to diagnose broken-refs in a collection of HTML files:

    (1) extract all the hrefs from each file F

        we keep hrefs with empty file-part, or a file-part that is
        equal to the basename of some file in our list.  All others
        are discarded.

    (2) extract all the ids from each file F

    (3) each href gets broken apart into its file-part and fragment (file-part might be empty)

    (4) and href in file F, with no file-part, that matches an ID
    declared in that file, gets the basename of F.  Otherwise, it gets
    an empty file-part.

    (5) each id is paired with the basename of the file F

    Questions to answer:

    (A) are there two IDs with different files but the same id ?

    (b) is there an href that doesn't match an ID (in this extended
    sense of pair (filename, fragment) ?  If so, then it's broken.

    (c) if there is a broken href, is there an ID in some file that
    matches the fragment-id of the broken href ?

 *)

let resolve_href file_basenames (basef, raw_ids) x =
  match String.split_on_char '#' x with
    ([] | _::_::_::_) ->
     Fmt.(failwithf "valid_local_href: really invalid href found: %a" Dump.string x)
  | [""; frag] ->
     if List.mem frag raw_ids then
       Some (basef, frag)
     else Some ("", frag)
  | [fpart; frag] ->
     let fpart = Filename.basename fpart in
     if List.mem fpart file_basenames then
       Some (fpart, frag)
     else begin
         Fmt.(pf stderr "resolve_href: ignore foreign URL %s\n" x) ;
         None
       end
  | [fpart] ->
     let fpart = Filename.basename fpart in
     if List.mem fpart file_basenames then
       Some (fpart, "")
     else None

let check_ids l =
  if not(Utils.distinct (List.map snd l)) then begin
    Fmt.(pf stdout "check_ids: IDs are not distinct") ;
    let partl = Utils.nway_partition snd Stdlib.compare l in
    let repeats = List.filter (function p -> List.length p > 1) partl in
    repeats |> List.iter (fun part ->
        let fpart = fst (List.hd part) in
        Fmt.(pf stdout "==== %s ====\n" fpart) ;
        part |> List.iter (fun (_,frag) -> Fmt.(pf stdout "%s\n" frag))
      )
  end

let process_hrefs_ids file_basenames (f, (raw_hrefs, raw_ids)) =
  let basef = Filename.basename f in
  let ids = raw_ids |> List.map (fun frag -> (basef, frag)) in
  let hrefs = raw_hrefs |> List.filter_map (resolve_href file_basenames (basef, raw_ids)) in
  (f, (hrefs, ids))

let fst_o_snd x = fst (snd x)
let snd_o_snd x = snd (snd x)

let diagnose fl =
  let file_basenames = List.map Filename.basename fl in
  let raw_hrefs_ids =
    fl |> List.map (fun f ->
              (f, (extract_hrefs f, extract_ids f))) in
  let hrefs_ids_list = List.map (process_hrefs_ids file_basenames) raw_hrefs_ids in
  let ids = List.concat_map snd_o_snd hrefs_ids_list in
  let idset = MHS.ofList ids 23 in
  let ids_frag2file = MHM.ofList 23 (List.map (fun (f, frag) -> (frag, f)) ids) in

  check_ids ids ;
  
  hrefs_ids_list
  |> List.iter (fun (f, (hrefs, _)) ->
         Fmt.(pf stdout "================ %s ================\n" f) ;
       hrefs |> List.iter (fun (basef, frag as href) ->
           if not(MHS.mem href idset) then
             if basef <> "" then
               Fmt.(pf stdout "REALLY BAD: href %s#%s not found among IDs\n" basef frag)
             else if MHM.in_dom ids_frag2file frag then
               Fmt.(pf stdout "FIXABLE ERROR: href #%s should have been %s#%s\n" frag (MHM.map ids_frag2file frag) frag)
             else Fmt.(pf stdout "UNFIXABLE ERROR: (file %a) href #%s not found anywhere in files\n"
                       Dump.string f
                         frag)
                  )
                    )

end
