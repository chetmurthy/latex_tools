(**pp -syntax camlp5o -package pa_ppx_regexp,pa_ppx.runtime,pa_ppx.runtime_fat,pa_ppx.base,pa_ppx.testutils,pa_ppx.utils,bos,rresult,markup *)

open Pa_ppx_base
open Pa_ppx_testutils
open Pa_ppx_utils
open Coll
open Ppxutil
open Texparse
open Parser_utils
open Tools
open Rresult

module MarkupIO = struct
open Markup
  
let stream_of_kstream ks = Stream.from (fun _ -> Markup.next ks)

let read_markup f =
  let (cs, closer) = file f in
  let strm = cs |> parse_html |> signals |> stream_of_kstream in
  [< strm ; (closer() ; [< >]) >]

let write_markup f strm =
  let open Markup in
  let oc = open_out f in
  let l = Std.list_of_stream strm in
      l
      |> of_list
      |> write_html
      |> to_channel oc ;
      close_out oc
end


module DiagnoseRefs = struct

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

module Fragment = struct
type t =
  GENERATED of int * string
| STRING of string

let pp_hum pps = function
    GENERATED (n, s) -> Fmt.(pf pps "x%d-%s" n s)
  | STRING s -> Fmt.(pf pps "%s" s)

let mk s =
  match [%match {|^x([1-9]\d*)-(\d+)$|} / strings (!1,!2)] s with
    None -> STRING s
  | Some(ns, s) -> GENERATED (int_of_string ns, s)

let is_generated = function
    GENERATED _ -> true
  | _ -> false

end

let pp_hum_href pps (fpart, frag) = Fmt.(pf pps "%s#%a" fpart Fragment.pp_hum frag)

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
  let open MarkupIO in
  let strm = read_markup f in
  strm |> hrefs_of |> Std.list_of_stream

let extract_ids f =
  let open MarkupIO in
  let strm = read_markup f in
  strm |> ids_of |> Std.list_of_stream

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
       Some (basef, Fragment.mk frag)
     else Some ("", Fragment.mk frag)
  | [fpart; frag] ->
     let fpart = Filename.basename fpart in
     if List.mem fpart file_basenames then
       Some (fpart, Fragment.mk frag)
     else begin
         Fmt.(pf stderr "resolve_href: ignore foreign URL %s\n" x) ;
         None
       end
  | [fpart] ->
     let fpart = Filename.basename fpart in
     if List.mem fpart file_basenames then
       Some (fpart, Fragment.mk "")
     else None

let suffix_of_generated =
  let open Fragment in
  function (_, GENERATED (_, s)) -> s
let filter_generated_ids l =
  let open Fragment in
  l |> List.filter_map (function
       (_, STRING _) -> None
     | (_, (GENERATED _) as id) -> Some id)

let ids_to_frag2file l =
  let partl = Utils.nway_partition snd Stdlib.compare l in
  let repeats = List.filter (function p -> List.length p > 1) partl in
  if repeats <> [] then begin
      Fmt.(pf stdout "ERROR ================ ids_to_frag2file: IDs are not distinct ================\n") ;
      repeats |> List.iter (fun part ->
                     let fpart = fst (List.hd part) in
                     Fmt.(pf stdout "==== %s ====\n" fpart) ;
                     part |> List.iter (fun (_,frag) -> Fmt.(pf stdout "%a\n" Fragment.pp_hum frag))
                   )
    end ;
  let m =
    partl
    |> List.map List.hd
    |> List.map (fun (f, frag) -> (frag, f))
    |> MHM.ofList 23 in
  (repeats,m)

let ids_to_suffix2id l =
  let open Fragment in
  let generated_ids = filter_generated_ids l in
  let partl = Utils.nway_partition suffix_of_generated Stdlib.compare generated_ids in
  let repeats = List.filter (function p -> List.length p > 1) partl in
  if repeats <> [] then begin
      Fmt.(pf stdout "======== ERROR ids_to_suffix2id: generated SUFFIXES of IDs are not distinct ========\n") ;
      repeats |> List.iter (fun part ->
                     let suff = suffix_of_generated(List.hd part) in
                     Fmt.(pf stdout "==== REPEATED SUFFIX %s ====\n" suff) ;
                     part |> List.iter (fun (f,frag) ->
                                 Fmt.(pf stdout "%a\n" pp_hum_href (f,frag)))
                   )
    end ;
  let m =
    partl
    |> List.map List.hd
    |> List.map (fun id -> (suffix_of_generated id, id))
    |> MHM.ofList 23 in
  (repeats, m)

let process_hrefs_ids file_basenames (f, (raw_hrefs, raw_ids)) =
  let basef = Filename.basename f in
  let ids = raw_ids |> List.map (fun frag -> (basef, Fragment.mk frag)) in
  let hrefs = raw_hrefs |> List.filter_map (resolve_href file_basenames (basef, raw_ids)) in
  (f, (hrefs, ids))

let fst_o_snd x = fst (snd x)
let snd_o_snd x = snd (snd x)

let fixup_file ~inplace f href_fixups =
  Fmt.(pf stdout "==== FIXUP %s ====\n" f) ;
  let href_fixups = href_fixups |> List.map (fun (frag, hr) ->
                                       (Fmt.(str "%a" Fragment.pp_hum frag),
                                        Fmt.(str "%a" pp_hum_href hr))) in
  let m = MHM.ofList 23 href_fixups in
  let contents = Bos.(f |> Fpath.v |> OS.File.read |> R.get_ok) in
  let subst s = if MHM.in_dom m s then
                  Fmt.(str "href='%s'" (MHM.map m s))
                else Fmt.(str "href='%s'" s) in
  let contents = [%subst {|href='#([^']+)'|} / {|subst $1$|} / pcre2 g e] contents in
  let newf = f^".NEW" in
  if not (R.is_ok (Bos.OS.File.write (Fpath.v newf) contents)) then
    Fmt.(failwithf "fixup_file %s: couldn't write result file %s" f newf) ;
  if inplace then
    Bos.OS.U.(rename (Fpath.v newf) (Fpath.v f) |> error_to_msg |> Rresult.R.failwith_error_msg)

let diagnose ~verbose ?(fixup=false) ?(inplace=false) fl =
  let file_basenames = List.map Filename.basename fl in
  let raw_hrefs_ids =
    fl |> List.map (fun f ->
              (f, (extract_hrefs f, extract_ids f))) in
  let hrefs_ids_list = List.map (process_hrefs_ids file_basenames) raw_hrefs_ids in
  let ids = List.concat_map snd_o_snd hrefs_ids_list in
  let idset = MHS.ofList ids 23 in
  let _,ids_frag2file = ids_to_frag2file ids in
  let _,ids_suffix2id = ids_to_suffix2id ids in
  
  if verbose > 2 then begin
      Fmt.(pf stdout "================ ids_frag2file ================\n") ;
      ids_frag2file
      |> MHM.toList
      |> List.sort Stdlib.compare
      |> List.iter (fun (frag, fpart) ->
             Fmt.(pf stdout "%a: %s\n" Fragment.pp_hum frag fpart)) ;
    end ;

  if verbose > 0 then begin
      Fmt.(pf stdout "%s\n" (String.make 80 '=')) ;
      Fmt.(pf stdout "%s\n" (String.make 80 '='))
    end ;

  let perfile_href_fixups =
    hrefs_ids_list
  |> List.map (fun (f, (hrefs, _)) ->
         if verbose > 0 then Fmt.(pf stdout "================ %s ================\n" f) ;
         (f,
          hrefs
          |> List.filter_map (fun (basef, frag as href) ->
                 if MHS.mem href idset then
                   None
                 else if basef <> "" then begin
                     Fmt.(pf stdout "REALLY BAD: href %a not found among IDs\n"
                            pp_hum_href href) ;
                     None
                   end
                 else if MHM.in_dom ids_frag2file frag then begin
                     if verbose > 0 then
                       Fmt.(pf stdout "%a: FIXABLE ERROR: href %a should have been %a\n"
                              Dump.string f
                              pp_hum_href href
                              pp_hum_href (MHM.map ids_frag2file frag, frag)) ;
                     Some (frag, (MHM.map ids_frag2file frag, frag))
                   end
                 else if Fragment.is_generated frag &&
                           MHM.in_dom ids_suffix2id (suffix_of_generated href) then begin
                     if verbose > 0 then
                       Fmt.(pf stdout "%a: FIXABLE ERROR: href %a MIGHT ought to have been %a\n"
                              Dump.string f
                              pp_hum_href href
                              pp_hum_href (MHM.map ids_suffix2id (suffix_of_generated href))) ;
                     Some (frag, (MHM.map ids_suffix2id (suffix_of_generated href)))
                   end
                 else begin
                     Fmt.(pf stdout "%a: UNFIXABLE ERROR: href %a not found anywhere in files\n"
                            Dump.string f
                            pp_hum_href href) ;
                     None
                   end
               )
          |> Std2.hash_uniq
         )
       ) in
  if fixup then
    perfile_href_fixups
    |> List.iter (fun (f, href_fixups) ->
           if href_fixups <> [] then
             fixup_file ~inplace f href_fixups)

end
