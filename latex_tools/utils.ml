(**pp -syntax camlp5o -package pa_ppx_regexp,pa_ppx.runtime,pa_ppx.runtime_fat,pa_ppx.utils,pa_ppx.testutils,bos,pa_ppx.import,pa_ppx.deriving_plugins.std *)

open OUnit2
open Pa_ppx_testutils
open Pa_ppx_base
open Ppxutil
open Pa_ppx_utils

let report_transducer_stream_error extract_ok report_underlying transduced_strm =
  let peek ok strm =
    match Stream.peek strm with
      _ -> [< >]
    | exception (Stream.Error _) ->
       report_underlying ok in
  let instrument_head ok strm =
    [< peek ok strm ; strm >] in

  let rec instrument_rest = parser
    [< 't ; strm >] ->
      let ok = extract_ok t in
      [< 't ; instrument_rest (instrument_head (Some ok) strm) >]
  | [< >] -> [< >] in

  instrument_head None [< instrument_rest transduced_strm >]


let make_loc ?(fname="") ?(line_nb=1) ?(bol_pos=0) ?(comm="") (bp,ep) =
  Ploc.make_loc fname line_nb bol_pos (bp,ep) comm

let nway_partition keyfun keycmp l =
  let l = List.sort (fun a b -> keycmp (keyfun a) (keyfun b)) l in
  let take k l =
    let rec takerec acc = function
        [] -> (List.rev acc, [])
      | h::t when 0 == keycmp k (keyfun h) ->
         takerec (h::acc) t
      | l -> (List.rev acc, l) in
    takerec [] l in
  let rec partrec acc = function
      [] -> List.rev acc
    | (h::_) as l ->
       let k = keyfun h in
       let (part, l) = take k l in
       partrec (part::acc) l
  in
  partrec [] l

let distinct l =
  let l = List.sort Stdlib.compare l in
  let rec drec = function
      ([]|[_]) -> true
    | h1::h2::t when 0 = Stdlib.compare h1 h2 -> false
    | h1::h2::t -> drec (h2::t)
  in drec l
