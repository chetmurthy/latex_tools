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
