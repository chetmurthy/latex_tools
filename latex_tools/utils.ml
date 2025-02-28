(**pp -syntax camlp5o -package pa_ppx_regexp,pa_ppx.runtime,pa_ppx.runtime_fat,pa_ppx.utils,pa_ppx.testutils,bos,pa_ppx.import,pa_ppx.deriving_plugins.std *)

open OUnit2
open Pa_ppx_testutils
open Pa_ppx_base
open Ppxutil
open Pa_ppx_utils

let report_transducer_stream_error report_underlying transduced_strm =
  let peek strm =
    match Stream.peek strm with
      _ -> [< >]
    | exception (Stream.Error _) ->
       report_underlying() in
  let instrument_head strm =
    [< peek strm ; strm >] in

  let rec instrument_rest = parser
    [< 't ; strm >] -> [< 't ; instrument_rest (instrument_head strm) >]
  | [< >] -> [< >] in

  instrument_head [< instrument_rest transduced_strm >]
