(**pp -syntax camlp5o -package pa_ppx.testutils,pa_ppx.utils,pa_ppx.deriving_plugins.std *)

open Latex_tokens

open Pa_ppx_runtime.Exceptions
type t +=
  ReportedStreamError of string * loc option * (string * loc) option[@name "ReportedStreamError"]
  [@@deriving show]


let report_token_transducer_error name ~underlying strm =
  let report_underlying tokopt =
    match Stream.peek underlying with
      Some tok ->
       raise (ReportedStreamError (name, tokopt, Some (tok.text, tok.loc)))
    | None -> raise (ReportedStreamError (name, tokopt, None)) in
  Utils.report_transducer_stream_error(fun tok -> tok.loc) report_underlying strm

let transduce name transformer strm =
  report_token_transducer_error name ~underlying:strm [< (transformer strm) >]
