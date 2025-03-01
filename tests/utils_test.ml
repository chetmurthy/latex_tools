(**pp -syntax camlp5o -package pa_ppx.testutils,pa_ppx.utils,latex_tools,pa_ppx.deriving_plugins.std *)
open OUnit2
open Pa_ppx_testutils
open Pa_ppx_utils
open Latex_tools
open Tools
open Texparse

let printer s = s

exception ReportedStreamError of string
let report1 name strm =
  let report_underlying () =
    raise (ReportedStreamError name) in
  Utils.report_transducer_stream_error report_underlying strm

let rec firstn_stream n strm =
  if n = 0 then [< >]
  else (parser
          [< 't ; strm >] -> [< 't ; firstn_stream (n-1) strm >]
       | [< >] -> [< >]) strm

let test_utils ctxt =
  ()
  ; assert_equal [();()] (Std.list_of_stream (firstn_stream 3 [< '() ; '() >]))
  ; assert_equal [();();()] (Std.list_of_stream (firstn_stream 3 [< '() ; '() ; '() ; (raise (Stream.Error "")) >]))
  ; assert_raises (Stream.Error"") (fun _ -> Std.list_of_stream [< (raise (Stream.Error "")) >])
  ; assert_raises (Stream.Error"") (fun _ -> Std.list_of_stream [< '() ; '() ; '() ; (raise (Stream.Error "")) >])
  ; assert_raises (Stream.Error"") (fun _ -> Std.list_of_stream (firstn_stream 4 [< '() ; '() ; '() ; (raise (Stream.Error "")) >]))

  ; assert_raises (ReportedStreamError"foo") (fun _ -> Std.list_of_stream (report1 "foo" [< (raise (Stream.Error "")) >]))
  ; assert_raises (ReportedStreamError"foo") (fun _ -> Std.list_of_stream (report1 "foo" [< '() ; '() ; '() ; (raise (Stream.Error "")) >]))
  ; assert_raises (ReportedStreamError"foo") (fun _ -> Std.list_of_stream (firstn_stream 4 (report1 "foo" [< '() ; '() ; '() ; (raise (Stream.Error "")) >])))
  ; assert_equal [();();()] (Std.list_of_stream (firstn_stream 3 (report1 "foo" [< '() ; '() ; '() ; (raise (Stream.Error "")) >])))


let transform1 = parser
  [< '"a" ; '"b" ; '"c" ; strm >] -> [< '"a" ; '"b" ; '"c" ; strm >]
| [< '"b" ; strm >] -> [< '"b" ; strm >]
| [< >] -> [< >]

let report2 ~underlying strm =
  let report_underlying () =
    match Stream.peek underlying with
      Some n -> raise (ReportedStreamError n)
    | None -> raise (ReportedStreamError "<end-of-stream>") in
  Utils.report_transducer_stream_error report_underlying strm

let transduce transformer strm =
  report2 ~underlying:strm [< (transformer strm) >]

let test_parser ctxt =
  ()
  ; assert_equal [] (Std.list_of_stream (transform1 [< >]))
  ; assert_equal ["b"] (Std.list_of_stream (transform1 [< '"b" >]))
  ; assert_equal ["a";"b";"c"] (Std.list_of_stream (transform1 [< '"a" ; '"b" ; '"c" >]))
  ; assert_raises (ReportedStreamError "d") (fun _ -> Std.list_of_stream (transduce transform1 [< '"a" ; '"d" >]))

let suite = "Test utils" >::: [
      "utils"   >:: test_utils
    ; "parser"   >:: test_parser
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

