(**pp -syntax camlp5o -package camlp5,pa_ppx.base,pa_ppx.utils,pa_ppx.deriving_plugins.show,pa_ppx.import,sedlex *)

open Pa_ppx_utils

open Latex_tokens

type loc = [%import: Latex_tokens.loc][@@deriving show { with_path = false }]
let equal_loc = equal_loc

type t = [%import: Latex_tokens.t][@@deriving show { with_path = false }, eq]
type 'a token = [%import: 'a Latex_tokens.token][@@deriving show { with_path = false }, eq]

let pp_tex = pp_tex

let stream_of_string s : t token Stream.t =
  let lexbuf = Sedlexing.Utf8.from_string s in
  stream_of_tokens Latex_lexer.token lexbuf

let stream_of_channel ?fname ic : t token Stream.t =
  let lexbuf = Sedlexing.Utf8.from_channel ic in
  (match fname with
     None -> ()
   | Some f -> Sedlexing.set_filename lexbuf f) ;
  stream_of_tokens Latex_lexer.token lexbuf

let list_of_string s : t token list =
  let lexbuf = Sedlexing.Utf8.from_string s in
  list_of_tokens Latex_lexer.token lexbuf

let list_of_channel ?fname ic : t token list =
  let lexbuf = Sedlexing.Utf8.from_channel ic in
  (match fname with
     None -> ()
   | Some f -> Sedlexing.set_filename lexbuf f) ;
  list_of_tokens Latex_lexer.token lexbuf

let list_to_tex pp1 l = List.map pp1 l
let stream_to_tex pp1 l = Std.stream_map pp1 l

let stream_to_string = stream_to_string
let list_to_string = list_to_string
