(**pp -syntax camlp5o -package camlp5,pa_ppx.base,pa_ppx.utils,pa_ppx.deriving_plugins.show *)

open Pa_ppx_base
open Pa_ppx_utils
open Ppxutil

type t =
  [ `Escape
  | `GroupBegin
  | `GroupEnd
  | `Comment
  | `MergedSpacer
  | `EscapedComment
  | `MathSwitch
  | `DisplayMathSwitch
  | `MathGroupBegin
  | `MathGroupEnd
  | `DisplayMathGroupBegin
  | `DisplayMathGroupEnd
  | `LineBreak
  | `CommandName
  | `Text
  | `BracketBegin
  | `BracketEnd
  | `ParenBegin
  | `ParenEnd
  | `PunctuationCommandName
  | `SizeCommand
  | `Spacer
  | `EOF ] [@@deriving show { with_path = false }]

type 'a token = { it : 'a ; text :  string ; loc : Ploc.t }

let pp_tex pps t = Fmt.(pf pps "%s" t.text)

let list_of_tokens token lexbuf =
  let rec lrec acc =
    match token lexbuf with
      [{it=`EOF}] -> List.rev acc
    | t -> lrec ((List.rev t) @ acc)
  in lrec []

let stream_of_tokens token lexbuf =
  let rec lrec () =
    match token lexbuf with
      [{it=`EOF}] -> [< >]
    | l -> [< Std.stream_of_list l ; lrec () >]
  in lrec ()

let list_to_string pp1 l =
  Fmt.(str "%a" (list ~sep:nop pp1) l)

let stream_to_string pp1 strm =
  Fmt.(str "%a" (iter ~sep:nop Stream.iter pp1) strm)
