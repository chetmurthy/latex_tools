(**pp -syntax camlp5o -package camlp5,pa_ppx.base,pa_ppx.deriving_plugins.show *)

open Pa_ppx_base
open Ppxutil

type t =
  | Escape
  | GroupBegin
  | GroupEnd
  | Comment
  | MergedSpacer
  | EscapedComment
  | MathSwitch
  | DisplayMathSwitch
  | MathGroupBegin
  | MathGroupEnd
  | DisplayMathGroupBegin
  | DisplayMathGroupEnd
  | LineBreak
  | CommandName
  | Text
  | BracketBegin
  | BracketEnd
  | ParenBegin
  | ParenEnd
  | PunctuationCommandName
  | SizeCommand
  | Spacer
  | EOF [@@deriving show { with_path = false }]
