(**pp -package sedlex.ppx,pa_ppx.base *)

open Pa_ppx_base
open Ppxutil
open Latex_tokens

(*
0	Escape character—tells TEX to start looking for a command	\
1	Start a group	{
2	End a group	}
3	Math shift—switch in/out of math mode	$
4	Alignment tab	&
5	End of line	ASCII code 13 (\r)
6	Macro parameter	#
7	Superscript—for typesetting math: $y=x^2$ 
ˆ
8	Subscript—for typesetting math: $y=x_2$ 
_
9	Ignored character	ASCII 0 <null>
10	Spacer	ASCII codes 32 (space) and 9 (tab character)
11	Letter	A...Z, a...z, (and thousands of Unicode characters)
12	Other	0...9 plus ,.;? '"' and many others
13	Active character	Special category code for creating single-character macros such as ˜
14	Comment character—ignore everything that follows until the end of the line	%
15	Invalid character, not allowed to appear in the .tex input file	ASCII code 127 (DEL)
 *)



let cc_Escape = [%sedlex.regexp? "\\"]
let cc_GroupBegin = [%sedlex.regexp? "{"]
let cc_GroupEnd = [%sedlex.regexp? "}"]
let cc_BracketBegin = [%sedlex.regexp? "["]
let cc_BracketEnd = [%sedlex.regexp? "]"]
let cc_ParenBegin = [%sedlex.regexp? "("]
let cc_ParenEnd = [%sedlex.regexp? ")"]
let cc_MathSwitch = [%sedlex.regexp? "$"]
let cc_Alignment = [%sedlex.regexp? "&"]
let cc_EndOfLine = [%sedlex.regexp? '\n' | '\r']
let cc_Macro = [%sedlex.regexp? '#']
let cc_Superscript = [%sedlex.regexp? '^']
let cc_Subscript = [%sedlex.regexp? '_']
let cc_Ignored = [%sedlex.regexp? '\000']
let cc_Spacer = [%sedlex.regexp? ' ' | '\t']
let cc_Letter = [%sedlex.regexp? 'A'..'Z' | 'a'..'z' | tr8876_ident_char]
let cc_Active = [%sedlex.regexp? '~']
let cc_Comment = [%sedlex.regexp? '%']

let text_char = [%sedlex.regexp? Sub(
                      any, (cc_Escape | cc_GroupBegin | cc_GroupEnd | cc_MathSwitch | cc_BracketBegin| cc_BracketEnd | cc_Comment)) ]

(*

  >>> string.ascii_letters
'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
 *)

let ascii_printable = [%sedlex.regexp?
 '0'..'9' | 'a'..'z' | 'A'..'Z'
|'!'|'"'|'#'|'$'|'%'|'&'|'\\'|'('|')'|'*'|'+'|','|'-'|'.'|'/'|':'|';'|'<'|'='|'>'|'?'|'@'|'['|']'|'^'|'_'|'`'|'{'|'|'|'}'|'~'|' '|'\t'|'\n'|'\r'|'\x0b'|'\x0c' ]

let ascii_letters = [%sedlex.regexp? 'a'..'z' | 'A'..'Z' ]

let other_disallowed = [%sedlex.regexp? 
  '{'|'}'|'\\'|'$'|'&'|'\n'|'\r'|'#'|'^'|'_'|'~'|'%'|'\x00'|'\x7d'|' '|'\t'|'['|']'|'('|')'
  ]

let cc_Other = [%sedlex.regexp? Sub(ascii_printable, (ascii_letters | other_disallowed))]

(*
    'Letter',
    'Other',
    'Active',
    'Comment',
    'Invalid',

    # custom
    'MathGroupBegin',
    'MathGroupEnd',
    'BracketBegin',
    'BracketEnd',
    'ParenBegin',
    'ParenEnd'
 *)


let spacer = [%sedlex.regexp? ((Star (cc_Spacer), cc_EndOfLine, Star (cc_Spacer))
     | (cc_EndOfLine, Star (cc_Spacer))
     | Plus (cc_Spacer)
    )]

let locate buf =
  let (spos, epos) = Sedlexing.lexing_positions buf in
  Ploc.make_unlined (spos.Lexing.pos_cnum, epos.Lexing.pos_cnum)

let token buf =
  match%sedlex buf with
(*
  | Plus (cc_Spacer | cc_EndOfLine) -> [(MergedSpacer, Sedlexing.Utf8.lexeme buf, locate buf)]
 *)
  | spacer -> [(MergedSpacer, Sedlexing.Utf8.lexeme buf, locate buf)]
  | (cc_Comment, Plus(Sub(any, cc_EndOfLine))) -> [(Comment, Sedlexing.Utf8.lexeme buf, locate buf)]
  | (cc_Escape, (cc_Letter, Star (cc_Letter|'*'))) ->
     let lexeme = Sedlexing.Utf8.lexeme buf in
     let len = String.length lexeme in
     let pos = locate buf in
     [
       (Escape, "\\", pos)
     ; (CommandName, String.sub lexeme 1 (len - 1), pos)
     ]

  | (cc_Escape, 
     (cc_Escape | cc_GroupBegin | cc_GroupEnd | cc_MathSwitch |
      cc_Alignment | cc_EndOfLine | cc_Macro| cc_Superscript |
      cc_Subscript | cc_Spacer| cc_Active | cc_Comment | cc_Other)) ->
     [(EscapedComment, Sedlexing.Utf8.lexeme buf, locate buf)]

  | (cc_Escape, cc_BracketBegin) -> [(DisplayMathGroupBegin, Sedlexing.Utf8.lexeme buf, locate buf)]
  | (cc_Escape, cc_BracketEnd) -> [(DisplayMathGroupEnd, Sedlexing.Utf8.lexeme buf, locate buf)]
  | (cc_Escape, cc_ParenBegin) -> [(MathGroupBegin, Sedlexing.Utf8.lexeme buf, locate buf)]
  | (cc_Escape, cc_ParenEnd) -> [(MathGroupEnd, Sedlexing.Utf8.lexeme buf, locate buf)]

  | cc_Escape -> [(Escape, Sedlexing.Utf8.lexeme buf, locate buf)]
  | cc_GroupBegin -> [(GroupBegin, Sedlexing.Utf8.lexeme buf, locate buf)]
  | cc_GroupEnd -> [(GroupEnd, Sedlexing.Utf8.lexeme buf, locate buf)]
  | cc_BracketBegin -> [(BracketBegin, Sedlexing.Utf8.lexeme buf, locate buf)]
  | cc_BracketEnd -> [(BracketEnd, Sedlexing.Utf8.lexeme buf, locate buf)]
(*
  | (Sub(text_char, (cc_Spacer | cc_EndOfLine)), Star(text_char)) -> [(Text, Sedlexing.Utf8.lexeme buf, locate buf)]
 *)
  | (Opt spacer, Sub(text_char, (cc_Spacer | cc_EndOfLine)), Star(text_char)) -> [(Text, Sedlexing.Utf8.lexeme buf, locate buf)]
  | (cc_MathSwitch, cc_MathSwitch) -> [(DisplayMathSwitch, Sedlexing.Utf8.lexeme buf, locate buf)]
  | eof -> [(EOF, Sedlexing.Utf8.lexeme buf, locate buf)]
  | _ -> Fmt.(raise_failwithf (locate buf) "Latex_tokens.token: unrecognized")
