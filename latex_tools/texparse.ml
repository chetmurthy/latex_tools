(**pp -syntax camlp5o -package pa_ppx_regexp,pa_ppx.runtime,pa_ppx.runtime_fat,pa_ppx.utils,pa_ppx.testutils,bos,pa_ppx.import,pa_ppx.deriving_plugins.std *)

open OUnit2
open Pa_ppx_testutils
open Pa_ppx_base
open Ppxutil
open Pa_ppx_utils
open Latex_tokens

let plist_until terminator elem = 
  let rec plist_rec acc = parser
      [< rv=terminator >] -> rv acc
    | [< e = elem; strm >] -> plist_rec (e::acc) strm
  in plist_rec []

let plistn n elem = 
  let rec plist_rec n accum strm =
    if n = 0 then List.rev accum
    else
      (parser
         [< e = elem; strm >] -> plist_rec (n-1) (e::accum) strm) strm
  in plist_rec n []

let plist_atmostn n elem = 
  let rec plist_rec n accum strm =
    if n = 0 then List.rev accum
    else
      (parser
         [< e = elem; strm >] -> plist_rec (n-1) (e::accum) strm
      | [< >]                 -> (List.rev accum)) strm
  in plist_rec n []


(* convert a LIST of RAW tokens into a list of raw tokens, BUT with
   begin/end environment markers recognized and converted into
   structures for further processing. *)

module StripSpaceAfterBeginEnd = struct

type t = [%import: Latex_tokens.t][@@deriving show { with_path = false }, eq]

let pp_tex = Latex_tokens.pp_tex

let stream strm =
  let rec conv = parser
    [< '{it=`Escape} as tok1 ; '{it=`CommandName; text="begin"} as tok2 ;
       _=Std.plist (parser [< '{it=`MergedSpacer} >] -> ()) ; strm >] -> [< 'tok1 ; 'tok2 ; conv strm >]

  | [< '{it=`Escape} as tok1 ; '{it=`CommandName; text="end"} as tok2 ;
       _=Std.plist (parser [< '{it=`MergedSpacer} >] -> ()) ; strm >] -> [< 'tok1 ; 'tok2 ; conv strm >]

  | [< '{it=`Escape} as tok1 ; strm >] -> [< 'tok1 ; conv strm >]

  | [< 't ; strm >] -> [< 't ; conv strm >]
  | [< >] -> [< >]
 in conv strm
end

module MarkEnvironmentBeginEnd = struct
type t = [ Latex_tokens.t | `EnvironBegin of string | `EnvironEnd of string ][@@deriving show { with_path = false }, eq]

let pp_tex pps (t : t token) =
  match t with
    {it=`EnvironBegin s} -> Fmt.(pf pps "\\begin{%s}" s) ;
  | {it=`EnvironEnd s} -> Fmt.(pf pps "\\end{%s}" s) ;
  | {text=s} -> Fmt.(pf pps "%s" s)

let stream ?(environs=[]) strm =
  let name_pred n = environs = [] || List.mem n environs in
  let rec conv = parser
    [< '{it=`Escape} as tok1; '{it=`CommandName; text="begin"} as tok2 ;
       '{it=`GroupBegin} as tok3; '{it=`Text} as tok4; '{it=`GroupEnd} as tok5 ; strm >] ->
      if  name_pred tok4.text then
        let loc = Ploc.encl tok1.loc tok5.loc in
        let t : t token = {it=`EnvironBegin tok4.text; text=tok1.text^tok2.text^tok3.text^tok4.text^tok5.text; loc} in
        [< 't ; conv strm >]
      else [< 'tok1; 'tok2; 'tok3 ; 'tok4 ; 'tok5 ; conv strm >]

  | [< '{it=`Escape} as tok1; '{it=`CommandName; text="begin"} as tok2 ; '{it=`GroupBegin} as tok3; '{it=`Text} as tok4 ; strm >] ->
      [< 'tok1; 'tok2; 'tok3 ; 'tok4 ; conv strm >]

  | [< '{it=`Escape} as tok1; '{it=`CommandName; text="begin"} as tok2 ; '{it=`GroupBegin} as tok3 ; strm >] ->
      [< 'tok1; 'tok2; 'tok3 ; conv strm >]

  | [< '{it=`Escape} as tok1; '{it=`CommandName; text="begin"} as tok2 ; strm >] -> [< 'tok1 ; 'tok2 ; conv strm >]

  | [< '{it=`Escape} as tok1; '{it=`CommandName; text="end"} as tok2 ;
       '{it=`GroupBegin} as tok3; '{it=`Text} as tok4; '{it=`GroupEnd} as tok5 ; strm >] ->
      if  name_pred tok4.text then
      let loc = Ploc.encl tok1.loc tok5.loc in
      let t : t token = {it=`EnvironEnd tok4.text; text=tok1.text^tok2.text^tok3.text^tok4.text^tok5.text; loc} in
      [< 't ; conv strm >]
      else [< 'tok1; 'tok2; 'tok3 ; 'tok4 ; 'tok5 ; conv strm >]

  | [< '{it=`Escape} as tok1; '{it=`CommandName; text="end"} as tok2 ; '{it=`GroupBegin} as tok3; '{it=`Text} as tok4 ; strm >] ->
      [< 'tok1; 'tok2; 'tok3 ; 'tok4 ; conv strm >]

  | [< '{it=`Escape} as tok1; '{it=`CommandName; text="end"} as tok2 ; '{it=`GroupBegin} as tok3 ; strm >] ->
      [< 'tok1; 'tok2; 'tok3 ; conv strm >]

  | [< '{it=`Escape} as tok1; '{it=`CommandName; text="end"} as tok2 ; strm >] -> [< 'tok1 ; 'tok2 ; conv strm >]

  | [< '{it=`Escape} as tok1 ; strm >] -> [< 'tok1 ; conv strm >]

 | [< '(t : Latex_tokens.t token) ; strm >] -> [< '(t : Latex_tokens.t token :> t token) ; conv strm >]
  | [< >] -> [< >]
 in conv strm

end

module CoalesceEnvironments = struct
module EM = MarkEnvironmentBeginEnd
type t = [ EM.t | `Environment of string * t token list ][@@deriving show { with_path = false }, eq]

let rec pp_tex pps (t : t token) =
  match t with
    {it=`Environment (name, cl)} ->
     Fmt.(pf pps "\\begin{%s}%a\\end{%s}" name (list ~sep:nop pp_tex) cl name)
  | {it=`EnvironBegin s} -> Fmt.(pf pps "\\begin{%s}" s) ;
  | {it=`EnvironEnd s} -> Fmt.(pf pps "\\end{%s}" s) ;
  | {text=s} -> Fmt.(pf pps "%s" s)

let rec pa_environment ~environs ~pa_child = parser
  [< '({it=`EnvironBegin name} as begin_tok) when environs=[] || List.mem name environs ;
     cl=plist_until
          (parser
             [< '({it=`EnvironEnd name'} as end_tok) when environs=[] || List.mem name' environs >] ->
           (fun rev_cl ->
             if name = name' then
               let ttok_cl = List.rev rev_cl in
               let ttok_text = Fmt.(str "%s%a%s" begin_tok.text (list ~sep:nop pp_tex) ttok_cl end_tok.text) in
               let ttok_loc = Ploc.encl begin_tok.loc end_tok.loc in
               {it=`Environment(name, ttok_cl); text=ttok_text; loc = ttok_loc}
             else raise (Stream.Error "")))
          pa_child
 >] -> cl

let stream ?(environs=[]) strm =
  let name_pred n = environs = [] || List.mem n environs in
  let rec conv  = parser
      [< '({it=`EnvironEnd name'} as tok) when name_pred name' >] ->
      Fmt.(raise_failwithf tok.loc "CoalesceEnvironments: top-level end-environment %s without matching begin-environment" name')
    | [< c = conv_child >] -> [< 'c ; conv strm >]
    | [< >] -> [< >]

  and conv_child = parser
    | [< c = pa_environment ~environs ~pa_child:conv_child >] -> c
    | [< 't >] -> (t : EM.t token :> t token)

  in conv strm

end

module CoalesceGroups = struct
module EM = MarkEnvironmentBeginEnd
type t = [ EM.t | `Group of t token list |  `Bracket of t token list ][@@deriving show { with_path = false }, eq]

let upcast (x : t) : EM.t =
  (match x with
    `Escape -> `Escape
  | `GroupBegin -> `GroupBegin
  | `GroupEnd -> `GroupEnd
  | `Comment -> `Comment
  | `MergedSpacer -> `MergedSpacer
  | `EscapedComment -> `EscapedComment
  | `MathSwitch -> `MathSwitch
  | `DisplayMathSwitch -> `DisplayMathSwitch
  | `MathGroupBegin -> `MathGroupBegin
  | `MathGroupEnd -> `MathGroupEnd
  | `DisplayMathGroupBegin -> `DisplayMathGroupBegin
  | `DisplayMathGroupEnd -> `DisplayMathGroupEnd
  | `LineBreak -> `LineBreak
  | `CommandName -> `CommandName
  | `Text -> `Text
  | `BracketBegin -> `BracketBegin
  | `BracketEnd -> `BracketEnd
  | `ParenBegin -> `ParenBegin
  | `ParenEnd -> `ParenEnd
  | `PunctuationCommandName -> `PunctuationCommandName
  | `SizeCommand -> `SizeCommand
  | `Spacer -> `Spacer
  | `EOF -> `EOF
  | `EnvironBegin s -> `EnvironBegin s
  | `EnvironEnd s -> `EnvironEnd s
  | `Group _ -> failwith "CoalesceGroup.upcast: `Group should never occur"
  | `Bracket _ -> failwith "CoalesceGroup.upcast: `Bracke should never occur")[@warnerror "+8"]

let upcast_token t =
  { it = upcast t.it ; text = t.text ; loc = t.loc }

let rec pp_tex pps (t : t token) =
  match t with
    {it=`Group cl} ->
     Fmt.(pf pps "{%a}" (list ~sep:nop pp_tex) cl)
  | {it=`Bracket cl} ->
     Fmt.(pf pps "[%a]" (list ~sep:nop pp_tex) cl)
  | {text=s} -> Fmt.(pf pps "%s" s)

let rec pa_group ~pa_child = parser
  [< '({it=`GroupBegin} as begin_tok) ;
     cl=plist_until
          (parser
             [< '({it=`GroupEnd} as end_tok) >] ->
           (fun rev_cl ->
             let ttok_cl = List.rev rev_cl in
             let ttok_text = Fmt.(str "{%a}" (list ~sep:nop pp_tex) ttok_cl) in
             let ttok_loc = Ploc.encl begin_tok.loc end_tok.loc in
             {it=`Group ttok_cl; text=ttok_text; loc = ttok_loc}))
          pa_child
 >] -> cl

let rec pa_bracket ~pa_child = parser
  [< '({it=`BracketBegin} as begin_tok) ;
     cl=plist_until
          (parser
             [< '({it=`BracketEnd} as end_tok) >] ->
           (fun rev_cl ->
             let ttok_cl = List.rev rev_cl in
             let ttok_text = Fmt.(str "{%a}" (list ~sep:nop pp_tex) ttok_cl) in
             let ttok_loc = Ploc.encl begin_tok.loc end_tok.loc in
             {it=`Bracket ttok_cl; text=ttok_text; loc = ttok_loc}))
          pa_child
 >] -> cl

let stream strm =
  let rec conv  = parser
      [< '({it=`GroupEnd} as tok) >] ->
      Fmt.(raise_failwithf tok.loc "CoalesceGroups: top-level GroupEnd without matching GroupBegin")
    | [< '({it=`BracketEnd} as tok) >] ->
      Fmt.(raise_failwithf tok.loc "CoalesceGroups: top-level BracketEnd without matching BracketBegin")
    | [< c = conv_child >] -> [< 'c ; conv strm >]
    | [< >] -> [< >]

  and conv_child = parser
    | [< c = pa_group ~pa_child:conv_child >] -> c
    | [< c = pa_bracket ~pa_child:conv_child >] -> c
    | [< 't >] -> (t : EM.t token :> t token)

  in conv strm

end

module Commands = struct
module EM = MarkEnvironmentBeginEnd
module CG = CoalesceGroups
type t = [ EM.t | `CommandGroup of t token list |  `CommandBracket of t token list | `Command of string * t token list * t token list ][@@deriving show { with_path = false }, eq]

let wrap (l,r) ppx pps x = Fmt.(pf pps "%s%a%s" l ppx x r)

let rec pp_tex pps (t : t token) =
  match t with
    {it=`Command(name, optargs, args)} ->
     Fmt.(pf pps "\\%s%a%a" name
            (list ~sep:nop pp_tex) optargs
            (list ~sep:nop pp_tex) args)
  | {it=`CommandGroup cl} ->
     Fmt.(pf pps "{%a}" (list ~sep:nop pp_tex) cl)
  | {it=`CommandBracket cl} ->
     Fmt.(pf pps "[%a]" (list ~sep:nop pp_tex) cl)
  | {text=s} -> Fmt.(pf pps "%s" s)

let stream ~cmdmap (strm : CG.t token Stream.t) : t token Stream.t =
  let rec conv = parser
      [< '({it=`Escape} as tok1) ;
         '({it=`CommandName} as tok2) when List.mem_assoc tok2.text cmdmap ; strm >] ->
        begin
          let name = tok2.text in
          match List.assoc name cmdmap with
          | (m,n) when 0<=n && n <= 1 && n <= m ->
             (parser
                [< optargs = plist_atmostn n (parser [< '{it=`Bracket _} as t >] -> t) ;
                 args = plistn (m-n) (parser [< '{it=`Group _} as t >] -> t) ; strm >] ->
              let optargs = List.map conv1 optargs in
              let args = List.map conv1 args in
              let ttok_text = Fmt.(str "\\%s%a%a" name
                                     (list ~sep:nop pp_tex) optargs
                                     (list ~sep:nop pp_tex) args) in
              let ttok_loc = Ploc.encl tok1.loc (Std.last args).loc in
              [< '{it=`Command (name, optargs, args); text=ttok_text; loc = ttok_loc} ; conv strm >]) strm

          | (m,n) ->
             Fmt.(failwithf "Commands.stream: #optional #args=%d, #args=%d: #optional must be in [0,1); #optional <= #args" m n)
        end

    | [< '({it=`Escape} as tok1) ; '({it=`CommandName} as tok2) ; strm >] ->
        [< '(tok1: EM.t token :> t token) ; '(tok2 : EM.t token :> t token) ; conv strm >]

    | [< '({it=`Escape} as tok1) ; strm >] ->
        [< '(tok1: EM.t token :> t token) ; conv strm >]

    | [< '({it=`Group _} as tok) ; strm >] -> [< '(conv1 tok) ; conv strm >]
    | [< '({it=`Bracket _} as tok) ; strm >] -> [< '(conv1 tok) ; conv strm >]

    | [< 'c ; strm >] -> [< '((CG.upcast_token c) : EM.t token :> t token) ; conv strm >]
    | [< >] -> [< >]

  and conv1 = function
      {it=`Group l} as tok -> {it=`CommandGroup (conv_list l); text = tok.text ; loc = tok.loc}
    | {it=`Bracket l} as tok -> {it=`CommandBracket (conv_list l); text = tok.text ; loc = tok.loc}
    | _ -> assert false

  and conv_list l =
    l |> Std.stream_of_list |> conv |> Std.list_of_stream

  in conv strm

end
