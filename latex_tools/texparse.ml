(**pp -syntax camlp5o -package pa_ppx_regexp,pa_ppx.runtime,pa_ppx.runtime_fat,pa_ppx.utils,pa_ppx.testutils,bos,pa_ppx.import,pa_ppx.deriving_plugins.std *)

open OUnit2
open Pa_ppx_testutils
open Pa_ppx_base
open Ppxutil
open Pa_ppx_utils
open Latex_tokens

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

let plist_until terminator elem = 
  let rec plist_rec acc = parser
      [< rv=terminator >] -> rv acc
    | [< e = elem; strm >] -> plist_rec (e::acc) strm
  in plist_rec []


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

let rec pp_tex pps (t : t token) =
  match t with
    {it=`Group cl} ->
     Fmt.(pf pps "{%a}" (list ~sep:nop pp_tex) cl)
  | {it=`Bracket cl} ->
     Fmt.(pf pps "[%a]" (list ~sep:nop pp_tex) cl)
  | {text=s} -> Fmt.(pf pps "%s" s)

let plist_until terminator elem = 
  let rec plist_rec acc = parser
      [< rv=terminator >] -> rv acc
    | [< e = elem; strm >] -> plist_rec (e::acc) strm
  in plist_rec []


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
(*
module Commands = struct
module CG = CoalesceGroups
type t = [ CG.t | `Command of string * t token list * t token list ][@@deriving show { with_path = false }, eq]

wrap (l,r) ppx pps x = Fmt.(pf pps "%s%a%s" l ppx x r)

let rec pp_tex pps (t : t token) =
  match t with
    {it=`Command(name, optargs, args)} ->
     Fmt.(pf pps "\\%s{%a}" name
            (list ~sep:nop (wrap ("[","]") pp_tex)) optargs
            (list ~sep:nop (wrap ("(",")") pp_tex)) args)
  | {text=s} -> Fmt.(pf pps "%s" s)

let plist_until terminator elem = 
  let rec plist_rec acc = parser
      [< rv=terminator >] -> rv acc
    | [< e = elem; strm >] -> plist_rec (e::acc) strm
  in plist_rec []

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
 *)
