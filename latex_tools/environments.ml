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

let list (l : t token list) =
  let rec conv (rev_lhs : t token list) = function
      ({it=`Escape} as tok1)
      ::({it=`CommandName; text="begin"} as tok2)
      ::{it=`MergedSpacer}
      ::tl
      ->
       conv rev_lhs (tok1::tok2::tl)

    | ({it=`Escape} as tok1)
      ::({it=`CommandName; text="begin"} as tok2)
      ::tl
      ->
       conv (tok2::tok1::rev_lhs) tl

    | ({it=`Escape} as tok1)
      ::({it=`CommandName; text="end"} as tok2)
      ::{it=`MergedSpacer}
      ::tl
      ->
       conv rev_lhs (tok1::tok2::tl)

    | ({it=`Escape} as tok1)
      ::({it=`CommandName; text="end"} as tok2)
      ::tl
      ->
       conv (tok2::tok1::rev_lhs) tl

    | (t : t token)::tl -> conv ((t : t token :> t token)::rev_lhs) tl
    | [] -> List.rev rev_lhs

  in conv ([] : t token list) l

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

let list (l : Latex_tokens.t token list) =
  let rec conv (rev_lhs : t token list) = function
      ({it=`Escape} as tok1)
      ::({it=`CommandName; text="begin"} as tok2)
      ::({it=`GroupBegin} as tok3)
      ::({it=`Text} as tok4)
      ::({it=`GroupEnd} as tok5)
      ::tl
      ->
      let loc = Ploc.encl tok1.loc tok5.loc in
      let t : t token = {it=`EnvironBegin tok4.text; text=tok1.text^tok2.text^tok3.text^tok4.text^tok5.text; loc} in
      conv (t::rev_lhs) tl

    | ({it=`Escape} as tok1)
      ::({it=`CommandName; text="end"} as tok2)
      ::({it=`GroupBegin} as tok3)
      ::({it=`Text} as tok4)
      ::({it=`GroupEnd} as tok5)
      ::tl
      ->
      let loc = Ploc.encl tok1.loc tok5.loc in
      let t : t token = {it=`EnvironEnd tok4.text; text=tok1.text^tok2.text^tok3.text^tok4.text^tok5.text; loc} in
      conv (t::rev_lhs) tl

    | (t : Latex_tokens.t token)::tl -> conv ((t : Latex_tokens.t token :> t token)::rev_lhs) tl
    | [] -> List.rev rev_lhs

  in conv ([] : t token list) l

let stream strm =
  let rec conv = parser
    [< '{it=`Escape} as tok1; '{it=`CommandName; text="begin"} as tok2 ;
       '{it=`GroupBegin} as tok3; '{it=`Text} as tok4; '{it=`GroupEnd} as tok5 ; strm >] ->
      let loc = Ploc.encl tok1.loc tok5.loc in
      let t : t token = {it=`EnvironBegin tok4.text; text=tok1.text^tok2.text^tok3.text^tok4.text^tok5.text; loc} in
      [< 't ; conv strm >]

  | [< '{it=`Escape} as tok1; '{it=`CommandName; text="begin"} as tok2 ; '{it=`GroupBegin} as tok3; '{it=`Text} as tok4 ; strm >] ->
      [< 'tok1; 'tok2; 'tok3 ; 'tok4 ; conv strm >]

  | [< '{it=`Escape} as tok1; '{it=`CommandName; text="begin"} as tok2 ; '{it=`GroupBegin} as tok3 ; strm >] ->
      [< 'tok1; 'tok2; 'tok3 ; conv strm >]

  | [< '{it=`Escape} as tok1; '{it=`CommandName; text="begin"} as tok2 ; strm >] -> [< 'tok1 ; 'tok2 ; conv strm >]

  | [< '{it=`Escape} as tok1; '{it=`CommandName; text="end"} as tok2 ;
       '{it=`GroupBegin} as tok3; '{it=`Text} as tok4; '{it=`GroupEnd} as tok5 ; strm >] ->
      let loc = Ploc.encl tok1.loc tok5.loc in
      let t : t token = {it=`EnvironEnd tok4.text; text=tok1.text^tok2.text^tok3.text^tok4.text^tok5.text; loc} in
      [< 't ; conv strm >]

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


let list (l : EM.t token list) : t token list =
  let rec conv (rev_lhs : t token list) = function
      ({it=`EnvironEnd name'} as tok)::tl ->
      Fmt.(raise_failwithf tok.loc "CoalesceEnvironments: top-level end-environment %s without matching begin-environment" name')
    | ({it=`EnvironBegin name} as begin_tok)::tl ->
       let (c, tl) = conv_children (name,begin_tok) [] tl in
       conv (c::rev_lhs) tl

    | t::tl -> conv ((t : EM.t token :> t token)::rev_lhs) tl
    | [] -> List.rev rev_lhs

  and conv_children (name, begin_tok) rev_lhs = function
      ({it=`EnvironEnd name'} as end_tok)::tl when name=name' ->
       let ttok_cl = List.rev rev_lhs in
       let ttok_text = Fmt.(str "%s%a%s" begin_tok.text (list ~sep:nop pp_tex) ttok_cl end_tok.text) in
       let ttok_loc = Ploc.encl begin_tok.loc end_tok.loc in
       let ttok = {it=`Environment(name, ttok_cl); text=ttok_text; loc = ttok_loc} in
       (ttok, tl)

    | ({it=`EnvironEnd name'} as tok)::tl ->
      Fmt.(raise_failwithf tok.loc "CoalesceEnvironments: end-environment %s when looking for end-environment %s" name' name)

    | ({it=`EnvironBegin name'} as begin_tok')::tl ->
       let (c, tl) = conv_children (name', begin_tok') [] tl in
       conv_children (name, begin_tok) (c::rev_lhs) tl

    | t::tl -> conv_children (name, begin_tok) ((t : EM.t token :> t token)::rev_lhs) tl
    | [] -> Fmt.(failwithf "CoalesceEnvironments: EOF when looking for end-environment %s" name)

  in conv [] l


let stream strm =
  let rec conv  = parser
      [< '({it=`EnvironEnd name'} as tok) >] ->
      Fmt.(raise_failwithf tok.loc "CoalesceEnvironments: top-level end-environment %s without matching begin-environment" name')
    | [< '({it=`EnvironBegin name} as begin_tok) ; c = conv_children (name,begin_tok) [] ; strm >] ->
       [< 'c ; conv strm >]
    | [< 't ; strm >] -> [< '(t : EM.t token :> t token) ; conv strm >]
    | [< >] -> [< >]

  and conv_children (name, begin_tok) rev_lhs = parser
      [< '({it=`EnvironEnd name'} as end_tok) when name=name' >] ->
       let ttok_cl = List.rev rev_lhs in
       let ttok_text = Fmt.(str "%s%a%s" begin_tok.text (list ~sep:nop pp_tex) ttok_cl end_tok.text) in
       let ttok_loc = Ploc.encl begin_tok.loc end_tok.loc in
       {it=`Environment(name, ttok_cl); text=ttok_text; loc = ttok_loc}

    | [< '({it=`EnvironEnd name'} as tok) >] ->
       Fmt.(raise_failwithf tok.loc "CoalesceEnvironments: end-environment %s when looking for end-environment %s" name' name)

    | [< '({it=`EnvironBegin name'} as begin_tok') ; c = conv_children (name', begin_tok') [] ; strm >] ->
       conv_children (name, begin_tok) (c::rev_lhs) strm

    | [< 't ; strm >] -> conv_children (name, begin_tok) ((t : EM.t token :> t token)::rev_lhs) strm
    | [< >] -> Fmt.(failwithf "CoalesceEnvironments: EOF when looking for end-environment %s" name)

  in conv strm

end
