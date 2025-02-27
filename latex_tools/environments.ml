(**pp -syntax camlp5o -package pa_ppx_regexp,pa_ppx.runtime,pa_ppx.runtime_fat,pa_ppx.utils,pa_ppx.testutils,bos *)

open OUnit2
open Pa_ppx_testutils
open Pa_ppx_utils
open Latex_tokens

(* convert a LIST of RAW tokens into a list of raw tokens, BUT with
   begin/end environment markers recognized and converted into
   structures for further processing. *)

module StripSpaceAfterBeginEnd = struct
let list (l : t token list) =
  let rec conv (rev_lhs : t token list) = function
      ({it=`Escape} as tok1)
      ::({it=`CommandName; text="begin"} as tok2)
      ::{it=`MergedSpacer}
      ::tl
      ->
       conv rev_lhs (tok1::tok2::tl)

    | ({it=`Escape} as tok1)
      ::({it=`CommandName; text="end"} as tok2)
      ::{it=`MergedSpacer}
      ::tl
      ->
       conv rev_lhs (tok1::tok2::tl)

    | (t : t token)::tl -> conv ((t : t token :> t token)::rev_lhs) tl
    | [] -> List.rev rev_lhs

  in conv ([] : t token list) l

let stream strm =
  let rec conv = parser
    [< '{it=`Escape} as tok1 ; '{it=`CommandName; text="begin"} as tok2 ; '{it=`MergedSpacer} ;
       _=Std.plist (parser [< '{it=`MergedSpacer} >] -> ()) ; strm >] -> [< 'tok1 ; 'tok2 ; conv strm >]

  | [< '{it=`Escape} as tok1 ; '{it=`CommandName; text="end"} as tok2 ; '{it=`MergedSpacer} ;
       _=Std.plist (parser [< '{it=`MergedSpacer} >] -> ()) ; strm >] -> [< 'tok1 ; 'tok2 ; conv strm >]

  | [< 't ; strm >] -> [< 't ; conv strm >]
  | [< >] -> [< >]
 in conv strm
end

module MarkEnvironmentBeginEnd = struct
type t = [ Latex_tokens.t | `EnvironBegin of string | `EnvironEnd of string ]

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
      let t : t token = {it=`EnvironBegin tok4.text; text=tok1.text^tok2.text^tok3.text^tok4.text^tok5.text; loc} in
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

  | [< '{it=`Escape} as tok1; '{it=`CommandName; text="end"} as tok2 ;
       '{it=`GroupBegin} as tok3; '{it=`Text} as tok4; '{it=`GroupEnd} as tok5 ; strm >] ->
      let loc = Ploc.encl tok1.loc tok5.loc in
      let t : t token = {it=`EnvironEnd tok4.text; text=tok1.text^tok2.text^tok3.text^tok4.text^tok5.text; loc} in
      [< 't ; conv strm >]

  | [< 't ; strm >] -> [< 't ; conv strm >]
  | [< >] -> [< >]
 in conv strm

end

module CoalesceEnvironments = struct
module EM = MarkEnvironmentBeginEnd
type t = [ EM.t | `Environment of string * (string * string) * t token list ]

let rec pp_tex pps t =
  match t with
    {it=`Environment (name, (begin_text, end_text), cl)} ->
     Fmt.(pf pps "%s%a%s" begin_text (list ~sep:nop pp_tex) cl end_text)
  | {it=`EnvironBegin s} -> Fmt.(pf pps "\\begin{%s}" s) ;
  | {it=`EnvironEnd s} -> Fmt.(pf pps "\\end{%s}" s) ;
  | {text=s} -> Fmt.(pf pps "%s" s)

let list (l : EM.t token list) : t token list =
  let rec conv (rev_lhs : t token list) stk = function
      (t: EM.t token)::tl ->
       begin
         match (stk, t) with
           (((name, begin_text), rev_cl)::stk, {it=`EnvironEnd name'}) when name = name' ->
            let ttok = {it=`Environment(name,(begin_text, t.text), List.rev rev_cl);
                        text="<environment>"; loc=Ploc.dummy} in
            let (rev_lhs, stk) = match (rev_lhs, stk) with
                (rev_lhs, ((name, begin_text), rev_cl)::stk) ->
                 (rev_lhs, ((name, begin_text), (ttok::rev_cl))::stk)
              | (rev_lhs, []) ->
                 (ttok::rev_lhs, []) in
            conv rev_lhs stk tl

         | (((name, begin_text), rev_cl)::stk, {it=`EnvironEnd name'}) ->
            let ttok = {it=`Environment(name,(begin_text, "<no end element found>"), List.rev rev_cl);
                        text="<bad environment>"; loc=Ploc.dummy} in
            let (rev_lhs, stk) = match (rev_lhs, stk) with
                (rev_lhs, ((name, begin_text), rev_cl)::stk) ->
                 (rev_lhs, ((name, begin_text), (ttok::rev_cl))::stk)
              | (rev_lhs, []) ->
                 (ttok::rev_lhs, []) in
            conv rev_lhs stk tl

         | ([], {it=`EnvironEnd name'}) ->
            conv ((t : EM.t token :> t token)::rev_lhs) [] tl

         | (stk, {it=`EnvironBegin name}) ->
            conv rev_lhs (((name, t.text), [])::stk) tl

         | (((name, begin_text), rev_cl)::stk, t) ->
            conv rev_lhs (((name, begin_text), ((t : EM.t token :> t token)::rev_cl))::stk) tl

         | (stk, t) ->
            conv ((t : EM.t token :> t token)::rev_lhs) stk tl
       end

    | [] -> List.rev rev_lhs
  in conv ([] : t token list) [] l

let stream strm =
  let rec conv stk = parser
    [< 't ; strm >] ->
       begin
         match (stk, t) with
           (((name, begin_text), rev_cl)::stk, {it=`EnvironEnd name'}) when name = name' ->
            let ttok = {it=`Environment(name,(begin_text, t.text), List.rev rev_cl);
                        text="<environment>"; loc=Ploc.dummy} in
            begin
              match stk with
                ((name, begin_text), rev_cl)::stk ->
                 conv (((name, begin_text), (ttok::rev_cl))::stk) strm
              | [] ->
                 [< 'ttok ; conv [] strm >]
            end
         
         | (((name, begin_text), rev_cl)::stk, {it=`EnvironEnd name'}) ->
            let ttok = {it=`Environment(name,(begin_text, "<no end element found>"), List.rev rev_cl);
                        text="<bad environment>"; loc=Ploc.dummy} in
            begin
              match stk with
                ((name, begin_text), rev_cl)::stk ->
                 conv  (((name, begin_text), (ttok::rev_cl))::stk) strm
              | [] ->
                 [< 'ttok ; conv [] strm >]
            end
         
         | ([], {it=`EnvironEnd name'}) ->
            [< '(t : EM.t token :> t token) ; conv [] strm >]
         
         | (stk, {it=`EnvironBegin name}) ->
            conv (((name, t.text), [])::stk) strm
         
         | (((name, begin_text), rev_cl)::stk, t) ->
            conv (((name, begin_text), ((t : EM.t token :> t token)::rev_cl))::stk) strm
         
         | (stk, t) ->
            [< '(t : EM.t token :> t token); conv stk strm >]
       end
  | [< >] -> [< >]
  in conv [] strm

end
