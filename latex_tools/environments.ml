(**pp -syntax camlp5o -package pa_ppx_regexp,pa_ppx.runtime,pa_ppx.runtime_fat,pa_ppx.testutils,latex_tools,bos *)

open OUnit2
open Pa_ppx_testutils
open Latex_tokens

(* convert a LIST of RAW tokens into a list of raw tokens, BUT with
   begin/end environment markers recognized and converted into
   structures for further processing. *)

let strip_spaces_after_begin_end (l : t_token list) =
  let rec conv (rev_lhs : t_token list) = function
      ((`Escape, t1, loc1) as tok1)
      ::((`CommandName, ("begin" as t2), _) as tok2)
      ::(`MergedSpacer, _, _)
      ::((`GroupBegin, t3, _) as tok3)
      ::tl
      ->
       conv rev_lhs (tok1::tok2::tok3::tl)

    | ((`Escape, t1, loc1) as tok1)
      ::((`CommandName, ("end" as t2), _) as tok2)
      ::(`MergedSpacer, _, _)
      ::((`GroupBegin, t3, _) as tok3)
      ::tl
      ->
       conv rev_lhs (tok1::tok2::tok3::tl)

    | (t : t_token)::tl -> conv ((t : t_token :> t_token)::rev_lhs) tl
    | [] -> List.rev rev_lhs

  in conv ([] : t_token list) l

type t' = [ t | `EnvironBegin of string | `EnvironEnd of string ]
type t'_token = (t' * string * Ploc.t)

let mark_environment_begin_end (l : t_token list) =
  let rec conv (rev_lhs : t'_token list) = function
      (`Escape, t1, loc1)
      ::(`CommandName, ("begin" as t2), _)
      ::(`GroupBegin, t3, _)
      ::(`Text, (name as t4), _)
      ::(`GroupEnd, t5, loc2)
      ::tl
      ->
      let loc = Ploc.encl loc1 loc2 in
      let t : t'_token = (`EnvironBegin name, t1^t2^t3^t4^t5, loc) in
      conv (t::rev_lhs) tl

    | (`Escape, t1, loc1)
      ::(`CommandName, ("end" as t2), _)
      ::(`GroupBegin, t3, _)
      ::(`Text, (name as t4), _)
      ::(`GroupEnd, t5, loc2)
      ::tl
      ->
      let loc = Ploc.encl loc1 loc2 in
      let t = (`EnvironEnd name, t1^t2^t3^t4^t5, loc) in
      conv (t::rev_lhs) tl

    | (t : t_token)::tl -> conv ((t : t_token :> t'_token)::rev_lhs) tl
    | [] -> List.rev rev_lhs

  in conv ([] : t'_token list) l


type t'' = [ t' | `Environment of string * (string * string) * t'_token list ]
type t''_token = (t'' * string * Ploc.t)

let coalesce_environments (l : t'_token list) : t''_token list =
  let rec conv (rev_lhs : t''_token list) stk = function
      (t: t'_token)::tl ->
       begin
         match (stk, t) with
           (((name, begin_text), rev_cl)::stk, (`EnvironEnd name', end_text, _)) when name = name' ->
            let ttok = `Environment(name,(begin_text, end_text), List.rev rev_cl) in
            conv ((ttok, "<environment>", Ploc.dummy)::rev_lhs) stk tl

         | (((name, begin_text), rev_cl)::stk, (`EnvironEnd name', end_text, _)) ->
            let ttok = `Environment(name,(begin_text, "<no end element found>"), List.rev rev_cl) in
            conv ((ttok, "<bad environment>", Ploc.dummy)::rev_lhs) stk (t::tl)

         | ([], (`EnvironEnd name', end_text, _)) ->
            conv ((t : t'_token :> t''_token)::rev_lhs) [] tl

         | (stk, (`EnvironBegin name, begin_text, _)) ->
            conv rev_lhs (((name, begin_text), [])::stk) tl

         | (((name, begin_text), rev_cl)::stk, t) ->
            conv rev_lhs (((name, begin_text), (t::rev_cl))::stk) tl

         | (stk, t) ->
            conv ((t : t'_token :> t''_token)::rev_lhs) stk tl
       end

    | [] -> List.rev rev_lhs
  in conv ([] : t''_token list) [] l
