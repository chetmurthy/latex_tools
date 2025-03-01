(**pp -syntax camlp5o -package pa_ppx_regexp,pa_ppx.runtime,pa_ppx.runtime_fat,pa_ppx.utils,pa_ppx.testutils,bos,pa_ppx.import,pa_ppx.deriving_plugins.std,pa_ppx_migrate *)

open OUnit2
open Pa_ppx_testutils
open Pa_ppx_base
open Ppxutil
open Pa_ppx_utils
open Latex_tokens

(* convert a LIST of RAW tokens into a list of raw tokens, BUT with
   begin/end environment markers recognized and converted into
   structures for further processing. *)

exception Migration_error of string

let migration_error feature =
  raise (Migration_error feature)

module StripSpaceAfterBeginEnd = struct

type t = [%import: Latex_tokens.t]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Latex_tokens
        ; dstmod = Latex_tokens
        ; types = [
            t
          ]
        }
      ]
    ; dispatchers = {
        migrate_t_token = {
          srctype = [%typ: t Latex_tokens.token]
        ; dsttype = [%typ: t Latex_tokens.token]
        ; code = (fun __dt__ x ->
                 { it = __dt__.migrate_t __dt__ x.it ; text = x.text ; loc = x.loc })
        }
      ; migrate_t_tokens = {
          srctype = [%typ: t Latex_tokens.token list]
        ; dsttype = [%typ: t Latex_tokens.token list]
        ; code = (fun __dt__ l ->
                    List.map (__dt__.migrate_t_token __dt__) l)
        }
      }

    }
]
end

module MarkEnvironmentBeginEnd = struct
type t = [%import: Texparse.MarkEnvironmentBeginEnd.t]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Texparse.MarkEnvironmentBeginEnd
        ; dstmod = Texparse.MarkEnvironmentBeginEnd
        ; types = [
            t
          ]
        }
      ]
    ; dispatchers = {
        migrate_t_token = {
          srctype = [%typ: t Latex_tokens.token]
        ; dsttype = [%typ: t Latex_tokens.token]
        ; code = (fun __dt__ x ->
                 { it = __dt__.migrate_t __dt__ x.it ; text = x.text ; loc = x.loc })
        }
      ; migrate_t_tokens = {
          srctype = [%typ: t Latex_tokens.token list]
        ; dsttype = [%typ: t Latex_tokens.token list]
        ; code = (fun __dt__ l ->
                    List.map (__dt__.migrate_t_token __dt__) l)
        }
      }

    }
]
end

module CoalesceEnvironments = struct

let _migrate_token subrw0 __dt__ t =
  { it = (subrw0 __dt__) t.it ; text = t.text ; loc = t.loc }

type t = [%import: Texparse.CoalesceEnvironments.t]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Texparse.CoalesceEnvironments
        ; dstmod = Texparse.CoalesceEnvironments
        ; types = [
            t
          ]
        }
      ]
    ; dispatchers = {
        migrate_t_token = {
          srctype = [%typ: t Latex_tokens.token]
        ; dsttype = [%typ: t Latex_tokens.token]
        ; code = (fun __dt__ x ->
                 { it = __dt__.migrate_t __dt__ x.it ; text = x.text ; loc = x.loc })
        }
      ; migrate_t_tokens = {
          srctype = [%typ: t Latex_tokens.token list]
        ; dsttype = [%typ: t Latex_tokens.token list]
        ; code = (fun __dt__ l ->
                    List.map (__dt__.migrate_t_token __dt__) l)
        }
      }
    }
]

end

