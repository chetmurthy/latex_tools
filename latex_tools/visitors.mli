(**pp -syntax camlp5o -package pa_ppx_regexp,pa_ppx.runtime,pa_ppx.runtime_fat,pa_ppx.utils,pa_ppx.testutils,bos,pa_ppx.import,pa_ppx.deriving_plugins.std,pa_ppx_migrate *)

open OUnit2
open Pa_ppx_testutils
open Pa_ppx_base
open Ppxutil
open Pa_ppx_utils
open Latex_tokens

exception Migration_error of string

module StripSpaceAfterBeginEnd : sig

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

module MarkEnvironmentBeginEnd : sig
type t = [%import: Environments.MarkEnvironmentBeginEnd.t]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Environments.MarkEnvironmentBeginEnd
        ; dstmod = Environments.MarkEnvironmentBeginEnd
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

module CoalesceEnvironments : sig

type t = [%import: Environments.CoalesceEnvironments.t]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Environments.CoalesceEnvironments
        ; dstmod = Environments.CoalesceEnvironments
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

