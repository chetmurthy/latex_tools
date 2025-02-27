(**pp -syntax camlp5o -package pa_ppx_regexp,pa_ppx.runtime,pa_ppx.runtime_fat,pa_ppx.utils,pa_ppx.testutils,bos,pa_ppx.import,pa_ppx.deriving_plugins.std *)

open Latex_tokens

module StripSpaceAfterBeginEnd :
  sig
    type t = [%import: Latex_tokens.t][@@deriving show { with_path = false }, eq]

    val pp_tex : Format.formatter -> 'a token -> unit
    val list : t Latex_tokens.token list -> t token list
    val stream : t token Stream.t -> t token Stream.t
  end
module MarkEnvironmentBeginEnd :
  sig
    type t = [ Latex_tokens.t | `EnvironBegin of string | `EnvironEnd of string ][@@deriving show { with_path = false }, eq]

    val pp_tex : Format.formatter -> t token -> unit
    val list :
      Latex_tokens.t token list -> t token list
    val stream :
      Latex_tokens.t token Stream.t -> t token Stream.t
  end
module CoalesceEnvironments :
  sig
    module EM = MarkEnvironmentBeginEnd
    type t = [ EM.t | `Environment of string * t token list ][@@deriving show { with_path = false }, eq]
    val pp_tex : t token Fmt.t
    val list : EM.t token list -> t token list
    val stream :
      EM.t token Stream.t -> t token Stream.t
  end
