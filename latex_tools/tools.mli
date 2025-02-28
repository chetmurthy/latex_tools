(**pp -syntax camlp5o -package camlp5,pa_ppx.base,pa_ppx.utils,pa_ppx.deriving_plugins.show,pa_ppx.import,sedlex *)

type loc = [%import: Latex_tokens.loc][@@deriving show { with_path = false }, eq]
type t = [%import: Latex_tokens.t][@@deriving show { with_path = false }, eq]
type 'a token = [%import: 'a Latex_tokens.token][@@deriving show { with_path = false }, eq]

val pp_tex : t token Fmt.t
val stream_of_string : string -> t token Stream.t
val stream_of_channel : ?fname:string -> in_channel -> t token Stream.t
val list_of_string : string -> t token list
val list_of_channel : ?fname:string -> in_channel -> t token list
val list_to_tex : ('a -> 'b) -> 'a list -> 'b list
val stream_to_tex : ('a -> 'b) -> 'a Stream.t -> 'b Stream.t
val stream_to_string : 'a Fmt.t -> 'a Stream.t -> string
val list_to_string : 'a Fmt.t -> 'a list -> string
