#include "version.inc"

let top = try Sys.getenv "TOP" with Not_found -> "" ;;
let destdir = ref (top ^ "/local-install") ;;

Arg.(parse [
         "-destdir", (Arg.String (fun s -> destdir := s)),
         ("<path>    Set the destination directory (default: " ^
	    !destdir ^ ")")
       ]
       (fun _ -> failwith "mk_meta: no anonymous args supported")
     "mk_meta -destdir <dir>")
;;

Fmt.(pf stdout {|
# Specifications for the "latex_tools" library:
requires = "pcre2,camlp5,fmt,pa_ppx.base,pa_ppx.utils,latex_tools_runtime,camlp5.parser_quotations,pa_ppx_regexp.runtime,sedlex,markup,bos"
archive(byte) = "latex_tools.cma"
archive(native) = "latex_tools.cmxa"
version = "%s"
description = "latex_tools: latex_tools library"

# For the toploop:
archive(byte,toploop) = "latex_tools.cma"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,fmt,pa_ppx.base,pa_ppx.utils,camlp5.parser_quotations,pa_ppx_regexp.runtime,sedlex,markup,bos"
  archive(syntax,preprocessor,-native) = "latex_tools.cma"
  archive(syntax,preprocessor,native) = "latex_tools.cmxa"

|} version)
