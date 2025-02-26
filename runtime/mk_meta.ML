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
# Specifications for the "latex_tools_runtime" package:
requires = "fmt,pa_ppx.base"
version = "%s"
description = "latex_tools runtime support"

# For linking
archive(byte) = "latex_tools_runtime.cmo"
archive(native) = "latex_tools_runtime.cmx"

# For the toploop:
archive(byte,toploop) = "latex_tools_runtime.cmo"

|} version)
