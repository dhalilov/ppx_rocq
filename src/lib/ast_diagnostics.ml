(** Methods for embedding diagnostics inside the OCaml AST. *)

open Ppxlib

let error ~loc format args =
  Ast_builder.Default.pexp_extension ~loc @@
    Location.error_extensionf ~loc format args
