(** Methods for embedding diagnostics inside the OCaml AST. *)

open Ppxlib

let error ~loc format args =
  Ast_builder.Default.pexp_extension ~loc @@
    Location.error_extensionf ~loc format args

let warn ~loc warning expr =
  let warning = Ast_builder.Default.estring ~loc warning in
  [%expr [%e expr] [@ocaml.ppwarning [%e warning]]]

let warn' warnings expr =
  List.fold_left (fun expr warning -> warn ~loc:warning.loc warning.txt expr) expr warnings
