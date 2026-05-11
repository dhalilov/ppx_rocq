(** AST traversal for hoisting. *)

open Ppxlib

val hoist : loc:location -> expression -> expression
(** [hoist e] marks expression [e] to be hoisted. *)

val expand_hoisting : structure -> structure_item list
(** [expand_hoisting structure] hoists all expressions marked as hoisted in the
    given module. *)
