(** AST traversal for hoisting. *)

open Ppxlib

val hoist : loc:location -> ?name:string -> expression -> expression
(** [hoist ~loc ?name e] marks expression [e] to be hoisted.
    The optional [name] argument controls the fresh name generation of the
    generated variable. *)

val expand_hoisting : structure -> structure_item list
(** [expand_hoisting structure] hoists all expressions marked as hoisted in the
    given module. *)
