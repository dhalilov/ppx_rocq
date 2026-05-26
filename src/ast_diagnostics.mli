(** Methods for embedding diagnostics inside the OCaml AST. *)

open Ppxlib

val error : loc:location -> ('a -> extension, Format.formatter, unit, extension) format4 -> 'a -> expression
(** [error ~loc format args] creates an expression that embeds the error [format], formatted with the given [args]. *)

val warn : loc:location -> string -> expression -> expression
(** [warn ~loc warning expr] attaches the given warning message to [expr]. *)

val warn' : string loc list -> expression -> expression
(** [warn' warnings expr] attaches the given list of warnings to [expr]. *)
