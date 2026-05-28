(** Methods for detecting and parsing pattern variables. *)

open Ppxlib

(** Kind of pattern variables. *)
type kind =
  | First_order  (** [?name] *)
  | Second_order (** [@?name] *)

(** Type of pattern variables. *)
type t = private
  { name: Located_string.t; (** Name of the pattern variable. *)
    kind: kind              (** Kind of the pattern variable. *)
  }

val equal : t -> t -> bool
(** [equal v1 v2] returns [true] if [v1] and [v2] have the same name. *)

val find_all : loc:location -> string -> t list
(** [find_all ~loc string] finds all pattern variables in [string]. *)
