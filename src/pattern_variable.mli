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

module Set : (Set.S with type elt := t)
(** Sets of pattern variables. *)

val find_all : loc:location -> string -> Set.t
(** [find_all ~loc string] finds all pattern variables in [string]. *)
