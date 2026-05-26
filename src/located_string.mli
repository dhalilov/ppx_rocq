(** Representation of strings with a source location. *)

open Ppxlib

(** We say that a string is {e located} if it has a corresponding location
    in the source code. *)

(** {1 Positions} *)

(** Type of positions inside a located string. *)
type position = private
  { pos : Lexing.position; (** Position inside the source file. *)
    index : int            (** Index inside the string. *)
  }

val start_position : location -> position
(** [start_position loc] returns the starting position of the given location. *)

val advance : string -> position -> position
(** [advance s pos] advances position [pos] by consuming the string [s]. *)

val previous : position -> position option
(** [previous pos] returns the position one character before [pos] in the
    current line, if any. *)

(** {1 Located strings} *)

type t = string loc
(** Type of located strings. *)

val lookup : position -> t -> char
(** [lookup pos string] returns the character at position [pos].

    @raise Invalid_argument If [pos] is not a valid position in [string].
 *)

val find : from:position -> char -> t -> position option
(** [find ~from c string] finds the position of the first occurence of [c] after
    [from] in [s], if any. *)

val find_f : from:position -> (char -> bool) -> t -> position option
(** [find_f ~from f string] finds the position of the first character [c]
    that satisfies [f] after [from] in [s], if any. *)

val substring : ?from:position -> ?until:position -> t -> t
(** [substring ?from ?until string] returns the located substring between [from]
    and [until].

    If [from] is unspecified, defaults to the start of the string. If [until] is
    unspecified, defaults to the end of the string. *)
